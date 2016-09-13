library (igraph)
library (Matrix)
library (RSpectra)

"%+%" <- function(...){
    paste0(...,sep="")
}

### EVOLUTION OF EGO-CENTRED COMMUNITY STRUCTURE
###              using TEMPORAL PERSONALISED PAGERANK

biz.scale <- function (values, eeps = 10**-20) {
    ## Yet another logarithmic scale
    values [values < eeps] = eeps
    max(abs(log(values))) -> mm
    scaled.values <- log(values)/mm + 1
    scaled.values[is.na(scaled.values)] = 0
    scaled.values
}


temporal.pagerank <- function (td.network, ego.centers, globality = 0.15) {
    ## Takes a big matrix 'td.network' in the form
    ##          nodeA nodeB     time     density slice
    ##   1
    ##   2
    ##
    ## Gives a matrix of PageRank vectors (as columns)
    ## calculated for each timeslice
    ##
    ## alpha controls globality
    ##       small values --> local
    ##       big values --> global
    results <- NULL
    length(unique(td.network[,5])) -> number.of.slices

    for (sliceid in 1:number.of.slices) {
        g <- slice (td.network, sliceid)
        
        M = as_adjacency_matrix (g
          , type = 'both'
          , sparse = TRUE
          , attr = 'weight')
       
        result <- pagerank (M, ego.centers, alpha = globality)

        ## denormalize 
        result <- result *  sum (M)
        
        ## desparsify
        result <- as.numeric (result)
        results <- cbind (results, result)

        ## debug
        print (sliceid)
    }

    ## renormalise
    results <- results / max(results)

    results <- abs(results)

    rownames(results) <- rownames(M)
    
    return (results)
}


visualise <- function (ranks, X, ...) {
    ## Visualise PageRank eigenvectors, uses X as timestamps of slices
    ##  and ... as additional parameters
    ##
    ##       slice1 slice2 ...
    ##   1
    ##   2
    ##   3
    ##
    ## TODO: indicate true times...

    xstep <- X[2] - X[1]
    dim(ranks)[1] -> n.nodes

    plot(c(min(X) - xstep/2, max(X) + xstep/2), c(0, n.nodes),
         type = "n", ...)

    for (i in 1:length(X)) {
        rank <- ranks[,i]
        rect (xleft = X[i] - xstep/2,
              ybottom = length(rank):1,
              xright = X[i] + xstep/2,
              ytop = (length(rank):1) + 1,
              border = NA,
##              lwd = 0.1, ## line width of border
              col = rgb ( colorRamp (c('darkblue','yellow','red')) (rank),  maxColorValue = 255)
              )
    }

}

slice <- function (td.network, sliceid) {
    ## take the result of time.density
    ##       (nodeA, nodeB, time, density, slice)
    ## and returns a graph that corresponds to the given slice
    ##
    links.slice <- td.network [ which (td.network[,5] == sliceid), ]
    linklist <- as.matrix(links.slice[,c(1,2)])
    ## just a hack, i'm to lazy to renumerate nodes...
    linklist <- apply (linklist, 2, as.character)
    g <- graph_from_edgelist (linklist,
                                  directed = FALSE)
    
    E(g)$weight <- links.slice[,4] 

    return (g)
}

time.density <- function (links, bandwidth = 1024 * 2, n = 512) {
    ## From discrete to continous and back again
    ## to the discrete but more smooth link flow
    ##
    ## links --- list of (timestamp, nodeA, nodeB)
    ##           supposed that nodeA <= nodeB
    ##           (or nodesA and nodesB are two sets with intersection 0)
    ##
    ## bandwith --- a parameter. small bandwith means detailed
    ##                           large bandwith means global view
    ##
    ## n --- the number of equally spaced points at which the density is
    ##       to be estimated.
    ##
    ## Returns a big matrix in the form
    ##          nodeA nodeB     time     density slice
    ##     [1,]  1560  1570 31220.00 0.002282170     1
    ##     [2,]  1560  1570 31448.77 0.002292691     2
    ##
    ## where slice is the identificator of the new timeslice
    ##
    ## the integral of the density of a given link
    ## over all time gives the number of links. (not one!!!)
    ##
    ## TODO: accelerate
    names (links) <- c('timestamp', 'nodeA', 'nodeB')
    t.left <- min(links$timestamp)
    t.right <- max(links$timestamp)
    
    ulinks <- unique(links[,2:3])
    nb <- dim (ulinks)[1]

    X <- seq(t.left, t.right, (t.right-t.left) / (n-1))

    res <- NULL
    for (i in 1:nb) {
        ulink <- ulinks[i,]
        chunk.links <- links [which (links$nodeA == ulink$nodeA &
                                         links$nodeB == ulink$nodeB),]

        mass <- length(chunk.links$timestamp)
        
        density (chunk.links$timestamp, bw = bandwidth, n = n,
                 from = t.left,
                 to = t.right) -> dens

        Y <- dens$y * mass

        data.frame(nodeA = ulink$nodeA, nodeB = ulink$nodeB,
              time = X, density = Y, slice = 1:length(X)) -> partial

        res <- rbind(res, partial)
        print (i %+% ' / ' %+% nb)
    }

    return (res)
}

pagerank <- function (M, ego.centers, alpha) {
    ## big alpha -- global
    ## small alpha -- local
    ## INITIALISATION
    X0 <- as.matrix(M[1, ])
    X0[,] <- 0
    X0[ego.centers, ] <- 1/length(ego.centers)

    ONE <- as.matrix(M[1, ])
    ONE[,] <- 1

    ## add self loops to isolated nodes
    ## WHY ?
    for (i in which(rowSums(M) == 0)) {
        M[i,i] <- 1
    }

    ## Mean Matrix
    1./rowSums(M) -> hz
    Dia = Diagonal (n = dim(M)[1],  hz)
    Mcursive <- t(Dia %*% M)
   
    Mchapo <- alpha * Mcursive + (1-alpha) * X0 %*% t(ONE)

    ## FIXME: Mchapo is dgCMatrix
    ##        But it seems that eigs over dgRMatrix
    ##        is faster than eigs over dgCMatrix
    ##
    ##        also we should not forget that eigs gives us 
    ##        only aproximative eigenvector/values
    ##
    ev <- eigs(Mchapo,1)$vectors[,1]

    ## return normalized ||x||_1 = 1 eigenvector
    return (ev / sum(ev))
}


