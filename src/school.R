source('evolution.R')

school <- read.table('../data/Primary school temporal network data/primaryschool.csv')
names(school) <- c('timestamp', 'nodeA', 'nodeB', 'classA', 'classB')

school.meta <- read.table('../data/Primary school temporal network data/metadata_primaryschool.txt')
names(school.meta) <- c('node', 'class', 'sex')

links <- school[,c(1,2,3)]

number.of.slices <- 100
number.of.nodes <- length ( unique (c(links[,2],links[,3])) )

td.network <- time.density (links, bandwidth = 600, n = number.of.slices)
timestamps <- sort(unique(td.network[,3]))

##ego.centers <- c('1500')
##ego.centers <- c('1521') ## teacher
##ego.centers <- c('1500', '1521')
ego.centers <- c('1500')
school.meta[school.meta$node==1500,2] -> her.class

results <- temporal.pagerank (td.network, ego.centers, globality = 0.2)

results.old <- results
## logify-magnify!
results <- biz.scale (results)

###33333333333333333333333333333333333333333333333333333333333333333##################
#### Order experiments start
###33333333333333333333333333333333333333333333333333333333333333333##################

### Order by values at 5th timestamp
## order (results[,5], decreasing = TRUE ) -> ord

### To order by rowsum
##   order (rowSums(results), decreasing=TRUE) -> ord

### By rowmax
##   order (apply(results,1,max), decreasing=TRUE) -> ord

### Or by something other
##   order (max.col(results), decreasing=TRUE) -> ord

### Sequential comparison..
### Order by min |f-g|
## comp <- function (x,y) min(abs(x - y))

### Order by min Σ|f-g|
## comp <- function (x,y) sum(1./abs(x - y))

### Order by correlation
## comp <- function (x,y) -abs(cor(x,y))
## ord <- 1:dim(results)[1]
## ord[1] <- 123
## ord[123] <- 1

## first line is 1 current equals 2
## for (current in 2 : length(ord) ) {
##    index.of.min <- current
##    current.min <- comp (results[ord[index.of.min], ],
##                            results[ord[current-1], ])
##    
##    for (index in current : length(ord)){
##
##        new.min <- comp(results[ord[index], ],
##                        results[ord[current-1], ])
##        if ( new.min < current.min ) {
##            index.of.min <- index
##            current.min <- new.min
##        }
##    }
##    
##    ## when we found a min
##    print (ord[index.of.min])
##    temp <- ord[current]
##    ord[current] <- ord[index.of.min]
##    ord[index.of.min] <- temp
##}

### One visualisation
## results <- results[ord,]
## isualise ( biz.scale (sresults),  timestamps,
##           yaxt = 'n', xlab = 'time',
##           ylab = '')


###33333333333333333333333333333333333333333333333333333333333333333##################
### / Order experiments stop
###33333333333333333333333333333333333333333333333333333333333333333##################


vis.macro <- function() {
    ## ATTENTION: a lot of global parameters

    sresults <- results[ord,]

    ## visualise results
    visualise (sresults,  timestamps,
               yaxt = 'n', xlab = 'time',
               ylab = ''
               )

    ## sort the metadata
    school.meta[match (names(sresults[,1]), school.meta$node),] ->
        school.meta.ord

    ## first nodes are in top

    ## Clases

    colors = c('yellow','blue','orange','violet','green','black',
                'red','gray','brown','magenta','cyan')
    classes = c("1A","1B","2A","2B","3A","3B",
                "4A","4B","5A","5B","Teachers")
    for (i in 1:length(colors)) {

        cl = classes[i]
        color = colors[i]
        number.of.nodes + 1 -
            which(school.meta.ord$class == cl) + 0.5  ->
                class.positions
        
        axis(2, at=class.positions, col=color,
             col.axis = 'white',
              lwd = 2
             ,labels = NULL#rep(cl,length(class.positions))
             )
        
    }

    ## Her class
    ## number.of.nodes + 1 -
    ##     which(school.meta.ord$class == her.class) + 0.5  ->
    ##         her.class.positions

    ## school.meta.ord$class -> labels.left
    ## axis(2, at=her.class.positions, col='black'
    ##     ,labels = rep(her.class,length(her.class.positions)))

    ## sex
    school.meta.ord$sex -> labels.right
    axis(4, at=(number.of.nodes:1) + 0.5, labels=labels.right)
}

### A sequence of visualisations
png("../figs/school%03d.png", width = 1200, height = 800)

for (i in 1:number.of.slices) {
    ## sort results
    order (results[,i], decreasing = TRUE ) -> ord

    vis.macro()

    ## timestamp that sorts
    abline(v=timestamps[i])
}
dev.off()


## to create a video run the following command
## ffmpeg -r 7 -i school%03d.png -vb 200M out.webm


png("../figs/school-row-sums.png", width = 1200, height = 800)
order (rowSums(results), decreasing=TRUE) -> ord
vis.macro()
dev.off()


png("../figs/school-strange-max.png", width = 1200, height = 800)
order (max.col(results), decreasing=TRUE) -> ord
vis.macro()
dev.off()


## 0 Preparation for line-student ordering

## Standart distances include euclidean, maximum, manhattan, canberra,
##          binary and minkowski

## More Distance and Similarity Measures could be found in the next library
library(proxy)

summary(pr_DB)
## * Similarity measures:
## Braun-Blanquet, Chi-squared, correlation, cosine, Cramer, Dice, eDice,
## eJaccard, Fager, Faith, Gower, Hamman, Jaccard, Kulczynski1,
## Kulczynski2, Michael, Mountford, Mozley, Ochiai, Pearson, Phi,
## Phi-squared, Russel, simple matching, Simpson, Stiles, Tanimoto,
## Tschuprow, Yule, Yule2

## * Distance measures:
## Bhjattacharyya, Bray, Canberra, Chord, divergence, Euclidean, fJaccard,
## Geodesic, Hellinger, Kullback, Levenshtein, Mahalanobis, Manhattan,
## Minkowski, Podani, Soergel, supremum, Wave, Whittaker


distances = dist (results, method = "euclidean")
## resonablement good: Bhjattacharyya, divergence, Euclidean, Mahattan, Soergel, supremum,
##                     eJaccard?, Kulczynski2?,
##
## not tested, because takes more time, computationnaly ineffective ?
##             Podani, Chi-squared, Cramer, Tschuprow,
##
## ATTEBSION! certains distances/similarity measures give errors, they need to be RETESTED

## 1 dimensional MDS as order for lines-students
order(cmdscale(distances, k=2)[,1]) -> ord
png("../figs/school-mds.png", width = 1200, height = 800)
vis.macro()
dev.off()
## not so good, no so lisse in local sens.

## 2 Pagerank
order(pagerank(as.matrix(distances), ego.centers,0.2)) -> ord
png("../figs/school-pagerank.png", width = 1200, height = 800)
vis.macro()
dev.off()
## not so good, no so lisse in local sens.
### Pagerank and Mds looks the same :)


## Greedy add the best (in _di_stance sense) lines add on top/bottom of current position
## TODO: move this code to 'evolution.R'
allnames <- names(results[,1])
ord <- c('1500')
##ord <- sample(allnames,1) ## random
restnames <- setdiff (allnames, ord)
di <- function (a, b) {
    return (sum( (a-b)**2))
}
for (i in 1:length(restnames)) {
    di.first = 1000000000
    di.last = 1000000000
    good.for.first = -1
    good.for.last = -1
    for (n in restnames) {
        first <- ord[1]
        last <- ord[length(ord)]
        if ( di( results[first,], results[n, ]) < di.first ) {
            di.first = di( results[first,], results[n, ])
            good.for.first = n
        }
        if ( di( results[last,], results[n, ]) < di.last ) {
            di.last = di( results[last,], results[n, ])
            good.for.last = n
        }
    }
    if  (di.first <= di.last) {
        ## add before
        ord <- c(good.for.first, ord)
        restnames <- setdiff(restnames, good.for.first)
    } else {
        ## add after
        ord <- c(ord, good.for.last)
        restnames <- setdiff(restnames, good.for.last)
    }
}
vis.macro()

png("../figs/school-greedy-top-add.png", width = 1200, height = 800)
vis.macro()
dev.off()
## TROP BIEN, need to normalize before distance application!!11
