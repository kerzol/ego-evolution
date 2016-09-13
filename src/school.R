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
    visualise ( biz.scale (sresults),  timestamps,
               yaxt = 'n', xlab = 'time',
               ylab = ''
               )

    ## sort the metadata
    school.meta[match (names(sresults[,i]), school.meta$node),] ->
        school.meta.ord

    ## first nodes are in top
    number.of.nodes + 1 -
        which(school.meta.ord$class == her.class) + 0.5  ->
            her.class.positions

    number.of.nodes + 1 -
        which(school.meta.ord$class == '5A') + 0.5  ->
            A5.class.positions
    
    school.meta.ord$class -> labels.left
    axis(2, at=her.class.positions, col='black'
        ,labels = rep(her.class,length(her.class.positions)))

    axis(2, at=A5.class.positions, col='grey', col.axis = 'grey'
        ,labels = rep('5A',length(A5.class.positions)))
    
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
