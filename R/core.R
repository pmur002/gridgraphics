
.gridGraphicsEnv <- new.env()

init <- function(dl) {
    if (dev.cur() == 1) {
        dev.new()
    }
    # The graphics device we will draw onto
    assign("pd", dev.cur(), .gridGraphicsEnv)
    din <- par("din")
    # An off-screen graphics device 
    pdf(NULL, width=din[1], height=din[2])
    assign("rd", dev.cur(), .gridGraphicsEnv)
    # NULL out the saved display list then replay it in order
    # to restore basic graphics settings (like device background)
    dlStub <- dl
    dlStub[1] <- list(NULL)
    replayPlot(dlStub)
    # Go back to device we will draw onto
    dev.set(playDev())
    initPlotIndex()
    initWindowIndex()
    initClip()
}

shutdown <- function() {
    # Close the off-screen graphics device
    dev.set(recordDev())
    invisible(dev.off())
}

initClip <- function() {
    assign("currentClip", NULL, .gridGraphicsEnv)
}

setClip <- function(x, y, w, h) {
    assign("currentClip", c(x, y, w, h), .gridGraphicsEnv)
}

getClip <- function() {
    get("currentClip", .gridGraphicsEnv)
}

playDev <- function() {
    get("pd", .gridGraphicsEnv)
}
    
recordDev <- function() {
    get("rd", .gridGraphicsEnv)
}

indexFuns <- function() {
    index <- 0
    init <- function() {
        index <<- 0
    }
    increment <- function() {
        index <<- index + 1
    }
    get <- function() {
        index
    }
    list(init=init, increment=increment, get=get)
}

pif <- indexFuns()
initPlotIndex <- pif$init
incrementPlotIndex <- pif$increment
plotIndex <- pif$get

wif <- indexFuns()
initWindowIndex <- wif$init
incrementWindowIndex <- wif$increment
windowIndex <- wif$get

prefixFuns <- function() {
    prefix <- "graphics"
    get <- function() {
        prefix
    }
    set <- function(x) {
        prefix <<- as.character(x)
    }
    list(get=get, set=set)
}
pf <- prefixFuns()

prefix <- pf$get
setPrefix <- pf$set

grobname <- function(label, unique=FALSE) {
    if (unique) {
        paste(prefix(), label, sep="-")
    } else {
        indexName <- paste0(label, "Index")
        if (exists(indexName, .gridGraphicsEnv)) {
            index <- get(indexName, .gridGraphicsEnv)
        } else {
            index <- 1
        }
        assign(indexName, index + 1, .gridGraphicsEnv)
        paste(prefix(), "plot", plotIndex(), label, index, sep="-")
    }
}

vpname <- function(type, clip=FALSE) {
    switch(type,
           root=,
           inner=paste(prefix(), type, sep="-"),
           figure=,
           plot={
               if (clip) {
                   paste(prefix(), type, plotIndex(), "clip", sep="-")
               } else {
                   paste(prefix(), type, plotIndex(), sep="-")
               }
           },
           window={
               if (clip) {
                   paste(prefix(), type, plotIndex(), windowIndex(),
                         "clip", sep="-")
               } else {
                   paste(prefix(), type, plotIndex(), windowIndex(), sep="-")
               }
           })
}
