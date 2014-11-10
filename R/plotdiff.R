
# Code used by test files

plotcompare <- function(label) {
    # TODO: Some sort of test to tell whether ImageMagick's 'compare'
    #       is available
    diffname <- paste0(label, "-diff.png")
    if (file.exists(diffname))
        stop("This comparison already exists")
    result <- system2("compare",
                      c("-metric AE",
                        paste0(label,
                               c("-graphics.png", "-grid.png", "-diff.png"))),
                      stdout=TRUE, stderr=TRUE)
    if (result == "0") {
        file.remove(diffname)
    }
    result
}

fungen <- function() {
    
    diffs <- NULL

    pdInit <- function() {
        diffs <<- NULL
    }
    
    # Generate PDF because that is where 'gridGraphics' will mimic best
    # Convert to PNG for compare because that will provide a little bit
    # of tolerance for infinitessimal differences (?)
    pd <- function(expr, label, dev="pdf",
                   antialias=TRUE, density=100, width=7, height=7) {
        suffix <- switch(dev, pdf=".pdf", png=".png",
                         stop("I do not like your choice of device"))
        switch(dev,
               pdf=pdf(paste0(label, "-graphics", suffix),
                       width=width, height=height, compress=FALSE),
               png=png(paste0(label, "-graphics", suffix),
                       width=width*100, height=height*100))
        dev.control("enable")
        eval(expr)
        dl <- recordPlot()
        dev.off()
        switch(dev,
               pdf=pdf(paste0(label, "-grid.pdf"),
                       width=width, height=height, compress=FALSE),
               png=png(paste0(label, "-grid.png"),
                       width=width*100, height=height*100))
        grid.echo(dl)
        dev.off()
        if (dev != "png") {
            options <- paste0("-density ", density, "x", density)
            # 'antialias' must be off to get reliable comparison of
            # images that include adjacent polygon fills
            if (!antialias)
                options <- c(options, "+antialias")
            system2("convert",
                    c(options,
                      paste0(label, c("-graphics.pdf", "-%d-graphics.png"))))
            system2("convert",
                    c(options,
                      paste0(label, c("-grid.pdf", "-%d-grid.png"))))
        }
        # Check for multiple-page PDF
        # If found, only compare the last page
        pngFiles <- list.files(pattern=paste0(label, "-[0-9]+-graphics.png"))
        numPNG <- length(pngFiles)
        if (numPNG > 1) {
            file.rename(pngFiles[numPNG], pngFiles[1])
        }
        pngLabel <- paste0(label, "-0")
        result <- plotcompare(pngLabel)
        if (result != "0") {
            diffs <<- c(diffs,
                        paste0(result,
                               if (result == 1) " difference "
                               else " differences ",
                               "detected (", pngLabel, "-diff.png)"))
        }
    }

    pdresult <- function(warn=FALSE) {
        if (length(diffs)) {
            cat(diffs, sep="\n")
            if (warn) {
                warning("Differences detected")
            } else {
                stop("Differences detected")
            }
        }
    }

    list(pdInit=pdInit, pd=pd, pdresult=pdresult)
}

funs <- fungen()

plotdiffInit <- funs$pdInit
plotdiff <- funs$pd
plotdiffResult <- funs$pdresult


