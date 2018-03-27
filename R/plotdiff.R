
# Code used by test files

plotcompare <- function(graphicsPNG, gridPNG, label) {
    diffFile <- paste0(label, "-diff.png")
    diff <- magick::image_compare(graphicsPNG, gridPNG, metric="AE")
    nDiff <- attr(diff, "distortion")
    if (nDiff > 0) {
        magick::image_write(diff, diffFile)
    }
    nDiff
}

fungen <- function() {

    diffs <- NULL
    version <- getRversion()
    haveRecentR <- version >= "3.2.0"
    haveWarned <- FALSE

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
        graphicsFile <- paste0(label, "-graphics", suffix)
        gridFile <- paste0(label, "-grid", suffix)
        curDev <- dev.cur()
        switch(dev,
               pdf=pdf(graphicsFile,
                       width=width, height=height, compress=FALSE),
               png=png(graphicsFile,
                       width=width*100, height=height*100))
        graphicsDev <- dev.cur()
        dev.control("enable")
        tryCatch(
        {
            eval(expr)
            dl <- recordPlot()
        },
        ## Try to clean up if we error out
        finally={
            dev.set(graphicsDev)
            dev.off()
            ## Do not reset current device if there were no devices open
            if (curDev != 1) dev.set(curDev)
        })
        switch(dev,
               pdf=pdf(gridFile,
                       width=width, height=height, compress=FALSE),
               png=png(gridFile,
                       width=width*100, height=height*100))
        gridDev <- dev.cur()
        tryCatch(
        {
            grid.echo(dl)
        },
        ## Try to clean up if we error out
        finally={
            dev.set(gridDev)
            dev.off()
            ## Do not reset current device if there were no devices open
            if (curDev != 1) dev.set(curDev)
        })
        # Only convert and compare if have the tools
        if (haveRecentR) {
            if (dev == "png") {
                graphicsPNG <- magick::image_read(graphicsFile)
                gridPNG <- magick::image_read(gridFile)
            } else { ## ASSUME dev == "pdf"
                ## TODO: Need to ADD ability to turn OFF antialias
                # 'antialias' must be off to get reliable comparison of
                # images that include adjacent polygon fills
                graphicsImage <- magick::image_read_pdf(graphicsFile,
                                                        density=density)
                graphicsPNG <- magick::image_convert(graphicsImage, "png")
                gridImage <- magick::image_read_pdf(gridFile, density=density)
                gridPNG <- magick::image_convert(gridImage, "png")
            } 
            ## Check for multiple-page PDF
            ## If found, only compare the last page
            numPNG <- length(graphicsPNG)
            if (numPNG > 1) {
                warning(paste0("Only comparing final page (of ",
                               numPNG, " pages)"))
            }
            result <- plotcompare(graphicsPNG[1], gridPNG[1], label)
            if (result > 0) {
                diffs <<- c(diffs,
                            paste0(result,
                                   if (result == 1) " difference "
                                   else " differences ",
                                   "detected (", label, "-diff.png)"))
            }
        } else{
            if (!haveWarned) {
                warning("Unable to test output for differences")
                haveWarned <<- TRUE
            }
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


