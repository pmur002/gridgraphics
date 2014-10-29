
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
    pd <- function(expr, label,
                   antialias=TRUE, density=100, width=7, height=7) {
        pdf(paste0(label, "-graphics.pdf"),
            width=width, height=height, compress=FALSE)
        dev.control("enable")
        eval(expr)
        dl <- recordPlot()
        dev.off()
        options <- paste0("-density ", density, "x", density)
        # 'antialias' must be off to get reliable comparison of
        # images that include adjacent polygon fills
        if (!antialias)
            options <- c(options, "+antialias")
        system2("convert",
                c(options,
                  paste0(label, c("-graphics.pdf", "-graphics.png"))))
        pdf(paste0(label, "-grid.pdf"), 
            width=width, height=height, compress=FALSE)
        dlReplay(dl)
        dev.off()
        system2("convert",
                c(options,
                  paste0(label, c("-grid.pdf", "-grid.png"))))
        result <- plotcompare(label)
        if (result != "0") {
            diffs <<- c(diffs,
                        paste0(result,
                               if (result == 1) " difference "
                               else " differences ",
                               "detected (", label, "-diff.png)"))
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

plotdiffInit <- funs
plotdiff <- funs$pd
plotdiffResult <- funs$pdresult


