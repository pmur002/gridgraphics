\name{grid.echo}
\alias{grid.echo}
\alias{echoGrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Echo \pkg{graphics} output using \pkg{grid} graphics
}
\description{
  Convert a scene that was drawn using the \pkg{graphics} package
  to an identical scene drawn with the \pkg{grid} package.
}
\usage{
grid.echo(x = NULL, newpage = TRUE, prefix = NULL, device = offscreen)

echoGrob(x = NULL, prefix = NULL, device = offscreen, name = NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Either \code{NULL}, a recorded plot, or a function.
    If \code{NULL}, the current graphics device is echoed.
  }
  \item{newpage}{
    A logical value indicating whether to start a new page.  If
    \code{FALSE}, echoing occurs in the current \pkg{grid} viewport.
  }
  \item{prefix}{
    A character value used as a prefix for all grob and viewport names.
    The default prefix is \code{"graphics"}.
  }
  \item{device}{
    A function that opens a graphics device for \code{grid.echo()} to
    work on.  By default this is an off-screen, in-memory device
    based on the \code{pdf} device.  This default device may not be
    satisfactory when using custom fonts.
  }
  \item{name}{
A character identifier.
  }
}
\details{
  If the first argument is a function, it must be a function with zero
  arguments.  If the function needs access to non-global data,
  use a closure.
  The function should not call functions that create or destroy graphics
  devices, or change the current graphics device.
}
\value{
  The \code{echoGrob} function returns a grob that will echo \code{x}
  when it is drawn.

  The \code{grid.echo} function is called for its side-effect of drawing on the
  current graphics device.
}
\author{
Paul Murrell
}
\examples{
\dontrun{
# Echo existing drawing
plot(1)
grid.echo()

# Echo result of call to a plotting function
plotfun <- function() plot(1:10)
grid.echo(plotfun)

# Echo result of a plotting function (anonymous) into current viewport
grid.newpage()
pushViewport(viewport(x=0, width=.5, just="left"))
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.echo(function() plot(1:10), newpage=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
