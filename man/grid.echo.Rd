\name{grid.echo}
\alias{grid.echo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Echo \pkg{graphics} output using \pkg{grid} graphics
}
\description{
  Convert a scene that was drawn using the \pkg{graphics} package
  to an identical scene drawn with the \pkg{grid} package.
}
\usage{
grid.echo(x = NULL, newpage = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    Either \code{NULL}, a recorded plot, or an expression (or call) to
    echo.  If \code{NULL}, the current scene is echoed.
  }
  \item{newpage}{
    A logical value indicating whether to start a new page.  If
    \code{FALSE}, echoing occurs in the current \pkg{grid} viewport.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Paul Murrell
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
# Echo existing drawing
plot(1)
grid.echo()

# Echo drawing expression
grid.echo(quote(plot(1:10)))

# Echo drawing expression into current viewport
grid.newpage()
pushViewport(viewport(x=0, width=.5, just="left"))
grid.rect(gp=gpar(col=NA, fill="grey"))
grid.echo(quote(plot(1:10)), newpage=FALSE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
