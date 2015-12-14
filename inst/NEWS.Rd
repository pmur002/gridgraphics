\name{NEWS}
\title{NEWS file for the gridGraphics package}
\encoding{UTF-8}

\section{Changes in version 0.1-5}{
  \itemize{
    \item Changed stop() to warning() when call grid.echo() with
    empty \dQuote{recordedplot} (suggested by Jonathon Godfrey).
    \item Fixed examples for \code{grid.echo()}
    (patch by Josh O'Brien).
    \item More robust Windows support for \code{plotdiff()} and
    \code{plotcompare()} (patch by Josh O'Brien).
    \item Added defence against \code{axis(line=)} specification
    of length greater than 1 (reported by Jimmy Oh).
    \item Added explicit handling of \code{C_layout()},
    though handling does nothing (reported by Jimmy Oh).
    \item Fixed drawing of ticks for \code{axis(at=)} when
    \code{at} exceeds range of axis scale.  The ticks outside
    the axis scale range are now correctly clipped
    (reported by Jimmy Oh).
    \item Future-proofed test script for \code{hist.POSIXt()}
    (so that the addition of new leap seconds does not generate errors
    about \code{breaks} not spanning the range of the data).
  }
}
\section{Changes in version 0.1-4}{
  \itemize{
    \item Added defence against \code{NA} text values in call to
    \code{title()}.
  }
}
\section{Changes in version 0.1-3}{
  \itemize{
    \item Added defence against code sending in NULL values in \code{par()}
    settings (e.g., \pkg{sp} does this with the \code{xpd} setting).
    These \code{par()} settings are ignored (to match internal
    \pkg{graphics} C code behaviour).
  }
}
\section{Changes in version 0.1-2}{
  \itemize{
    \item Fixed documentation of first argument in \code{grid.echo()}
    (it can be a function, not an expression).
    
    \item Added better checking in plotdiff() for existence of
    \code{convert} and \code{compare} tools and for recent-enough
    version of R (to be able to perform the strict check for
    differences).
  }
}
\section{Changes in version 0.1-1}{
  \itemize{
    \item First public release of \pkg{gridGraphics} package.
  }
}
