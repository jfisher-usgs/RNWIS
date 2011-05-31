\name{RNWIS-package}

\alias{RNWIS-package}

\docType{package}

\title{
National Water Information System: R Interface
}

\description{
This package provides access to water-resources data.
}

\details{
  \tabular{ll}{
    Package: \tab RNWIS\cr
    Type: \tab Package\cr
    Version: \tab 0.1-0\cr
    Date: \tab 2011-05-26\cr
    License: \tab GPL (>= 2)
  }
}

\note{
The \pkg{RNWIS} \acronym{GUI} requires \R operate as an \acronym{SDI}
application, using multiple top-level windows for the console, graphics, and
pager.

The set of standards used for coding \pkg{RNWIS} is documented in
\href{http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html}{Google's R Style Guide}.
}

\author{
Jason C. Fisher

Maintainer: <\email{jfisher@usgs.gov}>
}

\examples{
\donttest{library(RNWIS)
OpenRNWIS()
}
}

\keyword{package}

