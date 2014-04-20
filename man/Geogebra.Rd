\name{GeogebraR-package}
\alias{GeogebraR-package}
\alias{GeogebraR}
\docType{package}
\title{Link between Geogebra and R using OpenCPU}
\description{
 Examples on how to link Geogebra and R using the OpenCPU package.
}
%\usage{
%load(file, envir = parent.frame())
%}
%\arguments{
%  \item{file}{a connection or a character string giving the
%    name of the file to load.}
%  \item{envir}{the environment where the data should be
%    loaded.}
%}
%\seealso{
%  \code{\link{save}}.
%}
\details{
This package contains several examples to show how Geogebra and R can interact
efficiently through OpenCPU. Geogebra is a software for interactive
geometry that is used worldwide.
}
\examples{

\dontrun{
require(opencpu)
#Simple example
opencpu$browse('/library/GeogebraR/www/simpts.html')
#Central Limit Theorem
opencpu$browse('/library/GeogebraR/www/CLT.html')
}

}
\references{

\href{Geogebra}{http://www.geogebra.org/}

\href{Geogebra and Javascript}{http://wiki.geogebra.org/en/Reference:JavaScript}
}
\keyword{interface}
