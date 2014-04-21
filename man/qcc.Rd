\name{qccexample}
\alias{qccsample}
\alias{charlimits}

\title{Example on Quality Control Charts}

\description{
 These functions are used in the example included in qcc.html on
Quality Control Charts.
}

\usage{
 qccsample(n=20, mean=0, sd=1)
 charlimits(qccpts=NULL)
}

\arguments{
\item{n}{Number of points to sample.}
\item{mean}{Mean of the Gaussian distribution used to sample points.}
\item{sd}{Standard deviation of the Gaussian distribution used to sample points.}
\item{qccpts}{Vector with the values used to compute the control chart on the man.}
}

\details{
'qccsample' simply samples a few points from a Gaussian distribution.

'charlimits' computes the chart limits on the mean using function 'qcc'
from package 'qcc', dividing the data in groups of 4.
}

\examples{
require(opencpu)
opencpu$browse('/library/GeogebraR/www/qcc.html')
}

\keyword{interface}

