\name{CLT}
\alias{CLT}

\title{Central Limit Theorem example}

\description{
 This function simulates data and computes histogram, density and 
(asymptotic) density of the mean 
}

\usage{
 CLT(distr, breaks, params, nsims)
}

\arguments{
\item{distr}{Distribution to sample from.}
\item{breaks}{Sames as the 'breaks' parameter in 'hist()'.}
\item{params}{Vector of two elements with the parameters to use with 'distr'.}
\item{nsims}{Number of simulations.}
}

\details{
In this example, the user can choose among three different distributions to
sample from. In all cases, the simulated points are display, together a
histogram, the density distribution (in green) and the asymptotic distribution
of the mean of 'nsims' observations (in red).
}

\examples{
require(opencpu)
opencpu$browse('/library/GeogebraR/www/CLT.html')
}



\keyword{interface}

