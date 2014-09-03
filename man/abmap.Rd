\name{plotmap}
\alias{plotmap}

\title{Example on maps}

\description{
 This function is used to return the coordinates of the polygons from
 a map in SpatialPolygons or SpatialPolygonsDataFrame formats.
}

\usage{
 plotmap(map="albacete")
}

\arguments{
\item{map}{Map to plot. Either "albacete" or "granada".}
}


\details{
'plotmap' returns a list with the coordinates of the municipalities
in the province of Albacete (Castilla-La mancha, Spain) or Granada (Andalucía, Spain).
}

\examples{
require(opencpu)
opencpu$browse('/library/GeogebraR/www/albacete.html')
}

\keyword{interface}

