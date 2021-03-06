# Return boundaries of the municipalities in Albacete

plotmap <- function(map = "albacete"){

  require(sp)
  require(maptools)

  data(map, envir = environment())#, package="GeogebraR")

  if(map == "albacete"){xxmap <- albacete}
  if(map == "granada"){xxmap <- granada}
  if(map == "jaen"){xxmap <- jaen}

  # Get outer boundary
  xxmap <- unionSpatialPolygons(xxmap, rep("1", length(xxmap)))

  #bb<-bbox(xxmap)[,1]#Minimum coordinates
  bb <- c(10.20520, 84.17414)#Offset para colocar Albacete en el origen
  rescal <- 50000#Re-scale factor

  polys <- lapply(xxmap@polygons, function(X) {
    xx <- X@Polygons[[1]]@coords / rescal#Coords. are re-scaled
    xx <- xx - matrix(bb, nrow = nrow(xx), ncol = 2, byrow = TRUE)
    return(xx)
  })

  # Take one in 5 points...
  polys[[1]] <- polys[[1]][seq(1, nrow(polys[[1]]), by = 5), ]

  return(polys)
}
