#Return boundaries of the municipalities in Albacete

plotmap<-function(map="albacete"){

	require(sp)

	data(map, envir=environment())#, package="GeogebraR")

	if(map=="albacete"){xxmap<-albacete}
	if(map=="granada"){xxmap<-granada}

	bb<-bbox(xxmap)[,1]#Minimum coordinates
	rescal<-50000#Re-scale factor

	polys<-lapply(xxmap@polygons, function(X){
		xx<-X@Polygons[[1]]@coords/rescal#Coords. are re-scaled
		xx<-xx-matrix(bb/rescal, nrow=nrow(xx), ncol=2, byrow=TRUE)
		return(xx)
	})

	return(polys)
}
