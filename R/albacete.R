#Return boundaries of the municipalities in Albacete

plotmap<-function(map="albacete"){

	require(sp)

	data(map, envir=environment())#, package="GeogebraR")

	if(map=="albacete"){xxmap<-albacete}
	if(map=="granada"){xxmap<-granada}
	if(map=="jaen"){xxmap<-jaen}

	#bb<-bbox(xxmap)[,1]#Minimum coordinates
	bb<-c(10.20520, 84.17414)#Offset para colocar Albacete en el origen
	rescal<-50000#Re-scale factor

	polys<-lapply(xxmap@polygons, function(X){
		xx<-X@Polygons[[1]]@coords/rescal#Coords. are re-scaled
		xx<-xx-matrix(bb/rescal, nrow=nrow(xx), ncol=2, byrow=TRUE)
		return(xx)
	})

	return(polys)
}
