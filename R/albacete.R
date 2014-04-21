#Return boundaries of the municipalities in Albacete

abmap<-function(){

	require(sp)

	data(albacete, package="GeogebraR")

	bb<-bbox(albacete)[,1]#Minimum coordinates
	rescal<-50000#Re-scale factor

	polys<-lapply(albacete@polygons, function(X){
		xx<-X@Polygons[[1]]@coords/rescal#Coords. are re-scaled
		xx<-xx-matrix(bb/rescal, nrow=nrow(xx), ncol=2, byrow=TRUE)
		return(xx)
	})

	return(polys)
}
