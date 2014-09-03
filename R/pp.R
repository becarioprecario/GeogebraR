samplepp<-function(ncases=10, ncontrols=30)
{
	require(splancs)
	require(sp)

	bdy<-as.points(list(x=c(0,1,1,0), y=c(0,0,1,1)))

	#Cases
	#cc<-data.frame(x=runif(ncases), y=runif(ncases))
	cc<-data.frame(x=dnorm(ncases, .5, .1), y =dnorm(ncases, .5, .1))

	#Controls
	cc<-rbind(cc, data.frame(x=runif(ncontrols), y=runif(ncontrols)))

	rownames(cc)<-1:nrow(cc)

	#Convert to SpatialPoints
	cc<-SpatialPoints(cc)

	#Bandwidth for smoothing
	bw<-.3

	grd1 <- GridTopology(cellcentre.offset=c(0, 0), cellsize=c(0.1, 0.1), 
		cells.dim=c(11, 11))
	
	kcases <- spkernel2d(cc[1:ncases,], bdy, h0=bw, grd1)

	kcontrols <- spkernel2d(cc[1:ncontrols+ncases,], bdy, h0=bw, grd1)

	kernels <- SpatialGridDataFrame(grd1, 
		data=data.frame(kratio=kcases/kcontrols))

	cl = contourLines(as.image.SpatialGridDataFrame(kernels["kratio"]))

	return(list(ncases=ncases, ncontrols=ncontrols, cc=coordinates(cc),
		ncl=length(cl), cl=cl))

}

