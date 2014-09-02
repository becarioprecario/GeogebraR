#Sample coordinates for points
getdata<-function(dataset)
{

	xx<-switch(dataset,
		cars = cbind(cars$speed, cars$dist),
		BOD = cbind(BOD$Time, BOD$demand),
		women = cbind(women$height, women$weight)
	)

	return(xx)
}


#Fit different curves to the data
#1.- Lowess
#2.- Smooth spline
#3.- 5th degree polynomial
fitcurves<-function(xx){

	x<-xx[,1]
	y<-xx[,2]

	fitcvs<-list(NA, NA, NA)

	#Lowess
	fit1<-lowess(x,y)
	fitcvs[[1]]<-list(x=fit1$x, y=fit1$y)

	#Smooth spline
	fit2<-smooth.spline(x,y)
	fitcvs[[2]]<-list(x=fit2$x, y=fit2$y)


	#5th degree polynomial
	fit3<-lm(y~x+I(x*x)+I(x^3)+I(x^4)+I(x^5) )
	fitcvs[[3]]<-list(x=fit2$x, y=predict(fit3, data.frame(x=fit2$x)))

	return(fitcvs)

}

