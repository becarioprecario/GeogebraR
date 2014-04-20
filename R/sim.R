myrnorm<-function(seed=NULL, ...){
	if(!is.null(seed)){set.seed(seed)}
	return(rnorm(...))
}

#Sample coordinates for points
simpts<-function(n=1, seed=NULL)
{
	if(!is.null(seed)){set.seed(seed)}
	xx<-cbind(rnorm(n), rnorm(n))
	return(xx)
}

