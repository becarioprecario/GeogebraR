#Take distribution and paramateres to sample from
#NOTE: n is not used at the moment
CLT<-function(distr, breaks, params, nsims){

	params<-as.numeric(params)
	nsims<-as.numeric(nsims)

	xx<-switch(distr,
		rnorm = rnorm(nsims, mean=params[1], sd=params[2]),
		rexp = rexp(nsims, rate=1/params[1]),
		runif = runif(nsims, min=params[1], max=params[2])
	)	

	xxhist<-hist(xx, breaks=breaks, freq=FALSE, plot=FALSE)


	#Compute density distribution of points
	yydens<-switch(distr,
		rnorm = dnorm(xxdens<-seq(params[1]-params[2]*3, params[1]+params[2]*3, length.out=100), mean=params[1], sd=params[2]),
		rexp = dexp(xxdens<-seq(0, 20, length.out=100), rate=1/params[1]),
		runif = dunif(xxdens<-seq(params[1], params[2], length.out=100), min=params[1], max=params[2])
	)


	#Compute theoretical mean and st. dev. from params
	tparams<-switch(distr,
		rnorm = c(params[1], params[2]/sqrt(nsims)),
		rexp = c(params[1], params[2]/sqrt(nsims) ),
		runif = c(sum(params)/2, ((diff(params)^2)/12)/nsims)
	)

	#Points to evaluate the Normal distribution (of the average)
	xxmean<-seq(tparams[1]-tparams[2]*3, tparams[1]+tparams[2]*3, length.out=100) 
	yymean<-dnorm(xxmean, mean=tparams[1], sd=tparams[2])

	res<-list(xx=xx, breaks=xxhist$breaks, density=xxhist$density,
		xxdens=xxdens, yydens=yydens, xxmean=xxmean, yymean=yymean)

	return(res)
}
