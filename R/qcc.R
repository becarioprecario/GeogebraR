# Functions and data for the example on quality control charts

# Sample observations
qccsample <- function(n = 20, mean = 0, sd = 1)
{
  qccpts <- rnorm(n = n, mean = mean, sd = sd)

  return(qccpts)
}


charlimits <- function(qccpts = NULL)
{
  require(qcc)

  if(!is.null(qccpts))
  {
    cchart <- qcc(matrix(qccpts, ncol = 4), type = "xbar", plot = FALSE)
    return(list(lowlim = cchart$limits[1], uplim = cchart$limits[2]))
  }
  else
  {
    return(list(lowlim = NA, uplim = NA))
  }
}
