minus_likelihood <- function(theta,y,bounds) {
  # Calls (log)likelihood function
  # See also: LIKELIHOOD

  # Check if parameters outside bounds
  if (sum(theta<=bounds[,1] | theta>=bounds[,2]) > 0) { 
    val <- 10000
    }
  else {
    val <- -likelihood(theta,y)
    }
  return(val) 
}