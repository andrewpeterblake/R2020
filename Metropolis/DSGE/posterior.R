posterior <- function(theta,y,bounds,pdf_p) {
  # Calls (log)likelihood function and logprior
  # See also: LIKELIHOOD, LOGPRIOR

  # Check if parameters outside bounds
  if (sum(theta<=bounds[,1] | theta>=bounds[,2]) > 0) { 
    posterior <- -9999
    }
  else { 
    posterior <- likelihood(theta,y) + logprior(theta,pdf_p)
    }
  return(posterior) 
}