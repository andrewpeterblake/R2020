logprior <- function(theta,pdf_p) {

  # Evaluates the logprior for a vector of coefficients
  # Each coefficient can have a different prior density 
  out    <- 0L
  type   <- pdf_p$`PDF type`
  mu     <- pdf_p$mu
  sigma  <- pdf_p$sigma

  for (i in 1:length(theta)) {
    if (type[i] == 1) { 
      out <- out + log(dnorm(theta[i],mu[i],sigma[i])) 
      }
    if (type[i] == 2) {
      a   <- mu[i] - 0.5*sqrt(12*sigma[i]^2)
      b   <- mu[i] + 0.5*sqrt(12*sigma[i]^2)
      out <- out + log(dunif(theta[i],a,b))
      }
    if (type[i] == 3) {
      a   <- (1-mu[i])*(mu[i]^2)/(sigma[i]^2) - mu[i]
      b   <- (1-mu[i])^2*mu[i]/(sigma[i]^2) - (1-mu[i])
      out <- out + log(dbeta(theta[i],a,b))
      }
    if (type[i] == 4) {
      a   <- mu[i]^2/(sigma[i]^2)
      b   <- mu[i]/(sigma[i]^2)
      out <- out + log(dgamma(theta[i],a,b))
      }
    if (type[i] == 5) {
      a   <- mu[i]^2/sigma[i]^2 + 2
      b   <- mu[i]*(mu[i]^2/sigma[i]^2 + 1)
      out <- out + log(dgamma(1/theta[i],a,1/b))
      }
  } 
  return(out)
}