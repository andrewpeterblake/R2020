priors <- function(nms, bs, pdf_p) {

  # Evaluates the logprior for a vector of coefficients
  # Each coefficient can have a different prior density 
  type   <- pdf_p$`PDF type`
  mu     <- pdf_p$mu
  sigma  <- pdf_p$sigma
  low    <- bs$`Lower bound`
  upp    <- bs$`Upper bound`

  n <- 1000
  
  all <- NULL
  for (i in 1:length(type)) {
    dom <- seq(low[i], upp[i], (upp[i]-low[i])/n)
    dom <- dom[-1]
    if (type[i] == 1) { 
      a   <- mu[i]
      b   <- sigma[i]
      ran <- dnorm(dom,a,b)
      }
    if (type[i] == 2) {
      a   <- mu[i] - 0.5*sqrt(12*sigma[i]^2)
      b   <- mu[i] + 0.5*sqrt(12*sigma[i]^2)
      ran <- dunif(dom,a,b)
      }
    if (type[i] == 3) {
      a   <- (1-mu[i])*(mu[i]^2)/(sigma[i]^2) - mu[i]
      b   <- (1-mu[i])^2*mu[i]/(sigma[i]^2) - (1-mu[i])
      ran <- dbeta(dom,a,b)
      }
    if (type[i] == 4) {
      a   <- mu[i]^2/(sigma[i]^2)
      b   <- mu[i]/(sigma[i]^2)
      ran <- dgamma(dom,a,b)
      }
    if (type[i] == 5) {
      a   <- mu[i]^2/sigma[i]^2 + 2
      b   <- mu[i]*(mu[i]^2/sigma[i]^2 + 1)
      ran <- dinvgamma(dom,a,b)
    }
    all <- bind_rows(all, data.frame(Var = nms[i], d = dom, v = ran, 
                                     stringsAsFactors = FALSE))
  } 
  return(all)
}