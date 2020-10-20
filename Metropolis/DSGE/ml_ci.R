ml_ci <- function(intv, bs) {

  # Evaluates the normal ci for a vector of coefficients
  low <- bs$`Lower bound`
  upp <- bs$`Upper bound`
  n <- 100
  all <- NULL
  for (i in 1:nrow(intv)) {
    dom <- seq(low[i], upp[i], (upp[i]-low[i])/n)
    dom <- dom[-1]
    ran <- dnorm(dom,intv[i,2],intv[i,3])
    all <- bind_rows(all, data.frame(Var = intv[i,1], d = dom, v = ran))
  } 
  return(all)
}