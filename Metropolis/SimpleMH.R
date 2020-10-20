library(actuar)
library(tidyverse)

pp <- data.frame(name = c("beta", "rho[1]", "kappa",  "mu",    "sigma[1]",  "sigma[2]"), 
                 lb   = c(0.001,  0.001,    0.001,    -5,      0.001,       0.001),
                 ub   = c(0.999,  0.999,    3,        1,       5,           5),
                 PDF  = c("beta", "beta",   "gamma",  "norm",  "invgamma",  "invgamma"), 
                 p1   = c(2.3,    1.2,      2,        -2,      12,          9),
                 p2   = c(1.2,    1.8,      4,        0.55,    0.05,        0.075),
                 stringsAsFactors = FALSE)

## Analytic densities

densd <- NULL
for (k in 1:dim(pp)[1]) {
  x <- seq(pp$lb[k], pp$ub[k], length.out=100)
  p <- exec(paste0("d", pp$PDF[k]), x, pp$p1[k], pp$p2[k])
  densd <- bind_rows(densd,
                     data.frame(dens=p, dom=x, PDF=pp$PDF[k], Parameter=pp$name[k], 
                                stringsAsFactors = FALSE))
}

plt <- ggplot(densd) +
  geom_area(aes(x=dom, y=dens, group=Parameter, fill=PDF), color=NA, alpha=.7) +
  theme_minimal() +
  labs(title="Priors", x="", y="") +
  facet_wrap(~Parameter, scales="free", labeller=label_parsed, ncol=2)
plot(plt)

## Random draws

drawd <- NULL
n     <- 5000
for (k in 1:dim(pp)[1]) {
  d     <- exec(paste0("r", pp$PDF[k]), n, pp$p1[k], pp$p2[k])
  drawd <- bind_rows(drawd,
                     data.frame(val=d, PDF=pp$PDF[k], Parameter=pp$name[k], 
                                stringsAsFactors = FALSE))
}

plt <- ggplot(bind_rows(drawd, densd)) +
  geom_histogram(aes(x=val, y=..density.., group=PDF, fill=PDF), 
                 alpha=.5, bins=50, color="grey77") +
  geom_area(aes(x=dom, y=dens, group=PDF, fill=PDF), 
            color=NA, alpha=.3) +
  theme_minimal() +
  labs(title=paste("Simulated vs. theoretical densities, n =",n),x="",y="") +
  facet_wrap(~Parameter, ncol=2, scales = "free", labeller = label_parsed)
plot(plt)

evaluate_log_prior <- function(v, p) {
  if (length(v) != dim(p)[1]) {
    print("Priors/params inconsistent")
    return(0)
  }
  d <- matrix(0, length(v), 1)
  for (k in 1:length(v)) {
    d[k] <- exec(paste0("d", p$PDF[k]), v[k], p$p1[k], p$p2[k])
  }
  return(sum(log(d)))
}

posterior <- function(v, p) {
  return(evaluate_log_prior(v, p))
}


## A very simple problem

MH_RW <- function(init_val, p, reps, burn, scale) {
  np     <- length(init_val)
  b_old  <- init_val
  lp_old <- posterior(b_old, p)
  draws  <- matrix(0, np, reps-burn, dimnames=list(p$name))  # Store draws
  nacc   <- 0                                                # Number of acceptances
  for (i in 1:reps) {
    b_new <- b_old + scale*rnorm(np)                         # New draw of the parameers
    if (all(b_new < p$ub & b_new > p$lb)) {                  # Test if draw withing bounds
      lp_new <- posterior(b_new, p)
      if (is.nan(lp_new)) {lp_new <- -Inf}
      if (runif(1) < min(exp(lp_new-lp_old), 1)) {           # Test to accept or not
        b_old  <- b_new
        lp_old <- lp_new
        if (i > burn) nacc <- nacc+1
      }
    }
    if (i > burn) draws[,i-burn] <- b_old  # Store past the burn in period only
  }   
  print(paste("Acceptance ratio:", nacc/(reps-burn)))
  return(draws)
}


## Example 1


init_val  <- c(.9,.2,.4,-2,1.5,1.5)
lp <- posterior(init_val, pp)

reps  <- 100000
burn  <- reps/2
draws <- MH_RW(init_val, pp, reps, burn, 0.3) 

pmh <-  as.data.frame(t(draws)) %>% 
  mutate(X = 1:(reps-burn)) %>%
  pivot_longer(cols=-X, names_to="Parameter", values_to="val") %>%
  bind_rows(densd) %>%
  ggplot() + 
  geom_line(aes(x=X, y=val, color=Parameter), alpha=.75, show.legend=F) +
  facet_wrap(~Parameter, scales="free", labeller=label_parsed, ncol=2) +
  theme_minimal() + 
  labs(title="RW-MH estimate of pure prior", x="", y="") 
plot(pmh)

pmh <-  as.data.frame(t(draws)) %>% 
  mutate(X = 1:(reps-burn)) %>%
  slice(1:333) %>% 
  pivot_longer(cols=-X, names_to="Parameter", values_to="val") %>%
  bind_rows(densd) %>%
  ggplot() + 
  geom_line(aes(x=X, y=val, color=Parameter), alpha=.75, show.legend=F) +
  facet_wrap(~Parameter, scales="free", labeller=label_parsed, ncol=2) +
  theme_minimal() + 
  labs(title="RW-MH estimate of pure prior", x="", y="") 
plot(pmh)

pmh <-  as.data.frame(t(draws)) %>% 
  pivot_longer(cols=everything(), names_to="Parameter", values_to="val") %>%
  bind_rows(densd) %>%
  ggplot() + 
  geom_area(aes(x=dom, y=dens, group=PDF), fill="grey66", color=NA, alpha=.7) +
  geom_histogram(aes(x=val, y=..density.., color=Parameter, fill=Parameter), 
                 alpha=.3, bins=50, show.legend=F) +
  facet_wrap(~Parameter, scales="free", labeller=label_parsed, ncol=2) +
  theme_minimal() + 
  labs(title="RW-MH estimate of pure prior", x="", y="") 
plot(pmh)


## Example 2

p2 <- data.frame(name = c("eta", "zeta[1]", "zeta[2]", "delta", 
                          "alpha[1]", "alpha[2]", "upsilon[1]", "upsilon[2]"), 
                 lb   = c(0.25, 0.001, 0.001, 0.001, 1.001, 1.001, 0.001, 0.001),
                 ub   = c(5, 6, 3, 5, 5, 5, 0.999, 0.999),
                 PDF  = c("invweibull", "paralogis", "paralogis", "invpareto", 
                          "lgamma", "lgamma", "unif", "unif"), 
                 p1   = c(2.3, 1, 2,  2,   2, 3, 0.3, 0.5),
                 p2   = c(1.2, 4, 3,  0.3, 5, 4, 0.7, 1), 
                 stringsAsFactors = FALSE)

densd2 <- NULL
for (k in 1:dim(p2)[1]) {
  x <- seq(p2$lb[k], p2$ub[k], length.out=1000)
  p <- exec(paste0("d", p2$PDF[k]), x, p2$p1[k], p2$p2[k])
  densd2 <- bind_rows(densd2,
                      data.frame(dens=p, dom=x, PDF=p2$PDF[k], Parameter=p2$name[k], 
                                 stringsAsFactors = FALSE))
}

plt <- ggplot(densd2) +
  geom_area(aes(x=dom, y=dens, group=Parameter, fill=PDF), color=NA, alpha=.7) +
  theme_minimal() +
  labs(title="Priors", x="", y="") +
  facet_wrap(~Parameter, scales="free", labeller=label_parsed, ncol=2)
plot(plt)

init_val2  <- c(1,1,1,1,2,2,.5,.5)
draws2     <- MH_RW(init_val2, p2, reps, burn, 0.125) 
pmh <-  as.data.frame(t(draws2)) %>% 
  pivot_longer(cols=everything(), names_to="Parameter", values_to="val") %>%
  bind_rows(densd2) %>%
  ggplot() + 
  geom_histogram(aes(x=val, y=..density.., group=Parameter), 
                 alpha=.7, bins=75, color="grey55", fill="grey88") +
  geom_area(aes(x=dom, y=dens, group=PDF, fill=PDF), color=NA, alpha=.5) +
  facet_wrap(~Parameter, scales="free", labeller=label_parsed, ncol=2) +
  theme_minimal() + 
  labs(title="RW-MH estimate of pure prior, Example 2", x="", y="") 
plot(pmh)

