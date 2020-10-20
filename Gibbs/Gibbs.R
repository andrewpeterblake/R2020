## Gibbs sampling 

## Estimating a model for US inflation

library(tidyverse)
library(kableExtra)

max_lag <- 4

download.file("https://fred.stlouisfed.org/series/CPALTT01USQ661S/downloaddata/CPALTT01USQ661S.csv",
              "CPI.csv")
data <- readr::read_csv("CPI.csv")  %>% 
  rename(Date=DATE, value=VALUE) %>% 
  mutate(Vars = "INF", 
         Vals = 100*(value/lag(value,4)-1),
         lagi = list(0:max_lag)) %>%
  slice(-c(1:4)) %>%
  unnest(cols = c(lagi)) %>%
  group_by(lagi) %>%
  mutate(Vals = lag(Vals, lagi[1])) %>%
  ungroup() %>%
  mutate(Varl = paste(Vars, lagi, sep="_"), 
         Vars = if_else(lagi==0, Vars, Varl)) %>%
  select(Date,Vars,Vals) %>%
  pivot_wider(names_from = Vars, values_from = Vals) %>%
  slice(-c(1:max_lag)) %>%
  mutate(constant = 1)
ggplot(data) + 
  geom_line(aes(x=Date, y=INF), colour="red") +
  theme_minimal() +
  labs(title="US inflation data", x="", y="")

X <- data.matrix(select(data, -Date, -INF))
Y <- data.matrix(select(data, INF))
T <- nrow(Y)

# Save for later use
XX      <- crossprod(X)
XY      <- crossprod(X,Y)
iXX     <- chol2inv(chol(XX))

# Standard OLS
betahat <- iXX %*% XY
eta     <- Y - X %*% betahat
se      <- sum(eta^2)/(T-max_lag-1)
seb     <- sqrt(diag(iXX)*se)
res     <- tibble(Coefficient = c(names(data)[-(1:2)], "sigma"),
                  Estimate    = c(betahat, sqrt(se)), 
                  SE          = c(seb, NA), 
                  `t-stat`    = c(betahat/seb, NA)) 

### Gibbs samples

# Set priors and starting values
# priors for B
B0     <- matrix(c(1, rep(0, max_lag)),max_lag+1,1)
Sigma0 <- diag(max_lag+1)/1
# priors for sigma2
T0     <- 1L
D0     <- .1
# starting value for sigma2
sigma2 <- 1L

# Create matrices for repeated use
iS0   <- chol2inv(chol(Sigma0))
const <- matrix(0, max_lag, 1)
BB    <- matrix(c(1, rep(0, max_lag-1)), max_lag, 1)

# Parameters of/storage for Gibbs samples
reps <- 5000 # total numbers of Gibbs iterations
burn <- 3000  # number of burn-in iterations
out  <- matrix(0,reps-burn,max_lag+2)

for (i in 1:reps) {
  
  V <- chol2inv(chol(XX/sigma2 + iS0))
  M <- V %*% (XY/sigma2 + iS0 %*% B0)
  
  eigs <- 2
  while(max(abs(eigs)) > 1) {
    b    <- M + chol(V) %*% rnorm(max_lag+1)
    A    <- rbind(t(head(b, max_lag)), diag(1,max_lag-1,max_lag))
    eigs <- eigen(A, symmetric=FALSE, only.values=TRUE)$values
    }
  
  # sample sigma2 conditional on b from IG(T0,D0);
  e      <- Y - X%*%b
  sigma2 <- (D0 + sum(e^2))/sum(rnorm(T0+T)^2)
  
  if (i > burn) out[i-burn,] <- c(t(b), sigma2)  

}

# Rearrange output
ddens <- as_tibble(out) %>%
  rename_all( ~ c(paste0("beta[",1:max_lag,"]"), "alpha", "sigma")) %>%
  mutate(iter=1:(reps-burn)) %>%
  pivot_longer(names_to = "par", values_to = "val", cols = -iter)

# Plot sequences
sdens <- ggplot(ddens) + 
  geom_line(aes(x=iter, y=val, group=par, colour=par), show.legend=FALSE) +
  facet_wrap(~par, labeller=label_parsed, ncol=2, scales="free") +
  theme_minimal() + 
  labs(title="Sequences", x="", y="")
plot(sdens)

ddens %>% 
  rename(Parameter = par) %>%
  group_by(Parameter) %>% 
  summarise(Expected = mean(val), 
            SE       = sd(val), 
            lower_5  = quantile(val, 0.05), 
            upper_95 = quantile(val, 0.95)) %>% 
  kable(digits=3, caption="Bayesian estimates", booktabs=TRUE) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# Plot estimated densities
pdens <- ggplot(ddens) + 
  geom_histogram(aes(x=val, y=..density.., group=par, fill=par), alpha=0.33, bins=75) +
  geom_density(aes(x=val, group=par, colour=par)) +
  facet_wrap(~par, labeller=label_parsed, ncol=2, scales="free", strip.position="top") +
  theme_minimal() +
  theme(legend.position="none") + 
  labs(title="Histograms and kernel-smoothed estimated densities", x="", y="")
plot(pdens)
