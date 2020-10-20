## A Bayesian fan chart

library(tidyverse)

max_lag <- 4
series  <- "CPALTT01USQ661S"
url     <- paste0("https://fred.stlouisfed.org/series/", series, "/downloaddata/",series,".csv")

download.file(url, "CPI.csv")
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

X <- data %>%
  select(-Date, -INF) %>%
  data.matrix(.)
Y <- data %>%
  select(INF) %>%
  data.matrix(.)
T <- nrow(Y)

# Save for later use
XX      <- crossprod(X)
XY      <- crossprod(X,Y)
iXX     <- chol2inv(chol(XX))

# Periods to forecast/plot
nf <- 12
nb <- 16

# step 1 set priors and starting values
# priors for B
B0     <- matrix(0,max_lag+1,1)
Sigma0 <- diag(max_lag+1)
# priors for sigma2
T0     <- 1L
D0     <- 0.1

# starting values
B      <- B0
sigma2 <- 1

# Store further numbers for repeated use
iS0   <- chol2inv(chol(Sigma0))
T1    <- T0 + T
YL    <- tail(Y,4)[max_lag:1]
const <- matrix(0,max_lag,1)
BB    <- matrix(0,max_lag,1)

# Store Gibbs samples
reps <- 50000   # total numbers of Gibbs iterations
burn <- 10000   # number of burn-in iterations
out  <- matrix(0,reps-burn,max_lag+2)
outf <- matrix(0,reps-burn,nf)

for (i in 1:reps) {
  
  V <- XX/sigma2[1] + iS0
  M <- solve(V, XY/sigma2[1] + iS0%*%B0)
  
  eigs <- 2
  while(max(abs(eigs)) > 1) {
    b    <- M + crossprod(chol2inv(chol(V)),rnorm(max_lag+1))
    A    <- rbind(t(b[-length(b)]), diag(1,max_lag-1,max_lag))
    eigs <- eigen(A, symmetric=FALSE, only.values=TRUE)$values
    }
  
  # sample sigma2 conditional on b from IG(T1,D1);
  err    <- Y - X%*%b
  # compute posterior df and scale matrix
  D1     <- D0 + crossprod(err)
  # draw from IG
  z0     <- rnorm(T1)
  sigma2 <- D1/crossprod(z0)
  
  if (i > burn) { 
    out[i-burn,] <- c(t(b), sigma2)
    const[1]     <- b[length(b)]
    Yf           <- YL
    for (j in 1:nf) {
      BB[1]          <- sqrt(sigma2)*rnorm(1)
      Yf             <- const + A%*%Yf + BB
      outf[i-burn,j] <- Yf[1]
      }
    }
  }

# Fancharts
# Calculate bands
fdt <- seq.Date(tail(data$Date,1), by="quarter", length.out=nf+1)
qm  <-   apply(outf, 2, quantile, probs=c(.5))
mm  <- t(apply(outf, 2, quantile, probs=c(.05,.20,.35,.65,.80,.95))) %>% 
  rbind(rep(tail(Y,1),6), .)

pg  <- rbind(mm[,-6], mm[(nf+1):1,-1]) %>% 
  as_tibble() %>% 
  rename_all( ~ c(paste0("Area",1:5))) %>%
  mutate(Date = c(fdt,rev(fdt))) %>%
  gather(Area, coord, -Date)

bd  <- bind_rows(tibble(Date=tail(data$Date,nb), Data=tail(Y,nb)),
                 tibble(Date=fdt[-1],      Data=qm))

col_tail <- "grey95"
centre   <- c("tomato","maroon4","royalblue4","peru","seagreen")
j        <- 4
col      <- colorRampPalette(c(col_tail,centre[j],col_tail))(7)

# Rearrange output
fdens <- outf %>%
  as_tibble(.name_repair = "unique") %>% 
  rename_all(~ c(paste0("pi[T+",formatC(1:nf, width=2, flag="0"),"]"))) %>%
  mutate(iter = 1:(reps-burn)) %>%
  pivot_longer(cols = -iter, names_to = "par", values_to = "val")

### Sub draws

# Plot sequences
fsdens <- ggplot(fdens) + 
  geom_line(aes(x=iter, y=val, group=par, colour=par), show.legend=FALSE) +
  facet_wrap(~par, labeller=label_parsed, ncol=4) +
  theme_minimal() + 
  labs(title="Sequences", x="", y="")
plot(fsdens)

# Plot estimated densities
fpdens <- ggplot(fdens) + 
  geom_histogram(aes(x=val, y=..density.., group=par, fill=par), alpha=0.33, bins=50) +
  geom_density(aes(val, group=par, colour=par)) +
  # geom_density(aes(val, group=par, colour=par, fill=par), alpha = .1) +
  facet_wrap(~par, labeller=label_parsed, ncol=4) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title="Kernel-smoothed estimated forecast densities", x="", y="")
plot(fpdens)

# Plot estimated densities
fpdens <- ggplot(fdens) + 
  # geom_histogram(aes(x=val, y=..density.., group=par, fill=par), alpha=0.33, bins=50) +
  # geom_density(aes(val, group=par, colour=par)) +
  geom_density(aes(val, group=par, colour=par, fill=par), alpha = .1) +
  # facet_wrap(~par, labeller=label_parsed, ncol=4) +
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title="Kernel-smoothed estimated forecast densities", x="", y="")
plot(fpdens)

### Fan chart

ggi <- ggplot(pg) + 
  geom_rect(aes(xmin=min(Date), xmax=max(Date), ymin=-Inf, ymax=Inf), fill=col[1], alpha=.15) +
  geom_polygon(aes(x=Date, y=coord, group=Area, fill=Area)) +
  geom_line(data=bd,aes(x=Date, y=Data), col=centre[j]) +
  scale_fill_manual(values=col[2:6]) +
  labs(title="US Inflation", y="Percentage points", x="") +
  scale_x_date(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none") 
plot(ggi)

### HPDI Fan chart

library(HDInterval)
p3 <- hdi(outf, .3)
p6 <- hdi(outf, .6)
p9 <- hdi(outf, .9)

pp <- as_tibble(bind_cols(p9[1,],p6[1,],p3[1,],p3[2,],p6[2,],p9[2,]), .name_repair = "unique") %>%
  add_row(as_tibble(matrix(tail(data$INF,1),1,6), .name_repair = "unique"), .before = 1) %>%
  as.matrix()

ph  <- rbind(pp[,-6], pp[(nf+1):1,-1]) %>% 
  as_tibble() %>% 
  rename_all( ~ c(paste0("Area",1:5))) %>%
  mutate(Date = c(fdt,rev(fdt))) %>%
  gather(Area, coord, -Date)
ggi <- ggplot(ph) + 
  geom_rect(aes(xmin=min(Date), xmax=max(Date), ymin=-Inf, ymax=Inf), fill=col[1], alpha=.15) +
  geom_polygon(aes(x=Date, y=coord, group=Area, fill=Area)) +
  geom_line(data=bd,aes(x=Date, y=Data), col=centre[j]) +
  scale_fill_manual(values=col[2:6]) +
  labs(title="US Inflation", y="Percentage points", x="") +
  scale_x_date(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position = "none") 
plot(ggi)
