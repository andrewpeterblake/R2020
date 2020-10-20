## Coding up the model in R

library(tidyverse)

nf    <- 2
ns    <- 5
ne    <- 3
np    <- ns-nf

beta  <- 0.99   # Discount factor 
sigma <- 2.0    # Elas. substitution
kappa <- 0.075  # Slope PC
delta <- 1.5    # Inflation feedback
gamma <- 0.75   # Smoothing
rho_1 <- 0.9    # AR1
rho_2 <- 0.8    # AR1
Omega <- diag(c(0.33,0.33,0.33)) # SE of 3 shocks

labels <- c("e^1","e^2","i","y","pi")

E <- matrix(0,ns,ns)
A <- matrix(0,ns,ns)
B <- diag(1,ns,ne)

# Now put the equations in matrix form
diag(E[1:2,1:2]) <- 1
diag(A[1:2,1:2]) <- c(rho_1, rho_2)

E[3,3]             <- 1 
E[4,c(1, 3, 4, 5)] <- c(1, -1/sigma, 1, 1/sigma)
E[5,c(2, 5)]       <- c(1, beta)

A[3,c(3, 5)]       <- c(gamma, (1-gamma)*delta)
A[4,4]             <- 1
A[5,c(4,5)]        <- c(-kappa, 1)

C <- solve(E,A)
D <- solve(E,B)

impulse_responses <- function(P, Q, Omega, labels, T) {
  s   <- matrix(0, ncol(Q), 1)
  z   <- matrix(0, nrow(Q), T)
  rownames(z) <- labels
  dza <- NULL
  for (j in 1:ncol(Q)) {
    s[j]  <- Omega[j,j]
    z[,1] <- Q %*% s
    for (i in 1:(T-1)) {
      z[,i+1] <- P %*% z[,i]
    }
    s[j] <- 0
    dz <- as_tibble(t(z)) %>% 
      mutate(Period = 1:T, Shock = paste0("epsilon^",j))
    dza <- bind_rows(dza,dz)
  }
  return(dza)
}

response_plot <- function(series, title) {
  return(pivot_longer(series, cols = -c(Period,Shock), names_to="Var", values_to = "Val") %>%
           ggplot() +
           geom_line(aes(x=Period, y=Val, group=Shock, colour=Var), show.legend=FALSE) +
           facet_grid(Shock~Var, scales="free", labeller=label_parsed) +
           scale_x_continuous(expand=c(0,0)) +
           theme_minimal() +
           labs(title=title, x="",y=""))
}

T <- 250
z <- impulse_responses(C, D, Omega, labels, T)
response_plot(z, "Impulse responses: Taylor rule")

### R code

m  <- eigen(C, symmetric=FALSE)
iz <- 1:np
ix <- (np+1):ns
M  <- solve(m$vectors[,ns:1])        # Invert & reverse order for increasing abs value
N  <- -Re(solve(M[ix,ix], M[ix,iz])) # Drop tiny complex bits (if any)
G  <- solve((C[ix,ix] - N %*% C[iz,ix]), (N %*% D[iz,]- D[ix,]))
N
G

P  <- cbind(rbind((C[iz,iz] + C[iz,ix] %*% N), N), matrix(0,ns,nf))
Q  <- rbind(D[iz,] + C[iz,ix] %*% G, G)

## Impulse responses
T <- 25
z <- impulse_responses(P, Q, Omega, labels, T)
response_plot(z, "Impulse responses: Taylor rule")

solveGenBK <- function(E,A,B,n) {
  d  <- geigen::gqz(A, E, sort="S") 
  np <- d$sdim
  ns <- nrow(E)
  print(paste("Number of unstable roots is", ns-np))
  if (n == np) {
    iz <- 1:n
    ix <- (n+1):ns
    Ns <- d$Z[ix,iz] %*% solve(d$Z[iz,iz])
    H  <- solve(E[iz,iz] + E[iz,ix] %*% Ns)
    W  <- (E[ix,iz] + E[ix,ix] %*% Ns) %*% H
    Gs <- solve((A[ix,ix] - W %*% A[iz,ix]), (W %*% B[iz,] - B[ix,]))
    As <- H %*% (A[iz,iz] + A[iz,ix] %*% Ns)
    Bs <- H %*% (B[iz,] + A[iz,ix] %*% Gs)
    return(list(P=cbind(rbind(As,Ns),matrix(0,ns,ns-n)), Q=rbind(Bs, Gs)))
    } 
  else { 
    return(-1) 
    }
}

# Optimal policy

nf <- 2
ne <- 3
ns <- 6      # One extra state
np <- ns-nf
mu <- 0.75   # Representative trade-off

labels <- c("e^1","e^2","ylag","i","y","pi") # New variable order

E <- matrix(0,ns,ns)
A <- E
B <- matrix(0,ns,ne)
B[1,1] <- 1
B[2,2] <- 1
B[4,3] <- -1

diag(E[1:3,1:3]) <- 1
diag(A[1:2,1:2]) <- c(rho_1, rho_2)
A[3,5]           <- 1

E[4,3]           <- 1
A[4,c(3, 6)]     <- c(1, -1/mu)

E[5,c(1, 4, 5, 6)] <- c(1, -1/sigma, 1, 1/sigma)
A[5,5]           <- 1

E[6,c(2, 6)]     <- c(1, beta)
A[6,c(5, 6)]     <- c(-kappa, 1)

e <- geigen::gevalues(geigen::gqz(A, E, sort="S"))
e[abs(e) > 1]

E[4:5,] <- E[5:4,]
A[4:5,] <- A[5:4,]
B[4:5,] <- B[5:4,]

So <- solveGenBK(E,A,B,np)
Po <- So$P
Qo <- So$Q

### Optimal impulse responses
zo <- impulse_responses(Po, Qo, Omega, labels, T) %>%
  select(-ylag) # Drop duplicate series
response_plot(zo, "Impulse responses: Optimal policy")
