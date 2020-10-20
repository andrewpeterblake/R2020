model_file <- function(theta) {
  
  nf    <- 2
  ns    <- 6
  np    <- ns-nf
  nx    <- np+1
  
  beta  <- 0.99   #  calibrated
  sigma <- theta[1]
  kappa <- theta[2]
  delta <- theta[3]
  gamma <- theta[4]
  rho_1 <- theta[5]
  rho_2 <- theta[6]
  rho_3 <- theta[7]
  Omega <- diag(theta[c(8:10)])
  
  labels <- c("eta[1]","eta[2]","eta[3]","i","y","pi")
  
  E <- matrix(0,ns,ns)
  A <- E
  G <- diag(1,ns,3)
  
  diag(E[1:3,1:3]) <- 1
  diag(A[1:3,1:3]) <- c(rho_1, rho_2, rho_3)
  
  E[4,c(3, 4)]       <- c(-1, 1) 
  E[5,c(1, 4, 5, 6)] <- c(1, -1/sigma, 1, 1/sigma)
  E[6,c(2, 6)]       <- c(1, beta)
  
  A[4,c(4, 6)]       <- c(gamma, (1-gamma)*delta)
  A[5,5]             <- 1
  A[6,c(5,6)]        <- c(-kappa, 1)
  
  AA <- solve(E,A)
  BB <- solve(E,G)
  m  <- eigen(AA)
  
  Lambda <- diag(m$values)
  # print(Lambda[abs(Lambda)>1])
  M      <- solve(m$vectors[,ns:1])
  N      <- -Re(solve(M[nx:ns,nx:ns])%*%M[nx:ns,1:np])
  G      <- -solve((AA[nx:ns,nx:ns]-N%*%AA[1:np,nx:ns]), (BB[nx:ns,]-N%*%BB[1:np,]))
  PP     <- cbind(rbind((AA[1:np,1:np] + (AA[1:np,nx:ns] %*% N)), N), matrix(0,ns,nf))
  QQ     <- rbind(BB[1:np,] + AA[1:np,nx:ns] %*% G, G)

  return(list(PP=PP, QQ=QQ, Omega=Omega, labels=labels)) 
}

