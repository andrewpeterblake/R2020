ir <- function(theta, nimp) {

  modl   <- model_file(theta)
  PP     <- modl[[1]]
  QQ     <- modl[[2]]
  Omega  <- modl[[3]]
  labs   <- modl[[4]]

    ns  <- dim(QQ)[2]
  s   <- matrix(0, ns, 1)
  dza <- NULL
  z   <- list()
  for (j in 1:ns) {
    s[j]   <- Omega[j, j]
    z[[1]] <- QQ %*% s
    for (i in 2:nimp) {
      z[[i]] <- PP %*% z[[i - 1]]
    }
    s[j] <- 0
    dz <- data.frame(t(bind_cols(z))) %>%
      mutate(Period = 1:nimp, Shock = paste0("epsilon[", j, "]"))
    colnames(dz)[1:6] <- labs
    dza <- bind_rows(dza, dz)
  }
  
  gz <- gather(dza, Var, Val, -Period, -Shock)
  ir <- ggplot(gz) +
    geom_line(aes(x = Period, y = Val, group = Shock, colour = Var), 
              show.legend = FALSE) +
    facet_grid(Shock ~ Var, scales = "free", labeller = label_parsed) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() +
    labs(title = "Impulse responses", x = "", y = "")
  plot(ir)
  
}