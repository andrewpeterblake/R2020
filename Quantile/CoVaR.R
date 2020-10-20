library(quantreg)
library(tidyverse)

# CoVaR

HSBC     <- read_csv("Yahoo/HSBA.L.csv", na="null") %>% 
  select(Date, HSBC = Close)
LLOY     <- read_csv("Yahoo/LLOY.L.csv") %>% 
  select(Date, Lloyds = Close)
BARC     <- read_csv("Yahoo/BARC.L.csv") %>% 
  select(Date, Barclays = Close)

Data0    <- full_join(BARC, HSBC) %>% 
  full_join(LLOY) %>%
  pivot_longer(cols=-Date, names_to = "Vars", values_to = "Vals") %>%
  group_by(Vars) %>%
  mutate(Vals = (Vals/lag(Vals, 1) - 1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Vars, values_from = Vals)

Data0 %>% pivot_longer(cols=-Date, names_to="Vars", values_to = "Vals") %>% 
  ggplot() +
  geom_line(aes(x=Date, y=Vals, group=Vars, color=Vars)) +
  theme_minimal() + 
  facet_wrap(~Vars, scales = "free_y") +
  theme(legend.position = "none") +
  labs(title="Institution returns", x="", y="")

X_FTSE_1 <- read_csv("Yahoo/^FTSE_1.csv")
X_FTSE_2 <- read_csv("Yahoo/^FTSE_2.csv")
X_FTSE_3 <- read_csv("Yahoo/^FTSE_3.csv")
X_FTSE_4 <- read_csv("Yahoo/^FTSE_4.csv")
FTSE     <- full_join(X_FTSE_1, X_FTSE_2) %>% 
  full_join(X_FTSE_3) %>%
  full_join(X_FTSE_4) %>% 
  select(Date, FTSE = Close)
VIX      <- read_csv("Yahoo/^VIX.csv") %>% 
  select(Date, VIX = Close)

State    <-  full_join(FTSE, VIX, by="Date") %>%
  mutate(FTSE.return = 100*(log(FTSE) - lag(log(FTSE)))) %>%
  select(-FTSE) %>% 
  mutate(Date = lead(Date)) # Artificially create the data as lags

gather(State, Var, Val, -Date) %>% 
  ggplot() +
  geom_line(aes(x=Date, y=Val, group=Var, color=Var)) +
  theme_minimal() + 
  facet_wrap(~Var, scales="free_y") +
  theme(legend.position="none") +
  labs(title="Lagged states", x="", y="")

Data <- Data0 %>% 
  inner_join(State, by="Date") %>%
  drop_na()

# Full sample estimates

state_names <- names(State)[-1]
maxs        <- nrow(Data)

Results <- list()
for (j in 1:3)  { # Each institution
  
  wbank <- colnames(Data)[j+1] 
  Data2 <- Data %>%
    mutate(System.returns = rowMeans(select(., -Date, -all_of(wbank), -all_of(state_names)))) # Average return excluding Institution i
  eqn.inst <- formula(paste0(wbank, " ~ ", paste(state_names, collapse="+")))
  eqn.syst <- formula(paste0("System.returns ~ ", wbank, " + ", paste(state_names, collapse="+")))

  # Fit QR at 0.05 - VaR institution
  VaR.inst <- rq(eqn.inst, data=Data2, tau=0.05, ci=TRUE)
  print(summary(VaR.inst))
  
  # Fit QR at 0.05 - VaR system
  VaR.syst <- rq(eqn.syst, data=Data2, tau=0.05, ci=TRUE)
  print(summary(VaR.syst))
  
  # CoVaR
  fit.inst <- fitted(VaR.inst)     # VaR in every period
  bb       <- coef(VaR.syst)[,1]   # System parameters
  RHS      <- data.matrix(select(Data2, all_of(state_names)))  # Rest of predictors
  CoVaR    <- bb[1] + bb[2]*fit.inst + RHS %*% bb[-(1:2)]

  # Results
  Results[[j]] <- Data2 %>%
    mutate(VaR.institution = as.numeric(fit.inst), 
           VaR.system      = as.numeric(fitted(VaR.syst)), 
           CoVaR           = as.numeric(CoVaR),
           Bank            = wbank)
}

Results %>%
  bind_rows() %>%
  select(Date, CoVaR, Bank) %>%
  gather(Measure, Val, -Date, -Bank) %>% 
  ggplot() +
  geom_line(aes(x=Date, y=Val, group=Measure, color=Bank)) +
  theme_minimal() +
  facet_wrap(~ Bank) +
  labs(title="CoVaR", x="", y="")

## Recursive estimates

EstCoefI <- list()
EstCoefS <- list()
p        <- 0
for (j in 1:3)  { # Each institution
  
  wbank <- colnames(Data)[j+1] 
  Data1 <- Data %>%
    mutate(System.returns = rowMeans(select(., -all_of(wbank), -Date, -all_of(state_names))))
  
  eqn.inst <- formula(paste0(wbank, " ~ ", paste(state_names, collapse="+")))
  eqn.syst <- formula(paste0("System.returns ~ ", wbank, " + ",
                             paste(state_names, collapse="+")))
  
  for (j in seq(130, maxs, 1)) {

    Data2    <- head(Data1, j) # Simple way to control sample
    # Fit QR at 0.05 - VaR institution
    VaR.inst <- rq(eqn.inst, data=Data2, tau=0.05, ci=TRUE)
    # Fit QR at 0.05 - VaR system
    VaR.syst <- rq(eqn.syst, data=Data2, tau=0.05, ci=TRUE)
    # Results
    p <- p+1
    EstCoefI[[p]] <- data.frame(Coef   = rownames(VaR.inst$coefficients), 
                                VaR.inst$coefficients, 
                                Sample = max(Data2$Date),
                                Bank   = wbank, stringsAsFactors = FALSE)
    EstCoefS[[p]] <- data.frame(Coef   = rownames(VaR.syst$coefficients), 
                                VaR.syst$coefficients, 
                                Sample = max(Data2$Date),
                                Bank   = wbank, stringsAsFactors = FALSE)
  }
}

ff      <- names(Data)
ff[1]   <- "(Intercept)"

EstCoefI %>%
  bind_rows() %>%
  mutate(Coef = factor(Coef, levels=ff), Sample = as.Date(Sample)) %>%
  ggplot() + 
  geom_line(aes(x=Sample, y=coefficients, color=Bank))  + 
  geom_line(aes(x=Sample, y=lower.bd), color= "grey55", linetype=2)  + 
  geom_line(aes(x=Sample, y=upper.bd), color= "grey55", linetype=2)  + 
  facet_grid(Coef~Bank, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Recursive coefficient estimates: Individual", x="", y="")

EstCoefS %>%
  bind_rows() %>%
  mutate(Coef = factor(Coef, levels = ff), Sample = as.Date(Sample)) %>%
  ggplot() + 
  geom_line(aes(x=Sample, y=coefficients, color=Bank))  + 
  geom_line(aes(x=Sample, y=lower.bd), color= "grey55", linetype=2)  + 
  geom_line(aes(x=Sample, y=upper.bd), color= "grey55", linetype=2)  + 
  facet_grid(Coef~Bank, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Recursive coefficient estimates: System", x="", y="")


## Calculating $\Delta CoVaR$ and a look at uncertainty

AllData  <- list()
EstCoefI <- list()
EstCoefS <- list()
p        <- 0
for (j in 1:3)  { # Each institution
  
  wbank <- colnames(Data)[j+1] 
  Data1 <- Data %>%
    mutate(System.returns = rowMeans(select(., -c(all_of(wbank), Date, all_of(state_names))))) 

  eqn.inst <- formula(paste0(wbank," ~ ",paste(state_names,collapse=" + ")))
  eqn.syst <- formula(paste0("System.returns ~ ",wbank," + ",paste(state_names,collapse=" + ")))

  for (k in seq(132, maxs, 1)) {

    # Set sample
    Data2 <- head(Data1, k)
    
    # Fit QR at 0.05 - VaR institution
    VaR.inst <- rq(eqn.inst, data=Data2, tau=0.05, ci=TRUE)
    M.inst   <- rq(eqn.inst, data=Data2, tau=0.5, ci=TRUE)
    
    fit.inst <- fitted(VaR.inst)
    fit.M    <- fitted(M.inst)

    # Fit QR at 0.05 - VaR system
    VaR.syst <- rq(eqn.syst, data=Data2, tau=0.05, ci=TRUE)

    # CoVaR
    RHS    <- data.matrix(select(Data2, all_of(state_names)))
    bb     <- coef(VaR.syst)[,1]
    bbm    <- coef(VaR.syst)[2,1] # estimate
    bbl    <- coef(VaR.syst)[2,2] # lower bound 
    bbu    <- coef(VaR.syst)[2,3] # upper bound
    CoVaR  <- bb[1] + bb[2]*fit.inst + RHS %*% bb[-(1:2)]
    DCoVaRm <- bbm*(fit.inst - fit.M) # DCoVaR at etimate
    DCoVaRl <- bbl*(fit.inst - fit.M) # at lower bound
    DCoVaRu <- bbu*(fit.inst - fit.M) # at upper bound

    # Results
    p <- p+1
    AllData[[p]] <- Data2 %>%
      mutate(VaR.institution = as.numeric(fit.inst), 
             VaR.system      = as.numeric(fitted(VaR.syst)), 
             CoVaR           = as.numeric(CoVaR),
             DCoVaRm         = as.numeric(DCoVaRm),
             DCoVaRl         = as.numeric(DCoVaRl),
             DCoVaRu         = as.numeric(DCoVaRu),
             Bank            = wbank,
             Sample          = max(Date))
    
    EstCoefI[[p]] <- data.frame(Coef   = rownames(VaR.inst$coefficients), 
                                VaR.inst$coefficients, 
                                Sample = max(Data2$Date),
                                Bank   = wbank, 
                                stringsAsFactors = FALSE)
    EstCoefS[[p]] <- data.frame(Coef   = rownames(VaR.syst$coefficients), 
                                VaR.syst$coefficients, 
                                Sample = max(Data2$Date),
                                Bank   = wbank, 
                                stringsAsFactors = FALSE)
  }
}

AllData <- AllData %>% 
  bind_rows() %>%
  group_by(Date, Bank) %>%
  mutate(MaxC  = max(CoVaR),   MinC  = min(CoVaR)) %>%
  mutate(MaxDm = max(DCoVaRm), MinDm = min(DCoVaRm)) %>%
  mutate(MaxDl = max(DCoVaRl), MinDl = min(DCoVaRl)) %>%
  mutate(MaxDu = max(DCoVaRu), MinDu = min(DCoVaRu)) %>%
  ungroup()

AllData %>%
  select(Date, MaxC, MinC, CoVaR, Sample, Bank) %>%
  distinct() %>% 
  ggplot() +
  geom_ribbon(data= . %>% filter(Sample == max(Sample)),
    aes(x=Date, ymin=MinC, ymax=MaxC, group=Bank, fill=Bank, color=Bank)) +
  geom_line(data= . %>% filter(Sample == Date),
            aes(x=Date, y=CoVaR, group=Bank), size=0.75, color="grey11") +
  theme_minimal() +
  facet_wrap(~Bank) +
  labs(title="Spread of CoVaR estimates", x="", y="",
       subtitle="Shaded swathes of min-max recursive estimates, black real-time estimate") +
  theme(legend.position = "none")

AllData %>%
  select(Date, MaxC, MinC, CoVaR, Sample, Bank) %>%
  distinct() %>% 
  ggplot() +
  geom_ribbon(data= . %>% filter(Sample == max(Sample)),
    aes(x=Date, ymin=MinC, ymax=MaxC, group=Bank, fill=Bank, color=Bank)) +
  geom_line(data= . %>% filter(Sample == max(Date)),
            aes(x=Date, y=CoVaR, group=Bank), size=0.75, color="grey11") +
  theme_minimal() +
  facet_wrap(~Bank) +
  labs(title="Spread of CoVaR estimates", x="", y="",
       subtitle="Shaded swathes of min-max recursive estimates, black final sample estimate") +
  theme(legend.position = "none")

AllData %>%
  select(Date, MaxDl, MinDl, MaxDm, MinDm, MaxDu, MinDu, Bank) %>%
  distinct() %>% 
  ggplot() +
  geom_ribbon(aes(x=Date, ymin=MinDl, ymax=MaxDl), fill="red", color=NA, alpha = .4) +
  geom_ribbon(aes(x=Date, ymin=MinDu, ymax=MaxDu), fill="green", color=NA, alpha = .4) +
  theme_minimal() +
  facet_wrap(~Bank) +
  labs(title="Spread of DCoVaR estimates", x="", y="") +
  theme(legend.position = "none")
