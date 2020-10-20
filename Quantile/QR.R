## Quantile regression 

library(tidyverse)
library(lubridate)
library(readxl)
library(quantreg)
library(moments)
library(zoo)

fls     <- "spfmicrodata.xlsx"

UNRATE <- read_excel("UNRATE.xls", col_types = c("date", "numeric"), skip = 10) %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, UNRATE)

mx <- read_excel(fls, sheet="UNEMP", na="#N/A", col_types="numeric") %>%
  unite(Date, c(YEAR, QUARTER), sep=" ") %>% 
  mutate(Date=as.Date(as.yearqtr(Date, format="%Y %q"))) %>% 
  select(Date, UNEMP0=UNEMP2, UNEMP1=UNEMP3, UNEMP2=UNEMP4, UNEMP3=UNEMP5, UNEMP4=UNEMP6, ID) %>%
  pivot_longer(cols=-c(Date, ID), names_to="Horizon", values_to="Values") %>% 
  group_by(Date, Horizon) %>% 
  summarise(av   = mean(Values, na.rm=TRUE)) %>% 
  mutate(Horizon = as.integer(str_sub(Horizon,6,6))) %>% 
  drop_na()

# Pivot wider to put each series in a column to merge with UNEMP data
mxb <- mx %>% 
  pivot_longer(cols = -c(Date, Horizon), names_to = "Variable", values_to = "Values") %>% 
  unite("Names", Variable:Horizon, sep="") %>% 
  pivot_wider(names_from = Names, values_from = Values) 

UNEMP <- UNRATE %>% 
  group_by(paste(year(Date), quarter(Date))) %>% 
  summarise(Date=min(Date), UNRATE=mean(UNRATE)) %>% 
  select(Date, UNRATE) %>% 
  right_join(mxb, by="Date") 

# QReg 
sf <- seq(.05,.95,.15)[-4]

tail_colour   <- "grey95"
centre_colour <- "seagreen"

nq  <- length(sf)
nv  <- length(centre_colour)
col <- colorRampPalette(c(rbind(tail_colour, centre_colour), tail_colour))(nv*nq+1)[-1]

ystart <- 2017
ares   <- NULL
for (i in 0:4) {
          reg  <- c(paste0("av",i))
          eq   <- formula(paste0("lead(UNRATE,",(i+1),") ~ ", reg))
          eqrq <- rq(eq, data=UNEMP, tau=sf)
          res  <- broom::tidy(eqrq) %>% 
            group_by(tau) %>%
            mutate(dta = unlist(c(1, slice(select(UNEMP, reg), n())))) %>% 
            mutate(q   = sum(estimate*dta)) %>% 
            ungroup %>%
            mutate(Vintage = max(UNEMP$Date), Horizon = i) 
          ares <- bind_rows(ares, res)
}

aresx <- ares %>% 
  select(tau, q, Vintage, Horizon) %>% 
  distinct() %>%
  left_join(select(UNEMP, Date, UNRATE), by=c("Vintage" = "Date")) %>% 
  mutate(q = if_else(Horizon == 0, UNRATE, q)) %>% 
  pivot_wider(names_from = tau, names_prefix = "tau=", values_from = q) %>% 
  group_by(Vintage) %>% 
  mutate(fdate = seq.Date(from = Vintage[1], by = "quarter", length.out = 5)) %>% 
  ungroup() %>% 
  pivot_longer(starts_with("tau"), names_to = "q", values_to = "Vals") %>% 
  mutate(qs = paste0("Q", q)) %>%
  mutate(qs = as_factor(desc(qs))) %>% 
  select(-UNRATE)

bck <- UNEMP %>% 
  select(Date, UNRATE) %>%
  filter(year(Date) > ystart) %>%
  mutate(Vintage = list(unique(aresx$Vintage)))  %>% 
  unnest(cols = Vintage) %>% 
  group_by(Vintage) %>% 
  filter(Date <= Vintage) %>% 
  ungroup() %>% 
  arrange(Vintage, Date)

ggplot(aresx) +
  geom_area(aes(x=fdate, y=+Inf, group=q), fill=tail_colour, position = "identity") +
  geom_area(aes(x=fdate, y=Vals, group=qs, fill=qs), position = "identity") +
  scale_fill_manual(values=col) +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_line(data=bck, aes(x=Date, y=UNRATE), colour="grey44") + 
  facet_grid(Vintage ~ .) +
  labs(title=paste("US unemployment rate and forecast fanchart"), y="", x="") 


########################## Different horizons

# Add some scale/skew measures
mx <- read_excel(fls, sheet="UNEMP", na="#N/A", col_types="numeric") %>%
  unite(Date, c(YEAR, QUARTER), sep=" ") %>%
  mutate(Date=as.Date(as.yearqtr(Date, format="%Y %q"))) %>%
  select(Date, UNEMP0=UNEMP2, UNEMP1=UNEMP3, UNEMP2=UNEMP4, UNEMP3=UNEMP5, UNEMP4=UNEMP6, ID) %>%
  pivot_longer(cols=-c(Date, ID), names_to="Horizon", values_to="Values") %>%
  group_by(Date, Horizon) %>%
  summarise(n    = n(),
            av   = mean(Values, na.rm=TRUE),
            IQR  = IQR(Values,  na.rm=TRUE)/1.349,
            Var  = var(Values,  na.rm=TRUE),
            SE   = sqrt(Var),
            MAD  = mad(Values,  na.rm=TRUE),
            Skew = skewness(Values, na.rm=TRUE)) %>%
  mutate(Skew = replace_na(Skew, 0)) %>%
  mutate(Horizon = as.integer(str_sub(Horizon,6,6))) %>%
  drop_na()

# Pivot wider to put each series in a column to merge with UNEMP data
mxb <- mx %>%
  pivot_longer(cols = -c(Date, Horizon), names_to = "Variable", values_to = "Values") %>%
  unite("Names", Variable:Horizon, sep="") %>%
  pivot_wider(names_from = Names, values_from = Values)

UNEMP <- UNRATE %>%
  group_by(paste(year(Date), quarter(Date))) %>%
  summarise(Date=min(Date), UNRATE=mean(UNRATE)) %>%
  select(Date, UNRATE) %>%
  right_join(mxb, by="Date")

# QReg 
sf <- seq(.05,.95,.15)[-4]

tail_colour   <- "grey95"
centre_colour <- c("maroon4","seagreen")

nq  <- length(sf)
nv  <- length(centre_colour)
col <- colorRampPalette(c(rbind(tail_colour, centre_colour), tail_colour))(nv*nq+1)[-1]

N   <- nrow(UNEMP)

K   <- 12
nt  <- 6

ystart <- 2015
meas   <- c("None", "Var")
setype <- "rank" # "boot" "nid"

ares <- NULL
for (samp in seq(N-K,N,nt)) {
  UNEMPs <- head(UNEMP, samp)
  for (nm in 1:length(meas)) {
    for (i in 0:4) {
      reg  <- c(paste0("av",i))
      if(nm > 1) reg <- c(reg, paste0(meas[nm],i))
      eq   <- formula(paste0("lead(UNRATE,",(i+1),") ~ ", paste(reg, collapse = " + ")))
      eqrq <- rq(eq, data=UNEMPs, tau=sf)
      res  <- broom::tidy(eqrq, se.type=setype) %>% 
        group_by(tau) %>%
        mutate(dta = unlist(c(1, slice(select(UNEMPs, all_of(reg)), n())))) %>% 
        mutate(q   = sum(estimate*dta)) %>% 
        ungroup %>%
        mutate(Vintage = max(UNEMPs$Date), Horizon = i, Dispersion = meas[nm]) 
      ares <- bind_rows(ares, res)
    }
  }
}

aresx <- ares %>% 
  select(tau, q, Vintage, Horizon, Dispersion) %>% 
  distinct() %>%
  left_join(select(UNEMP, Date, UNRATE), by=c("Vintage" = "Date")) %>% 
  mutate(q = if_else(Horizon == 0, UNRATE, q)) %>% 
  pivot_wider(names_from = tau, names_prefix = "tau=", values_from = q) %>% 
  group_by(Dispersion, Vintage) %>% 
  mutate(fdate = seq.Date(from = Vintage[1], by = "quarter", length.out = 5)) %>% 
  ungroup() %>% 
  pivot_longer(starts_with("tau"), names_to = "q", values_to = "Vals") %>% 
  mutate(qs = paste(Dispersion, q)) %>%
  mutate(qs = as_factor(desc(qs))) %>% 
  mutate(Dispersion = as_factor(Dispersion)) %>% 
  select(-UNRATE)

bck <- UNEMP %>% 
  select(Date, UNRATE) %>%
  filter(year(Date) > ystart) %>%
  mutate(Vintage = list(unique(aresx$Vintage)))  %>% 
  unnest(cols = Vintage) %>% 
  group_by(Vintage) %>% 
  filter(Date <= Vintage) %>% 
  ungroup() %>% 
  arrange(Vintage, Date)

ggplot(aresx) +
  geom_area(aes(x=fdate, y=+Inf, group=q), fill=tail_colour, position = "identity") +
  geom_area(aes(x=fdate, y=Vals, group=qs, fill=qs), position = "identity") +
  scale_fill_manual(values=col) +
  theme_minimal() + 
  theme(legend.position = "none") +
  scale_x_date(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_line(data=bck, aes(x=Date, y=UNRATE), colour="grey44") + 
  facet_grid(Vintage ~ Dispersion) +
  labs(title=paste("US unemployment rate and forecast fanchart"), y="", x="") 
