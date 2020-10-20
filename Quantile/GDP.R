## Simple implementation of _GDP at Risk_ using R 

library(jsonlite)          # Read in UK data
library(zoo)               # Date stuff
library(quantreg)          # Quantile regression
library(ggridges)          # Ridgeline plots
library(viridis)           # Colours for graphs
library(fitdistrplus)      # Package to fit parametric distributions
library(sn)                # Skew-t distribution
library(tidyverse)         # Usual
library(lubridate)         # More usual
library(readxl)            # And more...

json <- fromJSON("ihyr.json")  # Use jsonlite to parse file

# Retrieve quarterly data, dates etc and calculate lags
qdata <- json$quarters %>% 
  mutate(Date   = as.Date(as.yearqtr(date, format = "%Y Q%q")), 
         Growth = as.numeric(value)) %>%
  select(Date, Growth) %>%
  mutate(Growth_1 = lag(Growth, 1), 
         Growth_4 = lag(Growth, 4)) %>%
  drop_na()

totcredit <- read_excel("totcredit.xlsx", 
                        sheet = "Quarterly Series",
                        col_types = c("date", rep("text", 1130))) %>%
  select(Date = "Back to menu", starts_with("United K")) %>%            # Find UK
  slice(-c(1:3)) %>% 
  select(-contains("US Dollar"), -contains("Unadjusted"), -contains("Domestic currency")) %>%
  mutate(Date = as.Date(Date)) 

nn  <- gsub("United Kingdom - ", "", names(totcredit))
nn  <- gsub(" - Adjusted for breaks", "", nn)
nn  <- gsub(" - Percentage of GDP", "", nn)
nn  <- gsub(" at Market value", "", nn)

totcredit <- rename_with(totcredit, ~ nn)

### Plots of pivoted data

dd <- totcredit %>%
  pivot_longer(cols=-Date, names_to = "Var", values_to = "Val") %>%
  mutate(Val = as.numeric(Val)) %>%
  filter(!is.na(Val))

ggplot(dd) + 
  geom_line(aes(x=Date, y=Val, group=Var, colour=Var), show.legend = FALSE) +
  facet_wrap(~Var, scales = "free") + 
  theme_minimal() +
  labs(x="", y="", title="Credit data; all as percentage of GDP")

### Difference data at required interval 

lagv <- 20

dd2 <- dd %>% 
  group_by(Var) %>% 
  mutate(Val = 100*(Val/lag(Val,lagv)-1)) %>% 
  ungroup() 

ggplot(dd2) + 
  geom_line(aes(x=Date, y=Val, group=Var, colour=Var), show.legend = FALSE) +
  facet_wrap(~Var, scales = "free") + 
  theme_minimal() +
  labs(x="",y="", title=paste("Credit data; Percentage difference over", lagv, "quarters"))

### Choose a variable

# Recall all the names are in nn
print(nn) # Choose 7

dd2a <- filter(dd2, Var == nn[5]) %>% 
  select(Date, Val) %>% 
  rename_with(~ c("Date", "GCredit")) %>% 
  mutate(Date = floor_date(Date, unit="quarter")) %>% 
  arrange(Date)

# Quick plot to check we have the right one
ggplot(dd2a) + 
  geom_line(aes(x=Date, y=GCredit), color = "red") +
  theme_minimal()

dataz <- left_join(qdata, dd2a, by="Date") %>% 
  mutate(GCredit_1 = lag(GCredit,1)) %>% 
  mutate(GCredit_4 = lag(GCredit,4)) %>% 
  drop_na()

head(dataz)

## Equation and estimates

fcast <- 4
inccg <- 1

if (inccg > 0) {
  eqn.q <- formula(paste0("Growth ~ Growth_", fcast, " + GCredit_", fcast))
} else {
  eqn.q <- formula(paste0("Growth ~ Growth_", fcast))  
}

q.inst <- rq(eqn.q, data=dataz, tau=seq(.05,.95,.025))
summary(q.inst)

## Non-parametric results

q.predict <- t(predict(q.inst)) %>%           # In-sample predictions
  as_tibble(.name_repair = "unique") %>% 
  rename_with(~ as.character(dataz$Date)) %>%
  pivot_longer(everything(), names_to = "Date", values_to = "Vals") %>%
  mutate(Date = as.Date(Date)) %>% 
  filter(lubridate::year(Date) > 1003)

ggplot(q.predict, aes(x=Vals, y=Date, group=Date)) + 
  geom_density_ridges(scale=5, colour="grey77", fill="slateblue1") +
  theme_ridges() + 
  labs(x="", y="", title = "GDP@Risk: Non-parametric density estimates")

ggplot(q.predict, aes(x=Vals, y=Date, group=Date, fill = 0.5-abs(0.5-stat(ecdf)))) + 
  stat_density_ridges(geom="density_ridges_gradient", calc_ecdf=TRUE, scale=5, colour="grey77") +
  scale_fill_viridis(option="D", direction=-1, alpha=.7) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  labs(x="", y="", title = "GDP@Risk: Non-parametric density estimates")

### Parametric results

dens <- NULL # Store densities
val5 <- NULL # Store 5% info
eall <- NULL # Store estimated parameters

x   <- seq(-5,7,0.05)                        # Evaluate fitted density over this interval

st  <- list(xi=2, omega=1, alpha=0, tau=0)
for (i in unique(q.predict$Date)) {
  
  pp  <- filter(q.predict, Date==i)                             # Predicted vals for i
  
  fsn <- fitdist(pp$Vals, "sn", method="mge", start=st)         # Fit the skew t
  e   <- fsn$estimate                                           # Fitted values
  y   <- dsn(x,    xi=e[1], omega=e[2], alpha=e[3], tau=e[4])   # Fitted density
  vr  <- qsn(0.05, xi=e[1], omega=e[2], alpha=e[3], tau=e[4])   # 5% quantile
  dr  <- dsn(vr,   xi=e[1], omega=e[2], alpha=e[3], tau=e[4])   # Density at that point
  
  dens <- bind_rows(dens, tibble(x=x,   y=y,   Date=i))
  val5 <- bind_rows(val5, tibble(vr=vr, dr=dr, Date=i))
  eall <- bind_rows(eall, tibble(Date=as.Date(i), xi=e[1], omega=e[2], alpha=e[3], tau=e[4]))
  
}

sc   <- 1750                                    # Scale factor

ggplot(dens) + 
  geom_ridgeline(aes(x=x, height=y, y=Date, group=Date), 
                 colour="grey77", fill="slateblue1", scale=sc) +
  geom_point(data=val5, aes(x = vr, y = Date), color="red", size=1.1) + 
  theme_ridges() + 
  labs(x="", y="", title = "GDP@Risk: Fitted skew-t")


dens <- mutate(dens, Datef=as.numeric(Date))    # Dates as numbers
val5 <- mutate(val5, Datef=as.numeric(Date))

ggplot(dens) + 
  geom_ridgeline(aes(x=x, height=y, y=Datef, group=Date),
                 colour="grey77", fill="slateblue1", scale=sc) +
  geom_segment(data=val5, aes(x=vr, xend=vr, y=Datef, yend=Datef+sc*dr), 
               color="red", size=0.75) + 
  theme_ridges() + 
  labs(x="", y="", title = "GDP@Risk: Fitted skew-t") +  
  theme(axis.text.y = element_blank()) 

