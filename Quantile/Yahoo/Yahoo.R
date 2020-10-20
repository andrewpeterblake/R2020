library(readr)
library(tidyverse)
library(lubridate)

X_FTSE_4 <- read_csv("Yahoo/^FTSE_4.csv")
X_FTSE_3 <- read_csv("Yahoo/^FTSE_3.csv")
X_FTSE_2 <- read_csv("Yahoo/^FTSE_2.csv")
X_FTSE_1 <- read_csv("Yahoo/^FTSE_1.csv")
FTSE     <- full_join(X_FTSE_1, X_FTSE_2) %>% 
  full_join(X_FTSE_3) %>%
  full_join(X_FTSE_4) %>% 
  select(Date, FTSE = Close)
VIX      <- read_csv("Yahoo/^VIX.csv") %>% 
  select(Date, VIX = Close)

HSBC     <- read_csv("Yahoo/HSBA.L.csv", na="null") %>% 
  select(Date, HSBC = Close)
LLOY     <- read_csv("Yahoo/LLOY.L.csv") %>% 
  select(Date, LLOY = Close)
BARC     <- read_csv("Yahoo/BARC.L.csv") %>% 
  select(Date, BARC = Close)

Banks    <- full_join(BARC, HSBC) %>%
  full_join(LLOY)

Data     <-  full_join(FTSE, VIX) 

B <- Banks %>% 
  pivot_longer(cols = -Date, names_to = "Variables", values_to = "Values") %>% 
  filter(year(Date) > 1999) %>% 
  mutate(Variables = factor(Variables, levels = unique(Variables)))

ggplot(B) + 
  geom_line(aes(x=Date, y=Values, color=Variables), size=1.2, show.legend = FALSE) +
  facet_wrap(~ Variables, ncol = 3, scales = "free_y") + 
  theme_minimal() + 
  labs(title= "CoVaR data", x="", y= "", caption = "Source: Yahoo Finance")

