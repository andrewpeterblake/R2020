# Difference in Differences

library(dplyr)       # We will filter the data
library(wooldridge)  # All Wooldridge datasets

data("kielmc")

eq1981 <- lm(rprice ~ nearinc, data=filter(kielmc, year==1981))
t1981  <- summary(eq1981)

eq1978 <- lm(rprice ~ nearinc, data=filter(kielmc, year==1978))
t1978  <- summary(eq1978)

coef(eq1981)["nearinc"] - coef(eq1978)["nearinc"]

# Estimating directly

eqdid  <- lm(rprice ~ y81 + nearinc + y81*nearinc, data=kielmc)
summary(eqdid)

eqdid2  <- lm(rprice ~ y81 + nearinc + y81*nearinc + age + agesq, data=kielmc)
summary(eqdid2)

seqdid3 <- lm(rprice ~ y81 + nearinc + y81*nearinc + age + agesq + intst + land + area + rooms + baths, data=kielmc)
broom::glance(seqdid3)

cval <- coef(seqdid3)["y81:nearinc"]
vval <- sqrt(vcov(seqdid3)["y81:nearinc","y81:nearinc"])

y81nearinc <- matrix(c(cval, vval, abs(cval/vval)),1,3)
colnames(y81nearinc) <- c("Coef.", "Std Err", "t-value")
y81nearinc

