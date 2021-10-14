# EDA

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gtsummary)
library(officer)
library(flextable)

hrt <- read.csv("LVF assessment.csv")
attach(hrt)

# Multiple histograms
par(mfrow=c(3, 3))
colnames <- dimnames(hrt)[[2]]
for (i in 4:6) {
  hist(hrt[,i], xlim=c(0, 3500), breaks=seq(0, 3500, 100), main=colnames[i], probability=TRUE, col="gray", border="white")
}
# NOT WORKING!!!!

hist(age)
shapiro.test(age) # Normal

hist(sbp)
shapiro.test(sbp) # NOT

hist(dbp)
shapiro.test(dbp) # NOT

hist(hr)
shapiro.test(hr) # NOT

hist(troponin)
shapiro.test(troponin) # NOT

hist(hb)
shapiro.test(hb) # NOT

hist(simpsons_LVESV)
shapiro.test(simpsons_LVESV) # NOT

hist(simpsons_LVEDV)
shapiro.test(simpsons_LVEDV) # NOT

hist(simpsons_LVEF)
shapiro.test(simpsons_LVEF) # NOT

hist(total_gls)
shapiro.test(total_gls) # NOT

#bdpv::BDtest(dta, pr = 0.25, conf.level = 0.90) # not working
