# Cox-Snell residuals are a type of standardized residuals used in reliability analysis. 
# A residual is the difference between an observed data point and a predicted or fitted 
# value. A Cox-Snell residual considers the distribution and estimated parameters from 
# the lifetime regression model.

# The Cox-Snell residuals are equal to the negative of the natural log of the survival 
# probability for each observation.

# 
library(afttest)
library(survival)

library(flexsurv)
library(goftest)

# Load R code
setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/source")
source("afttest_source_r.R")

Scenario = 11

N = 300

# Type 2 error check
gamma_0 = 0
# gamma_0 = 0.1
# gamma_0 = 0.2
# gamma_0 = 0.3
# gamma_0 = 0.4
# gamma_0 = 0.5

# Type 1 error control
alpha = 0.05

aa = numeric(1000)
bb = numeric(1000)
cc = numeric(1000)
for (s in 1:length(aa)) {
  # ------------------------------------------------------------------------------
  temp_data = generate_data(N,gamma_0,Scenario)
  X = temp_data$X
  D = temp_data$D
  Z = temp_data$Z
  # # fit = flexsurvreg(Surv(X,D)~Z, data = temp_data, na.action = "na.omit", dist = "lognormal")
  # # fitresid = c(log(X) - cbind(1,Z) %*% fit$coefficients[-2])
  # fit = survreg(Surv(X,D)~Z, data = temp_data, na.action = "na.omit", dist = "lognormal")
  # fitresid = c(log(X) - cbind(1,Z) %*% fit$coefficients)
  # # fitresid = fitresid[which(D==1)]
  # a = ks.test(scale(fitresid), 'pnorm')$p.value;a
  # b = ad.test(scale(fitresid), 'pnorm')$p.value;b
  # c = cvm.test(scale(fitresid), 'pnorm')$p.value;c

  fit = flexsurvreg(Surv(X,D)~Z, data = temp_data, na.action = "na.omit", dist = "exp")
  fitresid = coxsnell_flexsurvreg(fit)$est
  # fitresid = fitresid[which(D==1)]
  # fitresid[which(D==0)] = fitresid[which(D==0)] + 1
  # fitresid = fitresid/mean(fitresid)

  a = ks.test(fitresid, "pexp")$p.value;a
  b = ad.test(fitresid, 'pexp')$p.value;b
  c = cvm.test(fitresid, 'pexp')$p.value;c

  # fitresid = exp(-fitresid)
  # 
  # a = ks.test(fitresid, "punif")$p.value;a
  # b = ad.test(fitresid, 'punif')$p.value;b
  # c = cvm.test(fitresid, 'punif')$p.value;c
  
  aa[s] = a
  bb[s] = b
  cc[s] = c
}

# Since our p-value is less than alpha, we can reject the null hypothesis and conclude 
# that we have sufficient evidence to say this data does not follow a normal distribution.
rraa = length(which(aa<alpha))/length(aa);rraa
rrbb = length(which(bb<alpha))/length(bb);rrbb
rrcc = length(which(cc<alpha))/length(cc);rrcc

# plot(density(fitresid[D==1]),xlim=c(0,5));
# plot(density(rexp(1e5)),xlim=c(0,5))
# plot(sort(exp(-fitresid)))





