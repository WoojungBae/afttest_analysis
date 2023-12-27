#-------------------------------------------------------------
#---------------------------SETTING---------------------------
#-------------------------------------------------------------
library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(survival)
library(ggplot2)
library(gridExtra)
library(aftgee)
library(afttest)
library(knitr)
library(kableExtra)

#-------------------------------------------------------------
#---------------------------- DATA ---------------------------
#-------------------------------------------------------------
set.seed(1)

D_pbc = pbc$status; D_pbc[which(D_pbc==1)]=0; D_pbc[which(D_pbc==2)]=1
X_pbc = pbc$time

trt  = pbc$trt-1
edem = pbc$edem
age  = pbc$age; log_age=log(age)
bili = pbc$bili; log_bili=log(bili)
prot = pbc$prot; log_prot=log(prot)
albu = pbc$albu; log_albu=log(albu)

path = 5e2

# ------------------------------------------------------------------------------
# ------------------------------------ "mns" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------- Covariates: bili+prot+albu+age+edem+trt ------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result01_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="omni",eqType="mns")
result01_afttest_omni_mns$p_value
result01_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,stdType="unstd")
# afttestplot(result01_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
result01_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="link",eqType="mns")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,stdType="unstd")
# afttestplot(result01_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
result01_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="bili")
result01_afttest_form1_mns$p_value
result01_afttest_form1_mns$p_std_value
# afttestplot(result01_afttest_form1_mns,stdType="unstd")
# afttestplot(result01_afttest_form1_mns,stdType="std")

result01_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="prot")
result01_afttest_form2_mns$p_value
result01_afttest_form2_mns$p_std_value
# afttestplot(result01_afttest_form2_mns,stdType="unstd")
# afttestplot(result01_afttest_form2_mns,stdType="std")

result01_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="albu")
result01_afttest_form3_mns$p_value
result01_afttest_form3_mns$p_std_value
# afttestplot(result01_afttest_form3_mns,stdType="unstd")
# afttestplot(result01_afttest_form3_mns,stdType="std")

result01_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="age")
result01_afttest_form4_mns$p_value
result01_afttest_form4_mns$p_std_value
# afttestplot(result01_afttest_form4_mns,stdType="unstd")
# afttestplot(result01_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------------- Covariates: log_bili+prot+albu+age+edem+trt ----------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result02_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="omni",eqType="mns")
result02_afttest_omni_mns$p_value
result02_afttest_omni_mns$p_std_value
# afttestplot(result02_afttest_omni_mns,stdType="unstd")
# afttestplot(result02_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
result02_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="link",eqType="mns")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result02_afttest_link_mns,stdType="unstd")
# afttestplot(result02_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
result02_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="log_bili")
result02_afttest_form1_mns$p_value
result02_afttest_form1_mns$p_std_value
# afttestplot(result02_afttest_form1_mns,stdType="unstd")
# afttestplot(result02_afttest_form1_mns,stdType="std")

result02_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="prot")
result02_afttest_form2_mns$p_value
result02_afttest_form2_mns$p_std_value
# afttestplot(result02_afttest_form2_mns,stdType="unstd")
# afttestplot(result02_afttest_form2_mns,stdType="std")

result02_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="albu")
result02_afttest_form3_mns$p_value
result02_afttest_form3_mns$p_std_value
# afttestplot(result02_afttest_form3_mns,stdType="unstd")
# afttestplot(result02_afttest_form3_mns,stdType="std")

result02_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mns",form="age")
result02_afttest_form4_mns$p_value
result02_afttest_form4_mns$p_std_value
# afttestplot(result02_afttest_form4_mns,stdType="unstd")
# afttestplot(result02_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------------------------------------ "mis" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# ------------------- Covariates: bili+prot+albu+age+edem+trt ------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result01_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="omni",eqType="mis")
result01_afttest_omni_mis$p_value
result01_afttest_omni_mis$p_std_value
# afttestplot(result01_afttest_omni_mis,stdType="unstd")
# afttestplot(result01_afttest_omni_mis_mis,stdType="std")

# ------------------------------------ link ------------------------------------
result01_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="link",eqType="mis")
result01_afttest_link_mis$p_value
result01_afttest_link_mis$p_std_value
# afttestplot(result01_afttest_link_mis,stdType="unstd")
# afttestplot(result01_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
result01_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="bili")
result01_afttest_form1_mis$p_value
result01_afttest_form1_mis$p_std_value
# afttestplot(result01_afttest_form1_mis,stdType="unstd")
# afttestplot(result01_afttest_form1_mis,stdType="std")

result01_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="prot")
result01_afttest_form2_mis$p_value
result01_afttest_form2_mis$p_std_value
# afttestplot(result01_afttest_form2_mis,stdType="unstd")
# afttestplot(result01_afttest_form2_mis,stdType="std")

result01_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="albu")
result01_afttest_form3_mis$p_value
result01_afttest_form3_mis$p_std_value
# afttestplot(result01_afttest_form3_mis,stdType="unstd")
# afttestplot(result01_afttest_form3_mis,stdType="std")

result01_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="age")
result01_afttest_form4_mis$p_value
result01_afttest_form4_mis$p_std_value
# afttestplot(result01_afttest_form4_mis,stdType="unstd")
# afttestplot(result01_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------------- Covariates: log_bili+prot+albu+age+edem+trt ----------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
result02_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="omni",eqType="mis")
result02_afttest_omni_mis$p_value
result02_afttest_omni_mis$p_std_value
# afttestplot(result02_afttest_omni_mis,stdType="unstd")
# afttestplot(result02_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
result02_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="link",eqType="mis")
result02_afttest_link_mis$p_value
result02_afttest_link_mis$p_std_value
# afttestplot(result02_afttest_link_mis,stdType="unstd")
# afttestplot(result02_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
result02_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="log_bili")
result02_afttest_form1_mis$p_value
result02_afttest_form1_mis$p_std_value
# afttestplot(result02_afttest_form1_mis,stdType="unstd")
# afttestplot(result02_afttest_form1_mis,stdType="std")

result02_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="prot")
result02_afttest_form2_mis$p_value
result02_afttest_form2_mis$p_std_value
# afttestplot(result02_afttest_form2_mis,stdType="unstd")
# afttestplot(result02_afttest_form2_mis,stdType="std")

result02_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="albu")
result02_afttest_form3_mis$p_value
result02_afttest_form3_mis$p_std_value
# afttestplot(result02_afttest_form3_mis,stdType="unstd")
# afttestplot(result02_afttest_form3_mis,stdType="std")

result02_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=path,pathsave=50,testType="form",eqType="mis",form="age")
result02_afttest_form4_mis$p_value
result02_afttest_form4_mis$p_std_value
# afttestplot(result02_afttest_form4_mis,stdType="unstd")
# afttestplot(result02_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
beta01mns = aftsrr(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,eqType="mns",se="ISMB")
beta01mis = aftsrr(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,eqType="mis",se="ISMB")
summary(beta01mns)
cbind(coef(beta01mns) - 1.96 * sqrt(diag(vcov(beta01mns)$ISMB)),
      coef(beta01mns) + 1.96 * sqrt(diag(vcov(beta01mns)$ISMB)))
summary(beta01mis)
cbind(coef(beta01mis) - 1.96 * sqrt(diag(vcov(beta01mis)$ISMB)),
      coef(beta01mis) + 1.96 * sqrt(diag(vcov(beta01mis)$ISMB)))

beta02mns = aftsrr(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,eqType="mns",se="ISMB")
beta02mis = aftsrr(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,eqType="mis",se="ISMB")
summary(beta02mns)
cbind(coef(beta02mns) - 1.96 * sqrt(diag(vcov(beta02mns)$ISMB)),
      coef(beta02mns) + 1.96 * sqrt(diag(vcov(beta02mns)$ISMB)))
summary(beta02mis)
cbind(coef(beta02mis) - 1.96 * sqrt(diag(vcov(beta02mis)$ISMB)),
      coef(beta02mis) + 1.96 * sqrt(diag(vcov(beta02mis)$ISMB)))

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
filename = paste0("Results/PBCpackage.RData")
save.image(file = filename)
# setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/RealDataAnalysis/PBCpackage")
# save.image("C:/Users/WooJung/Documents/Rproject/afttest_analysis/RealDataAnalysis/PBCpackage/package_pbc0102.RData")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
{
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  cairo_ps("Results/pbc01_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_omni_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_omni_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_link_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_link_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form1_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form1_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form2_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form2_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form3_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form3_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form4_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form4_mis,stdType="std")
  dev.off()
  
  # ------------------------------------------------------------------------------
  cairo_ps("Results/pbc02_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_omni_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_omni_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_link_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_link_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form1_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form1_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form2_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form2_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form3_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form3_mis,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form4_mis,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form4_mis,stdType="std")
  dev.off()
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  cairo_ps("Results/pbc01_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_omni_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_omni_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_link_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_link_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form1_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form1_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form2_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form2_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form3_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form3_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc01_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form4_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc01_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result01_afttest_form4_mns,stdType="std")
  dev.off()
  
  # ------------------------------------------------------------------------------
  cairo_ps("Results/pbc02_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_omni_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_omni_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_link_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_link_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form1_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form1_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form2_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form2_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form3_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form3_mns,stdType="std")
  dev.off()
  cairo_ps("Results/pbc02_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form4_mns,stdType="unstd")
  dev.off()
  cairo_ps("Results/pbc02_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 300)
  afttestplot(result02_afttest_form4_mns,stdType="std")
  dev.off()
}
