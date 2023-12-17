#-------------------------------------------------------------
#---------------------------SETTING---------------------------
#-------------------------------------------------------------
#rm(list=ls());gc();

options(max.print=999999)
options(error=NULL)

# install.packages("devtools")
# install.packages("Rcpp")
# install.packages("RcppArmadillo")
# install.packages("survival")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("aftgee")
# install_github("WoojungBae/afttest")

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

# ------------------------------------------------------------------------------
# ------------------------------------ "mns" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------- Covariates: bili+prot+albu+age+edem+trt ---------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result01_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result01_afttest_omni_mns$p_value
result01_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,stdType="unstd")
# afttestplot(result01_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result01_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,stdType="unstd")
# afttestplot(result01_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result01_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result01_afttest_form1_mns$p_value
result01_afttest_form1_mns$p_std_value
# afttestplot(result01_afttest_form1_mns,stdType="unstd")
# afttestplot(result01_afttest_form1_mns,stdType="std")

# result01_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result01_afttest_form2_mns$p_value
result01_afttest_form2_mns$p_std_value
# afttestplot(result01_afttest_form2_mns,stdType="unstd")
# afttestplot(result01_afttest_form2_mns,stdType="std")

# result01_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result01_afttest_form3_mns$p_value
result01_afttest_form3_mns$p_std_value
# afttestplot(result01_afttest_form3_mns,stdType="unstd")
# afttestplot(result01_afttest_form3_mns,stdType="std")

# result01_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result01_afttest_form4_mns$p_value
result01_afttest_form4_mns$p_std_value
# afttestplot(result01_afttest_form4_mns,stdType="unstd")
# afttestplot(result01_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: log_bili+prot+albu+age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result02_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result02_afttest_omni_mns$p_value
result02_afttest_omni_mns$p_std_value
# afttestplot(result02_afttest_omni_mns,stdType="unstd")
# afttestplot(result02_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result02_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
result02_afttest_link_mns$p_value
result02_afttest_link_mns$p_std_value
# afttestplot(result02_afttest_link_mns,stdType="unstd")
# afttestplot(result02_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result02_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result02_afttest_form1_mns$p_value
result02_afttest_form1_mns$p_std_value
# afttestplot(result02_afttest_form1_mns,stdType="unstd")
# afttestplot(result02_afttest_form1_mns,stdType="std")

# result02_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result02_afttest_form2_mns$p_value
result02_afttest_form2_mns$p_std_value
# afttestplot(result02_afttest_form2_mns,stdType="unstd")
# afttestplot(result02_afttest_form2_mns,stdType="std")

# result02_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result02_afttest_form3_mns$p_value
result02_afttest_form3_mns$p_std_value
# afttestplot(result02_afttest_form3_mns,stdType="unstd")
# afttestplot(result02_afttest_form3_mns,stdType="std")

# result02_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result02_afttest_form4_mns$p_value
result02_afttest_form4_mns$p_std_value
# afttestplot(result02_afttest_form4_mns,stdType="unstd")
# afttestplot(result02_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+log_prot+albu+age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result03_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result03_afttest_omni_mns$p_value
result03_afttest_omni_mns$p_std_value
# afttestplot(result03_afttest_omni_mns,stdType="unstd")
# afttestplot(result03_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result03_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
result03_afttest_link_mns$p_value
result03_afttest_link_mns$p_std_value
# afttestplot(result03_afttest_link_mns,stdType="unstd")
# afttestplot(result03_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result03_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result03_afttest_form1_mns$p_value
result03_afttest_form1_mns$p_std_value
# afttestplot(result03_afttest_form1_mns,stdType="unstd")
# afttestplot(result03_afttest_form1_mns,stdType="std")

# result03_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result03_afttest_form2_mns$p_value
result03_afttest_form2_mns$p_std_value
# afttestplot(result03_afttest_form2_mns,stdType="unstd")
# afttestplot(result03_afttest_form2_mns,stdType="std")

# result03_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result03_afttest_form3_mns$p_value
result03_afttest_form3_mns$p_std_value
# afttestplot(result03_afttest_form3_mns,stdType="unstd")
# afttestplot(result03_afttest_form3_mns,stdType="std")

# result03_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result03_afttest_form4_mns$p_value
result03_afttest_form4_mns$p_std_value
# afttestplot(result03_afttest_form4_mns,stdType="unstd")
# afttestplot(result03_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+prot+log_albu+age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result04_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result04_afttest_omni_mns$p_value
result04_afttest_omni_mns$p_std_value
# afttestplot(result04_afttest_omni_mns,stdType="unstd")
# afttestplot(result04_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result04_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
result04_afttest_link_mns$p_value
result04_afttest_link_mns$p_std_value
# afttestplot(result04_afttest_link_mns,stdType="unstd")
# afttestplot(result04_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result04_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result04_afttest_form1_mns$p_value
result04_afttest_form1_mns$p_std_value
# afttestplot(result04_afttest_form1_mns,stdType="unstd")
# afttestplot(result04_afttest_form1_mns,stdType="std")

# result04_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result04_afttest_form2_mns$p_value
result04_afttest_form2_mns$p_std_value
# afttestplot(result04_afttest_form2_mns,stdType="unstd")
# afttestplot(result04_afttest_form2_mns,stdType="std")

# result04_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result04_afttest_form3_mns$p_value
result04_afttest_form3_mns$p_std_value
# afttestplot(result04_afttest_form3_mns,stdType="unstd")
# afttestplot(result04_afttest_form3_mns,stdType="std")

# result04_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result04_afttest_form4_mns$p_value
result04_afttest_form4_mns$p_std_value
# afttestplot(result04_afttest_form4_mns,stdType="unstd")
# afttestplot(result04_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+prot+albu+log_age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result05_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result05_afttest_omni_mns$p_value
result05_afttest_omni_mns$p_std_value
# afttestplot(result05_afttest_omni_mns,stdType="unstd")
# afttestplot(result05_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result05_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result05_afttest_link_mns$p_value
result05_afttest_link_mns$p_std_value
# afttestplot(result05_afttest_link_mns,stdType="unstd")
# afttestplot(result05_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result05_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result05_afttest_form1_mns$p_value
result05_afttest_form1_mns$p_std_value
# afttestplot(result05_afttest_form1_mns,stdType="unstd")
# afttestplot(result05_afttest_form1_mns,stdType="std")

# result05_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result05_afttest_form2_mns$p_value
result05_afttest_form2_mns$p_std_value
# afttestplot(result05_afttest_form2_mns,stdType="unstd")
# afttestplot(result05_afttest_form2_mns,stdType="std")

# result05_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result05_afttest_form3_mns$p_value
result05_afttest_form3_mns$p_std_value
# afttestplot(result05_afttest_form3_mns,stdType="unstd")
# afttestplot(result05_afttest_form3_mns,stdType="std")

# result05_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result05_afttest_form4_mns$p_value
result05_afttest_form4_mns$p_std_value
# afttestplot(result05_afttest_form4_mns,stdType="unstd")
# afttestplot(result05_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+log_prot+albu+age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result06_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result06_afttest_omni_mns$p_value
result06_afttest_omni_mns$p_std_value
# afttestplot(result06_afttest_omni_mns,stdType="unstd")
# afttestplot(result06_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result06_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
result06_afttest_link_mns$p_value
result06_afttest_link_mns$p_std_value
# afttestplot(result06_afttest_link_mns,stdType="unstd")
# afttestplot(result06_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result06_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result06_afttest_form1_mns$p_value
result06_afttest_form1_mns$p_std_value
# afttestplot(result06_afttest_form1_mns,stdType="unstd")
# afttestplot(result06_afttest_form1_mns,stdType="std")

# result06_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result06_afttest_form2_mns$p_value
result06_afttest_form2_mns$p_std_value
# afttestplot(result06_afttest_form2_mns,stdType="unstd")
# afttestplot(result06_afttest_form2_mns,stdType="std")

# result06_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result06_afttest_form3_mns$p_value
result06_afttest_form3_mns$p_std_value
# afttestplot(result06_afttest_form3_mns,stdType="unstd")
# afttestplot(result06_afttest_form3_mns,stdType="std")

# result06_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result06_afttest_form4_mns$p_value
result06_afttest_form4_mns$p_std_value
# afttestplot(result06_afttest_form4_mns,stdType="unstd")
# afttestplot(result06_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+prot+log_albu+age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result07_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result07_afttest_omni_mns$p_value
result07_afttest_omni_mns$p_std_value
# afttestplot(result07_afttest_omni_mns,stdType="unstd")
# afttestplot(result07_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result07_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
result07_afttest_link_mns$p_value
result07_afttest_link_mns$p_std_value
# afttestplot(result07_afttest_link_mns,stdType="unstd")
# afttestplot(result07_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result07_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result07_afttest_form1_mns$p_value
result07_afttest_form1_mns$p_std_value
# afttestplot(result07_afttest_form1_mns,stdType="unstd")
# afttestplot(result07_afttest_form1_mns,stdType="std")

# result07_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result07_afttest_form2_mns$p_value
result07_afttest_form2_mns$p_std_value
# afttestplot(result07_afttest_form2_mns,stdType="unstd")
# afttestplot(result07_afttest_form2_mns,stdType="std")

# result07_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result07_afttest_form3_mns$p_value
result07_afttest_form3_mns$p_std_value
# afttestplot(result07_afttest_form3_mns,stdType="unstd")
# afttestplot(result07_afttest_form3_mns,stdType="std")

# result07_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result07_afttest_form4_mns$p_value
result07_afttest_form4_mns$p_std_value
# afttestplot(result07_afttest_form4_mns,stdType="unstd")
# afttestplot(result07_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+prot+albu+log_age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result08_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result08_afttest_omni_mns$p_value
result08_afttest_omni_mns$p_std_value
# afttestplot(result08_afttest_omni_mns,stdType="unstd")
# afttestplot(result08_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result08_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result08_afttest_link_mns$p_value
result08_afttest_link_mns$p_std_value
# afttestplot(result08_afttest_link_mns,stdType="unstd")
# afttestplot(result08_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result08_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result08_afttest_form1_mns$p_value
result08_afttest_form1_mns$p_std_value
# afttestplot(result08_afttest_form1_mns,stdType="unstd")
# afttestplot(result08_afttest_form1_mns,stdType="std")

# result08_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result08_afttest_form2_mns$p_value
result08_afttest_form2_mns$p_std_value
# afttestplot(result08_afttest_form2_mns,stdType="unstd")
# afttestplot(result08_afttest_form2_mns,stdType="std")

# result08_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result08_afttest_form3_mns$p_value
result08_afttest_form3_mns$p_std_value
# afttestplot(result08_afttest_form3_mns,stdType="unstd")
# afttestplot(result08_afttest_form3_mns,stdType="std")

# result08_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result08_afttest_form4_mns$p_value
result08_afttest_form4_mns$p_std_value
# afttestplot(result08_afttest_form4_mns,stdType="unstd")
# afttestplot(result08_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_prot+log_albu+age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result09_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result09_afttest_omni_mns$p_value
result09_afttest_omni_mns$p_std_value
# afttestplot(result09_afttest_omni_mns,stdType="unstd")
# afttestplot(result09_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result09_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
result09_afttest_link_mns$p_value
result09_afttest_link_mns$p_std_value
# afttestplot(result09_afttest_link_mns,stdType="unstd")
# afttestplot(result09_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result09_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result09_afttest_form1_mns$p_value
result09_afttest_form1_mns$p_std_value
# afttestplot(result09_afttest_form1_mns,stdType="unstd")
# afttestplot(result09_afttest_form1_mns,stdType="std")

# result09_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result09_afttest_form2_mns$p_value
result09_afttest_form2_mns$p_std_value
# afttestplot(result09_afttest_form2_mns,stdType="unstd")
# afttestplot(result09_afttest_form2_mns,stdType="std")

# result09_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result09_afttest_form3_mns$p_value
result09_afttest_form3_mns$p_std_value
# afttestplot(result09_afttest_form3_mns,stdType="unstd")
# afttestplot(result09_afttest_form3_mns,stdType="std")

# result09_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result09_afttest_form4_mns$p_value
result09_afttest_form4_mns$p_std_value
# afttestplot(result09_afttest_form4_mns,stdType="unstd")
# afttestplot(result09_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_prot+albu+log_age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result10_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result10_afttest_omni_mns$p_value
result10_afttest_omni_mns$p_std_value
# afttestplot(result10_afttest_omni_mns,stdType="unstd")
# afttestplot(result10_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result10_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result10_afttest_link_mns$p_value
result10_afttest_link_mns$p_std_value
# afttestplot(result10_afttest_link_mns,stdType="unstd")
# afttestplot(result10_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result10_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result10_afttest_form1_mns$p_value
result10_afttest_form1_mns$p_std_value
# afttestplot(result10_afttest_form1_mns,stdType="unstd")
# afttestplot(result10_afttest_form1_mns,stdType="std")

# result10_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result10_afttest_form2_mns$p_value
result10_afttest_form2_mns$p_std_value
# afttestplot(result10_afttest_form2_mns,stdType="unstd")
# afttestplot(result10_afttest_form2_mns,stdType="std")

# result10_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result10_afttest_form3_mns$p_value
result10_afttest_form3_mns$p_std_value
# afttestplot(result10_afttest_form3_mns,stdType="unstd")
# afttestplot(result10_afttest_form3_mns,stdType="std")

# result10_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result10_afttest_form4_mns$p_value
result10_afttest_form4_mns$p_std_value
# afttestplot(result10_afttest_form4_mns,stdType="unstd")
# afttestplot(result10_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+prot+log_albu+log_age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result11_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result11_afttest_omni_mns$p_value
result11_afttest_omni_mns$p_std_value
# afttestplot(result11_afttest_omni_mns,stdType="unstd")
# afttestplot(result11_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result11_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result11_afttest_link_mns$p_value
result11_afttest_link_mns$p_std_value
# afttestplot(result11_afttest_link_mns,stdType="unstd")
# afttestplot(result11_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result11_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result11_afttest_form1_mns$p_value
result11_afttest_form1_mns$p_std_value
# afttestplot(result11_afttest_form1_mns,stdType="unstd")
# afttestplot(result11_afttest_form1_mns,stdType="std")

# result11_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result11_afttest_form2_mns$p_value
result11_afttest_form2_mns$p_std_value
# afttestplot(result11_afttest_form2_mns,stdType="unstd")
# afttestplot(result11_afttest_form2_mns,stdType="std")

# result11_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result11_afttest_form3_mns$p_value
result11_afttest_form3_mns$p_std_value
# afttestplot(result11_afttest_form3_mns,stdType="unstd")
# afttestplot(result11_afttest_form3_mns,stdType="std")

# result11_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result11_afttest_form4_mns$p_value
result11_afttest_form4_mns$p_std_value
# afttestplot(result11_afttest_form4_mns,stdType="unstd")
# afttestplot(result11_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_prot+log_albu+age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result12_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
result12_afttest_omni_mns$p_value
result12_afttest_omni_mns$p_std_value
# afttestplot(result12_afttest_omni_mns,stdType="unstd")
# afttestplot(result12_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result12_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
result12_afttest_link_mns$p_value
result12_afttest_link_mns$p_std_value
# afttestplot(result12_afttest_link_mns,stdType="unstd")
# afttestplot(result12_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result12_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result12_afttest_form1_mns$p_value
result12_afttest_form1_mns$p_std_value
# afttestplot(result12_afttest_form1_mns,stdType="unstd")
# afttestplot(result12_afttest_form1_mns,stdType="std")

# result12_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result12_afttest_form2_mns$p_value
result12_afttest_form2_mns$p_std_value
# afttestplot(result12_afttest_form2_mns,stdType="unstd")
# afttestplot(result12_afttest_form2_mns,stdType="std")

# result12_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result12_afttest_form3_mns$p_value
result12_afttest_form3_mns$p_std_value
# afttestplot(result12_afttest_form3_mns,stdType="unstd")
# afttestplot(result12_afttest_form3_mns,stdType="std")

# result12_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
result12_afttest_form4_mns$p_value
result12_afttest_form4_mns$p_std_value
# afttestplot(result12_afttest_form4_mns,stdType="unstd")
# afttestplot(result12_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_prot+albu+log_age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result13_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result13_afttest_omni_mns$p_value
result13_afttest_omni_mns$p_std_value
# afttestplot(result13_afttest_omni_mns,stdType="unstd")
# afttestplot(result13_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result13_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result13_afttest_link_mns$p_value
result13_afttest_link_mns$p_std_value
# afttestplot(result13_afttest_link_mns,stdType="unstd")
# afttestplot(result13_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result13_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result13_afttest_form1_mns$p_value
result13_afttest_form1_mns$p_std_value
# afttestplot(result13_afttest_form1_mns,stdType="unstd")
# afttestplot(result13_afttest_form1_mns,stdType="std")

# result13_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result13_afttest_form2_mns$p_value
result13_afttest_form2_mns$p_std_value
# afttestplot(result13_afttest_form2_mns,stdType="unstd")
# afttestplot(result13_afttest_form2_mns,stdType="std")

# result13_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
result13_afttest_form3_mns$p_value
result13_afttest_form3_mns$p_std_value
# afttestplot(result13_afttest_form3_mns,stdType="unstd")
# afttestplot(result13_afttest_form3_mns,stdType="std")

# result13_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result13_afttest_form4_mns$p_value
result13_afttest_form4_mns$p_std_value
# afttestplot(result13_afttest_form4_mns,stdType="unstd")
# afttestplot(result13_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+prot+log_albu+log_age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result14_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result14_afttest_omni_mns$p_value
result14_afttest_omni_mns$p_std_value
# afttestplot(result14_afttest_omni_mns,stdType="unstd")
# afttestplot(result14_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result14_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result14_afttest_link_mns$p_value
result14_afttest_link_mns$p_std_value
# afttestplot(result14_afttest_link_mns,stdType="unstd")
# afttestplot(result14_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result14_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result14_afttest_form1_mns$p_value
result14_afttest_form1_mns$p_std_value
# afttestplot(result14_afttest_form1_mns,stdType="unstd")
# afttestplot(result14_afttest_form1_mns,stdType="std")

# result14_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
result14_afttest_form2_mns$p_value
result14_afttest_form2_mns$p_std_value
# afttestplot(result14_afttest_form2_mns,stdType="unstd")
# afttestplot(result14_afttest_form2_mns,stdType="std")

# result14_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result14_afttest_form3_mns$p_value
result14_afttest_form3_mns$p_std_value
# afttestplot(result14_afttest_form3_mns,stdType="unstd")
# afttestplot(result14_afttest_form3_mns,stdType="std")

# result14_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result14_afttest_form4_mns$p_value
result14_afttest_form4_mns$p_std_value
# afttestplot(result14_afttest_form4_mns,stdType="unstd")
# afttestplot(result14_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: bili+log_prot+log_albu+log_age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result15_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result15_afttest_omni_mns$p_value
result15_afttest_omni_mns$p_std_value
# afttestplot(result15_afttest_omni_mns,stdType="unstd")
# afttestplot(result15_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result15_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result15_afttest_link_mns$p_value
result15_afttest_link_mns$p_std_value
# afttestplot(result15_afttest_link_mns,stdType="unstd")
# afttestplot(result15_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result15_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
result15_afttest_form1_mns$p_value
result15_afttest_form1_mns$p_std_value
# afttestplot(result15_afttest_form1_mns,stdType="unstd")
# afttestplot(result15_afttest_form1_mns,stdType="std")

# result15_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result15_afttest_form2_mns$p_value
result15_afttest_form2_mns$p_std_value
# afttestplot(result15_afttest_form2_mns,stdType="unstd")
# afttestplot(result15_afttest_form2_mns,stdType="std")

# result15_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result15_afttest_form3_mns$p_value
result15_afttest_form3_mns$p_std_value
# afttestplot(result15_afttest_form3_mns,stdType="unstd")
# afttestplot(result15_afttest_form3_mns,stdType="std")

# result15_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result15_afttest_form4_mns$p_value
result15_afttest_form4_mns$p_std_value
# afttestplot(result15_afttest_form4_mns,stdType="unstd")
# afttestplot(result15_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------- Covariates: log_bili+log_prot+log_albu+log_age+edem+trt -------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result16_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
result16_afttest_omni_mns$p_value
result16_afttest_omni_mns$p_std_value
# afttestplot(result16_afttest_omni_mns,stdType="unstd")
# afttestplot(result16_afttest_omni_mns,stdType="std")

# ------------------------------------ link ------------------------------------
# result16_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
result16_afttest_link_mns$p_value
result16_afttest_link_mns$p_std_value
# afttestplot(result16_afttest_link_mns,stdType="unstd")
# afttestplot(result16_afttest_link_mns,stdType="std")

# ------------------------------------ form ------------------------------------
# result16_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
result16_afttest_form1_mns$p_value
result16_afttest_form1_mns$p_std_value
# afttestplot(result16_afttest_form1_mns,stdType="unstd")
# afttestplot(result16_afttest_form1_mns,stdType="std")

# result16_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
result16_afttest_form2_mns$p_value
result16_afttest_form2_mns$p_std_value
# afttestplot(result16_afttest_form2_mns,stdType="unstd")
# afttestplot(result16_afttest_form2_mns,stdType="std")

# result16_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
result16_afttest_form3_mns$p_value
result16_afttest_form3_mns$p_std_value
# afttestplot(result16_afttest_form3_mns,stdType="unstd")
# afttestplot(result16_afttest_form3_mns,stdType="std")

# result16_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
result16_afttest_form4_mns$p_value
result16_afttest_form4_mns$p_std_value
# afttestplot(result16_afttest_form4_mns,stdType="unstd")
# afttestplot(result16_afttest_form4_mns,stdType="std")

# ------------------------------------------------------------------------------
# ------------------------------------ "mis" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------- Covariates: bili+prot+albu+age+edem+trt ---------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result01_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result01_afttest_omni_mis$p_value
result01_afttest_omni_mis$p_std_value
# afttestplot(result01_afttest_omni_mis,stdType="unstd")
# afttestplot(result01_afttest_omni_mis_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result01_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
result01_afttest_link_mis$p_value
result01_afttest_link_mis$p_std_value
# afttestplot(result01_afttest_link_mis,stdType="unstd")
# afttestplot(result01_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result01_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result01_afttest_form1_mis$p_value
result01_afttest_form1_mis$p_std_value
# afttestplot(result01_afttest_form1_mis,stdType="unstd")
# afttestplot(result01_afttest_form1_mis,stdType="std")

# result01_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result01_afttest_form2_mis$p_value
result01_afttest_form2_mis$p_std_value
# afttestplot(result01_afttest_form2_mis,stdType="unstd")
# afttestplot(result01_afttest_form2_mis,stdType="std")

# result01_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result01_afttest_form3_mis$p_value
result01_afttest_form3_mis$p_std_value
# afttestplot(result01_afttest_form3_mis,stdType="unstd")
# afttestplot(result01_afttest_form3_mis,stdType="std")

# result01_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result01_afttest_form4_mis$p_value
result01_afttest_form4_mis$p_std_value
# afttestplot(result01_afttest_form4_mis,stdType="unstd")
# afttestplot(result01_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: log_bili+prot+albu+age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result02_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result02_afttest_omni_mis$p_value
result02_afttest_omni_mis$p_std_value
# afttestplot(result02_afttest_omni_mis,stdType="unstd")
# afttestplot(result02_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result02_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
result02_afttest_link_mis$p_value
result02_afttest_link_mis$p_std_value
# afttestplot(result02_afttest_link_mis,stdType="unstd")
# afttestplot(result02_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result02_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result02_afttest_form1_mis$p_value
result02_afttest_form1_mis$p_std_value
# afttestplot(result02_afttest_form1_mis,stdType="unstd")
# afttestplot(result02_afttest_form1_mis,stdType="std")

# result02_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result02_afttest_form2_mis$p_value
result02_afttest_form2_mis$p_std_value
# afttestplot(result02_afttest_form2_mis,stdType="unstd")
# afttestplot(result02_afttest_form2_mis,stdType="std")

# result02_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result02_afttest_form3_mis$p_value
result02_afttest_form3_mis$p_std_value
# afttestplot(result02_afttest_form3_mis,stdType="unstd")
# afttestplot(result02_afttest_form3_mis,stdType="std")

# result02_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result02_afttest_form4_mis$p_value
result02_afttest_form4_mis$p_std_value
# afttestplot(result02_afttest_form4_mis,stdType="unstd")
# afttestplot(result02_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+log_prot+albu+age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result03_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result03_afttest_omni_mis$p_value
result03_afttest_omni_mis$p_std_value
# afttestplot(result03_afttest_omni_mis,stdType="unstd")
# afttestplot(result03_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result03_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
result03_afttest_link_mis$p_value
result03_afttest_link_mis$p_std_value
# afttestplot(result03_afttest_link_mis,stdType="unstd")
# afttestplot(result03_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result03_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result03_afttest_form1_mis$p_value
result03_afttest_form1_mis$p_std_value
# afttestplot(result03_afttest_form1_mis,stdType="unstd")
# afttestplot(result03_afttest_form1_mis,stdType="std")

# result03_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result03_afttest_form2_mis$p_value
result03_afttest_form2_mis$p_std_value
# afttestplot(result03_afttest_form2_mis,stdType="unstd")
# afttestplot(result03_afttest_form2_mis,stdType="std")

# result03_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result03_afttest_form3_mis$p_value
result03_afttest_form3_mis$p_std_value
# afttestplot(result03_afttest_form3_mis,stdType="unstd")
# afttestplot(result03_afttest_form3_mis,stdType="std")

# result03_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result03_afttest_form4_mis$p_value
result03_afttest_form4_mis$p_std_value
# afttestplot(result03_afttest_form4_mis,stdType="unstd")
# afttestplot(result03_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+prot+log_albu+age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result04_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result04_afttest_omni_mis$p_value
result04_afttest_omni_mis$p_std_value
# afttestplot(result04_afttest_omni_mis,stdType="unstd")
# afttestplot(result04_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result04_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
result04_afttest_link_mis$p_value
result04_afttest_link_mis$p_std_value
# afttestplot(result04_afttest_link_mis,stdType="unstd")
# afttestplot(result04_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result04_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result04_afttest_form1_mis$p_value
result04_afttest_form1_mis$p_std_value
# afttestplot(result04_afttest_form1_mis,stdType="unstd")
# afttestplot(result04_afttest_form1_mis,stdType="std")

# result04_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result04_afttest_form2_mis$p_value
result04_afttest_form2_mis$p_std_value
# afttestplot(result04_afttest_form2_mis,stdType="unstd")
# afttestplot(result04_afttest_form2_mis,stdType="std")

# result04_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result04_afttest_form3_mis$p_value
result04_afttest_form3_mis$p_std_value
# afttestplot(result04_afttest_form3_mis,stdType="unstd")
# afttestplot(result04_afttest_form3_mis,stdType="std")

# result04_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result04_afttest_form4_mis$p_value
result04_afttest_form4_mis$p_std_value
# afttestplot(result04_afttest_form4_mis,stdType="unstd")
# afttestplot(result04_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------------- Covariates: bili+prot+albu+log_age+edem+trt -------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result05_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result05_afttest_omni_mis$p_value
result05_afttest_omni_mis$p_std_value
# afttestplot(result05_afttest_omni_mis,stdType="unstd")
# afttestplot(result05_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result05_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result05_afttest_link_mis$p_value
result05_afttest_link_mis$p_std_value
# afttestplot(result05_afttest_link_mis,stdType="unstd")
# afttestplot(result05_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result05_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result05_afttest_form1_mis$p_value
result05_afttest_form1_mis$p_std_value
# afttestplot(result05_afttest_form1_mis,stdType="unstd")
# afttestplot(result05_afttest_form1_mis,stdType="std")

# result05_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result05_afttest_form2_mis$p_value
result05_afttest_form2_mis$p_std_value
# afttestplot(result05_afttest_form2_mis,stdType="unstd")
# afttestplot(result05_afttest_form2_mis,stdType="std")

# result05_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result05_afttest_form3_mis$p_value
result05_afttest_form3_mis$p_std_value
# afttestplot(result05_afttest_form3_mis,stdType="unstd")
# afttestplot(result05_afttest_form3_mis,stdType="std")

# result05_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result05_afttest_form4_mis$p_value
result05_afttest_form4_mis$p_std_value
# afttestplot(result05_afttest_form4_mis,stdType="unstd")
# afttestplot(result05_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+log_prot+albu+age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result06_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result06_afttest_omni_mis$p_value
result06_afttest_omni_mis$p_std_value
# afttestplot(result06_afttest_omni_mis,stdType="unstd")
# afttestplot(result06_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result06_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
result06_afttest_link_mis$p_value
result06_afttest_link_mis$p_std_value
# afttestplot(result06_afttest_link_mis,stdType="unstd")
# afttestplot(result06_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result06_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result06_afttest_form1_mis$p_value
result06_afttest_form1_mis$p_std_value
# afttestplot(result06_afttest_form1_mis,stdType="unstd")
# afttestplot(result06_afttest_form1_mis,stdType="std")

# result06_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result06_afttest_form2_mis$p_value
result06_afttest_form2_mis$p_std_value
# afttestplot(result06_afttest_form2_mis,stdType="unstd")
# afttestplot(result06_afttest_form2_mis,stdType="std")

# result06_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result06_afttest_form3_mis$p_value
result06_afttest_form3_mis$p_std_value
# afttestplot(result06_afttest_form3_mis,stdType="unstd")
# afttestplot(result06_afttest_form3_mis,stdType="std")

# result06_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result06_afttest_form4_mis$p_value
result06_afttest_form4_mis$p_std_value
# afttestplot(result06_afttest_form4_mis,stdType="unstd")
# afttestplot(result06_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+prot+log_albu+age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result07_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result07_afttest_omni_mis$p_value
result07_afttest_omni_mis$p_std_value
# afttestplot(result07_afttest_omni_mis,stdType="unstd")
# afttestplot(result07_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result07_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
result07_afttest_link_mis$p_value
result07_afttest_link_mis$p_std_value
# afttestplot(result07_afttest_link_mis,stdType="unstd")
# afttestplot(result07_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result07_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result07_afttest_form1_mis$p_value
result07_afttest_form1_mis$p_std_value
# afttestplot(result07_afttest_form1_mis,stdType="unstd")
# afttestplot(result07_afttest_form1_mis,stdType="std")

# result07_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result07_afttest_form2_mis$p_value
result07_afttest_form2_mis$p_std_value
# afttestplot(result07_afttest_form2_mis,stdType="unstd")
# afttestplot(result07_afttest_form2_mis,stdType="std")

# result07_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result07_afttest_form3_mis$p_value
result07_afttest_form3_mis$p_std_value
# afttestplot(result07_afttest_form3_mis,stdType="unstd")
# afttestplot(result07_afttest_form3_mis,stdType="std")

# result07_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result07_afttest_form4_mis$p_value
result07_afttest_form4_mis$p_std_value
# afttestplot(result07_afttest_form4_mis,stdType="unstd")
# afttestplot(result07_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: log_bili+prot+albu+log_age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result08_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result08_afttest_omni_mis$p_value
result08_afttest_omni_mis$p_std_value
# afttestplot(result08_afttest_omni_mis,stdType="unstd")
# afttestplot(result08_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result08_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result08_afttest_link_mis$p_value
result08_afttest_link_mis$p_std_value
# afttestplot(result08_afttest_link_mis,stdType="unstd")
# afttestplot(result08_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result08_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result08_afttest_form1_mis$p_value
result08_afttest_form1_mis$p_std_value
# afttestplot(result08_afttest_form1_mis,stdType="unstd")
# afttestplot(result08_afttest_form1_mis,stdType="std")

# result08_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result08_afttest_form2_mis$p_value
result08_afttest_form2_mis$p_std_value
# afttestplot(result08_afttest_form2_mis,stdType="unstd")
# afttestplot(result08_afttest_form2_mis,stdType="std")

# result08_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result08_afttest_form3_mis$p_value
result08_afttest_form3_mis$p_std_value
# afttestplot(result08_afttest_form3_mis,stdType="unstd")
# afttestplot(result08_afttest_form3_mis,stdType="std")

# result08_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result08_afttest_form4_mis$p_value
result08_afttest_form4_mis$p_std_value
# afttestplot(result08_afttest_form4_mis,stdType="unstd")
# afttestplot(result08_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_prot+log_albu+age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result09_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result09_afttest_omni_mis$p_value
result09_afttest_omni_mis$p_std_value
# afttestplot(result09_afttest_omni_mis,stdType="unstd")
# afttestplot(result09_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result09_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
result09_afttest_link_mis$p_value
result09_afttest_link_mis$p_std_value
# afttestplot(result09_afttest_link_mis,stdType="unstd")
# afttestplot(result09_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result09_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result09_afttest_form1_mis$p_value
result09_afttest_form1_mis$p_std_value
# afttestplot(result09_afttest_form1_mis,stdType="unstd")
# afttestplot(result09_afttest_form1_mis,stdType="std")

# result09_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result09_afttest_form2_mis$p_value
result09_afttest_form2_mis$p_std_value
# afttestplot(result09_afttest_form2_mis,stdType="unstd")
# afttestplot(result09_afttest_form2_mis,stdType="std")

# result09_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result09_afttest_form3_mis$p_value
result09_afttest_form3_mis$p_std_value
# afttestplot(result09_afttest_form3_mis,stdType="unstd")
# afttestplot(result09_afttest_form3_mis,stdType="std")

# result09_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result09_afttest_form4_mis$p_value
result09_afttest_form4_mis$p_std_value
# afttestplot(result09_afttest_form4_mis,stdType="unstd")
# afttestplot(result09_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+log_prot+albu+log_age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result10_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result10_afttest_omni_mis$p_value
result10_afttest_omni_mis$p_std_value
# afttestplot(result10_afttest_omni_mis,stdType="unstd")
# afttestplot(result10_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result10_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result10_afttest_link_mis$p_value
result10_afttest_link_mis$p_std_value
# afttestplot(result10_afttest_link_mis,stdType="unstd")
# afttestplot(result10_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result10_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result10_afttest_form1_mis$p_value
result10_afttest_form1_mis$p_std_value
# afttestplot(result10_afttest_form1_mis,stdType="unstd")
# afttestplot(result10_afttest_form1_mis,stdType="std")

# result10_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result10_afttest_form2_mis$p_value
result10_afttest_form2_mis$p_std_value
# afttestplot(result10_afttest_form2_mis,stdType="unstd")
# afttestplot(result10_afttest_form2_mis,stdType="std")

# result10_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result10_afttest_form3_mis$p_value
result10_afttest_form3_mis$p_std_value
# afttestplot(result10_afttest_form3_mis,stdType="unstd")
# afttestplot(result10_afttest_form3_mis,stdType="std")

# result10_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result10_afttest_form4_mis$p_value
result10_afttest_form4_mis$p_std_value
# afttestplot(result10_afttest_form4_mis,stdType="unstd")
# afttestplot(result10_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ----------- Covariates: bili+prot+log_albu+log_age+edem+trt -----------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result11_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result11_afttest_omni_mis$p_value
result11_afttest_omni_mis$p_std_value
# afttestplot(result11_afttest_omni_mis,stdType="unstd")
# afttestplot(result11_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result11_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result11_afttest_link_mis$p_value
result11_afttest_link_mis$p_std_value
# afttestplot(result11_afttest_link_mis,stdType="unstd")
# afttestplot(result11_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result11_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result11_afttest_form1_mis$p_value
result11_afttest_form1_mis$p_std_value
# afttestplot(result11_afttest_form1_mis,stdType="unstd")
# afttestplot(result11_afttest_form1_mis,stdType="std")

# result11_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result11_afttest_form2_mis$p_value
result11_afttest_form2_mis$p_std_value
# afttestplot(result11_afttest_form2_mis,stdType="unstd")
# afttestplot(result11_afttest_form2_mis,stdType="std")

# result11_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result11_afttest_form3_mis$p_value
result11_afttest_form3_mis$p_std_value
# afttestplot(result11_afttest_form3_mis,stdType="unstd")
# afttestplot(result11_afttest_form3_mis,stdType="std")

# result11_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result11_afttest_form4_mis$p_value
result11_afttest_form4_mis$p_std_value
# afttestplot(result11_afttest_form4_mis,stdType="unstd")
# afttestplot(result11_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_prot+log_albu+age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result12_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
result12_afttest_omni_mis$p_value
result12_afttest_omni_mis$p_std_value
# afttestplot(result12_afttest_omni_mis,stdType="unstd")
# afttestplot(result12_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result12_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
result12_afttest_link_mis$p_value
result12_afttest_link_mis$p_std_value
# afttestplot(result12_afttest_link_mis,stdType="unstd")
# afttestplot(result12_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result12_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result12_afttest_form1_mis$p_value
result12_afttest_form1_mis$p_std_value
# afttestplot(result12_afttest_form1_mis,stdType="unstd")
# afttestplot(result12_afttest_form1_mis,stdType="std")

# result12_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result12_afttest_form2_mis$p_value
result12_afttest_form2_mis$p_std_value
# afttestplot(result12_afttest_form2_mis,stdType="unstd")
# afttestplot(result12_afttest_form2_mis,stdType="std")

# result12_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result12_afttest_form3_mis$p_value
result12_afttest_form3_mis$p_std_value
# afttestplot(result12_afttest_form3_mis,stdType="unstd")
# afttestplot(result12_afttest_form3_mis,stdType="std")

# result12_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
result12_afttest_form4_mis$p_value
result12_afttest_form4_mis$p_std_value
# afttestplot(result12_afttest_form4_mis,stdType="unstd")
# afttestplot(result12_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+log_prot+albu+log_age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result13_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result13_afttest_omni_mis$p_value
result13_afttest_omni_mis$p_std_value
# afttestplot(result13_afttest_omni_mis,stdType="unstd")
# afttestplot(result13_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result13_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result13_afttest_link_mis$p_value
result13_afttest_link_mis$p_std_value
# afttestplot(result13_afttest_link_mis,stdType="unstd")
# afttestplot(result13_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result13_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result13_afttest_form1_mis$p_value
result13_afttest_form1_mis$p_std_value
# afttestplot(result13_afttest_form1_mis,stdType="unstd")
# afttestplot(result13_afttest_form1_mis,stdType="std")

# result13_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result13_afttest_form2_mis$p_value
result13_afttest_form2_mis$p_std_value
# afttestplot(result13_afttest_form2_mis,stdType="unstd")
# afttestplot(result13_afttest_form2_mis,stdType="std")

# result13_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
result13_afttest_form3_mis$p_value
result13_afttest_form3_mis$p_std_value
# afttestplot(result13_afttest_form3_mis,stdType="unstd")
# afttestplot(result13_afttest_form3_mis,stdType="std")

# result13_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result13_afttest_form4_mis$p_value
result13_afttest_form4_mis$p_std_value
# afttestplot(result13_afttest_form4_mis,stdType="unstd")
# afttestplot(result13_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# --------- Covariates: log_bili+prot+log_albu+log_age+edem+trt ---------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result14_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result14_afttest_omni_mis$p_value
result14_afttest_omni_mis$p_std_value
# afttestplot(result14_afttest_omni_mis,stdType="unstd")
# afttestplot(result14_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result14_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result14_afttest_link_mis$p_value
result14_afttest_link_mis$p_std_value
# afttestplot(result14_afttest_link_mis,stdType="unstd")
# afttestplot(result14_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result14_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result14_afttest_form1_mis$p_value
result14_afttest_form1_mis$p_std_value
# afttestplot(result14_afttest_form1_mis,stdType="unstd")
# afttestplot(result14_afttest_form1_mis,stdType="std")

# result14_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
result14_afttest_form2_mis$p_value
result14_afttest_form2_mis$p_std_value
# afttestplot(result14_afttest_form2_mis,stdType="unstd")
# afttestplot(result14_afttest_form2_mis,stdType="std")

# result14_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result14_afttest_form3_mis$p_value
result14_afttest_form3_mis$p_std_value
# afttestplot(result14_afttest_form3_mis,stdType="unstd")
# afttestplot(result14_afttest_form3_mis,stdType="std")

# result14_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result14_afttest_form4_mis$p_value
result14_afttest_form4_mis$p_std_value
# afttestplot(result14_afttest_form4_mis,stdType="unstd")
# afttestplot(result14_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# --------------- Covariates: bili+prot+albu+age+edem+trt ---------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result15_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result15_afttest_omni_mis$p_value
result15_afttest_omni_mis$p_std_value
# afttestplot(result15_afttest_omni_mis,stdType="unstd")
# afttestplot(result15_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result15_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result15_afttest_link_mis$p_value
result15_afttest_link_mis$p_std_value
# afttestplot(result15_afttest_link_mis,stdType="unstd")
# afttestplot(result15_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result15_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
result15_afttest_form1_mis$p_value
result15_afttest_form1_mis$p_std_value
# afttestplot(result15_afttest_form1_mis,stdType="unstd")
# afttestplot(result15_afttest_form1_mis,stdType="std")

# result15_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result15_afttest_form2_mis$p_value
result15_afttest_form2_mis$p_std_value
# afttestplot(result15_afttest_form2_mis,stdType="unstd")
# afttestplot(result15_afttest_form2_mis,stdType="std")

# result15_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result15_afttest_form3_mis$p_value
result15_afttest_form3_mis$p_std_value
# afttestplot(result15_afttest_form3_mis,stdType="unstd")
# afttestplot(result15_afttest_form3_mis,stdType="std")

# result15_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result15_afttest_form4_mis$p_value
result15_afttest_form4_mis$p_std_value
# afttestplot(result15_afttest_form4_mis,stdType="unstd")
# afttestplot(result15_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------- Covariates: log_bili+log_prot+log_albu+log_age+edem+trt -------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result16_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
result16_afttest_omni_mis$p_value
result16_afttest_omni_mis$p_std_value
# afttestplot(result16_afttest_omni_mis,stdType="unstd")
# afttestplot(result16_afttest_omni_mis,stdType="std")

# ------------------------------------ link ------------------------------------
# result16_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
result16_afttest_link_mis$p_value
result16_afttest_link_mis$p_std_value
# afttestplot(result16_afttest_link_mis,stdType="unstd")
# afttestplot(result16_afttest_link_mis,stdType="std")

# ------------------------------------ form ------------------------------------
# result16_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
result16_afttest_form1_mis$p_value
result16_afttest_form1_mis$p_std_value
# afttestplot(result16_afttest_form1_mis,stdType="unstd")
# afttestplot(result16_afttest_form1_mis,stdType="std")

# result16_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
result16_afttest_form2_mis$p_value
result16_afttest_form2_mis$p_std_value
# afttestplot(result16_afttest_form2_mis,stdType="unstd")
# afttestplot(result16_afttest_form2_mis,stdType="std")

# result16_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
result16_afttest_form3_mis$p_value
result16_afttest_form3_mis$p_std_value
# afttestplot(result16_afttest_form3_mis,stdType="unstd")
# afttestplot(result16_afttest_form3_mis,stdType="std")

# result16_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
result16_afttest_form4_mis$p_value
result16_afttest_form4_mis$p_std_value
# afttestplot(result16_afttest_form4_mis,stdType="unstd")
# afttestplot(result16_afttest_form4_mis,stdType="std")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
beta01mns = aftsrr(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,eqType="mns",se="ISMB")
beta01mis = aftsrr(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,eqType="mis",se="ISMB")
summary(beta01mns)
summary(beta01mis)

beta02mns = aftsrr(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,eqType="mns",se="ISMB")
beta02mis = aftsrr(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,eqType="mis",se="ISMB")
summary(beta02mns)
summary(beta02mis)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
{
  # ------------------------------------------------------------------------------
  {
    # ------------------------------------------------------------------------------
    cairo_ps("pbc01_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc01_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc01_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc01_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc01_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc01_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc02_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc02_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc02_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc02_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc02_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc02_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc03_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc03_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc03_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc03_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc03_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc03_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc04_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc04_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc04_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc04_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc04_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc04_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc05_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc05_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc05_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc05_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc05_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc05_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc06_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc06_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc06_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc06_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc06_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc06_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc07_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc07_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc07_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc07_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc07_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc07_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc08_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc08_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc08_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc08_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc08_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc08_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc09_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc09_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc09_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc09_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc09_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc09_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc10_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc10_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc10_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc10_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc10_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc10_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc11_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc11_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc11_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc11_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc11_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc11_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc12_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc12_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc12_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc12_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc12_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc12_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc13_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc13_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc13_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc13_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc13_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc13_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc14_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc14_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc14_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc14_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc14_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc14_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc15_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc15_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc15_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc15_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc15_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc15_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form4_mis,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc16_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_omni_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_omni_mis,stdType="std")
    dev.off()
    cairo_ps("pbc16_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_link_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_link_mis,stdType="std")
    dev.off()
    cairo_ps("pbc16_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form1_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form1_mis,stdType="std")
    dev.off()
    cairo_ps("pbc16_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form2_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form2_mis,stdType="std")
    dev.off()
    cairo_ps("pbc16_form3_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form3_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form3_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form3_mis,stdType="std")
    dev.off()
    cairo_ps("pbc16_form4_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form4_mis,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form4_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form4_mis,stdType="std")
    dev.off()
  }
  
  # ------------------------------------------------------------------------------
  {
    # ------------------------------------------------------------------------------
    cairo_ps("pbc01_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc01_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc01_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc01_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc01_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc01_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc01_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result01_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc02_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc02_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc02_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc02_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc02_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc02_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc02_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result02_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc03_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc03_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc03_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc03_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc03_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc03_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc03_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result03_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc04_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc04_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc04_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc04_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc04_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc04_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc04_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result04_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc05_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc05_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc05_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc05_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc05_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc05_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc05_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result05_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc06_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc06_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc06_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc06_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc06_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc06_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc06_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result06_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc07_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc07_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc07_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc07_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc07_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc07_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc07_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result07_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc08_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc08_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc08_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc08_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc08_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc08_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc08_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result08_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc09_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc09_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc09_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc09_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc09_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc09_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc09_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result09_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc10_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc10_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc10_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc10_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc10_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc10_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc10_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result10_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc11_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc11_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc11_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc11_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc11_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc11_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc11_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result11_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc12_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc12_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc12_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc12_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc12_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc12_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc12_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result12_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc13_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc13_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc13_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc13_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc13_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc13_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc13_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result13_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc14_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc14_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc14_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc14_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc14_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc14_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc14_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result14_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc15_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc15_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc15_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc15_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc15_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc15_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc15_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result15_afttest_form4_mns,stdType="std")
    dev.off()
    
    # ------------------------------------------------------------------------------
    cairo_ps("pbc16_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_omni_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_omni_mns,stdType="std")
    dev.off()
    cairo_ps("pbc16_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_link_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_link_mns,stdType="std")
    dev.off()
    cairo_ps("pbc16_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form1_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form1_mns,stdType="std")
    dev.off()
    cairo_ps("pbc16_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form2_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form2_mns,stdType="std")
    dev.off()
    cairo_ps("pbc16_form3_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form3_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form3_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form3_mns,stdType="std")
    dev.off()
    cairo_ps("pbc16_form4_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form4_mns,stdType="unstd")
    dev.off()
    cairo_ps("pbc16_form4_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
    afttestplot(result16_afttest_form4_mns,stdType="std")
    dev.off()
  }
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
{
  result_pbc = rbind(c(result01_afttest_omni_mns$p_std_value, result01_afttest_omni_mis$p_std_value,
                       result01_afttest_link_mns$p_std_value, result01_afttest_link_mis$p_std_value,
                       result01_afttest_form1_mns$p_std_value, result01_afttest_form1_mis$p_std_value,
                       result01_afttest_form2_mns$p_std_value, result01_afttest_form2_mis$p_std_value,
                       result01_afttest_form3_mns$p_std_value, result01_afttest_form3_mis$p_std_value,
                       result01_afttest_form4_mns$p_std_value, result01_afttest_form4_mis$p_std_value),
                     c(result01_afttest_omni_mns$p_value, result01_afttest_omni_mis$p_value,
                       result01_afttest_link_mns$p_value, result01_afttest_link_mis$p_value,
                       result01_afttest_form1_mns$p_value, result01_afttest_form1_mis$p_value,
                       result01_afttest_form2_mns$p_value, result01_afttest_form2_mis$p_value,
                       result01_afttest_form3_mns$p_value, result01_afttest_form3_mis$p_value,
                       result01_afttest_form4_mns$p_value, result01_afttest_form4_mis$p_value),
                     c(result02_afttest_omni_mns$p_std_value, result02_afttest_omni_mis$p_std_value,
                       result02_afttest_link_mns$p_std_value, result02_afttest_link_mis$p_std_value,
                       result02_afttest_form1_mns$p_std_value, result02_afttest_form1_mis$p_std_value,
                       result02_afttest_form2_mns$p_std_value, result02_afttest_form2_mis$p_std_value,
                       result02_afttest_form3_mns$p_std_value, result02_afttest_form3_mis$p_std_value,
                       result02_afttest_form4_mns$p_std_value, result02_afttest_form4_mis$p_std_value),
                     c(result02_afttest_omni_mns$p_value, result02_afttest_omni_mis$p_value,
                       result02_afttest_link_mns$p_value, result02_afttest_link_mis$p_value,
                       result02_afttest_form1_mns$p_value, result02_afttest_form1_mis$p_value,
                       result02_afttest_form2_mns$p_value, result02_afttest_form2_mis$p_value,
                       result02_afttest_form3_mns$p_value, result02_afttest_form3_mis$p_value,
                       result02_afttest_form4_mns$p_value, result02_afttest_form4_mis$p_value),
                     c(result03_afttest_omni_mns$p_std_value, result03_afttest_omni_mis$p_std_value,
                       result03_afttest_link_mns$p_std_value, result03_afttest_link_mis$p_std_value,
                       result03_afttest_form1_mns$p_std_value, result03_afttest_form1_mis$p_std_value,
                       result03_afttest_form2_mns$p_std_value, result03_afttest_form2_mis$p_std_value,
                       result03_afttest_form3_mns$p_std_value, result03_afttest_form3_mis$p_std_value,
                       result03_afttest_form4_mns$p_std_value, result03_afttest_form4_mis$p_std_value),
                     c(result03_afttest_omni_mns$p_value, result03_afttest_omni_mis$p_value,
                       result03_afttest_link_mns$p_value, result03_afttest_link_mis$p_value,
                       result03_afttest_form1_mns$p_value, result03_afttest_form1_mis$p_value,
                       result03_afttest_form2_mns$p_value, result03_afttest_form2_mis$p_value,
                       result03_afttest_form3_mns$p_value, result03_afttest_form3_mis$p_value,
                       result03_afttest_form4_mns$p_value, result03_afttest_form4_mis$p_value),
                     c(result04_afttest_omni_mns$p_std_value, result04_afttest_omni_mis$p_std_value,
                       result04_afttest_link_mns$p_std_value, result04_afttest_link_mis$p_std_value,
                       result04_afttest_form1_mns$p_std_value, result04_afttest_form1_mis$p_std_value,
                       result04_afttest_form2_mns$p_std_value, result04_afttest_form2_mis$p_std_value,
                       result04_afttest_form3_mns$p_std_value, result04_afttest_form3_mis$p_std_value,
                       result04_afttest_form4_mns$p_std_value, result04_afttest_form4_mis$p_std_value),
                     c(result04_afttest_omni_mns$p_value, result04_afttest_omni_mis$p_value,
                       result04_afttest_link_mns$p_value, result04_afttest_link_mis$p_value,
                       result04_afttest_form1_mns$p_value, result04_afttest_form1_mis$p_value,
                       result04_afttest_form2_mns$p_value, result04_afttest_form2_mis$p_value,
                       result04_afttest_form3_mns$p_value, result04_afttest_form3_mis$p_value,
                       result04_afttest_form4_mns$p_value, result04_afttest_form4_mis$p_value),
                     c(result05_afttest_omni_mns$p_std_value, result05_afttest_omni_mis$p_std_value,
                       result05_afttest_link_mns$p_std_value, result05_afttest_link_mis$p_std_value,
                       result05_afttest_form1_mns$p_std_value, result05_afttest_form1_mis$p_std_value,
                       result05_afttest_form2_mns$p_std_value, result05_afttest_form2_mis$p_std_value,
                       result05_afttest_form3_mns$p_std_value, result05_afttest_form3_mis$p_std_value,
                       result05_afttest_form4_mns$p_std_value, result05_afttest_form4_mis$p_std_value),
                     c(result05_afttest_omni_mns$p_value, result05_afttest_omni_mis$p_value,
                       result05_afttest_link_mns$p_value, result05_afttest_link_mis$p_value,
                       result05_afttest_form1_mns$p_value, result05_afttest_form1_mis$p_value,
                       result05_afttest_form2_mns$p_value, result05_afttest_form2_mis$p_value,
                       result05_afttest_form3_mns$p_value, result05_afttest_form3_mis$p_value,
                       result05_afttest_form4_mns$p_value, result05_afttest_form4_mis$p_value),
                     c(result06_afttest_omni_mns$p_std_value, result06_afttest_omni_mis$p_std_value,
                       result06_afttest_link_mns$p_std_value, result06_afttest_link_mis$p_std_value,
                       result06_afttest_form1_mns$p_std_value, result06_afttest_form1_mis$p_std_value,
                       result06_afttest_form2_mns$p_std_value, result06_afttest_form2_mis$p_std_value,
                       result06_afttest_form3_mns$p_std_value, result06_afttest_form3_mis$p_std_value,
                       result06_afttest_form4_mns$p_std_value, result06_afttest_form4_mis$p_std_value),
                     c(result06_afttest_omni_mns$p_value, result06_afttest_omni_mis$p_value,
                       result06_afttest_link_mns$p_value, result06_afttest_link_mis$p_value,
                       result06_afttest_form1_mns$p_value, result06_afttest_form1_mis$p_value,
                       result06_afttest_form2_mns$p_value, result06_afttest_form2_mis$p_value,
                       result06_afttest_form3_mns$p_value, result06_afttest_form3_mis$p_value,
                       result06_afttest_form4_mns$p_value, result06_afttest_form4_mis$p_value),
                     c(result07_afttest_omni_mns$p_std_value, result07_afttest_omni_mis$p_std_value,
                       result07_afttest_link_mns$p_std_value, result07_afttest_link_mis$p_std_value,
                       result07_afttest_form1_mns$p_std_value, result07_afttest_form1_mis$p_std_value,
                       result07_afttest_form2_mns$p_std_value, result07_afttest_form2_mis$p_std_value,
                       result07_afttest_form3_mns$p_std_value, result07_afttest_form3_mis$p_std_value,
                       result07_afttest_form4_mns$p_std_value, result07_afttest_form4_mis$p_std_value),
                     c(result07_afttest_omni_mns$p_value, result07_afttest_omni_mis$p_value,
                       result07_afttest_link_mns$p_value, result07_afttest_link_mis$p_value,
                       result07_afttest_form1_mns$p_value, result07_afttest_form1_mis$p_value,
                       result07_afttest_form2_mns$p_value, result07_afttest_form2_mis$p_value,
                       result07_afttest_form3_mns$p_value, result07_afttest_form3_mis$p_value,
                       result07_afttest_form4_mns$p_value, result07_afttest_form4_mis$p_value),
                     c(result08_afttest_omni_mns$p_std_value, result08_afttest_omni_mis$p_std_value,
                       result08_afttest_link_mns$p_std_value, result08_afttest_link_mis$p_std_value,
                       result08_afttest_form1_mns$p_std_value, result08_afttest_form1_mis$p_std_value,
                       result08_afttest_form2_mns$p_std_value, result08_afttest_form2_mis$p_std_value,
                       result08_afttest_form3_mns$p_std_value, result08_afttest_form3_mis$p_std_value,
                       result08_afttest_form4_mns$p_std_value, result08_afttest_form4_mis$p_std_value),
                     c(result08_afttest_omni_mns$p_value, result08_afttest_omni_mis$p_value,
                       result08_afttest_link_mns$p_value, result08_afttest_link_mis$p_value,
                       result08_afttest_form1_mns$p_value, result08_afttest_form1_mis$p_value,
                       result08_afttest_form2_mns$p_value, result08_afttest_form2_mis$p_value,
                       result08_afttest_form3_mns$p_value, result08_afttest_form3_mis$p_value,
                       result08_afttest_form4_mns$p_value, result08_afttest_form4_mis$p_value),
                     c(result09_afttest_omni_mns$p_std_value, result09_afttest_omni_mis$p_std_value,
                       result09_afttest_link_mns$p_std_value, result09_afttest_link_mis$p_std_value,
                       result09_afttest_form1_mns$p_std_value, result09_afttest_form1_mis$p_std_value,
                       result09_afttest_form2_mns$p_std_value, result09_afttest_form2_mis$p_std_value,
                       result09_afttest_form3_mns$p_std_value, result09_afttest_form3_mis$p_std_value,
                       result09_afttest_form4_mns$p_std_value, result09_afttest_form4_mis$p_std_value),
                     c(result09_afttest_omni_mns$p_value, result09_afttest_omni_mis$p_value,
                       result09_afttest_link_mns$p_value, result09_afttest_link_mis$p_value,
                       result09_afttest_form1_mns$p_value, result09_afttest_form1_mis$p_value,
                       result09_afttest_form2_mns$p_value, result09_afttest_form2_mis$p_value,
                       result09_afttest_form3_mns$p_value, result09_afttest_form3_mis$p_value,
                       result09_afttest_form4_mns$p_value, result09_afttest_form4_mis$p_value),
                     c(result10_afttest_omni_mns$p_std_value, result10_afttest_omni_mis$p_std_value,
                       result10_afttest_link_mns$p_std_value, result10_afttest_link_mis$p_std_value,
                       result10_afttest_form1_mns$p_std_value, result10_afttest_form1_mis$p_std_value,
                       result10_afttest_form2_mns$p_std_value, result10_afttest_form2_mis$p_std_value,
                       result10_afttest_form3_mns$p_std_value, result10_afttest_form3_mis$p_std_value,
                       result10_afttest_form4_mns$p_std_value, result10_afttest_form4_mis$p_std_value),
                     c(result10_afttest_omni_mns$p_value, result10_afttest_omni_mis$p_value,
                       result10_afttest_link_mns$p_value, result10_afttest_link_mis$p_value,
                       result10_afttest_form1_mns$p_value, result10_afttest_form1_mis$p_value,
                       result10_afttest_form2_mns$p_value, result10_afttest_form2_mis$p_value,
                       result10_afttest_form3_mns$p_value, result10_afttest_form3_mis$p_value,
                       result10_afttest_form4_mns$p_value, result10_afttest_form4_mis$p_value),
                     c(result11_afttest_omni_mns$p_std_value, result11_afttest_omni_mis$p_std_value,
                       result11_afttest_link_mns$p_std_value, result11_afttest_link_mis$p_std_value,
                       result11_afttest_form1_mns$p_std_value, result11_afttest_form1_mis$p_std_value,
                       result11_afttest_form2_mns$p_std_value, result11_afttest_form2_mis$p_std_value,
                       result11_afttest_form3_mns$p_std_value, result11_afttest_form3_mis$p_std_value,
                       result11_afttest_form4_mns$p_std_value, result11_afttest_form4_mis$p_std_value),
                     c(result11_afttest_omni_mns$p_value, result11_afttest_omni_mis$p_value,
                       result11_afttest_link_mns$p_value, result11_afttest_link_mis$p_value,
                       result11_afttest_form1_mns$p_value, result11_afttest_form1_mis$p_value,
                       result11_afttest_form2_mns$p_value, result11_afttest_form2_mis$p_value,
                       result11_afttest_form3_mns$p_value, result11_afttest_form3_mis$p_value,
                       result11_afttest_form4_mns$p_value, result11_afttest_form4_mis$p_value),
                     c(result12_afttest_omni_mns$p_std_value, result12_afttest_omni_mis$p_std_value,
                       result12_afttest_link_mns$p_std_value, result12_afttest_link_mis$p_std_value,
                       result12_afttest_form1_mns$p_std_value, result12_afttest_form1_mis$p_std_value,
                       result12_afttest_form2_mns$p_std_value, result12_afttest_form2_mis$p_std_value,
                       result12_afttest_form3_mns$p_std_value, result12_afttest_form3_mis$p_std_value,
                       result12_afttest_form4_mns$p_std_value, result12_afttest_form4_mis$p_std_value),
                     c(result12_afttest_omni_mns$p_value, result12_afttest_omni_mis$p_value,
                       result12_afttest_link_mns$p_value, result12_afttest_link_mis$p_value,
                       result12_afttest_form1_mns$p_value, result12_afttest_form1_mis$p_value,
                       result12_afttest_form2_mns$p_value, result12_afttest_form2_mis$p_value,
                       result12_afttest_form3_mns$p_value, result12_afttest_form3_mis$p_value,
                       result12_afttest_form4_mns$p_value, result12_afttest_form4_mis$p_value),
                     c(result13_afttest_omni_mns$p_std_value, result13_afttest_omni_mis$p_std_value,
                       result13_afttest_link_mns$p_std_value, result13_afttest_link_mis$p_std_value,
                       result13_afttest_form1_mns$p_std_value, result13_afttest_form1_mis$p_std_value,
                       result13_afttest_form2_mns$p_std_value, result13_afttest_form2_mis$p_std_value,
                       result13_afttest_form3_mns$p_std_value, result13_afttest_form3_mis$p_std_value,
                       result13_afttest_form4_mns$p_std_value, result13_afttest_form4_mis$p_std_value),
                     c(result13_afttest_omni_mns$p_value, result13_afttest_omni_mis$p_value,
                       result13_afttest_link_mns$p_value, result13_afttest_link_mis$p_value,
                       result13_afttest_form1_mns$p_value, result13_afttest_form1_mis$p_value,
                       result13_afttest_form2_mns$p_value, result13_afttest_form2_mis$p_value,
                       result13_afttest_form3_mns$p_value, result13_afttest_form3_mis$p_value,
                       result13_afttest_form4_mns$p_value, result13_afttest_form4_mis$p_value),
                     c(result14_afttest_omni_mns$p_std_value, result14_afttest_omni_mis$p_std_value,
                       result14_afttest_link_mns$p_std_value, result14_afttest_link_mis$p_std_value,
                       result14_afttest_form1_mns$p_std_value, result14_afttest_form1_mis$p_std_value,
                       result14_afttest_form2_mns$p_std_value, result14_afttest_form2_mis$p_std_value,
                       result14_afttest_form3_mns$p_std_value, result14_afttest_form3_mis$p_std_value,
                       result14_afttest_form4_mns$p_std_value, result14_afttest_form4_mis$p_std_value),
                     c(result14_afttest_omni_mns$p_value, result14_afttest_omni_mis$p_value,
                       result14_afttest_link_mns$p_value, result14_afttest_link_mis$p_value,
                       result14_afttest_form1_mns$p_value, result14_afttest_form1_mis$p_value,
                       result14_afttest_form2_mns$p_value, result14_afttest_form2_mis$p_value,
                       result14_afttest_form3_mns$p_value, result14_afttest_form3_mis$p_value,
                       result14_afttest_form4_mns$p_value, result14_afttest_form4_mis$p_value),
                     c(result15_afttest_omni_mns$p_std_value, result15_afttest_omni_mis$p_std_value,
                       result15_afttest_link_mns$p_std_value, result15_afttest_link_mis$p_std_value,
                       result15_afttest_form1_mns$p_std_value, result15_afttest_form1_mis$p_std_value,
                       result15_afttest_form2_mns$p_std_value, result15_afttest_form2_mis$p_std_value,
                       result15_afttest_form3_mns$p_std_value, result15_afttest_form3_mis$p_std_value,
                       result15_afttest_form4_mns$p_std_value, result15_afttest_form4_mis$p_std_value),
                     c(result15_afttest_omni_mns$p_value, result15_afttest_omni_mis$p_value,
                       result15_afttest_link_mns$p_value, result15_afttest_link_mis$p_value,
                       result15_afttest_form1_mns$p_value, result15_afttest_form1_mis$p_value,
                       result15_afttest_form2_mns$p_value, result15_afttest_form2_mis$p_value,
                       result15_afttest_form3_mns$p_value, result15_afttest_form3_mis$p_value,
                       result15_afttest_form4_mns$p_value, result15_afttest_form4_mis$p_value),
                     c(result16_afttest_omni_mns$p_std_value, result16_afttest_omni_mis$p_std_value,
                       result16_afttest_link_mns$p_std_value, result16_afttest_link_mis$p_std_value,
                       result16_afttest_form1_mns$p_std_value, result16_afttest_form1_mis$p_std_value,
                       result16_afttest_form2_mns$p_std_value, result16_afttest_form2_mis$p_std_value,
                       result16_afttest_form3_mns$p_std_value, result16_afttest_form3_mis$p_std_value,
                       result16_afttest_form4_mns$p_std_value, result16_afttest_form4_mis$p_std_value),
                     c(result16_afttest_omni_mns$p_value, result16_afttest_omni_mis$p_value,
                       result16_afttest_link_mns$p_value, result16_afttest_link_mis$p_value,
                       result16_afttest_form1_mns$p_value, result16_afttest_form1_mis$p_value,
                       result16_afttest_form2_mns$p_value, result16_afttest_form2_mis$p_value,
                       result16_afttest_form3_mns$p_value, result16_afttest_form3_mis$p_value,
                       result16_afttest_form4_mns$p_value, result16_afttest_form4_mis$p_value))
  colnames(result_pbc) = rep(c("mns","mis"),6)
  rownames(result_pbc) = rep(paste0("model",1:16), each = 2)
  result_pbc = data.frame(model = rep(paste0("model",1:16), each = 2), result_pbc)
  colnames(result_pbc) = NULL
  rownames(result_pbc) = NULL
  
  library(knitr)
  library(kableExtra)
  kable(result_pbc, digits = 3, "latex", booktabs = T, escape = F,
        col.names = c("model", rep(c("mns","mis"),6)),
        caption = "PBC data result",
        label = "tab:real:pbc", align = "c") %>%
    add_header_above(c(" "=1,"omni"=2, "link"=2, "form1"=2, "form2"=2, "form3"=2, "form4"=2)) %>%
    add_header_above(c(" "=1,"P-value"=12)) %>%
    kable_styling() %>%
    collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
    row_spec(row = (1:16*2 - 1), bold = TRUE)
}










