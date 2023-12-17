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

run_ID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if(run_ID == 1){
  # ------------------------------------------------------------------------------
  # ------------------------------------ "mns" -----------------------------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------
  # ----------------- Covariates: bili+prot+albu+age+edem+trt -----------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result01_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result01_afttest_omni_mns$p_value
  result01_afttest_omni_mns$p_std_value
  # afttestplot(result01_afttest_omni_mns,stdType="unstd")
  # afttestplot(result01_afttest_omni_mns,stdType="std")
} else if (run_ID == 2){
  # ------------------------------------ link ------------------------------------
  result01_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result01_afttest_link_mns$p_value
  result01_afttest_link_mns$p_std_value
  # afttestplot(result01_afttest_link_mns,stdType="unstd")
  # afttestplot(result01_afttest_link_mns,stdType="std")
} else if (run_ID == 3){
  # ------------------------------------ form ------------------------------------
  result01_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result01_afttest_form1_mns$p_value
  result01_afttest_form1_mns$p_std_value
  # afttestplot(result01_afttest_form1_mns,stdType="unstd")
  # afttestplot(result01_afttest_form1_mns,stdType="std")
} else if (run_ID == 4){
  result01_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result01_afttest_form2_mns$p_value
  result01_afttest_form2_mns$p_std_value
  # afttestplot(result01_afttest_form2_mns,stdType="unstd")
  # afttestplot(result01_afttest_form2_mns,stdType="std")
} else if (run_ID == 5){
  result01_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result01_afttest_form3_mns$p_value
  result01_afttest_form3_mns$p_std_value
  # afttestplot(result01_afttest_form3_mns,stdType="unstd")
  # afttestplot(result01_afttest_form3_mns,stdType="std")
} else if (run_ID == 6){
  result01_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result01_afttest_form4_mns$p_value
  result01_afttest_form4_mns$p_std_value
  # afttestplot(result01_afttest_form4_mns,stdType="unstd")
  # afttestplot(result01_afttest_form4_mns,stdType="std")
} else if (run_ID == 7){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: log_bili+prot+albu+age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result02_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result02_afttest_omni_mns$p_value
  result02_afttest_omni_mns$p_std_value
  # afttestplot(result02_afttest_omni_mns,stdType="unstd")
  # afttestplot(result02_afttest_omni_mns,stdType="std")
} else if (run_ID == 8){
  # ------------------------------------ link ------------------------------------
  result02_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result02_afttest_link_mns$p_value
  result02_afttest_link_mns$p_std_value
  # afttestplot(result02_afttest_link_mns,stdType="unstd")
  # afttestplot(result02_afttest_link_mns,stdType="std")
} else if (run_ID == 9){
  # ------------------------------------ form ------------------------------------
  result02_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result02_afttest_form1_mns$p_value
  result02_afttest_form1_mns$p_std_value
  # afttestplot(result02_afttest_form1_mns,stdType="unstd")
  # afttestplot(result02_afttest_form1_mns,stdType="std")
} else if (run_ID == 10){
  result02_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result02_afttest_form2_mns$p_value
  result02_afttest_form2_mns$p_std_value
  # afttestplot(result02_afttest_form2_mns,stdType="unstd")
  # afttestplot(result02_afttest_form2_mns,stdType="std")
} else if (run_ID == 11){
  result02_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result02_afttest_form3_mns$p_value
  result02_afttest_form3_mns$p_std_value
  # afttestplot(result02_afttest_form3_mns,stdType="unstd")
  # afttestplot(result02_afttest_form3_mns,stdType="std")
} else if (run_ID == 12){
  result02_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result02_afttest_form4_mns$p_value
  result02_afttest_form4_mns$p_std_value
  # afttestplot(result02_afttest_form4_mns,stdType="unstd")
  # afttestplot(result02_afttest_form4_mns,stdType="std")
} else if (run_ID == 13){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: bili+log_prot+albu+age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result03_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result03_afttest_omni_mns$p_value
  result03_afttest_omni_mns$p_std_value
  # afttestplot(result03_afttest_omni_mns,stdType="unstd")
  # afttestplot(result03_afttest_omni_mns,stdType="std")
} else if (run_ID == 14){
  # ------------------------------------ link ------------------------------------
  result03_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result03_afttest_link_mns$p_value
  result03_afttest_link_mns$p_std_value
  # afttestplot(result03_afttest_link_mns,stdType="unstd")
  # afttestplot(result03_afttest_link_mns,stdType="std")
} else if (run_ID == 15){
  # ------------------------------------ form ------------------------------------
  result03_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result03_afttest_form1_mns$p_value
  result03_afttest_form1_mns$p_std_value
  # afttestplot(result03_afttest_form1_mns,stdType="unstd")
  # afttestplot(result03_afttest_form1_mns,stdType="std")
} else if (run_ID == 16){
  result03_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result03_afttest_form2_mns$p_value
  result03_afttest_form2_mns$p_std_value
  # afttestplot(result03_afttest_form2_mns,stdType="unstd")
  # afttestplot(result03_afttest_form2_mns,stdType="std")
} else if (run_ID == 17){
  result03_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result03_afttest_form3_mns$p_value
  result03_afttest_form3_mns$p_std_value
  # afttestplot(result03_afttest_form3_mns,stdType="unstd")
  # afttestplot(result03_afttest_form3_mns,stdType="std")
} else if (run_ID == 18){
  result03_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result03_afttest_form4_mns$p_value
  result03_afttest_form4_mns$p_std_value
  # afttestplot(result03_afttest_form4_mns,stdType="unstd")
  # afttestplot(result03_afttest_form4_mns,stdType="std")
} else if (run_ID == 19){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: bili+prot+log_albu+age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result04_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result04_afttest_omni_mns$p_value
  result04_afttest_omni_mns$p_std_value
  # afttestplot(result04_afttest_omni_mns,stdType="unstd")
  # afttestplot(result04_afttest_omni_mns,stdType="std")
} else if (run_ID == 20){
  # ------------------------------------ link ------------------------------------
  result04_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result04_afttest_link_mns$p_value
  result04_afttest_link_mns$p_std_value
  # afttestplot(result04_afttest_link_mns,stdType="unstd")
  # afttestplot(result04_afttest_link_mns,stdType="std")
} else if (run_ID == 21){
  # ------------------------------------ form ------------------------------------
  result04_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result04_afttest_form1_mns$p_value
  result04_afttest_form1_mns$p_std_value
  # afttestplot(result04_afttest_form1_mns,stdType="unstd")
  # afttestplot(result04_afttest_form1_mns,stdType="std")
} else if (run_ID == 22){
  result04_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result04_afttest_form2_mns$p_value
  result04_afttest_form2_mns$p_std_value
  # afttestplot(result04_afttest_form2_mns,stdType="unstd")
  # afttestplot(result04_afttest_form2_mns,stdType="std")
} else if (run_ID == 23){
  result04_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result04_afttest_form3_mns$p_value
  result04_afttest_form3_mns$p_std_value
  # afttestplot(result04_afttest_form3_mns,stdType="unstd")
  # afttestplot(result04_afttest_form3_mns,stdType="std")
} else if (run_ID == 24){
  result04_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result04_afttest_form4_mns$p_value
  result04_afttest_form4_mns$p_std_value
  # afttestplot(result04_afttest_form4_mns,stdType="unstd")
  # afttestplot(result04_afttest_form4_mns,stdType="std")
} else if (run_ID == 25){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: bili+prot+albu+log_age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result05_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result05_afttest_omni_mns$p_value
  result05_afttest_omni_mns$p_std_value
  # afttestplot(result05_afttest_omni_mns,stdType="unstd")
  # afttestplot(result05_afttest_omni_mns,stdType="std")
} else if (run_ID == 26){
  # ------------------------------------ link ------------------------------------
  result05_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result05_afttest_link_mns$p_value
  result05_afttest_link_mns$p_std_value
  # afttestplot(result05_afttest_link_mns,stdType="unstd")
  # afttestplot(result05_afttest_link_mns,stdType="std")
} else if (run_ID == 27){
  # ------------------------------------ form ------------------------------------
  result05_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result05_afttest_form1_mns$p_value
  result05_afttest_form1_mns$p_std_value
  # afttestplot(result05_afttest_form1_mns,stdType="unstd")
  # afttestplot(result05_afttest_form1_mns,stdType="std")
} else if (run_ID == 28){
  result05_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result05_afttest_form2_mns$p_value
  result05_afttest_form2_mns$p_std_value
  # afttestplot(result05_afttest_form2_mns,stdType="unstd")
  # afttestplot(result05_afttest_form2_mns,stdType="std")
} else if (run_ID == 29){
  result05_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result05_afttest_form3_mns$p_value
  result05_afttest_form3_mns$p_std_value
  # afttestplot(result05_afttest_form3_mns,stdType="unstd")
  # afttestplot(result05_afttest_form3_mns,stdType="std")
} else if (run_ID == 30){
  result05_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result05_afttest_form4_mns$p_value
  result05_afttest_form4_mns$p_std_value
  # afttestplot(result05_afttest_form4_mns,stdType="unstd")
  # afttestplot(result05_afttest_form4_mns,stdType="std")
} else if (run_ID == 31){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: log_bili+log_prot+albu+age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result06_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result06_afttest_omni_mns$p_value
  result06_afttest_omni_mns$p_std_value
  # afttestplot(result06_afttest_omni_mns,stdType="unstd")
  # afttestplot(result06_afttest_omni_mns,stdType="std")
} else if (run_ID == 32){
  # ------------------------------------ link ------------------------------------
  result06_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result06_afttest_link_mns$p_value
  result06_afttest_link_mns$p_std_value
  # afttestplot(result06_afttest_link_mns,stdType="unstd")
  # afttestplot(result06_afttest_link_mns,stdType="std")
} else if (run_ID == 33){
  # ------------------------------------ form ------------------------------------
  result06_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result06_afttest_form1_mns$p_value
  result06_afttest_form1_mns$p_std_value
  # afttestplot(result06_afttest_form1_mns,stdType="unstd")
  # afttestplot(result06_afttest_form1_mns,stdType="std")
} else if (run_ID == 34){
  result06_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result06_afttest_form2_mns$p_value
  result06_afttest_form2_mns$p_std_value
  # afttestplot(result06_afttest_form2_mns,stdType="unstd")
  # afttestplot(result06_afttest_form2_mns,stdType="std")
} else if (run_ID == 35){
  result06_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result06_afttest_form3_mns$p_value
  result06_afttest_form3_mns$p_std_value
  # afttestplot(result06_afttest_form3_mns,stdType="unstd")
  # afttestplot(result06_afttest_form3_mns,stdType="std")
} else if (run_ID == 36){
  result06_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result06_afttest_form4_mns$p_value
  result06_afttest_form4_mns$p_std_value
  # afttestplot(result06_afttest_form4_mns,stdType="unstd")
  # afttestplot(result06_afttest_form4_mns,stdType="std")
} else if (run_ID == 37){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: log_bili+prot+log_albu+age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result07_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result07_afttest_omni_mns$p_value
  result07_afttest_omni_mns$p_std_value
  # afttestplot(result07_afttest_omni_mns,stdType="unstd")
  # afttestplot(result07_afttest_omni_mns,stdType="std")
} else if (run_ID == 38){
  # ------------------------------------ link ------------------------------------
  result07_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result07_afttest_link_mns$p_value
  result07_afttest_link_mns$p_std_value
  # afttestplot(result07_afttest_link_mns,stdType="unstd")
  # afttestplot(result07_afttest_link_mns,stdType="std")
} else if (run_ID == 39){
  # ------------------------------------ form ------------------------------------
  result07_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result07_afttest_form1_mns$p_value
  result07_afttest_form1_mns$p_std_value
  # afttestplot(result07_afttest_form1_mns,stdType="unstd")
  # afttestplot(result07_afttest_form1_mns,stdType="std")
} else if (run_ID == 40){
  result07_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result07_afttest_form2_mns$p_value
  result07_afttest_form2_mns$p_std_value
  # afttestplot(result07_afttest_form2_mns,stdType="unstd")
  # afttestplot(result07_afttest_form2_mns,stdType="std")
} else if (run_ID == 41){
  result07_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result07_afttest_form3_mns$p_value
  result07_afttest_form3_mns$p_std_value
  # afttestplot(result07_afttest_form3_mns,stdType="unstd")
  # afttestplot(result07_afttest_form3_mns,stdType="std")
} else if (run_ID == 42){
  result07_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result07_afttest_form4_mns$p_value
  result07_afttest_form4_mns$p_std_value
  # afttestplot(result07_afttest_form4_mns,stdType="unstd")
  # afttestplot(result07_afttest_form4_mns,stdType="std")
} else if (run_ID == 43){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: log_bili+prot+albu+log_age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result08_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result08_afttest_omni_mns$p_value
  result08_afttest_omni_mns$p_std_value
  # afttestplot(result08_afttest_omni_mns,stdType="unstd")
  # afttestplot(result08_afttest_omni_mns,stdType="std")
} else if (run_ID == 44){
  # ------------------------------------ link ------------------------------------
  result08_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result08_afttest_link_mns$p_value
  result08_afttest_link_mns$p_std_value
  # afttestplot(result08_afttest_link_mns,stdType="unstd")
  # afttestplot(result08_afttest_link_mns,stdType="std")
} else if (run_ID == 45){
  # ------------------------------------ form ------------------------------------
  result08_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result08_afttest_form1_mns$p_value
  result08_afttest_form1_mns$p_std_value
  # afttestplot(result08_afttest_form1_mns,stdType="unstd")
  # afttestplot(result08_afttest_form1_mns,stdType="std")
} else if (run_ID == 46){
  result08_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result08_afttest_form2_mns$p_value
  result08_afttest_form2_mns$p_std_value
  # afttestplot(result08_afttest_form2_mns,stdType="unstd")
  # afttestplot(result08_afttest_form2_mns,stdType="std")
} else if (run_ID == 47){
  result08_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result08_afttest_form3_mns$p_value
  result08_afttest_form3_mns$p_std_value
  # afttestplot(result08_afttest_form3_mns,stdType="unstd")
  # afttestplot(result08_afttest_form3_mns,stdType="std")
} else if (run_ID == 48){
  result08_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result08_afttest_form4_mns$p_value
  result08_afttest_form4_mns$p_std_value
  # afttestplot(result08_afttest_form4_mns,stdType="unstd")
  # afttestplot(result08_afttest_form4_mns,stdType="std")
} else if (run_ID == 49){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: bili+log_prot+log_albu+age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result09_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result09_afttest_omni_mns$p_value
  result09_afttest_omni_mns$p_std_value
  # afttestplot(result09_afttest_omni_mns,stdType="unstd")
  # afttestplot(result09_afttest_omni_mns,stdType="std")
} else if (run_ID == 50){
  # ------------------------------------ link ------------------------------------
  result09_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result09_afttest_link_mns$p_value
  result09_afttest_link_mns$p_std_value
  # afttestplot(result09_afttest_link_mns,stdType="unstd")
  # afttestplot(result09_afttest_link_mns,stdType="std")
} else if (run_ID == 51){
  # ------------------------------------ form ------------------------------------
  result09_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result09_afttest_form1_mns$p_value
  result09_afttest_form1_mns$p_std_value
  # afttestplot(result09_afttest_form1_mns,stdType="unstd")
  # afttestplot(result09_afttest_form1_mns,stdType="std")
} else if (run_ID == 52){
  result09_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result09_afttest_form2_mns$p_value
  result09_afttest_form2_mns$p_std_value
  # afttestplot(result09_afttest_form2_mns,stdType="unstd")
  # afttestplot(result09_afttest_form2_mns,stdType="std")
} else if (run_ID == 53){
  result09_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result09_afttest_form3_mns$p_value
  result09_afttest_form3_mns$p_std_value
  # afttestplot(result09_afttest_form3_mns,stdType="unstd")
  # afttestplot(result09_afttest_form3_mns,stdType="std")
} else if (run_ID == 54){
  result09_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result09_afttest_form4_mns$p_value
  result09_afttest_form4_mns$p_std_value
  # afttestplot(result09_afttest_form4_mns,stdType="unstd")
  # afttestplot(result09_afttest_form4_mns,stdType="std")
} else if (run_ID == 55){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: bili+log_prot+albu+log_age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result10_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result10_afttest_omni_mns$p_value
  result10_afttest_omni_mns$p_std_value
  # afttestplot(result10_afttest_omni_mns,stdType="unstd")
  # afttestplot(result10_afttest_omni_mns,stdType="std")
} else if (run_ID == 56){
  # ------------------------------------ link ------------------------------------
  result10_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result10_afttest_link_mns$p_value
  result10_afttest_link_mns$p_std_value
  # afttestplot(result10_afttest_link_mns,stdType="unstd")
  # afttestplot(result10_afttest_link_mns,stdType="std")
} else if (run_ID == 57){
  # ------------------------------------ form ------------------------------------
  result10_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result10_afttest_form1_mns$p_value
  result10_afttest_form1_mns$p_std_value
  # afttestplot(result10_afttest_form1_mns,stdType="unstd")
  # afttestplot(result10_afttest_form1_mns,stdType="std")
} else if (run_ID == 58){
  result10_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result10_afttest_form2_mns$p_value
  result10_afttest_form2_mns$p_std_value
  # afttestplot(result10_afttest_form2_mns,stdType="unstd")
  # afttestplot(result10_afttest_form2_mns,stdType="std")
} else if (run_ID == 59){
  result10_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result10_afttest_form3_mns$p_value
  result10_afttest_form3_mns$p_std_value
  # afttestplot(result10_afttest_form3_mns,stdType="unstd")
  # afttestplot(result10_afttest_form3_mns,stdType="std")
} else if (run_ID == 60){
  result10_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result10_afttest_form4_mns$p_value
  result10_afttest_form4_mns$p_std_value
  # afttestplot(result10_afttest_form4_mns,stdType="unstd")
  # afttestplot(result10_afttest_form4_mns,stdType="std")
} else if (run_ID == 61){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: bili+prot+log_albu+log_age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result11_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result11_afttest_omni_mns$p_value
  result11_afttest_omni_mns$p_std_value
  # afttestplot(result11_afttest_omni_mns,stdType="unstd")
  # afttestplot(result11_afttest_omni_mns,stdType="std")
} else if (run_ID == 62){
  # ------------------------------------ link ------------------------------------
  result11_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result11_afttest_link_mns$p_value
  result11_afttest_link_mns$p_std_value
  # afttestplot(result11_afttest_link_mns,stdType="unstd")
  # afttestplot(result11_afttest_link_mns,stdType="std")
} else if (run_ID == 63){
  # ------------------------------------ form ------------------------------------
  result11_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result11_afttest_form1_mns$p_value
  result11_afttest_form1_mns$p_std_value
  # afttestplot(result11_afttest_form1_mns,stdType="unstd")
  # afttestplot(result11_afttest_form1_mns,stdType="std")
} else if (run_ID == 64){
  result11_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result11_afttest_form2_mns$p_value
  result11_afttest_form2_mns$p_std_value
  # afttestplot(result11_afttest_form2_mns,stdType="unstd")
  # afttestplot(result11_afttest_form2_mns,stdType="std")
} else if (run_ID == 65){
  result11_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result11_afttest_form3_mns$p_value
  result11_afttest_form3_mns$p_std_value
  # afttestplot(result11_afttest_form3_mns,stdType="unstd")
  # afttestplot(result11_afttest_form3_mns,stdType="std")
} else if (run_ID == 66){
  result11_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result11_afttest_form4_mns$p_value
  result11_afttest_form4_mns$p_std_value
  # afttestplot(result11_afttest_form4_mns,stdType="unstd")
  # afttestplot(result11_afttest_form4_mns,stdType="std")
} else if (run_ID == 67){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: log_bili+log_prot+log_albu+age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result12_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mns")
  result12_afttest_omni_mns$p_value
  result12_afttest_omni_mns$p_std_value
  # afttestplot(result12_afttest_omni_mns,stdType="unstd")
  # afttestplot(result12_afttest_omni_mns,stdType="std")
} else if (run_ID == 68){
  # ------------------------------------ link ------------------------------------
  result12_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mns")
  result12_afttest_link_mns$p_value
  result12_afttest_link_mns$p_std_value
  # afttestplot(result12_afttest_link_mns,stdType="unstd")
  # afttestplot(result12_afttest_link_mns,stdType="std")
} else if (run_ID == 69){
  # ------------------------------------ form ------------------------------------
  result12_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result12_afttest_form1_mns$p_value
  result12_afttest_form1_mns$p_std_value
  # afttestplot(result12_afttest_form1_mns,stdType="unstd")
  # afttestplot(result12_afttest_form1_mns,stdType="std")
} else if (run_ID == 70){
  result12_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result12_afttest_form2_mns$p_value
  result12_afttest_form2_mns$p_std_value
  # afttestplot(result12_afttest_form2_mns,stdType="unstd")
  # afttestplot(result12_afttest_form2_mns,stdType="std")
} else if (run_ID == 71){
  result12_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result12_afttest_form3_mns$p_value
  result12_afttest_form3_mns$p_std_value
  # afttestplot(result12_afttest_form3_mns,stdType="unstd")
  # afttestplot(result12_afttest_form3_mns,stdType="std")
} else if (run_ID == 72){
  result12_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mns",form="age")
  result12_afttest_form4_mns$p_value
  result12_afttest_form4_mns$p_std_value
  # afttestplot(result12_afttest_form4_mns,stdType="unstd")
  # afttestplot(result12_afttest_form4_mns,stdType="std")
} else if (run_ID == 73){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: log_bili+log_prot+albu+log_age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result13_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result13_afttest_omni_mns$p_value
  result13_afttest_omni_mns$p_std_value
  # afttestplot(result13_afttest_omni_mns,stdType="unstd")
  # afttestplot(result13_afttest_omni_mns,stdType="std")
} else if (run_ID == 74){
  # ------------------------------------ link ------------------------------------
  result13_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result13_afttest_link_mns$p_value
  result13_afttest_link_mns$p_std_value
  # afttestplot(result13_afttest_link_mns,stdType="unstd")
  # afttestplot(result13_afttest_link_mns,stdType="std")
} else if (run_ID == 75){
  # ------------------------------------ form ------------------------------------
  result13_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result13_afttest_form1_mns$p_value
  result13_afttest_form1_mns$p_std_value
  # afttestplot(result13_afttest_form1_mns,stdType="unstd")
  # afttestplot(result13_afttest_form1_mns,stdType="std")
} else if (run_ID == 76){
  result13_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result13_afttest_form2_mns$p_value
  result13_afttest_form2_mns$p_std_value
  # afttestplot(result13_afttest_form2_mns,stdType="unstd")
  # afttestplot(result13_afttest_form2_mns,stdType="std")
} else if (run_ID == 77){
  result13_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="albu")
  result13_afttest_form3_mns$p_value
  result13_afttest_form3_mns$p_std_value
  # afttestplot(result13_afttest_form3_mns,stdType="unstd")
  # afttestplot(result13_afttest_form3_mns,stdType="std")
} else if (run_ID == 78){
  result13_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result13_afttest_form4_mns$p_value
  result13_afttest_form4_mns$p_std_value
  # afttestplot(result13_afttest_form4_mns,stdType="unstd")
  # afttestplot(result13_afttest_form4_mns,stdType="std")
} else if (run_ID == 79){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: log_bili+prot+log_albu+log_age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result14_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result14_afttest_omni_mns$p_value
  result14_afttest_omni_mns$p_std_value
  # afttestplot(result14_afttest_omni_mns,stdType="unstd")
  # afttestplot(result14_afttest_omni_mns,stdType="std")
} else if (run_ID == 80){
  # ------------------------------------ link ------------------------------------
  result14_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result14_afttest_link_mns$p_value
  result14_afttest_link_mns$p_std_value
  # afttestplot(result14_afttest_link_mns,stdType="unstd")
  # afttestplot(result14_afttest_link_mns,stdType="std")
} else if (run_ID == 81){
  # ------------------------------------ form ------------------------------------
  result14_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result14_afttest_form1_mns$p_value
  result14_afttest_form1_mns$p_std_value
  # afttestplot(result14_afttest_form1_mns,stdType="unstd")
  # afttestplot(result14_afttest_form1_mns,stdType="std")
} else if (run_ID == 82){
  result14_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="prot")
  result14_afttest_form2_mns$p_value
  result14_afttest_form2_mns$p_std_value
  # afttestplot(result14_afttest_form2_mns,stdType="unstd")
  # afttestplot(result14_afttest_form2_mns,stdType="std")
} else if (run_ID == 83){
  result14_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result14_afttest_form3_mns$p_value
  result14_afttest_form3_mns$p_std_value
  # afttestplot(result14_afttest_form3_mns,stdType="unstd")
  # afttestplot(result14_afttest_form3_mns,stdType="std")
} else if (run_ID == 84){
  result14_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result14_afttest_form4_mns$p_value
  result14_afttest_form4_mns$p_std_value
  # afttestplot(result14_afttest_form4_mns,stdType="unstd")
  # afttestplot(result14_afttest_form4_mns,stdType="std")
} else if (run_ID == 85){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: bili+log_prot+log_albu+log_age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result15_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result15_afttest_omni_mns$p_value
  result15_afttest_omni_mns$p_std_value
  # afttestplot(result15_afttest_omni_mns,stdType="unstd")
  # afttestplot(result15_afttest_omni_mns,stdType="std")
} else if (run_ID == 86){
  # ------------------------------------ link ------------------------------------
  result15_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result15_afttest_link_mns$p_value
  result15_afttest_link_mns$p_std_value
  # afttestplot(result15_afttest_link_mns,stdType="unstd")
  # afttestplot(result15_afttest_link_mns,stdType="std")
} else if (run_ID == 87){
  # ------------------------------------ form ------------------------------------
  result15_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="bili")
  result15_afttest_form1_mns$p_value
  result15_afttest_form1_mns$p_std_value
  # afttestplot(result15_afttest_form1_mns,stdType="unstd")
  # afttestplot(result15_afttest_form1_mns,stdType="std")
} else if (run_ID == 88){
  result15_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result15_afttest_form2_mns$p_value
  result15_afttest_form2_mns$p_std_value
  # afttestplot(result15_afttest_form2_mns,stdType="unstd")
  # afttestplot(result15_afttest_form2_mns,stdType="std")
} else if (run_ID == 89){
  result15_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result15_afttest_form3_mns$p_value
  result15_afttest_form3_mns$p_std_value
  # afttestplot(result15_afttest_form3_mns,stdType="unstd")
  # afttestplot(result15_afttest_form3_mns,stdType="std")
} else if (run_ID == 90){
  result15_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result15_afttest_form4_mns$p_value
  result15_afttest_form4_mns$p_std_value
  # afttestplot(result15_afttest_form4_mns,stdType="unstd")
  # afttestplot(result15_afttest_form4_mns,stdType="std")
} else if (run_ID == 91){
  # ------------------------------------------------------------------------------
  # --------- Covariates: log_bili+log_prot+log_albu+log_age+edem+trt ---------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result16_afttest_omni_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mns")
  result16_afttest_omni_mns$p_value
  result16_afttest_omni_mns$p_std_value
  # afttestplot(result16_afttest_omni_mns,stdType="unstd")
  # afttestplot(result16_afttest_omni_mns,stdType="std")
} else if (run_ID == 92){
  # ------------------------------------ link ------------------------------------
  result16_afttest_link_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mns")
  result16_afttest_link_mns$p_value
  result16_afttest_link_mns$p_std_value
  # afttestplot(result16_afttest_link_mns,stdType="unstd")
  # afttestplot(result16_afttest_link_mns,stdType="std")
} else if (run_ID == 93){
  # ------------------------------------ form ------------------------------------
  result16_afttest_form1_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_bili")
  result16_afttest_form1_mns$p_value
  result16_afttest_form1_mns$p_std_value
  # afttestplot(result16_afttest_form1_mns,stdType="unstd")
  # afttestplot(result16_afttest_form1_mns,stdType="std")
} else if (run_ID == 94){
  result16_afttest_form2_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_prot")
  result16_afttest_form2_mns$p_value
  result16_afttest_form2_mns$p_std_value
  # afttestplot(result16_afttest_form2_mns,stdType="unstd")
  # afttestplot(result16_afttest_form2_mns,stdType="std")
} else if (run_ID == 95){
  result16_afttest_form3_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_albu")
  result16_afttest_form3_mns$p_value
  result16_afttest_form3_mns$p_std_value
  # afttestplot(result16_afttest_form3_mns,stdType="unstd")
  # afttestplot(result16_afttest_form3_mns,stdType="std")
} else if (run_ID == 96){
  result16_afttest_form4_mns=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mns",form="log_age")
  result16_afttest_form4_mns$p_value
  result16_afttest_form4_mns$p_std_value
  # afttestplot(result16_afttest_form4_mns,stdType="unstd")
  # afttestplot(result16_afttest_form4_mns,stdType="std")
} else if (run_ID == 97){
  # ------------------------------------------------------------------------------
  # ------------------------------------ "mis" -----------------------------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------
  # ----------------- Covariates: bili+prot+albu+age+edem+trt -----------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result01_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result01_afttest_omni_mis$p_value
  result01_afttest_omni_mis$p_std_value
  # afttestplot(result01_afttest_omni_mis,stdType="unstd")
  # afttestplot(result01_afttest_omni_mis_mis,stdType="std")
} else if (run_ID == 98){
  # ------------------------------------ link ------------------------------------
  result01_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result01_afttest_link_mis$p_value
  result01_afttest_link_mis$p_std_value
  # afttestplot(result01_afttest_link_mis,stdType="unstd")
  # afttestplot(result01_afttest_link_mis,stdType="std")
} else if (run_ID == 99){
  # ------------------------------------ form ------------------------------------
  result01_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result01_afttest_form1_mis$p_value
  result01_afttest_form1_mis$p_std_value
  # afttestplot(result01_afttest_form1_mis,stdType="unstd")
  # afttestplot(result01_afttest_form1_mis,stdType="std")
} else if (run_ID == 100){
  result01_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result01_afttest_form2_mis$p_value
  result01_afttest_form2_mis$p_std_value
  # afttestplot(result01_afttest_form2_mis,stdType="unstd")
  # afttestplot(result01_afttest_form2_mis,stdType="std")
} else if (run_ID == 101){
  result01_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result01_afttest_form3_mis$p_value
  result01_afttest_form3_mis$p_std_value
  # afttestplot(result01_afttest_form3_mis,stdType="unstd")
  # afttestplot(result01_afttest_form3_mis,stdType="std")
} else if (run_ID == 102){
  result01_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result01_afttest_form4_mis$p_value
  result01_afttest_form4_mis$p_std_value
  # afttestplot(result01_afttest_form4_mis,stdType="unstd")
  # afttestplot(result01_afttest_form4_mis,stdType="std")
} else if (run_ID == 103){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: log_bili+prot+albu+age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result02_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result02_afttest_omni_mis$p_value
  result02_afttest_omni_mis$p_std_value
  # afttestplot(result02_afttest_omni_mis,stdType="unstd")
  # afttestplot(result02_afttest_omni_mis,stdType="std")
} else if (run_ID == 104){
  # ------------------------------------ link ------------------------------------
  result02_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result02_afttest_link_mis$p_value
  result02_afttest_link_mis$p_std_value
  # afttestplot(result02_afttest_link_mis,stdType="unstd")
  # afttestplot(result02_afttest_link_mis,stdType="std")
} else if (run_ID == 105){
  # ------------------------------------ form ------------------------------------
  result02_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result02_afttest_form1_mis$p_value
  result02_afttest_form1_mis$p_std_value
  # afttestplot(result02_afttest_form1_mis,stdType="unstd")
  # afttestplot(result02_afttest_form1_mis,stdType="std")
} else if (run_ID == 106){
  result02_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result02_afttest_form2_mis$p_value
  result02_afttest_form2_mis$p_std_value
  # afttestplot(result02_afttest_form2_mis,stdType="unstd")
  # afttestplot(result02_afttest_form2_mis,stdType="std")
} else if (run_ID == 107){
  result02_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result02_afttest_form3_mis$p_value
  result02_afttest_form3_mis$p_std_value
  # afttestplot(result02_afttest_form3_mis,stdType="unstd")
  # afttestplot(result02_afttest_form3_mis,stdType="std")
} else if (run_ID == 108){
  result02_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result02_afttest_form4_mis$p_value
  result02_afttest_form4_mis$p_std_value
  # afttestplot(result02_afttest_form4_mis,stdType="unstd")
  # afttestplot(result02_afttest_form4_mis,stdType="std")
} else if (run_ID == 109){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: bili+log_prot+albu+age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result03_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result03_afttest_omni_mis$p_value
  result03_afttest_omni_mis$p_std_value
  # afttestplot(result03_afttest_omni_mis,stdType="unstd")
  # afttestplot(result03_afttest_omni_mis,stdType="std")
} else if (run_ID == 110){
  # ------------------------------------ link ------------------------------------
  result03_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result03_afttest_link_mis$p_value
  result03_afttest_link_mis$p_std_value
  # afttestplot(result03_afttest_link_mis,stdType="unstd")
  # afttestplot(result03_afttest_link_mis,stdType="std")
} else if (run_ID == 111){
  # ------------------------------------ form ------------------------------------
  result03_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result03_afttest_form1_mis$p_value
  result03_afttest_form1_mis$p_std_value
  # afttestplot(result03_afttest_form1_mis,stdType="unstd")
  # afttestplot(result03_afttest_form1_mis,stdType="std")
} else if (run_ID == 112){
  result03_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result03_afttest_form2_mis$p_value
  result03_afttest_form2_mis$p_std_value
  # afttestplot(result03_afttest_form2_mis,stdType="unstd")
  # afttestplot(result03_afttest_form2_mis,stdType="std")
} else if (run_ID == 113){
  result03_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result03_afttest_form3_mis$p_value
  result03_afttest_form3_mis$p_std_value
  # afttestplot(result03_afttest_form3_mis,stdType="unstd")
  # afttestplot(result03_afttest_form3_mis,stdType="std")
} else if (run_ID == 114){
  result03_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result03_afttest_form4_mis$p_value
  result03_afttest_form4_mis$p_std_value
  # afttestplot(result03_afttest_form4_mis,stdType="unstd")
  # afttestplot(result03_afttest_form4_mis,stdType="std")
} else if (run_ID == 115){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: bili+prot+log_albu+age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result04_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result04_afttest_omni_mis$p_value
  result04_afttest_omni_mis$p_std_value
  # afttestplot(result04_afttest_omni_mis,stdType="unstd")
  # afttestplot(result04_afttest_omni_mis,stdType="std")
} else if (run_ID == 116){
  # ------------------------------------ link ------------------------------------
  result04_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result04_afttest_link_mis$p_value
  result04_afttest_link_mis$p_std_value
  # afttestplot(result04_afttest_link_mis,stdType="unstd")
  # afttestplot(result04_afttest_link_mis,stdType="std")
} else if (run_ID == 117){
  # ------------------------------------ form ------------------------------------
  result04_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result04_afttest_form1_mis$p_value
  result04_afttest_form1_mis$p_std_value
  # afttestplot(result04_afttest_form1_mis,stdType="unstd")
  # afttestplot(result04_afttest_form1_mis,stdType="std")
} else if (run_ID == 118){
  result04_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result04_afttest_form2_mis$p_value
  result04_afttest_form2_mis$p_std_value
  # afttestplot(result04_afttest_form2_mis,stdType="unstd")
  # afttestplot(result04_afttest_form2_mis,stdType="std")
} else if (run_ID == 119){
  result04_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result04_afttest_form3_mis$p_value
  result04_afttest_form3_mis$p_std_value
  # afttestplot(result04_afttest_form3_mis,stdType="unstd")
  # afttestplot(result04_afttest_form3_mis,stdType="std")
} else if (run_ID == 120){
  result04_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result04_afttest_form4_mis$p_value
  result04_afttest_form4_mis$p_std_value
  # afttestplot(result04_afttest_form4_mis,stdType="unstd")
  # afttestplot(result04_afttest_form4_mis,stdType="std")
} else if (run_ID == 121){
  # ------------------------------------------------------------------------------
  # --------------- Covariates: bili+prot+albu+log_age+edem+trt ---------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result05_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result05_afttest_omni_mis$p_value
  result05_afttest_omni_mis$p_std_value
  # afttestplot(result05_afttest_omni_mis,stdType="unstd")
  # afttestplot(result05_afttest_omni_mis,stdType="std")
} else if (run_ID == 122){
  # ------------------------------------ link ------------------------------------
  result05_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result05_afttest_link_mis$p_value
  result05_afttest_link_mis$p_std_value
  # afttestplot(result05_afttest_link_mis,stdType="unstd")
  # afttestplot(result05_afttest_link_mis,stdType="std")
} else if (run_ID == 123){
  # ------------------------------------ form ------------------------------------
  result05_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result05_afttest_form1_mis$p_value
  result05_afttest_form1_mis$p_std_value
  # afttestplot(result05_afttest_form1_mis,stdType="unstd")
  # afttestplot(result05_afttest_form1_mis,stdType="std")
} else if (run_ID == 124){
  result05_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result05_afttest_form2_mis$p_value
  result05_afttest_form2_mis$p_std_value
  # afttestplot(result05_afttest_form2_mis,stdType="unstd")
  # afttestplot(result05_afttest_form2_mis,stdType="std")
} else if (run_ID == 125){
  result05_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result05_afttest_form3_mis$p_value
  result05_afttest_form3_mis$p_std_value
  # afttestplot(result05_afttest_form3_mis,stdType="unstd")
  # afttestplot(result05_afttest_form3_mis,stdType="std")
} else if (run_ID == 126){
  result05_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result05_afttest_form4_mis$p_value
  result05_afttest_form4_mis$p_std_value
  # afttestplot(result05_afttest_form4_mis,stdType="unstd")
  # afttestplot(result05_afttest_form4_mis,stdType="std")
} else if (run_ID == 127){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: log_bili+log_prot+albu+age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result06_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result06_afttest_omni_mis$p_value
  result06_afttest_omni_mis$p_std_value
  # afttestplot(result06_afttest_omni_mis,stdType="unstd")
  # afttestplot(result06_afttest_omni_mis,stdType="std")
} else if (run_ID == 128){
  # ------------------------------------ link ------------------------------------
  result06_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result06_afttest_link_mis$p_value
  result06_afttest_link_mis$p_std_value
  # afttestplot(result06_afttest_link_mis,stdType="unstd")
  # afttestplot(result06_afttest_link_mis,stdType="std")
} else if (run_ID == 129){
  # ------------------------------------ form ------------------------------------
  result06_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result06_afttest_form1_mis$p_value
  result06_afttest_form1_mis$p_std_value
  # afttestplot(result06_afttest_form1_mis,stdType="unstd")
  # afttestplot(result06_afttest_form1_mis,stdType="std")
} else if (run_ID == 130){
  result06_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result06_afttest_form2_mis$p_value
  result06_afttest_form2_mis$p_std_value
  # afttestplot(result06_afttest_form2_mis,stdType="unstd")
  # afttestplot(result06_afttest_form2_mis,stdType="std")
} else if (run_ID == 131){
  result06_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result06_afttest_form3_mis$p_value
  result06_afttest_form3_mis$p_std_value
  # afttestplot(result06_afttest_form3_mis,stdType="unstd")
  # afttestplot(result06_afttest_form3_mis,stdType="std")
} else if (run_ID == 132){
  result06_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result06_afttest_form4_mis$p_value
  result06_afttest_form4_mis$p_std_value
  # afttestplot(result06_afttest_form4_mis,stdType="unstd")
  # afttestplot(result06_afttest_form4_mis,stdType="std")
} else if (run_ID == 133){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: log_bili+prot+log_albu+age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result07_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result07_afttest_omni_mis$p_value
  result07_afttest_omni_mis$p_std_value
  # afttestplot(result07_afttest_omni_mis,stdType="unstd")
  # afttestplot(result07_afttest_omni_mis,stdType="std")
} else if (run_ID == 134){
  # ------------------------------------ link ------------------------------------
  result07_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result07_afttest_link_mis$p_value
  result07_afttest_link_mis$p_std_value
  # afttestplot(result07_afttest_link_mis,stdType="unstd")
  # afttestplot(result07_afttest_link_mis,stdType="std")
} else if (run_ID == 135){
  # ------------------------------------ form ------------------------------------
  result07_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result07_afttest_form1_mis$p_value
  result07_afttest_form1_mis$p_std_value
  # afttestplot(result07_afttest_form1_mis,stdType="unstd")
  # afttestplot(result07_afttest_form1_mis,stdType="std")
} else if (run_ID == 136){
  result07_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result07_afttest_form2_mis$p_value
  result07_afttest_form2_mis$p_std_value
  # afttestplot(result07_afttest_form2_mis,stdType="unstd")
  # afttestplot(result07_afttest_form2_mis,stdType="std")
} else if (run_ID == 137){
  result07_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result07_afttest_form3_mis$p_value
  result07_afttest_form3_mis$p_std_value
  # afttestplot(result07_afttest_form3_mis,stdType="unstd")
  # afttestplot(result07_afttest_form3_mis,stdType="std")
} else if (run_ID == 138){
  result07_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result07_afttest_form4_mis$p_value
  result07_afttest_form4_mis$p_std_value
  # afttestplot(result07_afttest_form4_mis,stdType="unstd")
  # afttestplot(result07_afttest_form4_mis,stdType="std")
} else if (run_ID == 139){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: log_bili+prot+albu+log_age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result08_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result08_afttest_omni_mis$p_value
  result08_afttest_omni_mis$p_std_value
  # afttestplot(result08_afttest_omni_mis,stdType="unstd")
  # afttestplot(result08_afttest_omni_mis,stdType="std")
} else if (run_ID == 140){
  # ------------------------------------ link ------------------------------------
  result08_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result08_afttest_link_mis$p_value
  result08_afttest_link_mis$p_std_value
  # afttestplot(result08_afttest_link_mis,stdType="unstd")
  # afttestplot(result08_afttest_link_mis,stdType="std")
} else if (run_ID == 141){
  # ------------------------------------ form ------------------------------------
  result08_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result08_afttest_form1_mis$p_value
  result08_afttest_form1_mis$p_std_value
  # afttestplot(result08_afttest_form1_mis,stdType="unstd")
  # afttestplot(result08_afttest_form1_mis,stdType="std")
} else if (run_ID == 142){
  result08_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result08_afttest_form2_mis$p_value
  result08_afttest_form2_mis$p_std_value
  # afttestplot(result08_afttest_form2_mis,stdType="unstd")
  # afttestplot(result08_afttest_form2_mis,stdType="std")
} else if (run_ID == 143){
  result08_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result08_afttest_form3_mis$p_value
  result08_afttest_form3_mis$p_std_value
  # afttestplot(result08_afttest_form3_mis,stdType="unstd")
  # afttestplot(result08_afttest_form3_mis,stdType="std")
} else if (run_ID == 144){
  result08_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result08_afttest_form4_mis$p_value
  result08_afttest_form4_mis$p_std_value
  # afttestplot(result08_afttest_form4_mis,stdType="unstd")
  # afttestplot(result08_afttest_form4_mis,stdType="std")
} else if (run_ID == 145){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: bili+log_prot+log_albu+age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result09_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result09_afttest_omni_mis$p_value
  result09_afttest_omni_mis$p_std_value
  # afttestplot(result09_afttest_omni_mis,stdType="unstd")
  # afttestplot(result09_afttest_omni_mis,stdType="std")
} else if (run_ID == 146){
  # ------------------------------------ link ------------------------------------
  result09_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result09_afttest_link_mis$p_value
  result09_afttest_link_mis$p_std_value
  # afttestplot(result09_afttest_link_mis,stdType="unstd")
  # afttestplot(result09_afttest_link_mis,stdType="std")
} else if (run_ID == 147){
  # ------------------------------------ form ------------------------------------
  result09_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result09_afttest_form1_mis$p_value
  result09_afttest_form1_mis$p_std_value
  # afttestplot(result09_afttest_form1_mis,stdType="unstd")
  # afttestplot(result09_afttest_form1_mis,stdType="std")
} else if (run_ID == 148){
  result09_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result09_afttest_form2_mis$p_value
  result09_afttest_form2_mis$p_std_value
  # afttestplot(result09_afttest_form2_mis,stdType="unstd")
  # afttestplot(result09_afttest_form2_mis,stdType="std")
} else if (run_ID == 149){
  result09_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result09_afttest_form3_mis$p_value
  result09_afttest_form3_mis$p_std_value
  # afttestplot(result09_afttest_form3_mis,stdType="unstd")
  # afttestplot(result09_afttest_form3_mis,stdType="std")
} else if (run_ID == 150){
  result09_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result09_afttest_form4_mis$p_value
  result09_afttest_form4_mis$p_std_value
  # afttestplot(result09_afttest_form4_mis,stdType="unstd")
  # afttestplot(result09_afttest_form4_mis,stdType="std")
} else if (run_ID == 151){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: bili+log_prot+albu+log_age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result10_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result10_afttest_omni_mis$p_value
  result10_afttest_omni_mis$p_std_value
  # afttestplot(result10_afttest_omni_mis,stdType="unstd")
  # afttestplot(result10_afttest_omni_mis,stdType="std")
} else if (run_ID == 152){
  # ------------------------------------ link ------------------------------------
  result10_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result10_afttest_link_mis$p_value
  result10_afttest_link_mis$p_std_value
  # afttestplot(result10_afttest_link_mis,stdType="unstd")
  # afttestplot(result10_afttest_link_mis,stdType="std")
} else if (run_ID == 153){
  # ------------------------------------ form ------------------------------------
  result10_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result10_afttest_form1_mis$p_value
  result10_afttest_form1_mis$p_std_value
  # afttestplot(result10_afttest_form1_mis,stdType="unstd")
  # afttestplot(result10_afttest_form1_mis,stdType="std")
} else if (run_ID == 154){
  result10_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result10_afttest_form2_mis$p_value
  result10_afttest_form2_mis$p_std_value
  # afttestplot(result10_afttest_form2_mis,stdType="unstd")
  # afttestplot(result10_afttest_form2_mis,stdType="std")
} else if (run_ID == 155){
  result10_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result10_afttest_form3_mis$p_value
  result10_afttest_form3_mis$p_std_value
  # afttestplot(result10_afttest_form3_mis,stdType="unstd")
  # afttestplot(result10_afttest_form3_mis,stdType="std")
} else if (run_ID == 156){
  result10_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result10_afttest_form4_mis$p_value
  result10_afttest_form4_mis$p_std_value
  # afttestplot(result10_afttest_form4_mis,stdType="unstd")
  # afttestplot(result10_afttest_form4_mis,stdType="std")
} else if (run_ID == 157){
  # ------------------------------------------------------------------------------
  # ------------- Covariates: bili+prot+log_albu+log_age+edem+trt -------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result11_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result11_afttest_omni_mis$p_value
  result11_afttest_omni_mis$p_std_value
  # afttestplot(result11_afttest_omni_mis,stdType="unstd")
  # afttestplot(result11_afttest_omni_mis,stdType="std")
} else if (run_ID == 158){
  # ------------------------------------ link ------------------------------------
  result11_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result11_afttest_link_mis$p_value
  result11_afttest_link_mis$p_std_value
  # afttestplot(result11_afttest_link_mis,stdType="unstd")
  # afttestplot(result11_afttest_link_mis,stdType="std")
} else if (run_ID == 159){
  # ------------------------------------ form ------------------------------------
  result11_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result11_afttest_form1_mis$p_value
  result11_afttest_form1_mis$p_std_value
  # afttestplot(result11_afttest_form1_mis,stdType="unstd")
  # afttestplot(result11_afttest_form1_mis,stdType="std")
} else if (run_ID == 160){
  result11_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result11_afttest_form2_mis$p_value
  result11_afttest_form2_mis$p_std_value
  # afttestplot(result11_afttest_form2_mis,stdType="unstd")
  # afttestplot(result11_afttest_form2_mis,stdType="std")
} else if (run_ID == 161){
  result11_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result11_afttest_form3_mis$p_value
  result11_afttest_form3_mis$p_std_value
  # afttestplot(result11_afttest_form3_mis,stdType="unstd")
  # afttestplot(result11_afttest_form3_mis,stdType="std")
} else if (run_ID == 162){
  result11_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result11_afttest_form4_mis$p_value
  result11_afttest_form4_mis$p_std_value
  # afttestplot(result11_afttest_form4_mis,stdType="unstd")
  # afttestplot(result11_afttest_form4_mis,stdType="std")
} else if (run_ID == 163){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: log_bili+log_prot+log_albu+age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result12_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="omni",eqType="mis")
  result12_afttest_omni_mis$p_value
  result12_afttest_omni_mis$p_std_value
  # afttestplot(result12_afttest_omni_mis,stdType="unstd")
  # afttestplot(result12_afttest_omni_mis,stdType="std")
} else if (run_ID == 164){
  # ------------------------------------ link ------------------------------------
  result12_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="link",eqType="mis")
  result12_afttest_link_mis$p_value
  result12_afttest_link_mis$p_std_value
  # afttestplot(result12_afttest_link_mis,stdType="unstd")
  # afttestplot(result12_afttest_link_mis,stdType="std")
} else if (run_ID == 165){
  # ------------------------------------ form ------------------------------------
  result12_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result12_afttest_form1_mis$p_value
  result12_afttest_form1_mis$p_std_value
  # afttestplot(result12_afttest_form1_mis,stdType="unstd")
  # afttestplot(result12_afttest_form1_mis,stdType="std")
} else if (run_ID == 166){
  result12_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result12_afttest_form2_mis$p_value
  result12_afttest_form2_mis$p_std_value
  # afttestplot(result12_afttest_form2_mis,stdType="unstd")
  # afttestplot(result12_afttest_form2_mis,stdType="std")
} else if (run_ID == 167){
  result12_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result12_afttest_form3_mis$p_value
  result12_afttest_form3_mis$p_std_value
  # afttestplot(result12_afttest_form3_mis,stdType="unstd")
  # afttestplot(result12_afttest_form3_mis,stdType="std")
} else if (run_ID == 168){
  result12_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+age+edem+trt,path=200,testType="form",eqType="mis",form="age")
  result12_afttest_form4_mis$p_value
  result12_afttest_form4_mis$p_std_value
  # afttestplot(result12_afttest_form4_mis,stdType="unstd")
  # afttestplot(result12_afttest_form4_mis,stdType="std")
} else if (run_ID == 169){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: log_bili+log_prot+albu+log_age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result13_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result13_afttest_omni_mis$p_value
  result13_afttest_omni_mis$p_std_value
  # afttestplot(result13_afttest_omni_mis,stdType="unstd")
  # afttestplot(result13_afttest_omni_mis,stdType="std")
} else if (run_ID == 170){
  # ------------------------------------ link ------------------------------------
  result13_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result13_afttest_link_mis$p_value
  result13_afttest_link_mis$p_std_value
  # afttestplot(result13_afttest_link_mis,stdType="unstd")
  # afttestplot(result13_afttest_link_mis,stdType="std")
} else if (run_ID == 171){
  # ------------------------------------ form ------------------------------------
  result13_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result13_afttest_form1_mis$p_value
  result13_afttest_form1_mis$p_std_value
  # afttestplot(result13_afttest_form1_mis,stdType="unstd")
  # afttestplot(result13_afttest_form1_mis,stdType="std")
} else if (run_ID == 172){
  result13_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result13_afttest_form2_mis$p_value
  result13_afttest_form2_mis$p_std_value
  # afttestplot(result13_afttest_form2_mis,stdType="unstd")
  # afttestplot(result13_afttest_form2_mis,stdType="std")
} else if (run_ID == 173){
  result13_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="albu")
  result13_afttest_form3_mis$p_value
  result13_afttest_form3_mis$p_std_value
  # afttestplot(result13_afttest_form3_mis,stdType="unstd")
  # afttestplot(result13_afttest_form3_mis,stdType="std")
} else if (run_ID == 174){
  result13_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result13_afttest_form4_mis$p_value
  result13_afttest_form4_mis$p_std_value
  # afttestplot(result13_afttest_form4_mis,stdType="unstd")
  # afttestplot(result13_afttest_form4_mis,stdType="std")
} else if (run_ID == 175){
  # ------------------------------------------------------------------------------
  # ----------- Covariates: log_bili+prot+log_albu+log_age+edem+trt -----------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result14_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result14_afttest_omni_mis$p_value
  result14_afttest_omni_mis$p_std_value
  # afttestplot(result14_afttest_omni_mis,stdType="unstd")
  # afttestplot(result14_afttest_omni_mis,stdType="std")
} else if (run_ID == 176){
  # ------------------------------------ link ------------------------------------
  result14_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result14_afttest_link_mis$p_value
  result14_afttest_link_mis$p_std_value
  # afttestplot(result14_afttest_link_mis,stdType="unstd")
  # afttestplot(result14_afttest_link_mis,stdType="std")
} else if (run_ID == 177){
  # ------------------------------------ form ------------------------------------
  result14_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result14_afttest_form1_mis$p_value
  result14_afttest_form1_mis$p_std_value
  # afttestplot(result14_afttest_form1_mis,stdType="unstd")
  # afttestplot(result14_afttest_form1_mis,stdType="std")
} else if (run_ID == 178){
  result14_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="prot")
  result14_afttest_form2_mis$p_value
  result14_afttest_form2_mis$p_std_value
  # afttestplot(result14_afttest_form2_mis,stdType="unstd")
  # afttestplot(result14_afttest_form2_mis,stdType="std")
} else if (run_ID == 179){
  result14_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result14_afttest_form3_mis$p_value
  result14_afttest_form3_mis$p_std_value
  # afttestplot(result14_afttest_form3_mis,stdType="unstd")
  # afttestplot(result14_afttest_form3_mis,stdType="std")
} else if (run_ID == 180){
  result14_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result14_afttest_form4_mis$p_value
  result14_afttest_form4_mis$p_std_value
  # afttestplot(result14_afttest_form4_mis,stdType="unstd")
  # afttestplot(result14_afttest_form4_mis,stdType="std")
} else if (run_ID == 181){
  # ------------------------------------------------------------------------------
  # ----------------- Covariates: bili+prot+albu+age+edem+trt -----------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result15_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result15_afttest_omni_mis$p_value
  result15_afttest_omni_mis$p_std_value
  # afttestplot(result15_afttest_omni_mis,stdType="unstd")
  # afttestplot(result15_afttest_omni_mis,stdType="std")
} else if (run_ID == 182){
  # ------------------------------------ link ------------------------------------
  result15_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result15_afttest_link_mis$p_value
  result15_afttest_link_mis$p_std_value
  # afttestplot(result15_afttest_link_mis,stdType="unstd")
  # afttestplot(result15_afttest_link_mis,stdType="std")
} else if (run_ID == 183){
  # ------------------------------------ form ------------------------------------
  result15_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="bili")
  result15_afttest_form1_mis$p_value
  result15_afttest_form1_mis$p_std_value
  # afttestplot(result15_afttest_form1_mis,stdType="unstd")
  # afttestplot(result15_afttest_form1_mis,stdType="std")
} else if (run_ID == 184){
  result15_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result15_afttest_form2_mis$p_value
  result15_afttest_form2_mis$p_std_value
  # afttestplot(result15_afttest_form2_mis,stdType="unstd")
  # afttestplot(result15_afttest_form2_mis,stdType="std")
} else if (run_ID == 185){
  result15_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result15_afttest_form3_mis$p_value
  result15_afttest_form3_mis$p_std_value
  # afttestplot(result15_afttest_form3_mis,stdType="unstd")
  # afttestplot(result15_afttest_form3_mis,stdType="std")
} else if (run_ID == 186){
  result15_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result15_afttest_form4_mis$p_value
  result15_afttest_form4_mis$p_std_value
  # afttestplot(result15_afttest_form4_mis,stdType="unstd")
  # afttestplot(result15_afttest_form4_mis,stdType="std")
} else if (run_ID == 187){
  # ------------------------------------------------------------------------------
  # --------- Covariates: log_bili+log_prot+log_albu+log_age+edem+trt ---------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------ omni ------------------------------------
  result16_afttest_omni_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="omni",eqType="mis")
  result16_afttest_omni_mis$p_value
  result16_afttest_omni_mis$p_std_value
  # afttestplot(result16_afttest_omni_mis,stdType="unstd")
  # afttestplot(result16_afttest_omni_mis,stdType="std")
} else if (run_ID == 188){
  # ------------------------------------ link ------------------------------------
  result16_afttest_link_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="link",eqType="mis")
  result16_afttest_link_mis$p_value
  result16_afttest_link_mis$p_std_value
  # afttestplot(result16_afttest_link_mis,stdType="unstd")
  # afttestplot(result16_afttest_link_mis,stdType="std")
} else if (run_ID == 189){
  # ------------------------------------ form ------------------------------------
  result16_afttest_form1_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_bili")
  result16_afttest_form1_mis$p_value
  result16_afttest_form1_mis$p_std_value
  # afttestplot(result16_afttest_form1_mis,stdType="unstd")
  # afttestplot(result16_afttest_form1_mis,stdType="std")
} else if (run_ID == 190){
  result16_afttest_form2_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_prot")
  result16_afttest_form2_mis$p_value
  result16_afttest_form2_mis$p_std_value
  # afttestplot(result16_afttest_form2_mis,stdType="unstd")
  # afttestplot(result16_afttest_form2_mis,stdType="std")
} else if (run_ID == 191){
  result16_afttest_form3_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_albu")
  result16_afttest_form3_mis$p_value
  result16_afttest_form3_mis$p_std_value
  # afttestplot(result16_afttest_form3_mis,stdType="unstd")
  # afttestplot(result16_afttest_form3_mis,stdType="std")
} else if (run_ID == 192){
  result16_afttest_form4_mis=afttest(Surv(X_pbc,D_pbc)~log_bili+log_prot+log_albu+log_age+edem+trt,path=200,testType="form",eqType="mis",form="log_age")
  result16_afttest_form4_mis$p_value
  result16_afttest_form4_mis$p_std_value
  # afttestplot(result16_afttest_form4_mis,stdType="unstd")
  # afttestplot(result16_afttest_form4_mis,stdType="std")
} 

filename = paste0("Results/PBCpackage",run_ID,".RData")
save.image(file = filename)