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

path = 200

cgd_data = subset(cgd,enum==1)

D_cgd = cgd_data$status
X_cgd = cgd_data$tstop - cgd_data$tstart
X_cgd = X_cgd + runif(length(X_cgd))/1e4

trt = ifelse(cgd_data$treat=="placebo",0,1)
str = cgd_data$steroids
age = cgd_data$age; log_age = log(age)
wei = cgd_data$weight; log_wei = log(wei)

# ------------------------------------------------------------------------------
# ------------------------------------ "mns" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------------------- Covariates: trt+str+age --------------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result01_afttest_omni_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="omni",eqType="mns")
result01_afttest_omni_mns$p_value
result01_afttest_omni_mns$p_std_value
# afttestplot(result01_afttest_omni_mns,std="unstd")
# afttestplot(result01_afttest_omni_mns,std="std")

# ------------------------------------ link ------------------------------------
# result01_afttest_link_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="link",eqType="mns")
result01_afttest_link_mns$p_value
result01_afttest_link_mns$p_std_value
# afttestplot(result01_afttest_link_mns,std="unstd")
# afttestplot(result01_afttest_link_mns,std="std")

# ------------------------------------ form ------------------------------------
# result01_afttest_form1_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="form",eqType="mns",form="age")
result01_afttest_form1_mns$p_value
result01_afttest_form1_mns$p_std_value
# afttestplot(result01_afttest_form1_mns,std="unstd")
# afttestplot(result01_afttest_form1_mns,std="std")

# result01_afttest_form2_mns=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="form",eqType="mns",form="wei")
result01_afttest_form2_mns$p_value
result01_afttest_form2_mns$p_std_value
# afttestplot(result01_afttest_form2_mns,std="unstd")
# afttestplot(result01_afttest_form2_mns,std="std")

# ------------------------------------------------------------------------------
# ------------------------------------ "mis" -----------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# --------------------------- Covariates: trt+str+age --------------------------
# ------------------------------------------------------------------------------

# ------------------------------------ omni ------------------------------------
# result01_afttest_omni_mis=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="omni",eqType="mis")
result01_afttest_omni_mis$p_value
result01_afttest_omni_mis$p_std_value
# afttestplot(result01_afttest_omni_mis,std="unstd")
# afttestplot(result01_afttest_omni_mis,std="std")

# ------------------------------------ link ------------------------------------
# result01_afttest_link_mis=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="link",eqType="mis")
result01_afttest_link_mis$p_value
result01_afttest_link_mis$p_std_value
# afttestplot(result01_afttest_link_mis,std="unstd")
# afttestplot(result01_afttest_link_mis,std="std")

# ------------------------------------ form ------------------------------------
# result01_afttest_form1_mis=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="form",eqType="mis",form="age")
result01_afttest_form1_mis$p_value
result01_afttest_form1_mis$p_std_value
# afttestplot(result01_afttest_form1_mis,std="unstd")
# afttestplot(result01_afttest_form1_mis,std="std")

# result01_afttest_form2_mis=afttest(Surv(X_cgd,D_cgd)~trt+str+age+wei,path=path,testType="form",eqType="mis",form="wei")
result01_afttest_form2_mis$p_value
result01_afttest_form2_mis$p_std_value
# afttestplot(result01_afttest_form2_mis,std="unstd")
# afttestplot(result01_afttest_form2_mis,std="std")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# beta01mns = aftsrr(Surv(X_cgd,D_cgd)~trt+str+age+wei,eqType="mns",se="ISMB")
# beta01mis = aftsrr(Surv(X_cgd,D_cgd)~trt+str+age+wei,eqType="mis",se="ISMB")
summary(beta01mns)
summary(beta01mis)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
cairo_ps("cgd01_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_omni_mns,std="unstd")
dev.off()
cairo_ps("cgd01_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_omni_mns,std="std")
dev.off()
cairo_ps("cgd01_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_link_mns,std="unstd")
dev.off()
cairo_ps("cgd01_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_link_mns,std="std")
dev.off()
cairo_ps("cgd01_form1_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form1_mns,std="unstd")
dev.off()
cairo_ps("cgd01_form1_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form1_mns,std="std")
dev.off()
cairo_ps("cgd01_form2_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form2_mns,std="unstd")
dev.off()
cairo_ps("cgd01_form2_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form2_mns,std="std")
dev.off()

cairo_ps("cgd01_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_omni_mis,std="unstd")
dev.off()
cairo_ps("cgd01_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_omni_mis,std="std")
dev.off()
cairo_ps("cgd01_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_link_mis,std="unstd")
dev.off()
cairo_ps("cgd01_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_link_mis,std="std")
dev.off()
cairo_ps("cgd01_form1_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form1_mis,std="unstd")
dev.off()
cairo_ps("cgd01_form1_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form1_mis,std="std")
dev.off()
cairo_ps("cgd01_form2_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form2_mis,std="unstd")
dev.off()
cairo_ps("cgd01_form2_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result01_afttest_form2_mis,std="std")
dev.off()

# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# result_cgd = rbind(c(result01_afttest_omni_mns$p_std_value, result01_afttest_omni_mis$p_std_value,
#                      result01_afttest_link_mns$p_std_value, result01_afttest_link_mis$p_std_value,
#                      result01_afttest_form_mns$p_std_value, result01_afttest_form_mis$p_std_value),
#                    c(result01_afttest_omni_mns$p_value, result01_afttest_omni_mis$p_value,
#                      result01_afttest_link_mns$p_value, result01_afttest_link_mis$p_value,
#                      result01_afttest_form_mns$p_value, result01_afttest_form_mis$p_value),
#                    c(result02_afttest_omni_mns$p_std_value, result02_afttest_omni_mis$p_std_value,
#                      result02_afttest_link_mns$p_std_value, result02_afttest_link_mis$p_std_value,
#                      result02_afttest_form_mns$p_std_value, result02_afttest_form_mis$p_std_value),
#                    c(result02_afttest_omni_mns$p_value, result02_afttest_omni_mis$p_value,
#                      result02_afttest_link_mns$p_value, result02_afttest_link_mis$p_value,
#                      result02_afttest_form_mns$p_value, result02_afttest_form_mis$p_value))
# colnames(result_cgd) = rep(c("mns","mis"),3)
# rownames(result_cgd) = rep(paste0("model",1:2), each = 2)
# result_cgd = data.frame(model = rep(paste0("model",1:2), each = 2), result_cgd)
# colnames(result_cgd) = NULL
# rownames(result_cgd) = NULL
# 
# kable(result_cgd, digits = 3, "latex", booktabs = T, escape = F,
#       col.names = c("model", rep(c("mns","mis"),3)),
#       caption = "CGD data result", label = "tab:real:cgd", align = "c") %>%
#   add_header_above(c(" "=1,"omni"=2,"link"=2,"form"=2)) %>%
#   add_header_above(c(" "=1,"P-value"=6)) %>%
#   kable_styling() %>%
#   collapse_rows(columns = 1:2, latex_hline = "major", valign = "middle") %>%
#   row_spec(row = (1:2*2 - 1), bold = TRUE)




