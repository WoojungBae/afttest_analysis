# Load R packages
library(Rcpp)
library(RcppArmadillo)
library(survival)
library(ggplot2)
library(gridExtra)
library(aftgee)
library(afttest)

# Load R code
setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/source")
source("afttest_source_r.R")

Scenario = 21

# Define number of observations for each dataset
N = 300

# Type 2 error check
gamma_0 = 0.3

# The number of the approximated paths for each simulation is 200
path = 200

# ------------------------------------------------------------------------------
set.seed(1)

# ------------------------------------------------------------------------------
temp_data = generate_data(N,gamma_0,Scenario)
X = temp_data$X
D = temp_data$D
Z = temp_data$Z
Z1 = Z[,1]
Z2 = Z[,2]

# ------------------------------------ omni ------------------------------------
result_omni_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='omni', eqType='mns')
result_omni_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='omni', eqType='mis')

# ------------------------------------ link ------------------------------------
result_link_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='link', eqType='mns')
result_link_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='link', eqType='mis')

# ------------------------------------ form ------------------------------------
result_form_mns = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='form', eqType='mns', form = 2)
result_form_mis = afttest(Surv(X, D) ~ Z1 + Z2, path=path,testtype='form', eqType='mis', form = 2)

# ------------------------------------------------------------------------------
allinfo = data.frame("omni_mns_pvalue" = result_omni_mns$p_value, 
                     "omni_mns_stdpvalue" = result_omni_mns$p_std_value,
                     "omni_mis_pvalue" = result_omni_mis$p_value, 
                     "omni_mis_stdpvalue" = result_omni_mis$p_std_value,
                     "link_mns_pvalue" = result_link_mns$p_value, 
                     "link_mns_stdpvalue" = result_link_mns$p_std_value,
                     "link_mis_pvalue" = result_link_mis$p_value, 
                     "link_mis_stdpvalue" = result_link_mis$p_std_value,
                     "form_mns_pvalue" = result_form_mns$p_value, 
                     "form_mns_stdpvalue" = result_form_mns$p_std_value,
                     "form_mis_pvalue" = result_form_mis$p_value, 
                     "form_mis_stdpvalue" = result_form_mis$p_std_value)
allinfo

setwd("C:/Users/WooJung/Documents/Rproject/afttest_analysis/SimulationStudy/figure")
save.image("C:/Users/WooJung/Documents/Rproject/afttest_analysis/SimulationStudy/figure/sim2.RData")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
cairo_ps("sim2_omni_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_omni_mns,std="unstd")
dev.off()
cairo_ps("sim2_omni_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_omni_mns,std="std")
dev.off()
cairo_ps("sim2_link_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_link_mns,std="unstd")
dev.off()
cairo_ps("sim2_link_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_link_mns,std="std")
dev.off()
cairo_ps("sim2_form_mns_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_form_mns,std="unstd")
dev.off()
cairo_ps("sim2_form_mns_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_form_mns,std="std")
dev.off()

cairo_ps("sim2_omni_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_omni_mis,std="unstd")
dev.off()
cairo_ps("sim2_omni_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_omni_mis,std="std")
dev.off()
cairo_ps("sim2_link_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_link_mis,std="unstd")
dev.off()
cairo_ps("sim2_link_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_link_mis,std="std")
dev.off()
cairo_ps("sim2_form_mis_unstd.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_form_mis,std="unstd")
dev.off()
cairo_ps("sim2_form_mis_std.eps",onefile=F,height=4,width=8, fallback_resolution = 600)
afttestplot(result_form_mis,std="std")
dev.off()




