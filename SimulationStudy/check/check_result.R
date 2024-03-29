# setwd("/Users/WooJung/Desktop")
# setwd("/Users/WooJung/Documents/Rproject/afttest_analysis")
# setwd("/Users/WooJung/Documents/Rproject/afttest_analysis/SimulationStudy/NewSim")
setwd("/Users/WooJung/Documents/Rproject/afttest_analysis/SimulationStudy/OldSim")

# Type 1 error control
alpha = 0.05

# 
Scenario = 11
N = 500

# ------------------------------------------------------------------------------
gamma_0 = 0
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma0_sim = nrow(gamma0_result)
gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
gamma0_rejectionratio = round(gamma0_rejectionratio,3)
rownames(gamma0_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma0_rejectionratio) = c("omni","link","form")
gamma0_rejectionratio;gamma0_sim

# ------------------------------------------------------------------------------
gamma_0 = 0.1
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma1_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma1_sim = nrow(gamma1_result)
gamma1_rejectionratio = matrix(apply(gamma1_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma1_sim
gamma1_rejectionratio = round(gamma1_rejectionratio,3)
rownames(gamma1_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma1_rejectionratio) = c("omni","link","form")
gamma1_rejectionratio;gamma1_sim

# ------------------------------------------------------------------------------
gamma_0 = 0.2
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma2_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma2_sim = nrow(gamma2_result)
gamma2_rejectionratio = matrix(apply(gamma2_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma2_sim
gamma2_rejectionratio = round(gamma2_rejectionratio,3)
rownames(gamma2_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma2_rejectionratio) = c("omni","link","form")
gamma2_rejectionratio;gamma2_sim

# ------------------------------------------------------------------------------
gamma_0 = 0.3
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma3_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma3_sim = nrow(gamma3_result)
gamma3_rejectionratio = matrix(apply(gamma3_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma3_sim
gamma3_rejectionratio = round(gamma3_rejectionratio,3)
rownames(gamma3_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma3_rejectionratio) = c("omni","link","form")
gamma3_rejectionratio;gamma3_sim

# ------------------------------------------------------------------------------
gamma_0 = 0.4
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma4_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma4_sim = nrow(gamma4_result)
gamma4_rejectionratio = matrix(apply(gamma4_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma4_sim
gamma4_rejectionratio = round(gamma4_rejectionratio,3)
rownames(gamma4_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma4_rejectionratio) = c("omni","link","form")
gamma4_rejectionratio;gamma4_sim

# ------------------------------------------------------------------------------
gamma_0 = 0.5
txt.title = paste0("afttest","Scn",Scenario,"N",N,"gamma",gamma_0*10,"_result.txt")

gamma5_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
gamma5_sim = nrow(gamma5_result)
gamma5_rejectionratio = matrix(apply(gamma5_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma5_sim
gamma5_rejectionratio = round(gamma5_rejectionratio,3)
rownames(gamma5_rejectionratio) = c("mns","mnsstd","mis","misstd")
colnames(gamma5_rejectionratio) = c("omni","link","form")
gamma5_rejectionratio;gamma5_sim

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
gamma0_rejectionratio
gamma1_rejectionratio
gamma2_rejectionratio
gamma3_rejectionratio
gamma4_rejectionratio
gamma5_rejectionratio

# gamma0_sim
# gamma1_sim
# gamma2_sim
# gamma3_sim
# gamma4_sim
# gamma5_sim
