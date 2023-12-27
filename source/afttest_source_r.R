# Define functions -----------------------------------------------------------------

# Define function to generate datasets
# generate_data = function(n,gamma0,Scenario) {
#   if (Scenario==11){
#     # --------------------------------- Scenario 1 ---------------------------------
#     # True Coefficient: beta_0 = -4, beta_1 = 1
#     beta_0 = -4
#     beta_1 = 1
#     if(gamma0 == 0){
#       tau = 3.455
#     } else if(gamma0 == 0.1){
#       tau = 3.185
#     } else if(gamma0 == 0.2){
#       tau = 2.953
#     } else if(gamma0 == 0.3){
#       tau = 2.745
#     } else if(gamma0 == 0.4){
#       tau = 2.555
#     } else if(gamma0 == 0.5){
#       tau = 2.37
#     }
#     
#     # ------------------------------------------------------------------------------
#     # Generate covariate Z
#     Z = rnorm(n,2,1)
#     
#     # ------------------------------------------------------------------------------
#     # censoring rate 20% 
#     # T: rnorm(n,0,1)
#     # C: exp(rnorm(n,tau,1))
#     
#     # T_data: true event time
#     # C_data: true censoring time
#     # X_data: observed time
#     # D_data: observed indicator
#     # Z_data: covariates fitted
#     T_data = exp(-beta_0-beta_1*Z-gamma0*(Z^{2})+rnorm(n,0,1))
#     C_data = exp(rnorm(n,tau,1))
#     X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
#     D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
#     Z_data = Z
#     
#   } else if (Scenario==12){
#     # --------------------------------- Scenario 1 ---------------------------------
#     # True Coefficient: beta_0 = -4, beta_1 = 1
#     beta_0 = -4
#     beta_1 = 1
#     if(gamma0 == 0){
#       tau = 2.438
#     } else if(gamma0 == 0.1){
#       tau = 2.046
#     } else if(gamma0 == 0.2){
#       tau = 1.690
#     } else if(gamma0 == 0.3){
#       tau = 1.352
#     } else if(gamma0 == 0.4){
#       tau = 1.020
#     } else if(gamma0 == 0.5){
#       tau = 0.701
#     }
#     
#     # ------------------------------------------------------------------------------
#     # Generate covariate Z
#     Z = rnorm(n,2,1)
#     
#     # ------------------------------------------------------------------------------
#     # censoring rate 40% 
#     # T: rnorm(n,0,1)
#     # C: exp(rnorm(n,tau,1))
#     
#     # T_data: true event time
#     # C_data: true censoring time
#     # X_data: observed time
#     # D_data: observed indicator
#     # Z_data: covariates fitted
#     T_data = exp(-beta_0-beta_1*Z-gamma0*(Z^{2})+rnorm(n,0,1))
#     C_data = exp(rnorm(n,tau,1))
#     X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
#     D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
#     Z_data = Z
#     
#   } else if (Scenario==21){
#     # --------------------------------- Scenario 2 ---------------------------------
#     # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
#     beta_0 = -4
#     beta_1 = 1
#     beta_2 = 1
#     if(gamma0 == 0){
#       tau = 3.017
#     } else if(gamma0 == 0.1){
#       tau = 2.740
#     } else if(gamma0 == 0.2){
#       tau = 2.500
#     } else if(gamma0 == 0.3){
#       tau = 2.281
#     } else if(gamma0 == 0.4){
#       tau = 2.085
#     } else if(gamma0 == 0.5){
#       tau = 1.901
#     }
#     
#     # ------------------------------------------------------------------------------
#     # Generate covariate Z
#     Z1 = rbinom(n,1,0.5)
#     Z2 = rnorm(n,2,1)
#     
#     # ------------------------------------------------------------------------------
#     # censoring rate 20% 
#     # T: rnorm(n,0,1)
#     # C: exp(rnorm(n,tau,1))
#     
#     # T_data: true event time
#     # C_data: true censoring time
#     # X_data: observed time
#     # D_data: observed indicator
#     # Z_data: covariates fitted
#     T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * (Z2^{2}) + rnorm(n,0,1))
#     C_data = exp(rnorm(n,tau,1))
#     X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
#     D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
#     Z_data = cbind(Z1,Z2)
#     
#   } else if (Scenario==22){
#     # --------------------------------- Scenario 2 ---------------------------------
#     # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
#     beta_0 = -4
#     beta_1 = 1
#     beta_2 = 1
#     if(gamma0 == 0){
#       tau = 1.955
#     } else if(gamma0 == 0.1){
#       tau = 1.560
#     } else if(gamma0 == 0.2){
#       tau = 1.200
#     } else if(gamma0 == 0.3){
#       tau = 0.850
#     } else if(gamma0 == 0.4){
#       tau = 0.523
#     } else if(gamma0 == 0.5){
#       tau = 0.205
#     }
#     
#     # ------------------------------------------------------------------------------
#     # Generate covariate Z
#     Z1 = rbinom(n,1,0.5)
#     Z2 = rnorm(n,2,1)
#     
#     # ------------------------------------------------------------------------------
#     # censoring rate 40% 
#     # T: rnorm(n,0,1)
#     # C: exp(rnorm(n,tau,1))
#     
#     # T_data: true event time
#     # C_data: true censoring time
#     # X_data: observed time
#     # D_data: observed indicator
#     # Z_data: covariates fitted
#     T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * (Z2^{2}) + rnorm(n,0,1))
#     C_data = exp(rnorm(n,tau,1))
#     X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
#     D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
#     Z_data = cbind(Z1,Z2)
#     
#   } else if (Scenario==31){
#     # --------------------------------- Scenario 3 ---------------------------------
#     # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
#     beta_0 = -4
#     beta_1 = 1
#     beta_2 = 1
#     if(gamma0 == 0){
#       tau = 3.018
#     } else if(gamma0 == 0.1){
#       tau = 3.001
#     } else if(gamma0 == 0.2){
#       tau = 2.988
#     } else if(gamma0 == 0.3){
#       tau = 2.979
#     } else if(gamma0 == 0.4){
#       tau = 2.973
#     } else if(gamma0 == 0.5){
#       tau = 2.962
#     }
#     
#     # ------------------------------------------------------------------------------
#     # Generate covariate Z
#     Z1 = rbinom(n,1,0.5)
#     Z2 = rnorm(n,2,1)
#     
#     # ------------------------------------------------------------------------------
#     # censoring rate 20% 
#     # T: rnorm(n,0,1)
#     # C: exp(rnorm(n,tau,1))
#     
#     # T_data: true event time
#     # C_data: true censoring time
#     # X_data: observed time
#     # D_data: observed indicator
#     # Z_data: covariates fitted
#     T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * lgamma(1+0.3*Z2^{2}) + rnorm(n,0,1))
#     C_data = exp(rnorm(n,tau,1))
#     X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
#     D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
#     Z_data = cbind(Z1,Z2)
#     
#   } else if (Scenario==32){
#     # --------------------------------- Scenario 3 ---------------------------------
#     # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
#     beta_0 = -4
#     beta_1 = 1
#     beta_2 = 1
#     if(gamma0 == 0){
#       tau = 1.955
#     } else if(gamma0 == 0.1){
#       tau = 1.921
#     } else if(gamma0 == 0.2){
#       tau = 1.892
#     } else if(gamma0 == 0.3){
#       tau = 1.866
#     } else if(gamma0 == 0.4){
#       tau = 1.840
#     } else if(gamma0 == 0.5){
#       tau = 1.820
#     }
#     
#     # ------------------------------------------------------------------------------
#     # Generate covariate Z
#     Z1 = rbinom(n,1,0.5)
#     Z2 = rnorm(n,2,1)
#     
#     # ------------------------------------------------------------------------------
#     # censoring rate 40% 
#     # T: rnorm(n,0,1)
#     # C: exp(rnorm(n,tau,1))
#     
#     # T_data: true event time
#     # C_data: true censoring time
#     # X_data: observed time
#     # D_data: observed indicator
#     # Z_data: covariates fitted
#     T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * lgamma(1+0.3*Z2^{2}) + rnorm(n,0,1))
#     C_data = exp(rnorm(n,tau,1))
#     X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
#     D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
#     Z_data = cbind(Z1,Z2)
#     
#   }
#   
#   return(list(X=X_data, D=D_data, Z=Z_data))
# }

rejectionratio = function(Scenario) {
  # Type 1 error control
  alpha = 0.05
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  S1 = as.numeric(paste0(Scenario,1))
  {
    N = 100
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma0 = 0
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.1
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma01_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma01_sim = nrow(gamma01_result)
      gamma01_rejectionratio = matrix(apply(gamma01_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma01_sim
      gamma01_rejectionratio = round(gamma01_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.2
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma02_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma02_sim = nrow(gamma02_result)
      gamma02_rejectionratio = matrix(apply(gamma02_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma02_sim
      gamma02_rejectionratio = round(gamma02_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.3
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma03_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma03_sim = nrow(gamma03_result)
      gamma03_rejectionratio = matrix(apply(gamma03_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma03_sim
      gamma03_rejectionratio = round(gamma03_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.4
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma04_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma04_sim = nrow(gamma04_result)
      gamma04_rejectionratio = matrix(apply(gamma04_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma04_sim
      gamma04_rejectionratio = round(gamma04_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.5
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma05_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma05_sim = nrow(gamma05_result)
      gamma05_rejectionratio = matrix(apply(gamma05_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma05_sim
      gamma05_rejectionratio = round(gamma05_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn1n100 = cbind(gamma0_rejectionratio,gamma01_rejectionratio,gamma02_rejectionratio,
                       gamma03_rejectionratio,gamma04_rejectionratio,gamma05_rejectionratio)
    }
    
    N = 300
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma0 = 0
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.1
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma01_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma01_sim = nrow(gamma01_result)
      gamma01_rejectionratio = matrix(apply(gamma01_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma01_sim
      gamma01_rejectionratio = round(gamma01_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.2
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma02_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma02_sim = nrow(gamma02_result)
      gamma02_rejectionratio = matrix(apply(gamma02_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma02_sim
      gamma02_rejectionratio = round(gamma02_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.3
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma03_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma03_sim = nrow(gamma03_result)
      gamma03_rejectionratio = matrix(apply(gamma03_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma03_sim
      gamma03_rejectionratio = round(gamma03_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.4
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma04_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma04_sim = nrow(gamma04_result)
      gamma04_rejectionratio = matrix(apply(gamma04_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma04_sim
      gamma04_rejectionratio = round(gamma04_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.5
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma05_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma05_sim = nrow(gamma05_result)
      gamma05_rejectionratio = matrix(apply(gamma05_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma05_sim
      gamma05_rejectionratio = round(gamma05_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn1n300 = cbind(gamma0_rejectionratio,gamma01_rejectionratio,gamma02_rejectionratio,
                       gamma03_rejectionratio,gamma04_rejectionratio,gamma05_rejectionratio)
    }
    
    N = 500
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma0 = 0
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.1
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma01_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma01_sim = nrow(gamma01_result)
      gamma01_rejectionratio = matrix(apply(gamma01_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma01_sim
      gamma01_rejectionratio = round(gamma01_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.2
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma02_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma02_sim = nrow(gamma02_result)
      gamma02_rejectionratio = matrix(apply(gamma02_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma02_sim
      gamma02_rejectionratio = round(gamma02_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.3
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma03_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma03_sim = nrow(gamma03_result)
      gamma03_rejectionratio = matrix(apply(gamma03_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma03_sim
      gamma03_rejectionratio = round(gamma03_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.4
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma04_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma04_sim = nrow(gamma04_result)
      gamma04_rejectionratio = matrix(apply(gamma04_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma04_sim
      gamma04_rejectionratio = round(gamma04_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.5
      txt.title = paste0("afttest","Scn",S1,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma05_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma05_sim = nrow(gamma05_result)
      gamma05_rejectionratio = matrix(apply(gamma05_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma05_sim
      gamma05_rejectionratio = round(gamma05_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn1n500 = cbind(gamma0_rejectionratio,gamma01_rejectionratio,gamma02_rejectionratio,
                       gamma03_rejectionratio,gamma04_rejectionratio,gamma05_rejectionratio)
    }
  }
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  S2 = as.numeric(paste0(Scenario,2))
  {
    N = 100
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma0 = 0
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.1
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma01_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma01_sim = nrow(gamma01_result)
      gamma01_rejectionratio = matrix(apply(gamma01_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma01_sim
      gamma01_rejectionratio = round(gamma01_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.2
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma02_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma02_sim = nrow(gamma02_result)
      gamma02_rejectionratio = matrix(apply(gamma02_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma02_sim
      gamma02_rejectionratio = round(gamma02_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.3
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma03_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma03_sim = nrow(gamma03_result)
      gamma03_rejectionratio = matrix(apply(gamma03_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma03_sim
      gamma03_rejectionratio = round(gamma03_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.4
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma04_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma04_sim = nrow(gamma04_result)
      gamma04_rejectionratio = matrix(apply(gamma04_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma04_sim
      gamma04_rejectionratio = round(gamma04_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.5
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma05_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma05_sim = nrow(gamma05_result)
      gamma05_rejectionratio = matrix(apply(gamma05_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma05_sim
      gamma05_rejectionratio = round(gamma05_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn2n100 = cbind(gamma0_rejectionratio,gamma01_rejectionratio,gamma02_rejectionratio,
                       gamma03_rejectionratio,gamma04_rejectionratio,gamma05_rejectionratio)
    }
    
    N = 300
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma0 = 0
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.1
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma01_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma01_sim = nrow(gamma01_result)
      gamma01_rejectionratio = matrix(apply(gamma01_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma01_sim
      gamma01_rejectionratio = round(gamma01_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.2
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma02_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma02_sim = nrow(gamma02_result)
      gamma02_rejectionratio = matrix(apply(gamma02_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma02_sim
      gamma02_rejectionratio = round(gamma02_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.3
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma03_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma03_sim = nrow(gamma03_result)
      gamma03_rejectionratio = matrix(apply(gamma03_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma03_sim
      gamma03_rejectionratio = round(gamma03_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.4
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma04_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma04_sim = nrow(gamma04_result)
      gamma04_rejectionratio = matrix(apply(gamma04_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma04_sim
      gamma04_rejectionratio = round(gamma04_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.5
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma05_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma05_sim = nrow(gamma05_result)
      gamma05_rejectionratio = matrix(apply(gamma05_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma05_sim
      gamma05_rejectionratio = round(gamma05_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn2n300 = cbind(gamma0_rejectionratio,gamma01_rejectionratio,gamma02_rejectionratio,
                       gamma03_rejectionratio,gamma04_rejectionratio,gamma05_rejectionratio)
    }
    
    N = 500
    {
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      gamma0 = 0
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma0_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma0_sim = nrow(gamma0_result)
      gamma0_rejectionratio = matrix(apply(gamma0_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma0_sim
      gamma0_rejectionratio = round(gamma0_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.1
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma01_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma01_sim = nrow(gamma01_result)
      gamma01_rejectionratio = matrix(apply(gamma01_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma01_sim
      gamma01_rejectionratio = round(gamma01_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.2
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma02_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma02_sim = nrow(gamma02_result)
      gamma02_rejectionratio = matrix(apply(gamma02_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma02_sim
      gamma02_rejectionratio = round(gamma02_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.3
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma03_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma03_sim = nrow(gamma03_result)
      gamma03_rejectionratio = matrix(apply(gamma03_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma03_sim
      gamma03_rejectionratio = round(gamma03_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.4
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma04_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma04_sim = nrow(gamma04_result)
      gamma04_rejectionratio = matrix(apply(gamma04_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma04_sim
      gamma04_rejectionratio = round(gamma04_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      gamma0 = 0.5
      txt.title = paste0("afttest","Scn",S2,"N",N,"gamma0",gamma0*10,"_result.txt")
      
      gamma05_result = read.table(txt.title, header = TRUE, sep = "", dec = ".", fill = T)
      gamma05_sim = nrow(gamma05_result)
      gamma05_rejectionratio = matrix(apply(gamma05_result[,-1], 2, function(l) sum(l<alpha, na.rm = T)),ncol=3)/gamma05_sim
      gamma05_rejectionratio = round(gamma05_rejectionratio,4)
      
      # ------------------------------------------------------------------------------
      # ------------------------------------------------------------------------------
      Scn2n500 = cbind(gamma0_rejectionratio,gamma01_rejectionratio,gamma02_rejectionratio,
                       gamma03_rejectionratio,gamma04_rejectionratio,gamma05_rejectionratio)
    }
  }
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  result = t(rbind(Scn1n100,Scn1n300,Scn1n500,Scn2n100,Scn2n300,Scn2n500))
  
  row2 = 1:18*2; row1 = row2 - 1
  resultdataframe = matrix(nrow=36, ncol=12)
  for (cc in 1:12) {
    ccc = cc*2
    cccc = ccc - 1
    resultdataframe[row1,cc] = result[,ccc]  # odd: standardized test 
    resultdataframe[row2,cc] = result[,cccc] # even: unstandardized test
  }
  colnames(resultdataframe) = rep(c("mns","mis"),6)
  rownames(resultdataframe) = rep(c("omni", "link", "form"),6, each = 2)
  
  resultdataframe = data.frame(gamma0 = rep(paste0("gamma0",0:5), each = 6),
                               rep(c("omni", "link", "form"),6, each = 2),
                               resultdataframe)
  colnames(resultdataframe) = NULL
  rownames(resultdataframe) = NULL
  
  return(resultdataframe)
}

# Define function to generate datasets
generate_data = function(n,gamma0,Scenario) {
  if (Scenario==11){
    # --------------------------------- Scenario 1 ---------------------------------
    # True Coefficient: beta_0 = -4, beta_1 = 1
    beta_0 = -4
    beta_1 = 1
    if (gamma0 == 0){
      tau = 583
    } else if (gamma0 == 0.1){
      tau = 527
    } else if (gamma0 == 0.2){
      tau = 475
    } else if (gamma0 == 0.3){
      tau = 435
    } else if (gamma0 == 0.4){
      tau = 398
    } else if (gamma0 == 0.5){
      tau = 369
    }

    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z = rnorm(n,0,1)

    # ------------------------------------------------------------------------------
    # censoring rate 20%
    # T: rnorm(n,0,1)
    # C: runif(n,0,tau)

    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(-beta_0-beta_1*Z-gamma0*(Z^{2})-rnorm(n,0,1))
    C_data = runif(n,0,tau)
    X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
    D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
    Z_data = Z

  } else if (Scenario==12){
    # --------------------------------- Scenario 1 ---------------------------------
    # True Coefficient: beta_0 = -4, beta_1 = 1
    beta_0 = -4
    beta_1 = 1
    if (gamma0 == 0){
      tau = 208
    } else if (gamma0 == 0.1){
      tau = 194
    } else if (gamma0 == 0.2){
      tau = 180
    } else if (gamma0 == 0.3){
      tau = 167
    } else if (gamma0 == 0.4){
      tau = 154.5
    } else if (gamma0 == 0.5){
      tau = 143.4
    }

    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z = rnorm(n,0,1)

    # ------------------------------------------------------------------------------
    # censoring rate 40%
    # T: rnorm(n,0,1)
    # C: runif(n,0,tau)

    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(-beta_0-beta_1*Z-gamma0*(Z^{2})-rnorm(n,0,1))
    C_data = runif(n,0,tau)
    X_data = C_data*(T_data>C_data)+T_data*(T_data<=C_data)
    D_data = 0*(T_data>C_data)+1*(T_data<=C_data)
    Z_data = Z

  } else if (Scenario==21){
    # --------------------------------- Scenario 2 ---------------------------------
    # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
    beta_0 = -4
    beta_1 = 1
    beta_2 = 1
    if (gamma0 == 0){
      tau = 379
    } else if (gamma0 == 0.1){
      tau = 343.5
    } else if (gamma0 == 0.2){
      tau = 312
    } else if (gamma0 == 0.3){
      tau = 283
    } else if (gamma0 == 0.4){
      tau = 261
    } else if (gamma0 == 0.5){
      tau = 241
    }

    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(n,1,0.5)
    Z2 = rnorm(n,0,1)

    # ------------------------------------------------------------------------------
    # censoring rate 20%
    # T: rnorm(n,0,1)
    # C: runif(n,0,tau)

    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * (Z2^{2}) - rnorm(n,0,1))
    C_data = runif(n,0,tau)
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)

  } else if (Scenario==22){
    # --------------------------------- Scenario 2 ---------------------------------
    # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
    beta_0 = -4
    beta_1 = 1
    beta_2 = 1
    if (gamma0 == 0){
      tau = 129.7
    } else if (gamma0 == 0.1){
      tau = 120.5
    } else if (gamma0 == 0.2){
      tau = 112
    } else if (gamma0 == 0.3){
      tau = 103.5
    } else if (gamma0 == 0.4){
      tau = 95.8
    } else if (gamma0 == 0.5){
      tau = 88.8
    }

    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(n,1,0.5)
    Z2 = rnorm(n,0,1)

    # ------------------------------------------------------------------------------
    # censoring rate 40%
    # T: rnorm(n,0,1)
    # C: runif(n,0,tau)

    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * (Z2^{2}) - rnorm(n,0,1))
    C_data = runif(n,0,tau)
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)

  } else if (Scenario==31){
    # --------------------------------- Scenario 3 ---------------------------------
    # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
    beta_0 = -4
    beta_1 = 1
    beta_2 = 1
    if (gamma0 == 0){
      tau = 379
    } else if (gamma0 == 0.1){
      tau = 372
    } else if (gamma0 == 0.2){
      tau = 368.5
    } else if (gamma0 == 0.3){
      tau = 365
    } else if (gamma0 == 0.4){
      tau = 362.6
    } else if (gamma0 == 0.5){
      tau = 360
    }

    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(n,1,0.5)
    Z2 = rnorm(n,0,1)

    # ------------------------------------------------------------------------------
    # censoring rate 20%
    # T: rnorm(n,0,1)
    # C: runif(n,0,tau)

    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * lgamma(1+0.3*(2+Z2)^{2}) - rnorm(n,0,1))
    C_data = runif(n,0,tau)
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)

  } else if (Scenario==32){
    # --------------------------------- Scenario 3 ---------------------------------
    # True Coefficient: beta_0 = -4, beta_1 = 1, beta_2 = 1
    beta_0 = -4
    beta_1 = 1
    beta_2 = 1
    if (gamma0 == 0){
      tau = 129.3
    } else if (gamma0 == 0.1){
      tau = 125.9
    } else if (gamma0 == 0.2){
      tau = 122.6
    } else if (gamma0 == 0.3){
      tau = 119.7
    } else if (gamma0 == 0.4){
      tau = 116.9
    } else if (gamma0 == 0.5){
      tau = 114.8
    }

    # ------------------------------------------------------------------------------
    # Generate covariate Z
    Z1 = rbinom(n,1,0.5)
    Z2 = rnorm(n,0,1)

    # ------------------------------------------------------------------------------
    # censoring rate 40%
    # T: rnorm(n,0,1)
    # C: runif(n,0,tau)

    # T_data: true event time
    # C_data: true censoring time
    # X_data: observed time
    # D_data: observed indicator
    # Z_data: covariates fitted
    T_data = exp(- beta_0 - beta_1 * Z1 - beta_2 * Z2 - gamma0 * lgamma(1+0.3*(2+Z2)^{2}) - rnorm(n,0,1))
    C_data = runif(n,0,tau)
    X_data = C_data * (T_data > C_data) + T_data * (T_data <= C_data)
    D_data = 0 * (T_data > C_data) + 1 * (T_data <= C_data)
    Z_data = cbind(Z1,Z2)

  }

  return(list(X=X_data, D=D_data, Z=Z_data))
}

# End function definitions ---------------------------------------------------------