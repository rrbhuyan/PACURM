setwd("~/Desktop/PACURM")

library(data.table)
library(MASS)
library(COMPoissonReg)
library(do)
library(invgamma)

set.seed(2024)
source("Functions/CURM1_fun.R")

CURM1_test_data = readRDS("Data/CURM1_test_data.RDS")
CURM1_train_data = readRDS("Data/CURM1_train_data.RDS")

y_cols = colnames(CURM1_train_data)[2:5]
x_global_cols = colnames(CURM1_train_data)[6:14]
glm_link = c("normal","poisson","normal","normal")
n_clust = 5

k_iter = 15
mcmc_iter = 5000
burn = 1000
thinning = 10

if(file.exists("Output/prior.RDS")){prior_init = readRDS("Output/prior.RDS")
}else{prior_init = CURM1_prior_init(CURM1_train_data, y_cols, x_global_cols, glm_link)}
k_means_init = CURM1_k_means_init(CURM1_train_data, y_cols, x_global_cols, glm_link, prior_init, n_clust)
k_means_clust = CURM1_kmeans(CURM1_train_data, y_cols, x_global_cols, glm_link, prior_init,k_means_init,k_iter,mcmc_iter,burn,thinning)

test_cluster = CURM1_assign_clusters(CURM1_test_data, y_cols, x_global_cols, glm_link,k_means_clust$param_list)

saveRDS(prior_init,"Output/prior.RDS")  
saveRDS(k_means_init,paste0("Output/k_means_init_",n_clust,".RDS")) 
saveRDS(k_means_clust,paste0("Output/k_means_",n_clust,".RDS")) 
saveRDS(test_cluster,paste0("Output/k_means_test_",n_clust,".RDS"))
 