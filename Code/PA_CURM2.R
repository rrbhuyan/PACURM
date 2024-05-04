setwd("~/Desktop/PACURM")
n_clust = 10

library(data.table)
library(MASS)
library(dplyr)
library(do)
library(Matrix)
library(BayesLogit)
library(LaplacesDemon)
library(gdata)

set.seed(2024)
source("Functions/CURM2_fun.R")


CURM1_test_data = readRDS("Data/CURM1_test_data.RDS")
CURM1_train_data = readRDS("Data/CURM1_train_data.RDS")

full_data_test = readRDS("Data/CURM2_test_data.RDS")
full_data_train = readRDS("Data/CURM2_train_data.RDS")

train_ad_stock_mat = readRDS("Data/CURM2_train_ad_stock.RDS")
train_dyn_eng_mat = readRDS("Data/CURM2_train_dyn_eng.RDS")

test_ad_stock_mat = readRDS("Data/CURM2_test_ad_stock.RDS")
test_dyn_eng_mat = readRDS("Data/CURM2_test_dyn_eng.RDS")

train_clust_all = readRDS(paste0("Output/k_means_",n_clust,".RDS")) 
train_clust = train_clust_all$cluster_assign
test_clust = readRDS(paste0("Output/k_means_test_",n_clust,".RDS"))

clust_all = data.frame(recipient_id = c(CURM1_train_data$recipient_id, CURM1_test_data$recipient_id),clust = c(train_clust, test_clust))

full_data_train = merge(full_data_train,clust_all)
full_data_test = merge(full_data_test,clust_all)

cust_id_col = colnames(full_data_train)[1]
coupon_date_col = colnames(full_data_train)[3]
open_col = colnames(full_data_train)[4]
purc_col = colnames(full_data_train)[5]
y_cols = colnames(full_data_train)[4:5]
x_cols = colnames(full_data_train)[6:14]
comp_col = colnames(full_data_train)[15:23]
rho_eta_grid = c(2,4,6,8)/10
rho_zeta_grid = c(2,4,6,8)/10
camp_col = colnames(full_data_train)[2]
clust_col = tail(colnames(full_data_train),1)
ad_eng = TRUE


CURM = readRDS(paste0("Output/CURM2_CURM_",n_clust,".RDS"))
opt_join_list = readRDS(paste0("Output/merge_clust_list_",n_clust,".RDS"))


est_param = CURM$param_opt$param_est


comp_names = paste0("_comp",1:9,"_")
comp_names = c("_", comp_names)
comp_names = c(paste0(comp_names,1), paste0(comp_names,2))

for (k in 1:dim(opt_join_list)[1]) {
  i = opt_join_list[k,1]
  j = opt_join_list[k,2]
  
  param_i = est_param[paste0("clust",i,comp_names)]
  param_j = est_param[paste0("clust",j,comp_names)]
  
  param_i[is.na(param_i)] = 0
  param_j[is.na(param_j)] = 0
  
  n_i = sum(full_data_test$clust == i)
  n_j = sum(full_data_test$clust == j)
  
  full_data_test$clust[which(full_data_test$clust == j)] = i
  
  param_temp = (n_i*param_i+n_j*param_j)/(n_i + n_j)
  names(param_temp) = paste0("clust",i,comp_names)
  
  est_param = est_param[!startsWith(names(est_param),paste0("clust",i))]
  est_param = est_param[!startsWith(names(est_param),paste0("clust",j))]
  
  est_param = c(est_param, param_temp)
  
  test_prob = logit_prob_2(est_param, full_data_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, test_ad_stock_mat ,CURM$rho_eta_opt, test_dyn_eng_mat ,CURM$rho_zeta_opt)
  saveRDS(test_prob,paste0("Output/PA_curm2_",(n_clust-k),".RDS"))
  write.csv(est_param,paste0("Output/PA_curm2_model_",(n_clust-k),".csv"))
}
