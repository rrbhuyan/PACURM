n_clust = 5
mcmc_iter = 500
burn = 100
thinning = 4

setwd("~/Desktop/PACURM")

library(data.table)
library(MASS)
library(dplyr)
library(do)
library(Matrix)
library(BayesLogit)
library(LaplacesDemon)

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

#########
## URM ##
#########

camp_col = NULL
clust_col = NULL
ad_eng = FALSE

curm2_res = CURM2(full_data_train, n_clust, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat, rho_eta_grid, train_dyn_eng_mat, rho_zeta_grid)
saveRDS(curm2_res,paste0("Output/CURM2_URM_",n_clust,".RDS"))
test_prob = logit_prob(curm2_res$param_opt$param_est, full_data_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, test_ad_stock_mat ,curm2_res$rho_eta_opt, test_dyn_eng_mat ,curm2_res$rho_zeta_opt)
train_prob = logit_prob(curm2_res$param_opt$param_est, full_data_train, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat ,curm2_res$rho_eta_opt, train_dyn_eng_mat, curm2_res$rho_zeta_opt)
saveRDS(test_prob,paste0("Output/CURM2_URM_test_prob_",n_clust,".RDS"))
saveRDS(train_prob,paste0("Output/CURM2_URM_train_prob_",n_clust,".RDS"))

##############
## CURM_0_1 ##
##############

camp_col = NULL
clust_col = tail(colnames(full_data_train),1)
ad_eng = FALSE

curm2_res = CURM2(full_data_train, n_clust, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat, rho_eta_grid, train_dyn_eng_mat, rho_zeta_grid)
saveRDS(curm2_res,paste0("Output/CURM2_CURM_0_1_",n_clust,".RDS"))
test_prob = logit_prob(curm2_res$param_opt$param_est, full_data_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, test_ad_stock_mat ,curm2_res$rho_eta_opt, test_dyn_eng_mat ,curm2_res$rho_zeta_opt)
train_prob = logit_prob(curm2_res$param_opt$param_est, full_data_train, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat ,curm2_res$rho_eta_opt, train_dyn_eng_mat, curm2_res$rho_zeta_opt)
saveRDS(test_prob,paste0("Output/CURM2_CURM_0_1_test_prob_",n_clust,".RDS"))
saveRDS(train_prob,paste0("Output/CURM2_CURM_0_1_train_prob_",n_clust,".RDS"))


##############
## CURM_0_2 ##
##############

camp_col = NULL
clust_col = tail(colnames(full_data_train),1)
ad_eng = TRUE

curm2_res = CURM2(full_data_train, n_clust, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat, rho_eta_grid, train_dyn_eng_mat, rho_zeta_grid)
saveRDS(curm2_res,paste0("Output/CURM2_CURM_0_2_",n_clust,".RDS"))
test_prob = logit_prob(curm2_res$param_opt$param_est, full_data_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, test_ad_stock_mat ,curm2_res$rho_eta_opt, test_dyn_eng_mat ,curm2_res$rho_zeta_opt)
train_prob = logit_prob(curm2_res$param_opt$param_est, full_data_train, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat ,curm2_res$rho_eta_opt, train_dyn_eng_mat, curm2_res$rho_zeta_opt)
saveRDS(test_prob,paste0("Output/CURM2_CURM_0_2_test_prob_",n_clust,".RDS"))
saveRDS(train_prob,paste0("Output/CURM2_CURM_0_2_train_prob_",n_clust,".RDS"))

##########
## CURM ##
##########

camp_col = colnames(full_data_train)[2]
clust_col = tail(colnames(full_data_train),1)
ad_eng = TRUE

curm2_res = CURM2(full_data_train, n_clust, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat, rho_eta_grid, train_dyn_eng_mat, rho_zeta_grid)
saveRDS(curm2_res,paste0("Output/CURM2_CURM_",n_clust,".RDS"))
test_prob = logit_prob(curm2_res$param_opt$param_est, full_data_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, test_ad_stock_mat ,curm2_res$rho_eta_opt, test_dyn_eng_mat ,curm2_res$rho_zeta_opt)
train_prob = logit_prob(curm2_res$param_opt$param_est, full_data_train, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, train_ad_stock_mat ,curm2_res$rho_eta_opt, train_dyn_eng_mat, curm2_res$rho_zeta_opt)
saveRDS(test_prob,paste0("Output/CURM2_CURM_test_prob_",n_clust,".RDS"))
saveRDS(train_prob,paste0("Output/CURM2_CURM_train_prob_",n_clust,".RDS"))
