setwd("~/Desktop/PACURM")
source("Functions/CURM2_fun.R")

library(data.table)
library(MASS)
library(dplyr)

train_data = fread("Data/training_data.txt")
test_data = fread("Data/predict_log.csv")

coupon_comp = fread("Data/coupon_comp.csv")
colnames(coupon_comp)[1] = "ID"
colnames(coupon_comp)[2:10] = paste0("comp",1:9)

full_data = rbind(train_data, test_data[,1:20])

cols_stage2 = c("recipient_id", "sendDate", "opened", "ordered", "age", "income","ID")
full_data = full_data[,..cols_stage2]

full_data = full_data[ , .SD[which.max(opened)], by = list(recipient_id, sendDate)]
income = full_data$income
income_cat = model.matrix(~income)
income_var_name = "income"
full_data = cbind(full_data[,-..income_var_name], income_cat[,2:dim(income_cat)[2]])

full_data = as.data.frame(full_data)
full_data = merge(full_data, coupon_comp, by = "ID")

full_data_test = full_data[which(full_data$recipient_id %in% test_data$recipient_id),]
full_data_train = full_data[which(full_data$recipient_id %in% train_data$recipient_id),]

open_col = colnames(full_data_train)[4]
cust_id_col = colnames(full_data_train)[2]
coupon_date_col = colnames(full_data_train)[3]

train_ad_stock_mat = create_ad_stock_mat(full_data_train,cust_id_col,coupon_date_col)
train_dyn_eng_mat = create_dyn_eng_mat(full_data_train,cust_id_col,coupon_date_col,open_col)

test_ad_stock_mat = create_ad_stock_mat(full_data_test, cust_id_col,coupon_date_col)
test_dyn_eng_mat = create_dyn_eng_mat(full_data_test,cust_id_col,coupon_date_col,open_col)

saveRDS(full_data_test,"Data/CURM2_test_data.RDS")
saveRDS(full_data_train,"Data/CURM2_train_data.RDS")

saveRDS(train_ad_stock_mat,"Data/CURM2_train_ad_stock.RDS")
saveRDS(train_dyn_eng_mat,"Data/CURM2_train_dyn_eng.RDS")

saveRDS(test_ad_stock_mat,"Data/CURM2_test_ad_stock.RDS")
saveRDS(test_dyn_eng_mat,"Data/CURM2_test_dyn_eng.RDS")
