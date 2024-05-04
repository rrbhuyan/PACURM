setwd("~/Desktop/PACURM")

library(data.table)
library(MASS)

train_data = fread("Data/training_data.txt")
test_data = fread("Data/predict_log.csv")

full_data = rbind(train_data, test_data[,1:20])
past_data = full_data[ , .SD[which.min(sendDate)], by = recipient_id]

cols_clust = c("recipient_id", "aov_web", "Order_cnt", "daysSinceOpened", "daysSincePurchased", "age", "income")
past_data_clust = past_data[,..cols_clust]
past_data_clust[,2] = log(past_data_clust[,2]+1)
colnames(past_data_clust)[2] = "log_aov_web"

income = past_data_clust$income
income_cat = model.matrix(~income)
income_var_name = "income"
past_data_clust = cbind(past_data_clust[,-..income_var_name], income_cat[,2:dim(income_cat)[2]])

past_data_clust = as.data.frame(past_data_clust)
past_data_all = past_data_clust

past_data_test = past_data_all[which(past_data_all$recipient_id %in% test_data$recipient_id),]
past_data_train = past_data_all[which(past_data_all$recipient_id %in% train_data$recipient_id),]

saveRDS(past_data_test,"Data/CURM1_test_data.RDS")
saveRDS(past_data_train,"Data/CURM1_train_data.RDS")
