library(dplyr)

setwd("~/Desktop/PACURM")
n_clust = 5

CURM1_test_data = readRDS("Data/CURM1_test_data.RDS")
CURM1_train_data = readRDS("Data/CURM1_train_data.RDS")

train_clust = readRDS(paste0("Output/k_means_",n_clust,".RDS"))
train_clust = train_clust$cluster_assign
test_clust = readRDS(paste0("Output/k_means_test_",n_clust,".RDS"))

past_data = rbind(CURM1_train_data, CURM1_test_data)
clust_data = c(train_clust, test_clust)

past_data$clust = clust_data
past_data$clust = as.factor(past_data$clust)
past_data_RFM = past_data[,c(2:5,15)]

summary_RFM = past_data_RFM %>% 
                 group_by(clust) %>% 
                 summarise(across(everything(), mean))

summary_RFM = merge(count(past_data_RFM, clust),summary_RFM)

saveRDS(summary_RFM, paste0("Output/clust_summary_",dim(summary_RFM)[1],".RDS"))
