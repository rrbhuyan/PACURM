n_clust = 5
library(COMPoissonReg)
library(ggplot2)

setwd("~/Desktop/PACURM")
CURM1_test_data = readRDS("Data/CURM1_test_data.RDS")
CURM1_train_data = readRDS("Data/CURM1_train_data.RDS")

y_cols = colnames(CURM1_test_data)[2:5]
x_global_cols = colnames(CURM1_test_data)[6:14]
glm_link = c("normal","poisson","normal","normal")

source("Functions/CURM1_fun.R")

k_means_clust = readRDS(paste0("Output/k_means_",n_clust,".RDS")) 
train_cluster = k_means_clust$cluster_assign
test_cluster = readRDS(paste0("Output/k_means_test_",n_clust,".RDS"))

llhood_train  = lhood_cluster_assign(CURM1_train_data, y_cols, x_global_cols, k_means_clust$param_list$clust[train_cluster,] , glm_link, k_means_clust$param_list$mean_param, k_means_clust$param_list$var_param)
llhood_test  = lhood_cluster_assign(CURM1_test_data, y_cols, x_global_cols, k_means_clust$param_list$clust[test_cluster,] , glm_link, k_means_clust$param_list$mean_param, k_means_clust$param_list$var_param)

k_means_clust_1 = k_means_clust
k_means_clust_1$param_list$var_param[2] = 1

train_cluster_1 = CURM1_assign_clusters(CURM1_train_data, y_cols, x_global_cols, glm_link,k_means_clust_1$param_list)
test_cluster_1 = CURM1_assign_clusters(CURM1_test_data, y_cols, x_global_cols, glm_link,k_means_clust_1$param_list)

llhood_train_1  = lhood_cluster_assign(CURM1_train_data, y_cols, x_global_cols, k_means_clust_1$param_list$clust[train_cluster_1,] , glm_link, k_means_clust_1$param_list$mean_param, k_means_clust_1$param_list$var_param)
llhood_test_1  = lhood_cluster_assign(CURM1_test_data, y_cols, x_global_cols, k_means_clust_1$param_list$clust[test_cluster_1,] , glm_link, k_means_clust_1$param_list$mean_param, k_means_clust_1$param_list$var_param)

table(test_cluster, test_cluster_1)

x_beta_test = as.matrix(CURM1_test_data[,x_global_cols])%*%as.matrix(k_means_clust$param_list$mean_param)[,2] +  k_means_clust$param_list$clust[test_cluster,2]
test_ll_diff = llhood_test-llhood_test_1
dat_test = data.frame(id = 1:10000, Orders = CURM1_test_data$Order_cnt,y = test_ll_diff, z= exp(x_beta_test))
qplot(id, y,data=dat_test,colour = Orders) + 
  ylab("Difference in Log Likelihooda") + 
  xlab("") +
  scale_colour_gradient(low="red", high="blue") +
  geom_hline(yintercept=0, linetype="dashed")
