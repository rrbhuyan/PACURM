setwd("~/Desktop/PACURM")

library(data.table)
library(utils)

set.seed(2024)

CURM1_train_data = readRDS("Data/CURM1_train_data.RDS")

y_cols = colnames(CURM1_train_data)[2:5]
x_global_cols = colnames(CURM1_train_data)[6:14]

glm_link = c("normal","poisson","normal","normal")
n_clust_orig = 10

k_means_clust = readRDS(paste0("Output/k_means_",n_clust_orig,".RDS")) 
param_clust = k_means_clust$param_list$clust
clust_ass = k_means_clust$cluster_assign

y = CURM1_train_data[,y_cols]
x_global = CURM1_train_data[,x_global_cols]
clust_mat = param_clust[clust_ass,]
center_mat = as.matrix(x_global)%*%k_means_clust$param_list$mean_param + clust_mat

km_llhood = 0
for(l in 1:length(glm_link)){
  if(glm_link[l] == "normal"){
    log_like_l = dnorm(y[,l], mean = center_mat[,l], sd = sqrt(k_means_clust$param_list$var_param[l]), log = TRUE)
  }
  else if(glm_link[l] == "poisson"){
    log_like_l = dcmp(y[,l], lambda = exp(center_mat[,l]), nu = k_means_clust$param_list$var_param[l], log = TRUE)
  }
  km_llhood = km_llhood + sum(log_like_l)
}
km_llhood_list = km_llhood

clust_ass_step = NULL
opt_join_list = NULL

for(n_clust in n_clust_orig:3){
  llhood = -Inf
  all_comb = combn(sort(unique(clust_ass),decreasing = TRUE),2)
  for (k in 1:dim(all_comb)[2]) {
    i = all_comb[1,k]
    j = all_comb[2,k]
    data_i = CURM1_train_data[which(clust_ass == i),]
    data_j = CURM1_train_data[which(clust_ass == j),]
    
    data_temp = rbind(data_i, data_j)
    
    cluster_center_temp = (dim(data_i)[1]*param_clust[i,] + dim(data_j)[1]*param_clust[j,] )/(dim(data_i)[1] + dim(data_j)[1])
    y_temp = data_temp[,y_cols]
    x_temp = data_temp[,x_global_cols]
    
    center_temp = sweep(as.matrix(x_temp)%*%k_means_clust$param_list$mean_param,2,cluster_center_temp,"+")
    log_like = 0
    for(l in 1:length(glm_link)){
      if(glm_link[l] == "normal"){
        log_like_l = dnorm(y_temp[,l], mean = center_temp[,l], sd = sqrt(k_means_clust$param_list$var_param[l]), log = TRUE)
      }
      else if(glm_link[l] == "poisson"){
        log_like_l = dcmp(y_temp[,l], lambda = exp(center_temp[,l]), nu = k_means_clust$param_list$var_param[l], log = TRUE)
      }
      log_like = log_like + sum(log_like_l)
    }
    
    if(log_like>llhood){
      llhood = log_like
      opt_join = c(i,j)
      opt_center = cluster_center_temp
    }
  }
opt_join_list = rbind(opt_join_list, opt_join)
clust_ass[which(clust_ass == opt_join[2])] = opt_join[1]
param_clust[opt_join[1],] = cluster_center_temp
param_clust[opt_join[2],] = 0
clust_ass_step = cbind(clust_ass_step, clust_ass)

clust_mat = param_clust[clust_ass,]
center_mat = as.matrix(x_global)%*%k_means_clust$param_list$mean_param + clust_mat

km_llhood = 0
for(l in 1:length(glm_link)){
  if(glm_link[l] == "normal"){
    log_like_l = dnorm(y[,l], mean = center_mat[,l], sd = sqrt(k_means_clust$param_list$var_param[l]), log = TRUE)
  }
  else if(glm_link[l] == "poisson"){
    log_like_l = dcmp(y[,l], lambda = exp(center_mat[,l]), nu = k_means_clust$param_list$var_param[l], log = TRUE)
  }
  km_llhood = km_llhood + sum(log_like_l)
}
km_llhood_list = c(km_llhood_list, km_llhood)
}

saveRDS(opt_join_list,paste0("Output/merge_clust_list_",n_clust_orig,".RDS"))
