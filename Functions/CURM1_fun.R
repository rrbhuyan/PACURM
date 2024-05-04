select_rand_max = function(myVector){
  temp = which( myVector == max(myVector) )
  
  if(length(temp) == 1){
    return(temp)
  }
  temp1 = sample(temp,1)
  return(temp1)
}

CURM1_prior_init = function(dataset, y_cols, x_global_cols, glm_link){
  data_subset = sample(dim(dataset)[1],min(dim(dataset)[1],5000))
  dataset = dataset[data_subset,]
  L = length(y_cols)
  n_cust = dim(dataset)[1]
  P = length(x_global_cols)
  
  prior_mean = matrix(0, nrow = P, ncol = L)
  prior_sd = prior_mean
  clust_prior = matrix(0, nrow = 2, ncol = L)
  
  var_init = rep(0, L)
  for(l in 1:L){
    formula_l = paste(c(y_cols[l],"~",paste(x_global_cols, collapse = '+')), collapse = "")
    if(glm_link[l] == "normal"){
      model_l = do::exec(paste0("summary(lm(", formula_l, ",data = dataset))"))
      intercept_l = model_l$coefficients[1,1:2]
      param_l = model_l$coefficients[2:dim(model_l$coefficients)[1],1:2]
      var_l = sqrt(sum(model_l$residuals^2/n_cust))
    }
    else if(glm_link[l] == "poisson"){
      model_l = do::exec(paste0("summary(glm.cmp(", formula_l, ",data = dataset))"))
      param_l = model_l$DF[2:(dim(model_l$DF)[1]-1),1:2]
      intercept_l = model_l$DF[1,1:2]
      var_l = exp(model_l$DF[dim(model_l$DF)[1],1])
    }
    prior_mean[,l] = param_l[,1]
    clust_prior[,l] = as.numeric(intercept_l)
    prior_sd[,l] = param_l[,2]
    var_init[l] = var_l
  }
  mean_init = prior_mean
  list(prior_mean = prior_mean, prior_sd = prior_sd, var_init = var_init, mean_init = mean_init, clust_prior = clust_prior)
}

lhood = function(dataset, y_cols, x_global_cols, clust_center , glm_link, prior_mean, var_init){
  y = dataset[,y_cols]
  x_global = dataset[,x_global_cols]
  center = sweep(as.matrix(x_global)%*%prior_mean,2,clust_center,"+")
  log_like = rep(0,dim(dataset)[1])
  for(l in 1:length(glm_link)){
    if(glm_link[l] == "normal"){
      log_like_l = dnorm(y[,l], mean = center[,l], sd = sqrt(var_init[l]), log = TRUE)
    }
    else if(glm_link[l] == "poisson"){
      log_like_l = dcmp(y[,l], lambda = exp(center[,l]), nu = var_init[l], log = TRUE)
    }
    log_like = log_like + log_like_l
  }
  log_like
}

lhood_cluster_assign = function(dataset, y_cols, x_global_cols, clust_center_assigned , glm_link, prior_mean, var_init){
  y = dataset[,y_cols]
  x_global = dataset[,x_global_cols]
  center = as.matrix(x_global)%*%prior_mean + clust_center_assigned
  log_like = rep(0,dim(dataset)[1])
  for(l in 1:length(glm_link)){
    if(glm_link[l] == "normal"){
      log_like_l = dnorm(y[,l], mean = center[,l], sd = sqrt(var_init[l]), log = TRUE)
    }
    else if(glm_link[l] == "poisson"){
      log_like_l = dcmp(y[,l], lambda = exp(center[,l]), nu = var_init[l], log = TRUE)
    }
    log_like = log_like + log_like_l
  }
  log_like
}

prior_ldens = function(param_list, prior_init){
  prior_ldens = NULL
  for(l in 1:dim(param_list$clust)[2]){
    prior_l = sum(dnorm(param_list$mean_param[,l], prior_init$prior_mean[,l], prior_init$prior_sd[,l], log = TRUE))
    prior_l = prior_l + dinvgamma(param_list$var_param[l], (prior_init$var_init[l]+1)/prior_init$var_init[l], log = TRUE)
    prior_l = prior_l + dnorm(param_list$clust[,l], prior_init$clust_prior[1,l], prior_init$clust_prior[2,l], log = TRUE)
    prior_ldens = sum(prior_ldens, prior_l)
  }
  prior_ldens
}

center = function(y_obs, x_obs, glm_link, prior_mean, var_init){
  L = length(glm_link)
  clust_center = rep(0,L)
  x_beta = as.matrix(x_obs)%*%prior_mean
  for(l in 1:L){
    if(glm_link[l] == "normal"){
      clust_center[l] = y_obs[l] - x_beta[l]
    }
    else if(glm_link[l] == "poisson"){
      clust_center[l] = var_init[l]*log(y_obs[l]+0.5) - x_beta[l]
    }
  }
  as.numeric(clust_center)
}

CURM1_k_means_init = function(dataset, y_cols, x_global_cols, glm_link, prior_init, n_clust){
  n_cust = dim(dataset)[1]
  x_global = dataset[,x_global_cols]
  Y = dataset[,y_cols]
  
  prior_mean = prior_init$prior_mean
  var_init = prior_init$var_init
  
  center1_obs = sample(n_cust, 1)
  clust_center = center(Y[center1_obs,], x_global[center1_obs,], glm_link, prior_mean, var_init)
  clust_center = matrix(clust_center, nrow = 1)
  
  for(m in 2:n_clust){
    lhood_m = rep(-Inf,n_cust)
    for(j in 1:(m-1)){
      lhood_m = pmax(lhood_m, lhood(dataset, y_cols, x_global_cols, clust_center[j,] , glm_link, prior_mean, var_init))
    }
    centerm_obs = sample(n_cust, 1, prob = -lhood_m)
    clust_center_m = center(Y[centerm_obs,], x_global[centerm_obs,], glm_link, prior_mean, var_init)
    clust_center = rbind(clust_center, clust_center_m)
  }
  
  like_matrix = matrix(0, nrow = n_cust, ncol = n_clust) 
  for(m in 1:n_clust){
    like_matrix[,m] = lhood(dataset, y_cols, x_global_cols, clust_center[m,] , glm_link, prior_mean, var_init)
  }
  cluster_assign = apply(like_matrix, 1, select_rand_max)
  list(clust = clust_center, cluster_assign = cluster_assign)
}

CURM1_mcmc = function(dataset, y_cols, x_global_cols, glm_link, param_list, prior_init,cluster_assign,mcmc_iter,burn, thinning){
  P = length(x_global_cols)
  L = length(y_cols)
  M = dim(param_list$clust)[1]
  
  var_proposal_sd = prior_init$var_init/10
  mean_proposal_sd = prior_init$prior_sd/10
  clust_proposal_sd = prior_init$clust_prior[2,]/10
  
  like_total = sum(lhood_cluster_assign(dataset, y_cols, x_global_cols, param_list$clust[cluster_assign,] , glm_link, param_list$mean_param, param_list$var_param))
  like_total = like_total + prior_ldens(param_list, prior_init)
  
  mean_change = param_list$mean_param
  var_change = param_list$var_param
  clust_change = param_list$clust
  
  new_param = param_list
  total_output = NULL
  
  for(ll in 1:mcmc_iter){
    for (l in 1:L) {
      mean_change[,l] = rnorm(P, sd = mean_proposal_sd[,l])
      var_change[l] = rnorm(1, sd = var_proposal_sd[l])
    }
    for(m in 1:M){
      clust_change[m,] = rnorm(L, sd = clust_proposal_sd)
    }
    new_param$mean_param = param_list$mean_param + mean_change
    new_param$var_param = pmax(param_list$var_param + var_change, rep(0.01,L))
    new_param$clust = param_list$clust + clust_change
      
    new_like_total = sum(lhood_cluster_assign(dataset, y_cols, x_global_cols, new_param$clust[cluster_assign,] , glm_link, new_param$mean_param, new_param$var_param))
    new_like_total = new_like_total + prior_ldens(new_param, prior_init)
    
    u = runif(1)
    
    if(((new_like_total) - (like_total)) >= dim(dataset)[1]*log(u)){
      param_list = new_param
      like_total = new_like_total
    }
    if(ll>burn){
      total_beta = append(as.matrix(param_list$mean_param), as.matrix(param_list$clust))
      total_beta = append(total_beta, param_list$var_param)
      
      total_output = rbind(total_output, total_beta)
    }
  }
  thinned_mean = colMeans(total_output[thinning*(1:floor((mcmc_iter-burn)/thinning)),])
  param_list$mean_param = matrix(thinned_mean[1:(P*L)], ncol = L)
  param_list$clust = matrix(thinned_mean[((P*L)+1):((P*L)+(L*M))], ncol = L)
  param_list$var_param = thinned_mean[((P*L)+(L*M)+1):((P*L)+(L*M)+L)]
  
  param_list
}
  
CURM1_kmeans = function(dataset, y_cols, x_global_cols, glm_link, prior_init,k_means_init,k_iter,mcmc_iter,burn,thinning){
  n_cust = dim(dataset)[1]
  x_global = dataset[,x_global_cols]
  Y = dataset[,y_cols]
  
  prior_mean = prior_init$prior_mean
  var_init = prior_init$var_init
  clust = k_means_init$clust
  cluster_assign = k_means_init$cluster_assign
  
  param_list = list(mean_param = prior_mean, var_param = var_init, clust = clust)
  
  for(counter in 1:k_iter){
    print(counter)
    param_list = CURM1_mcmc(dataset, y_cols, x_global_cols, glm_link, param_list, prior_init,cluster_assign,mcmc_iter,burn,thinning)
    like_matrix = matrix(0, nrow = n_cust, ncol = n_clust) 
    for(m in 1:n_clust){
      like_matrix[,m] = lhood(dataset, y_cols, x_global_cols, param_list$clust[m,] , glm_link, param_list$mean_param, param_list$var_param)
    }
    cluster_assign = apply(like_matrix, 1, select_rand_max)
  }
  list(param_list = param_list, cluster_assign = cluster_assign)
}

CURM1_assign_clusters = function(dataset, y_cols, x_global_cols, glm_link,param_list){
  n_cust = dim(dataset)[1]
  n_clust = dim(param_list$clust)[1]
  like_matrix = matrix(0, nrow = n_cust, ncol = n_clust) 
  for(m in 1:n_clust){
    like_matrix[,m] = lhood(dataset, y_cols, x_global_cols, param_list$clust[m,] , glm_link, param_list$mean_param, param_list$var_param)
  }
  cluster_assign = apply(like_matrix, 1, select_rand_max)
}