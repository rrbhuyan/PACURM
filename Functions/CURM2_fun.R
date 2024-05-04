create_ad_stock_mat = function(dataset,cust_id_col,coupon_date_col){
  data_use = dataset[,c(cust_id_col, coupon_date_col)]
  max_days = find_max_days(data_use, cust_id_col,coupon_date_col)
  colnames(data_use) = c("cust_id", "send_date")
  data_use_0 = data_use
  for(i in 1:max_days){
    data_use_i = data_use_0
    data_use_i$send_date = data_use_i$send_date + i
    data_use_i$ind = i-1
    data_use = merge(data_use, data_use_i, by = c("cust_id", "send_date"), all.x = TRUE)
    colnames(data_use)[i+2] = paste0("ind_",i)
  }
  colnames(data_use)[1:2] = c(cust_id_col, coupon_date_col)
  data_use[is.na(data_use)] = Inf
  data_use
}

find_max_days = function(dataset, cust_id_col,coupon_date_col){
  colnames(dataset) = c("cust_id", "send_date")
  max_min = aggregate(send_date ~ cust_id, data = dataset, FUN = function(x) c(min_date = min(x), max_date = max(x) ) )[,2]
  max_days = max(max_min[,2] - max_min[,1])
  max_days
}


ad_stock_var = function(dataset, ad_stock_mat, rho_eta){
  ad_stock_data = ad_stock_mat[,1:2]
  ad_stock_data$adstock = rowSums(rho_eta^(ad_stock_mat[,-c(1:2)]))
  ad_stock_data
}

create_dyn_eng_mat = function(dataset,cust_id_col,coupon_date_col,open_col){
  data_use = dataset[,c(cust_id_col, coupon_date_col,open_col)]
  max_days = find_max_days(data_use, cust_id_col,coupon_date_col)
  colnames(data_use) = c("cust_id", "send_date","open")
  data_use_0 = data_use[which(data_use$open ==1),1:2]
  data_use = data_use[,1:2]
  for(i in 1:max_days){
    data_use_i = data_use_0
    data_use_i$send_date = data_use_i$send_date + i
    data_use_i$ind = i
    data_use = merge(data_use, data_use_i, by = c("cust_id", "send_date"), all.x = TRUE)
    colnames(data_use)[i+2] = paste0("ind_",i)
  }
  colnames(data_use)[1:2] = c(cust_id_col, coupon_date_col)
  data_use[is.na(data_use)] = Inf
  data_use
}

dyn_eng_var = function(dataset, dyn_eng_mat, rho_zeta){
  dyn_eng_data = dyn_eng_mat[,1:2]
  dyn_eng_data$dyn_eng = rowSums(rho_zeta^(dyn_eng_mat[,-c(1:2)]))
  dyn_eng_data
}

pur_eng_var = function(dataset, cust_id_col, coupon_date_col, purc_col){
  data_use = dataset[,c(cust_id_col, coupon_date_col, purc_col)]
  colnames(data_use) = c("cust_id", "send_date", "purc")
  data_purc = data_use[data_use$purc == 1,]
  min_purc = aggregate(send_date ~ cust_id, data_purc, function(x) min(x))
  colnames(min_purc)[2] = "min_purc_date"
  data_use_with_purc = merge(data_use,min_purc,by = "cust_id")
  data_use_with_purc$pur_eng = ifelse(data_use_with_purc$send_date > data_use_with_purc$min_purc_date,1,0)
  data_use_with_purc = data_use_with_purc[,c(1,2,5)]
  data_use = merge(data_use, data_use_with_purc, by = c("cust_id", "send_date"), all.x = TRUE)
  data_use[is.na(data_use)] = 0
  colnames(data_use)[1:2] = c(cust_id_col, coupon_date_col)
  
  data_use
}

CURM2_data_flat = function(dataset, y_cols, x_cols){
  L = length(y_cols)
  X = dataset[,x_cols]
  
  X_1 = X
  colnames(X_1) = paste0(colnames(X_1),"_1")
  dataset_concat = cbind(response = dataset[,y_cols[1]], X_1)
  for(l in 2:L){
    X_l = X[which(dataset[,y_cols[l-1]]!=0),]
    colnames(X_l)  = paste0(colnames(X_l),"_",l)
    dataset_concat = bind_rows(dataset_concat, cbind(response = dataset[which(dataset[,y_cols[l-1]]!=0),y_cols[l]],X_l))
  }
  dataset_concat[is.na(dataset_concat)] = 0
  dataset_concat
}

CURM2_prior_init = function(dataset, y_cols, x_cols, clust_col = NULL){
  L = length(y_cols)
  X = dataset[,x_cols]

  X_1 = X
  colnames(X_1) = paste0(colnames(X_1),"_1")
  dataset_concat = cbind(response = dataset[,y_cols[1]], X_1)
  for(l in 2:L){
    X_l = X[which(dataset[,y_cols[l-1]]!=0),]
    colnames(X_l)  = paste0(colnames(X_l),"_",l)
    dataset_concat = bind_rows(dataset_concat, cbind(response = dataset[which(dataset[,y_cols[l-1]]!=0),y_cols[l]],X_l))
  }
  dataset_concat[is.na(dataset_concat)] = 0
  
  if(dim(dataset_concat)[1]>50000){
    transaction_subsample = sample(dim(dataset_concat)[1],50000)
    dataset_concat_subset = dataset_concat[transaction_subsample,]
  }else{dataset_concat_subset = dataset_concat}
  
  model_logit = glm(response ~ .-1, data = dataset_concat_subset, family = "binomial")
  temp = summary(model_logit)
  
  if(length(which(is.na(model_logit$coefficients)))!=0){
  dataset_concat_remove_NA = dataset_concat[, -(which(is.na(model_logit$coefficients))+1)]
  }else{
    dataset_concat_remove_NA = dataset_concat
  }

  prior_mean = temp$coefficients[,1]
  prior_sd = temp$coefficients[,2]
  param_init = prior_mean
  
  prior_init = list(dataset_concat = dataset_concat_remove_NA, prior_mean = prior_mean, prior_sd = prior_sd, param_init = param_init)
  
  if(is.null(clust_col) == FALSE){
    clust = as.factor(dataset[,clust_col])
    X_clust = data.frame(model.matrix(~clust-1))
    X_clust_concat = X_clust
    colnames(X_clust_concat) = paste0(colnames(X_clust_concat),"_1")
    for(l in 2:L){
      X_clust_l = data.frame(X_clust[which(dataset[,y_cols[l-1]]!=0),])
      colnames(X_clust_l)  = paste0(colnames(X_clust_l),"_",l)
      X_clust_concat = bind_rows(X_clust_concat, X_clust_l)
    }
    X_clust_concat[is.na(X_clust_concat)] = 0
    if(dim(X_clust_concat)[1]>length(model_logit$residuals)){X_clust_concat = X_clust_concat[transaction_subsample,]}
    temp_res = data.frame(res = model_logit$residuals, X_clust_concat)
    res = matrix(lm(res~.-1, temp_res)$coefficients, ncol = L)
    
    deg_free = dim(X_clust)[2]
    psi_prior_var = (deg_free - L -1)*t(res)%*%(res)/(L)
    psi_prior = list(deg_free = deg_free, psi_prior_var = psi_prior_var)
    
    prior_init = list(dataset_concat = dataset_concat_remove_NA,prior_mean = prior_mean, prior_sd = prior_sd, param_init = param_init, psi_prior = psi_prior)
  }
  prior_init
}

CURM2_data_prepare = function(dataset, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col = NULL, purc_col = NULL, clust_col = NULL, ad_eng = FALSE, camp_col = NULL, ad_stock_mat = NULL, rho_eta = NULL, dyn_eng_mat = NULL, rho_zeta = NULL){
  if(is.null(clust_col)){
    x_cols = c(x_cols, comp_col)
  }else{
    clust = as.factor(dataset[,clust_col])
    comp = dataset[, comp_col]
    if(is.null(camp_col)){
      str = "~clust"
     }else{
      camp_no = as.factor(dataset[, camp_col])
      str = paste0("~clust+camp_no")  
     }
    for(i in 1:(length(comp_col))){
      str = paste0(str,"+",comp_col[i],"+", comp_col[i],":clust")
    }
    clust_comp = do::exec(paste0("model.matrix(", str, ",data = comp)"))
    clust_comp = clust_comp[,-which(colnames(clust_comp)%in%comp_col)]
    clust_comp = clust_comp[,-1]
    colnames(clust_comp) = gsub(":", "_", colnames(clust_comp))
    dataset = cbind(dataset, clust_comp)
    x_cols = c(x_cols, colnames(clust_comp))
  }
  if(ad_eng){
      dyn_eng = dyn_eng_var(dataset, dyn_eng_mat, rho_zeta)
      ad_stock = ad_stock_var(dataset, ad_stock_mat, rho_eta)
      pur_eng = pur_eng_var(dataset, cust_id_col, coupon_date_col, purc_col)
      
      dataset = merge(dataset, ad_stock, by = c(cust_id_col, coupon_date_col))
      dataset = merge(dataset, pur_eng, by = c(cust_id_col, coupon_date_col))
      
      x_cols = c(x_cols, colnames(dataset)[dim(dataset)[2]-2])
      x_cols = c(x_cols, colnames(dataset)[dim(dataset)[2]])
      dataset = merge(dataset, dyn_eng, by = c(cust_id_col, coupon_date_col))
      x_cols = c(x_cols, "dyn_eng")
    }
  dataset$Intercept = 1
  x_cols = c("Intercept", x_cols)
  
  list(data_prep = dataset, x_cols = x_cols)
 }

rand_eff_mat = function(X, clust_col, n_clust, l){
  clust_cols_l =  paste0(clust_col,2:n_clust,"_",l)
  temp_l = X[,clust_cols_l]
  temp_1_l = X$Intercept_1 - rowSums(temp_l)
  temp_l = cbind(temp_1_l, temp_l)
  colnames(temp_l) = paste0(clust_col,1:n_clust,"_rand_",l)
  temp_l
}

create_var_0_hat = function(prior_init, sigma_hat, n_clust){
  var_0_inv = diag(1/prior_init$prior_sd^2)
  sigma_hat_inv = solve(sigma_hat)
  str_sig_inv = ""
  for(i in 1:n_clust){
    str_sig_inv = paste0(str_sig_inv,", sigma_hat_inv")
  }
  var_0_hat = do::exec(paste0("bdiag(var_0_inv", str_sig_inv, ")"))
  var_0_hat = as.matrix(var_0_hat)
}

llhood_logit = function(response, x_beta) {
  sum(response*x_beta - log(1+exp(x_beta)))
  }

CURM2_param_est = function(prior_init, n_clust, clust_col, L, rho_eta, rho_zeta, mcmc_iter, burn, thinning){
  response = prior_init$dataset_concat$response
  X_star = prior_init$dataset_concat[,-which(colnames(prior_init$dataset_concat) == "response")]
  
  m_0 = prior_init$prior_mean
  var_0_hat_inv = diag(1/prior_init$prior_sd^2)
  
  if(!is.null(clust_col)){
    rand_eff = rand_eff_mat(X_star, clust_col, n_clust, 1)
    for(l in 2:L){rand_eff = cbind(rand_eff, rand_eff_mat(X_star, clust_col, n_clust, l))}
    X_star = cbind(X_star, rand_eff)
    
    m_0 = c(m_0, rep(0,n_clust*L))
    sigma_hat = prior_init$psi_prior$psi_prior_var/(prior_init$psi_prior$deg_free-L-1)
  }
  A_hat = m_0
  X_star_mat = as.matrix(X_star)
  
  total_output = NULL
  for(ll in 1:mcmc_iter){
    if(ll %% 100 == 0){print(ll)}
    omega_ll = rpg(dim(X_star)[1], 1, (X_star_mat%*%A_hat))
    
    if(!is.null(clust_col)){var_0_hat_inv = create_var_0_hat(prior_init, sigma_hat, n_clust)}
    
    Omega_ll = replicate(dim(X_star_mat)[2],omega_ll)
    var_ll = solve(t(X_star_mat)%*%(Omega_ll*X_star_mat) + var_0_hat_inv)
    m_ll = var_ll%*%(t(X_star_mat)%*%(response - 0.5) + var_0_hat_inv%*%m_0)
    A_hat = mvrnorm(mu = m_ll, Sigma = var_ll)
    
    if(!is.null(clust_col)){
      rand_comp = matrix(tail(A_hat, L*n_clust), ncol = 2)
      
      sigma_ll_deg_free = prior_init$psi_prior$deg_free + n_clust
      sigma_ll_mean = prior_init$psi_prior$psi_prior_var + t(rand_comp)%*%(rand_comp)
      sigma_hat = rinvwishart(sigma_ll_deg_free, sigma_ll_mean) 
    
      if((ll>burn) && ((ll-burn)%%thinning == 0)){
        total_beta = append(A_hat, sigma_hat)
        total_output = rbind(total_output, total_beta)
      }}else{
        if((ll>burn) && ((ll-burn)%%thinning == 0)){
          total_beta = A_hat
          total_output = rbind(total_output, total_beta)
        }
      }
  }
  thinned_samples = total_output
  thinned_mean = colMeans(thinned_samples)
  param_est = thinned_mean[1:(dim(prior_init$dataset_concat)[2]-1)]
  
  
  llhood = llhood_logit(response, (X_star_mat[,1:(dim(prior_init$dataset_concat)[2]-1)]%*%param_est))
  llhood = llhood + sum(dnorm(param_est,prior_init$prior_mean, prior_init$prior_sd, log = TRUE))
  curm2_mcmc = list(param_est = param_est, thinned_samples = thinned_samples, llhood = llhood)

  if(!is.null(clust_col)){
    sigma_est = matrix(tail(thinned_mean,L^2),ncol = 2)
    llhood = llhood + dinvwishart(sigma_est,prior_init$psi_prior$deg_free, prior_init$psi_prior$psi_prior_var, log = TRUE)
    curm2_mcmc = list(param_est = param_est, sigma_est = sigma_est, thinned_samples = thinned_samples, llhood = llhood)
  } 
  curm2_mcmc
}

CURM2 = function(full_data_train, n_clust, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col,ad_stock_mat, rho_eta_grid,dyn_eng_mat, rho_zeta_grid){
  max_ll = -Inf
  for(i in 1:length(rho_eta_grid)){
    print(i)
    for(j in 1:length(rho_zeta_grid)){
      print(j)
      rho_eta = rho_eta_grid[i]
      rho_zeta = rho_zeta_grid[j]
      
      dataset_full = CURM2_data_prepare(full_data_train, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col,ad_stock_mat, rho_eta,dyn_eng_mat, rho_zeta)
      prior_init = CURM2_prior_init(dataset_full$data_prep, y_cols, dataset_full$x_cols, clust_col)
      curm_param = CURM2_param_est(prior_init, n_clust, clust_col, length(y_cols), rho_eta, rho_zeta,mcmc_iter, burn, thinning)
      
      if(curm_param$llhood > max_ll){
        max_ll = curm_param$llhood
        rho_eta_opt = rho_eta
        rho_zeta_opt = rho_zeta
        param_opt = curm_param
        prior_opt = prior_init
        dataset_opt = dataset_full
      }
    }
  }
  list(rho_eta_opt = rho_eta_opt, rho_zeta_opt = rho_zeta_opt, param_opt = param_opt)
}

logit_prob = function(curm_param_est, dataset_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, ad_stock_mat, rho_eta,dyn_eng_mat, rho_zeta){
  dataset_full = CURM2_data_prepare(dataset_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col,ad_stock_mat, rho_eta,dyn_eng_mat, rho_zeta)
  test_data_concat = CURM2_data_flat(dataset_full$data_prep, y_cols, dataset_full$x_cols)
  response_test = test_data_concat$response
  X_test = test_data_concat[,-which(colnames(test_data_concat)=="response")]
  
  prob_test = 1/(1+exp(-as.matrix(X_test[,which(colnames(X_test)%in%names(curm_param_est))])%*%curm_param_est))
  test_data_res = cbind(response_test, X_test[,which(colnames(X_test)%in%paste0("Intercept_",1:length(y_cols)))])
  test_data_res = cbind(test_data_res, prob_test)
}


logit_prob_2 = function(curm_param_est, dataset_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col, ad_stock_mat, rho_eta,dyn_eng_mat, rho_zeta){
  dataset_full = CURM2_data_prepare(dataset_test, y_cols, x_cols, comp_col, cust_id_col, coupon_date_col,open_col, purc_col, clust_col, ad_eng, camp_col,ad_stock_mat, rho_eta,dyn_eng_mat, rho_zeta)
  test_data_concat = CURM2_data_flat(dataset_full$data_prep, y_cols, dataset_full$x_cols)
  temp = CURM2_prior_init(dataset_full$data_prep, y_cols, dataset_full$x_cols, clust_col)
  curm_param_est = temp$prior_mean
  response_test = test_data_concat$response
  X_test = test_data_concat[,-which(colnames(test_data_concat)=="response")]
  prob_test = 1/(1+exp(-as.matrix(X_test[,which(colnames(X_test)%in%names(curm_param_est))])%*%curm_param_est))
  test_data_res = cbind(response_test, X_test[,which(colnames(X_test)%in%paste0("Intercept_",1:length(y_cols)))])
  test_data_res = cbind(test_data_res, prob_test)
}
