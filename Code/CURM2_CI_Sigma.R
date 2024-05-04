n_clust = 5

setwd("~/Desktop/PACURM")

model_list = c("CURM", "CURM_0_2_", "CURM_0_1_", "URM")
new_model_list = model_list
i = 1
for(model in model_list){
  model_est = readRDS(paste0("Output/CURM2_",model,"_",n_clust,".RDS"))
  est_param = model_est$param_opt$param_est
  param_CI = apply(model_est$param_opt$thinned_samples , 2 , quantile , probs = c(0.025,0.975) , na.rm = TRUE)
  param_CI = param_CI[,1:length(est_param)]
  mean_CI = as.data.frame(t(rbind(est_param, param_CI)))
  mean_CI = rbind(mean_CI, rho_eta = rep(model_est$rho_eta_opt, 3))
  mean_CI = rbind(mean_CI, rho_zeta = rep(model_est$rho_zeta_opt, 3))
  
  colnames(mean_CI) = c("Posterior Mean", "Lower CI", "Upper CI")
  new_model = new_model_list[i]
  i = i+1
  write.csv(mean_CI, paste0("Output/CURM2_",new_model,"_mean_CI_",n_clust,".csv"))
  if(!is.null(model_est$param_opt$sigma_est)) write.csv(model_est$param_opt$sigma_est,  paste0("Output/CURM2_",new_model,"_Sigma_",n_clust,".csv"))
}

n_clust = 10

model = "CURM"

model_est = readRDS(paste0("Output/CURM2_",model,"_",n_clust,".RDS"))
est_param = model_est$param_opt$param_est
param_CI = apply(model_est$param_opt$thinned_samples , 2 , quantile , probs = c(0.025,0.975) , na.rm = TRUE)
param_CI = param_CI[,1:length(est_param)]
mean_CI = as.data.frame(t(rbind(est_param, param_CI)))
mean_CI = rbind(mean_CI, rho_eta = rep(model_est$rho_eta_opt, 3))
mean_CI = rbind(mean_CI, rho_zeta = rep(model_est$rho_zeta_opt, 3))

colnames(mean_CI) = c("Posterior Mean", "Lower CI", "Upper CI")

new_model = "CURM"
write.csv(mean_CI, paste0("Output/CURM2_",new_model,"_mean_CI_",n_clust,".csv"))
write.csv(model_est$param_opt$sigma_est,  paste0("Output/CURM2_",new_model,"_Sigma_",n_clust,".csv"))
