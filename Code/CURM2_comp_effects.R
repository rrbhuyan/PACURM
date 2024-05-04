n_clust = 5

setwd("~/Desktop/PACURM")
coupon_comp = c("30% off","40% off","50% off","Product","Clearance","Mystery","Free Gift","Free Shipping")

CURM_model = readRDS(paste0("Output/CURM2_CURM_",n_clust,".RDS"))
comp_sample_open = CURM_model$param_opt$thinned_samples[,39:74]
comp_sample_purc = CURM_model$param_opt$thinned_samples[,116:151]

a = comp_sample_open[,1:32]
for(i in 1:8){
  a[,(1+(i-1)*4):(i*4)] = a[,(1+(i-1)*4):(i*4)]- comp_sample_open[,33:36]
}
comp_sample_open_adj = a

a = comp_sample_purc[,1:32]
for(i in 1:8){
  a[,(1+(i-1)*4):(i*4)] = a[,(1+(i-1)*4):(i*4)]- comp_sample_purc[,33:36]
}
comp_sample_purc_adj = a

a = matrix(nrow = 100, ncol = 8)
for (i in 1:8) {
  a[,i] = rowMeans(comp_sample_open_adj[,(1+(i-1)*4):(i*4)])
}
comp_effect_open_samples = a

a = matrix(nrow = 100, ncol = 8)
for (i in 1:8) {
  a[,i] = rowMeans(comp_sample_purc_adj[,(1+(i-1)*4):(i*4)])
}
comp_effect_purc_samples = a

open_cross_CI = apply( comp_sample_open_adj , 2 , quantile , probs = c(0.025,0.975) , na.rm = TRUE )
purc_cross_CI = apply( comp_sample_purc_adj , 2 , quantile , probs = c(0.025,0.975) , na.rm = TRUE )

open_CI = apply( comp_effect_open_samples , 2 , quantile , probs = c(0.025,0.975) , na.rm = TRUE )
purc_CI = apply( comp_effect_purc_samples , 2 , quantile , probs = c(0.025,0.975) , na.rm = TRUE )

open_cross_mean = colMeans(comp_sample_open_adj)
purc_cross_mean = colMeans(comp_sample_purc_adj)  

open_mean = colMeans(comp_effect_open_samples)
purc_mean = colMeans(comp_effect_purc_samples) 

open_across = as.data.frame(t(rbind(t(open_cross_mean),open_cross_CI)))
purc_across = as.data.frame(t(rbind(t(purc_cross_mean),purc_cross_CI)))
open = as.data.frame(t(rbind(t(open_mean),open_CI)))
purc = as.data.frame(t(rbind(t(purc_mean),purc_CI)))

write.csv(open_across, "Output/CURM2_open_cross_CI.csv")
write.csv(purc_across, "Output/CURM2_purc_cross_CI.csv")
write.csv(open, "Output/CURM2_open_CI.csv")
write.csv(purc, "Output/CURM2_purc_CI.csv")
