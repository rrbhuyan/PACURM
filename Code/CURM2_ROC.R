setwd("~/Desktop/PACURM")
n_clust = 5

library(ggplot2)
library(plotROC)
library(pROC)


test_CURM = readRDS(paste0("Output/CURM2_CURM_test_prob_",n_clust,".RDS"))
test_CURM_0_2 = readRDS(paste0("Output/CURM2_CURM_0_2_test_prob_",n_clust,".RDS"))
test_CURM_0_1 = readRDS(paste0("Output/CURM2_CURM_0_1_test_prob_",n_clust,".RDS"))
test_URM = readRDS(paste0("Output/CURM2_URM_test_prob_",n_clust,".RDS"))

open_CURM = test_CURM[which(test_CURM$Intercept_1 == 1), c(1,4)]
open_CURM_0_2 = test_CURM_0_2[which(test_CURM_0_2$Intercept_1 == 1), c(1,4)]
open_CURM_0_1 = test_CURM_0_1[which(test_CURM_0_1$Intercept_1 == 1), c(1,4)]
open_URM = test_URM[which(test_URM$Intercept_1 == 1), c(1,4)]

open_CURM$Model = "CURM"
open_CURM_0_2$Model = "CURM.0.2"
open_CURM_0_1$Model = "CURM.0.1"
open_URM$Model = "URM"

open_mod = rbind(open_CURM, open_CURM_0_2)
open_mod = rbind(open_mod, open_CURM_0_1)
open_mod = rbind(open_mod, open_URM)

ggplot(open_mod) + 
  geom_roc(n.cuts = 0, linealpha = 0.7, aes(m = prob_test, d = response_test, color = Model)) +
  ylab("True Positive Rate") + 
  xlab("False Positive Rate") + 
  theme(legend.position = c(0.85, 0.2)) +
  theme(text = element_text(size = 15))

auc(roc(open_CURM$response_test, open_CURM$prob_test))
auc(roc(open_CURM_0_2$response_test, open_CURM_0_2$prob_test))
auc(roc(open_CURM_0_1$response_test, open_CURM_0_1$prob_test))
auc(roc(open_URM$response_test, open_URM$prob_test))

purc_CURM = test_CURM[which(test_CURM$Intercept_2 == 1), c(1,4)]
purc_CURM_0_2 = test_CURM_0_2[which(test_CURM_0_2$Intercept_2 == 1), c(1,4)]
purc_CURM_0_1 = test_CURM_0_1[which(test_CURM_0_1$Intercept_2 == 1), c(1,4)]
purc_URM = test_URM[which(test_URM$Intercept_2 == 1), c(1,4)]

purc_CURM$Model = "CURM"
purc_CURM_0_2$Model = "CURM.0.2"
purc_CURM_0_1$Model = "CURM.0.1"
purc_URM$Model = "URM"

purc_mod = rbind(purc_CURM, purc_CURM_0_2)
purc_mod = rbind(purc_mod, purc_CURM_0_1)
purc_mod = rbind(purc_mod, purc_URM)

ggplot(purc_mod) + geom_roc(n.cuts = 0, linealpha = 0.7, aes(m = prob_test, d = response_test, color = Model))+
  ylab("True Positive Rate") + 
  xlab("False Positive Rate") + 
  theme(legend.position = c(0.85, 0.2)) +
  theme(text = element_text(size = 15))

auc(roc(purc_CURM$response_test, purc_CURM$prob_test))
auc(roc(purc_CURM_0_2$response_test, purc_CURM_0_2$prob_test))
auc(roc(purc_CURM_0_1$response_test, purc_CURM_0_1$prob_test))
auc(roc(purc_URM$response_test, purc_URM$prob_test))

