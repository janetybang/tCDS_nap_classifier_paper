library(papaja)
library(tidyverse)
library(here)
library(NbClust)
library(e1071)
library(MASS)
#library(relaimpo)
library(DiagrammeR)
library(data.table)
library(rpart)
library(rpart.plot)
library(caret)
library(xgboost)
library(pROC)
#library(xgboostExplainer)
library(randomForest)
library(ggpubr)

load("data/combined_data_5min.Rdata")
d$time = NULL

# nap decision tree
source("two-class.R")
# re-code naps as 1, all else as 0
nap_d <- d %>% mutate(cds_ohs = ifelse(cds_ohs=="nap", 1, 0))
nap_dat <- add_features(nap_d, with_demo=F, prop_meaningful=F, per_child_norms=F)
nap_d_tt = get_train_test_cv(nap_dat, train_proportion=.9)

nap_dtree <- create_single_tree(nap_d_tt$train, nap_d_tt$test, nap_d_tt$cv, save.plot=F, show.plot=F)
nap_auc = round(nap_dtree$tree.roc$auc, 3)
saveRDS(nap_dtree, file=here("nap_classifier.Rds"))
#nap_dtree <- readRDS("nap_classifier.Rds")

nap_cm <- tibble(nap_prob = nap_dtree$tree.preds,
                 actual = nap_d[as.numeric(names(nap_dtree$tree.preds)),]$cds_ohs) %>%
  mutate(nap_pred = ifelse(nap_prob > .5, 1, 0))
table(nap_cm$nap_pred, nap_cm$actual)
# (actual)
#      0    1
# 0 1084   39  
# 1   32  139 (predicted)
table(nap_cm$nap_pred) # predicted 171 naps
table(nap_cm$actual) # test set had 178 naps
# accuracy = TP + TN / all
# (139 + 1084) / 1294 = .945

pdf("nap_classifier.pdf", width=6, height=3.2)
par(mfrow=c(1,2))
plot(nap_dtree$tree.roc, xlim=c(1,0), cex.lab=.8, cex.axis=.8)
text(x=.45, y=.1, paste0("AUC = ",nap_auc), cex=.8) 
rpart.plot(nap_dtree$tree.model, type = 2, extra = 7, fallen.leaves = T) # extra=2 for raw counts
dev.off()


pdf("nap_classifier_gray.pdf", width=6, height=3.2)
par(mfrow=c(1,2))
plot(nap_dtree$tree.roc, xlim=c(1,0), cex.lab=.8, cex.axis=.8)
text(x=.45, y=.1, paste0("AUC = ",nap_auc), cex=.8) 
rpart.plot(nap_dtree$tree.model, type = 2, extra = 7, box.palette = "Grays",
           fallen.leaves = T) # extra=2 for raw counts
dev.off()

pdf("nap_classifier_counts.pdf", width=3.5, height=3.2)
rpart.plot(nap_dtree$tree.model, type = 2, extra = 2, box.palette = "Grays",
           fallen.leaves = T) 
dev.off()

# final nap classifier trained on all data
create_final_tree <- function(dat, cv, seed=42, save.plot=T, show.plot=T) {
  set.seed(seed) # reproducible
  cds_ohs = which(names(dat)=="cds_ohs") # class column number
  ctrl <- trainControl(method = "boot") # "repeatedcv"/"LOOCV"
  
  tree <- train(x = dat[,-cds_ohs], y = as.factor(dat[,"cds_ohs"]), method = "rpart2", 
                    trControl = ctrl, control = rpart.control()) # tuneLength = 7,
  tree.model = tree$finalModel
  #rpart.plot(tree.model, type = 2, extra = 7, fallen.leaves = T) # label all nodes; draw the split labels below the node labels
  #rpart.plot(tree.model, type = 2, extra = 2, fallen.leaves = T) # counts
  tree.preds = predict(tree.model, dat)[,2]
  tree.roc_obj <- roc(dat[,cds_ohs], tree.preds)
  return(list(tree.roc = tree.roc_obj, tree.model = tree.model, tree.preds = tree.preds))
}

fnap_dtree <- create_final_tree(nap_dat, save.plot=F, show.plot=F)
fnap_auc = round(fnap_dtree$tree.roc$auc, 3)
saveRDS(fnap_dtree, file=here("nap_classifier_final.Rds"))

pdf("final_nap_classifier_gray.pdf", width=6, height=3.2)
par(mfrow=c(1,2))
plot(fnap_dtree$tree.roc, xlim=c(1,0))
text(x=.65, y=.6, paste0("AUC = ",fnap_auc)) 
rpart.plot(fnap_dtree$tree.model, type = 2, extra = 7, box.palette = "Grays",
           fallen.leaves = T) # extra=2 for raw counts
dev.off()

pdf("final_nap_classifier_gray_counts.pdf", width=3.5, height=3.2)
rpart.plot(fnap_dtree$tree.model, type = 2, extra = 2, box.palette = "Grays",
           fallen.leaves = T) 
dev.off()


# CDS/ODS decision tree
d_nonaps <- d %>% filter(cds_ohs!="nap")
#dat <- add_features(d_nonaps, with_demo=F, prop_meaningful=F, per_child_norms=F) # AUC=.68
dat <- add_features(d_nonaps, with_demo=F, prop_meaningful=T, per_child_norms=T) # AUC=.71
dd = get_train_test_cv(dat, train_proportion=.9) # has some data from all kids in both train and test

dtree <- create_single_tree(dd$train, dd$test, dd$cv, save.plot=F, show.plot=F) # .67
auc = round(dtree$tree.roc$auc, 3) # .654

par(mfrow=c(1,2))
rpart.plot(dtree$tree.model, type = 2, extra = 7, fallen.leaves = T) # extra=2 for raw counts
plot(dtree$tree.roc, xlim=c(1,0))
text(x=.57, y=.6, paste0("AUC = ",auc)) 

# try with all data
dat <- add_features(d_nonaps, with_demo=F, prop_meaningful=F, per_child_norms=F) 
fdtree <- create_final_tree(dat, save.plot=F, show.plot=F) # .685
fauc = round(fdtree$tree.roc$auc, 3)

rpart.plot(fdtree$tree.model, type = 2, extra = 7, box.palette = "Grays",
           fallen.leaves = T)

# cut text (possibly for an appendix?)
# As a proof-of-concept, we first train a single decision tree on raw LENA features to distinguish child-directed speech segments from all other segments (overheard speech, and split segments).
# The decision tree trained on raw LENA features achieves an AUC of `r auc`, which is reasonably good performance.
# One advantage of a single decision tree is interpretability: for example, the first split made by the decision tree is that if CTC is less than 1.1 (turns per minute) and CVC is <2.1/min then a segment is unlikely to be CDS--which correctly classified X/Y (66%) of segments in this category.
# On the other hand, if a segment has more than 1.1 CTC, then the segment is classified as child-directed (correctly classifying 71% of XX CDS segments).


# CDS/ODS XGboost classifier (with split segments as CDS)

d_nonaps <- d %>% filter(cds_ohs!="nap")
dat <- add_features(d_nonaps, with_demo=F, prop_meaningful=F, per_child_norms=F) 
# bal acc: .67 auc: .749
dat_mean <- add_features(d_nonaps, with_demo=F, prop_meaningful=T, per_child_norms=F) 
# bal acc: .68 auc: .753

dd = get_train_test_cv(dat, train_proportion=.9) # has some data from all kids in both train and test
dd_mean = get_train_test_cv(dat_mean, train_proportion=.9)

xgb = create_xgboost_model(dd$train, dd$test, dd$cv, graph=F)
xgb_mean = create_xgboost_model(dd_mean$train, dd_mean$test, dd_mean$cv, graph=F)
# should retrain model on all data and save it for future use
saveRDS(xgb, file="xgb_model.rds") # AUC = .72 bal acc: .67
saveRDS(xgb_mean, file="xgb_model_meaningful_normed.rds") # AUC=.73 bal acc: .67

# child-left-out CV
child_cv <- do_child_level_cv(dat %>% mutate(id = d_nonaps$id), fname="rawLENA")
# mean AUC: 0.72, mean test accuracy: 0.65

# train final classifier on all data - use this for our comparison to outcomes
train_final_xgb_classifier <- function(dat) {
  set.seed(107)
  cds_ohs = which(names(dat)=="cds_ohs") # class column number
  xgb.train.data = xgb.DMatrix(data.matrix(dat[,-cds_ohs]), label = dat[,cds_ohs], missing = NA)
  param <- list(objective = "binary:logistic", base_score = 0.5)
  cv <- createFolds(dat[,"cds_ohs"], k = 5)
  xgboost.cv = xgb.cv(param=param, data = xgb.train.data, folds = cv, 
                      nrounds = 1000, early_stopping_rounds = 100, metrics='auc')
  # train-auc: .84, test-auc: .74
  best_iteration = xgboost.cv$best_iteration
  xgb.model <- xgboost(param=param, data=xgb.train.data, nrounds=best_iteration)
  xgb.preds = predict(xgb.model, xgb.train.data)
  dat$pred = xgb.preds
  dat$CDSpred = ifelse(dat$pred > .5, 1, 0)
  # ROC on the test set (do we want it on combined test and train set?)
  xgb.roc_obj <- roc(dat[,cds_ohs], xgb.preds)
  # AUC = .83
  pdf("xgb-final-rawLENA-ROC.pdf", width=4, height=4)
  plot(xgb.roc_obj)
  text(x=.3, y=.2, paste("AUC =",round(xgb.roc_obj$auc, 3)))
  dev.off()
  
  col_names = attr(xgb.train.data, ".Dimnames")[[2]]
  imp = xgb.importance(col_names, xgb.model)
  roc_plot = plot(xgb.roc_obj)
  pdf("xgb-final-rawLENA-importance.pdf", width=4.5, height=4.5)
  xgb.plot.importance(imp)
  dev.off()
  
  xgb <- list(model = xgb.model, preds = dat, importance=imp)
  xgb.save(xgb.model, "final_rawLENA_xgb.model")
  saveRDS(xgb, file="final_rawLENA_xgb_model.Rds")
}

#train_final_xgb_classifier(dat)

# Train with each dataset left out
dset_left_out <- tibble()
for(dset in unique(d$Dataset)) {
  d_nonaps <- d %>% filter(cds_ohs!="nap", Dataset!=dset) # train without dataset
  d_test <- d %>% filter(cds_ohs!="nap", Dataset==dset) # test on held-out dataset
  tdat <- add_features(d_nonaps, with_demo=F, prop_meaningful=F, per_child_norms=F) 
  tdd = get_train_test_cv(tdat, train_proportion=0.9) 
  txgb = create_xgboost_model(tdd$train, tdd$test, tdd$cv, graph=F)
  #cv <- createFolds(tdat[,"cds_ohs"], k = 5)
  #txgb = create_xgboost_model(d_nonaps, d_test, cv, graph=F)
  dset_left_out <- bind_rows(dset_left_out,
                             bind_cols(list(Dataset = dset, 
                                            Accuracy = as.numeric(txgb$cmOOF$overall['Accuracy']),
                                            AUC = as.numeric(txgb$roc$auc))))
}

dset_left_out
# acc / AUC are somewhat lower if SOT Outreach/Stanford are left out; 
# leaving out CONTX / WF is pretty good (~.7 acc, .74-.78 AUC)
# Dataset      Accuracy   AUC
# WF              0.708 0.786
# CONTX           0.683 0.723
# SOT Outreach    0.666 0.721
# SOT Stanford    0.628 0.690

# Decision Forest - overfits
create_decision_forest(dd$train, dd$test, dd$cv)
