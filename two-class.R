
#full <- read.csv(here("data.csv"), header=T, stringsAsFactors = F)
# problem: some meaningful_min==0, so propAWC etc can be Inf - so we add .01
# dur_min can also be quite small (mostly ~5, but min=.07, 16 segments <1min)

# this function adds/removes features to the dataset
add_features <- function(full, with_demo=T, prop_meaningful=T, per_child_norms=T) {
  daf <- full %>% 
    mutate(AWC = AWC / dur_min, # per minute log(AWC+.01)
           CTC = CTC / dur_min, 
           CVC = CVC / dur_min,
           noise = noise_min / dur_min, # make these all proportion of total segment?
           silence = silence_min / dur_min,
           distant = distant_min / dur_min,
           tv = tv_min / dur_min,
           meaningful = meaningful_min / dur_min,
           cds_ohs = ifelse(cds_ohs=="split" | cds_ohs=="nap", 0, cds_ohs), # re-classify "split" & "nap" as OHS
           sex = ifelse(sex=="M", 1, 0)) %>%
    dplyr::select(-dur_min, -noise_min, -silence_min, -distant_min, -tv_min)

  # normalize by meaningful_mins, add some combinations
  if(prop_meaningful) {
    daf <- daf %>% 
      mutate(propAWC = AWC / (meaningful_min+.01), # per meaningful minute
             propCTC = CTC / (meaningful_min+.01), 
             propCVC = CVC / (meaningful_min+.01) )
             #AWCxCTC = propAWC*propCTC,
             #CTCxCVC = propCTC*propCVC,
             #AWCxCVC = propAWC*propCVC) 
  } else {
    # feature combinations?
  }
  
  daf <- daf %>% dplyr::select(-meaningful_min)
  
  # add per-child norms or not (substitute AWC for AWCnorm or no?)
  if(per_child_norms) {
    ch_mean <- full %>% 
      group_by(id) %>%
      summarise(AWCmean = mean(AWC), 
                CTCmean = mean(CTC),
                CVCmean = mean(CVC)) 

    daf <- daf %>% left_join(ch_mean, by="id")
    #add AWCnorm (per child)
    daf <- daf %>% 
      mutate(AWCnorm = AWC / AWCmean, # or propAWC / AWCmean
             CTCnorm = CTC / CTCmean,
             CVCnorm = CVC / CVCmean) 
  }
  
  if(per_child_norms | prop_meaningful) { # if both true, keep raw AWC etc
    daf <- daf %>% dplyr::select(-AWC, -CTC, -CVC)
  }
  
  # keep demographic variables
  if(with_demo) {
    dat <- dplyr::select(daf, -id) 
  } else{ # without demographic vars
    dat <- dplyr::select(daf, -id, -months_age, -sex, -language, -Dataset) 
  }
  
  return(dat)
  }


# this just re-orders df?
#full = full[sample(.N)]


#### Create Train / Test and Folds
get_train_test_cv <- function(dat, train_proportion=.8, seed=42) {
  set.seed(seed) # reproducible sampling
  train_size <- floor(train_proportion * nrow(dat))
  train_ind <- sample(seq_len(nrow(dat)), size = train_size)
  train = dat[train_ind,] # 2053
  test = dat[-train_ind,] # 514
  cv <- createFolds(train[,"cds_ohs"], k = 5)
  return(list(train=train, test=test, train_ind=train_ind, cv=cv))
}


# leaves out a random sample of children each time
get_train_test_cv_children_left_behind <- function(dat, prop_kept=.8, seed=42) {
  set.seed(seed) # reproducible sampling
  ids = unique(dat$id)
  train_size <- floor(prop_kept * length(ids)) # 116 / 146
  train_ids = sample(ids, train_size)
  train_ind <- which(dat$id %in% train_ids)
  print(paste("Train on", train_size, "of", length(ids), "subjects, representing", 
              length(train_ind), "/", nrow(dat), "data points.")) # 256
  dat$id = NULL
  train = dat[train_ind,] # 2053
  test = dat[-train_ind,] # 514
  cv <- createFolds(train[,"cds_ohs"], k = 5)
  return(list(train=train, test=test, train_ind=train_ind, cv=cv))
}

# leaves out a fifth of the children each time and trains the model, then averages
# the AUROC and test accuracy
do_child_level_cv <- function(dat, fname='', k=5) {
  ids = unique(dat$id)
  cv_ch = createFolds(ids, k=k) # ~31 kids per fold
  xg_mods = list()
  accuracy = rep(NA, k)
  auc = rep(NA, k)
  auroc = tribble(~ fold, ~ index, ~ sensitivity, ~ specificity)
  for(i in 1:k) { 
    test_ids = ids[ cv_ch[[i]] ]
    test_ind <- which(dat$id %in% test_ids)
    test = dat[test_ind,]
    train = dat[-test_ind,]
    test$id = NULL
    train$id = NULL
    cv <- createFolds(train[,"cds_ohs"], k = k) 
    xg_mods[[i]] = create_xgboost_model(train, test, cv, graph=F)
    accuracy[i] = xg_mods[[i]]$cmOOF$overall["Accuracy"]
    auc[i] = xg_mods[[i]]$roc$auc
    sens = xg_mods[[i]]$roc$sensitivities
    spec = xg_mods[[i]]$roc$specificities
    auroc = rbind(auroc, cbind(rep(i, length(sens)), 
                               1:length(sens),
                               sens,
                               spec))
  }
  names(auroc) = c("fold", "index", "sensitivity", "specificity")
  # sens/specs are different lengths per fold, so we need to cut and average
  #auroc$cut_ind = cut_interval(auroc$index, 100)
  if(fname=='') fname = ncol(train-1)
  
  ggplot(auroc) + 
    geom_line(aes(x=specificity, y=sensitivity, group=fold), alpha=.7) + theme_classic() +
    scale_x_reverse() + 
    geom_label(aes(label=paste0("Mean AUC: ", round(mean(auc), 2)), x=.5, y=.2) )
  #geom_segment(x = 1, xend=0, y=0, yend=1, color='gray') + # messed up due to reversed x-axis
  #geom_abline(slope=1, yintercept=0, color="gray") +
  ggsave(file=paste0("figs/xgb-multiROC-",fname,".pdf"), width=3.5, height=3.5)
  
  # try to shade lower right triangle?
  # annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, alpha = .5)
  
  #xg_mods[[i]]$preds
  print(paste0("Mean AUC: ", round(mean(auc), 2))) # .71
  print(paste0("Mean test accuracy: ",round(mean(accuracy), 2))) # .66
  return(list(auroc=auroc, auc=auc, test_acc=accuracy))
}

# use just raw LENA features
create_decision_forest <- function(train, test, cv, seed=42) {
  train$months_age = NULL
  train$language = NULL
  train$cds_ohs = as.factor(train$cds_ohs)
  rf1 <- randomForest(cds_ohs ~ ., data = train, 
                         ntree = 500, mtry = 2, importance = T)
  # OOB error rate: mtry=6 
  # mtry = 5 
  # mtry = 4 33.91%
  # mtry = 3 33.38%
  # mtry = 2 33.14%
  
  # Predicting on train set
  predTrain <- predict(rf1, train, type = "class")
  # Checking classification accuracy
  table(predTrain, train$cds_ohs)
  
  train$predicted = predTrain
  
  train %>% group_by(predicted, cds_ohs) %>% 
    summarize(AWCmean=mean(AWC), CTCmean=mean(CTC), CVCmean=mean(CVC),
              noise=mean(noise), silence=mean(silence), distant=mean(distant),
              tv = mean(tv) )
              #propCVC=mean(propCVC), propCTC=mean(propCTC), propAWC = mean(propAWC))
  
  test$months_age = NULL
  test$language = NULL
  test$cds_ohs = as.factor(test$cds_ohs)
  
  # Predicting on Validation set
  predValid <- predict(rf1, test, type = "class")
  # Checking classification accuracy
  mean(predValid == test$cds_ohs)  # .66              
  table(predValid, test$cds_ohs)
  # predValid   0   1
  #         0 605 233
  #         1 306 818
  test$predicted = predValid
  
  rfpreds = rbind(train, test)
  write.csv(rfpreds, file="rf-predicted.csv")
  
  # check important variables
  importance(rf1)        
  varImpPlot(rf1) 
  
  # retrain on all data
  alld = rfpreds %>% dplyr::select(-predicted)
  rfa <- randomForest(cds_ohs ~ ., data = alld, 
                      ntree = 1000, mtry = 2, importance = T)
  varImpPlot(rfa)
  alld$predicted = predict(rfa, alld, type = "class")
  
  rft = table(alld$predicted, alld$cds_ohs)
  #       0    1
  #  0 5239   10
  #  1    0 5808
  round(rft / rowSums(rft), 2)
}

#### Train Tree
create_single_tree <- function(train, test, cv, seed=42, save.plot=T, show.plot=T) {
  set.seed(seed) # reproducible
  cds_ohs = which(names(train)=="cds_ohs") # class column number
  ctrl <- trainControl(method = "cv",index = cv)
  
  tree.cv <- train(x = train[,-cds_ohs], y = as.factor(train[,"cds_ohs"]), method = "rpart2", 
                   tuneLength = 7, trControl = ctrl, control = rpart.control())
  tree.model = tree.cv$finalModel
  if(show.plot) rpart.plot(tree.model, type = 2, extra = 7, fallen.leaves = T) # label all nodes; draw the split labels below the node labels
  if(show.plot) rpart.plot(tree.model, type = 2, extra = 2, fallen.leaves = T) # 
  tree.preds = predict(tree.model, test)[,2]
  tree.roc_obj <- roc(test[,cds_ohs], tree.preds)
  #cat("Tree AUC ", auc(tree.roc_obj)) 
  if(save.plot) pdf(paste0("figs/treeROC-",ncol(train)-1,".pdf"), width=3.5, height=3.5)
  if(show.plot) plot(tree.roc_obj)
  if(show.plot) text(x=.5, y=.1, paste0("AUC = ",round(auc(tree.roc_obj), 3))) 
  if(save.plot) dev.off()
  return(list(tree.roc = tree.roc_obj, tree.model = tree.model, tree.preds = tree.preds))
}

create_xgboost_model <- function(train, test, cv, fname='', seed=42, graph=T) {
  set.seed(seed) # reproducible
  cds_ohs = which(names(train)=="cds_ohs") # class column number
  xgb.train.data = xgb.DMatrix(data.matrix(train[,-cds_ohs]), label = train[,cds_ohs], missing = NA)
  param <- list(objective = "binary:logistic", base_score = 0.5)
  xgboost.cv = xgb.cv(param=param, data = xgb.train.data, folds = cv, 
                      nrounds = 1000, early_stopping_rounds = 100, metrics='auc')
  best_iteration = xgboost.cv$best_iteration
  xgb.model <- xgboost(param=param, data=xgb.train.data, nrounds=best_iteration)
  xgb.test.data = xgb.DMatrix(data.matrix(test[,-cds_ohs]), missing = NA)
  xgb.preds = predict(xgb.model, xgb.test.data)
  
  # ROC on the test set (do we want it on combined test and train set?)
  xgb.roc_obj <- roc(test[,cds_ohs], xgb.preds)
  
  # add predicted column to both training and test sets
  train$pred = predict(xgb.model, xgb.train.data)
  train$set = "train"
  test$pred = xgb.preds
  test$set = "test"
  
  ttdat = rbind(train, test)
  ttdat$CDSpred = ifelse(ttdat$pred > .5, 1, 0)
  
  #xgb.train.roc_obj <- roc(train[,cds_ohs], xgb.train.preds)
  #plot(xgb.train.roc_obj)
  #text(x=.5, y=.1, paste0("AUC = ",round(auc(xgb.train.roc_obj), 3))) # raw LENA AUC: .835
  
  if(graph) {
    if(fname=='') fname = ncol(train)-1
    pdf(paste0("figs/xgbROC-",fname,".pdf"), width=3.5, height=3.5)
    plot(xgb.roc_obj)
    text(x=.5, y=.1, paste0("AUC = ",round(auc(xgb.roc_obj), 3))) 
    dev.off()
  
    #### XGB importance
    col_names = attr(xgb.train.data, ".Dimnames")[[2]]
    imp = xgb.importance(col_names, xgb.model)
    pdf(paste0("figs/xgb-importance-",fname,".pdf"), width=6, height=6)
    xgb.plot.importance(imp)
    dev.off()
    
    importance_matrix <- xgb.importance(col_names, model = xgb.model)
    print(importance_matrix)
  }
  
  OOF_prediction <- data.frame(max_prob=round(xgb.preds), label=test$cds_ohs) 
  
  cmOOF = confusionMatrix(factor(OOF_prediction$label), 
                          factor(OOF_prediction$max_prob),
                          mode = "everything")
  #print(paste("XGB AUC =", auc(xgb.roc_obj))) 
  
  print(cmOOF) 
  
  # should retrain on all data and save that model..
  
  return(list(model=xgb.model, train.data=xgb.train.data, test.data=xgb.test.data, 
              ttdat = ttdat, # train and test data with predicted
              cmOOF=cmOOF, roc=xgb.roc_obj, preds=xgb.preds)) 
}

#### XGBoost Explainer
explain_xgboost_model <- function(xgb) {
  explainer = buildExplainer(xgb$model, xgb$train.data, type="binary", 
                             base_score = 0.5, trees_idx = NULL)
  # Error in while (currentnode > 0) { : argument is of length zero
  pred.breakdown = explainPredictions(xgb$model, explainer, xgb$test.data)
  cat('Breakdown Complete','\n')
  weights = rowSums(pred.breakdown)
  pred.xgb = 1/(1+exp(-weights))
  cat(max(xgb$preds-pred.xgb),'\n')
  return(list(explainer = explainer, 
              pred.breakdown = pred.breakdown,
              pred.xgb = pred.xgb))
}


show_xgboost_example <- function(xgb, explainer, test, idx_to_get) {
  cds_ohs = which(names(train)=="cds_ohs") # class column number
  showWaterfall(xgb$model, explainer, xgb$test.data, data.matrix(test[,-cds_ohs]),
                idx_to_get, type = "binary")
}
