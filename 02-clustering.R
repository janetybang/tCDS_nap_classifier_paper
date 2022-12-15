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
require(kableExtra)

load("data/combined_data_5min.Rdata")

d_allfeat <- d

d$time = NULL

source("two-class.R")
dat <- add_features(d, with_demo=F, prop_meaningful=F, per_child_norms=F)
dat$cds_ohs = d$cds_ohs

plot_wss_vs_clusters <- function(dat_sc, title) {
  wss <- (nrow(dat_sc)-1) * sum(apply(dat_sc,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(dat_sc, centers=i)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters", main=title,
       ylab="Within groups sum of squares")
}

# scale features
all_dat_sc = scale(dat %>% dplyr::select(-cds_ohs)) 


plot_wss_vs_clusters(all_dat_sc, "") # "All segments" 6-7 clusters
# saved in paper/clustered_segments.pdf / .png

set.seed(42)
kfit <- kmeans(all_dat_sc, 7) # 
# get cluster means
# append cluster assignment

all_dat_cl <- with(kfit, data.frame(dat, cluster))
#save(all_dat_cl, file=here("data/all_clusters_raw_lena_5mins.Rdata"))

