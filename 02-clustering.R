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

# Remove fat_ed and mom_ed
d$time = NULL
d$fat_ed = NULL
d$mom_ed = NULL


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


# (slow)
set.seed(42)
nb <- NbClust(all_dat_sc, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=12, method = "kmeans", index = "all", alphaBeale = 0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
# * Among all indices:                                                
# 1 proposed 2 as the best number of clusters 
# 5 proposed 3 as the best number of clusters 
# 9 proposed 4 as the best number of clusters 
# 1 proposed 5 as the best number of clusters 
# 1 proposed 6 as the best number of clusters 
# 2 proposed 7 as the best number of clusters 
# 4 proposed 12 as the best number of clusters 

#par(mfrow=c(1,3))
#plot_wss_vs_clusters(dat_sc, "Child-directed speech")
#plot_wss_vs_clusters(ohs_dat_sc, "Overheard speech")
plot_wss_vs_clusters(all_dat_sc, "") # "All segments" 6-7 clusters
# saved in paper/clustered_segments.pdf / .png

set.seed(42)
kfit <- kmeans(all_dat_sc, 7) # 
# get cluster means
# append cluster assignment

all_dat_cl <- with(kfit, data.frame(dat, cluster))
#save(all_dat_cl, file="../data/all_clusters_raw_lena_5mins.Rdata")

# mclust - maybe more principled than k-means?
library(mclust)
fit <- Mclust(all_dat_sc) 
# "Mclust selects the optimal model according to BIC for EM initialized by hierarchical clustering for parameterized Gaussian mixture models"
plot(fit) # plot results
summary(fit) # display the best model
# Clustering table:
#  1    2    3    4    5    6    7    8    9 
# 678 2559  252 1341 2915  725 2076 1438  952 
t(fit$parameters$mean)


chil_mcl <- d %>% mutate(cluster = fit$classification) %>% 
  group_by(id, cluster) %>% summarise(n = n()) %>%
  group_by(cluster) %>% 
  summarise(Nclust=n())


all_dat_cl <- d %>% 
  mutate(nap = ifelse(cds_ohs=="nap", 1, 0),
         cds = ifelse(cds_ohs==1, 1, 0),
         ohs = ifelse(cds_ohs==0 | cds_ohs=="split", 1, 0),
         #split = ifelse(cds_ohs=="split", 1, 0) # lump into OHS or keep separate?
  ) %>% dplyr::select(-cds_ohs)

all_tab <- aggregate(all_dat_cl, by=list(cluster=fit$classification), FUN=mean) %>%
  dplyr::select(-id, -sex, -months_age, -dur_min, -Dataset, -language, -hi) %>% 
  mutate(N = table(fit$classification))
all_tab %>% arrange(cds, ohs) %>%
  kbl(caption = "Means of LENA variables by cluster.", digits=2) %>%
  kable_classic(full_width = F, html_font = "Cambria") 

