require(here)

d <- read.csv(here("data/data_WF_withNAPS_210307.csv"), header=T, stringsAsFactors = F) %>% # 2567 5-min segments from 29 families
  mutate(Dataset = "WF", # Weisleder & Fernald (2013)
         language = "spanish", # 29 Spanish-speaking families
         Sleep = NULL) %>% 
  dplyr::select(-mom_ed, -hour, -minutes, -minutes_prop, -time2)

#d2 <- read.csv(here("data/data_contx.csv"), header=T, stringsAsFactors=F) # 1279 10-min segments from 90 subjects
# original CONTX: 1279 10-min segments
# with 5-min segments: 1279*2, + 876 nap segments (in cds_)
# now with 5-minute segments
d25 <- read.csv(here("data/data_contx_5min_and_10min_withNAPS_200820.csv"), stringsAsFactors=F)
# Janet: 10-min data: filter by the contx_inc - and any value here of 1 or 0 means that you can go off the 10-min seg LENA variables (this is 11 or so observations off from the last set I gave you but everyone has their dense hour so i think it’s fine)

d25[which(is.na(d25$months_age)),]$months_age = 25 # from original CONTX data file
d25[which(is.na(d25$hi)),]$hi = 58 # from original CONTX


d2 <- subset(d25, !is.na(contx_inc)) # 1268 10-min segments
# what is the correlation of LENA vars in 

# all 5- vs. 10-min correlations quite high
cor(d2$AWC_5min, d2$AWC_10min) # 0.900
cor(d2$CVC_5min, d2$CVC_10min) # 0.902
cor(d2$CTC_5min, d2$CTC_10min) # 0.917
cor(d2$meaningful_5min, d2$meaningful_10min) # .897
cor(d2$silence_5min, d2$silence_10min) # .899
cor(d2$noise_5min, d2$noise_10min) # .758 - not quite as high!
cor(d2$tv_5min, d2$tv_10min) # .924
cor(d2$distant_5min, d2$distant_10min) # .933

# segmentnum = coded in order from densest -> least dense segment until 6 segments of primarily CDS are found

# let's sub in the 5-min segments in place of the 10-min segments
d2 <- d25 %>% #filter(cds_ohs!="nap") %>% 
  dplyr::select(-rectime, -X, -segment_num, -contx_inc, -mom_ed) %>%
  dplyr::select(-contains("_10min")) %>%
  rename(AWC = AWC_5min, CTC = CTC_5min, CVC = CVC_5min,
         noise_min = noise_5min, silence_min = silence_5min, distant_min = distant_5min,
         meaningful_min = meaningful_5min, tv_min = tv_5min, dur_min = dur_5min) %>%
  mutate(Dataset = "CONTX")

# check columns
intersect(names(d), names(d2)) # 16 shared
setdiff(names(d), names(d2)) 
setdiff(names(d2), names(d)) 

table(d2$language)


d4 <- read.csv(here("data/data_SOT_Outreach_withNAPS_210307.csv"), header=T, stringsAsFactors = F) %>%
  mutate(language="english",
         Dataset = "SOT Outreach") %>%
  dplyr::select(-mom_ed, -Sleep)
# n = 29 kids at 17 - 19months -- this sample repeats with the CONTX sample, 
# but the kids are younger. also, we have a couple of kids where they were sampled on two different days and this changes their age e.g., from 17 to 18 months. 
#summary(d4) # 502 NA fat_ed

d = rbind(d, d2, d4)
d$language = as.factor(d$language)
# - removing time causes .01-.03 decrease in AUC
# - include language (in demographic vars): done, not usually important
# Repeated CV leaving out 20% of the children each time: done
# when testing child-normed / meaningful-normed, remove the raw LENA variables: done
# Look at misclassifications (average values for LENA vars esp?)
# ..see how badly we do without CTC? 

# adding third dataset - "the Stanford families" 27 18-month-olds, full-day recording
d3 <- read.csv(here("data/data_SOT_Stanford_withNAPS_210307.csv"), header=T, stringsAsFactors = F) %>%
  mutate(language="english",
         Dataset = "SOT Outreach") %>%
  dplyr::select(-mom_ed, -Sleep)
intersect(names(d3), names(d)) # SOT is already coded in the same way
setdiff(names(d3), names(d)) # time, mom_ed
setdiff(names(d), names(d3)) # language
d = rbind(d, d3)

d[which(d$cds_ohs=="naps"),]$cds_ohs = "nap"

save(d, file="data/combined_data_5min.Rdata") # 12936

d %>% group_by(Dataset, id) %>% 
  summarise(n = n()) %>% 
  group_by(Dataset) %>% 
  summarise(n=n())
