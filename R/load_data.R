library(lme4)
library(lmerTest)
library(tidyverse)
library("readxl")
setwd("/home/groh/github/yourfeed_analysis/R")
options("scipen"=100, "digits"=4)


pretest <- read_excel("../data/static/20210129 pretest.xlsx")
item_level <-read_csv("../data/static/yourfeed_item_level.csv")


torchmoji <- read_csv("../data/static/yourfeed_torchmoji.csv", col_names=F)
item_level <- cbind(item_level, torchmoji)
item_level$nchar <- nchar(item_level$text)
res.pca <- prcomp(item_level[,121:2424])
item_level$deepmoji1 <-res.pca$x[,1]
item_level$deepmoji2 <-res.pca$x[,2]
item_level$deepmoji3 <-res.pca$x[,3]
item_level$real = as.numeric(!grepl("f_", item_level$filename))-0.5

pretest = as.data.frame(pretest)
cov <- c("likely_true", "favors_r", "benefits_r", "important", "funny", "surprising", "reputation_overall", 'reputation_partyloyalty', 'reputation_engage' ,'likely_share')
for (c in cov){
  col_valz <- c()
  for (i in 1:140){
    col = paste(c, "(", i, ")", sep="")
    col_valz <- c(col_valz,(mean(pretest[,col], na.rm=T)))
  }
  item_level[,c] = scale(col_valz)
}

long_data_url <- "https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1"
long_raw <- read_csv(long_data_url)
long_raw$crt <- (as.numeric(long_raw$crt_ages == 4) + as.numeric(long_raw$crt_printer == 10) + as.numeric(long_raw$crt_bread == 39) + as.numeric(long_raw$crt_race == 2) + as.numeric(long_raw$crt_sheep == 8))/5

long <- long_raw %>% mutate(
  crt = (as.numeric(crt_ages == 4) + as.numeric(crt_printer == 10) + as.numeric(crt_bread == 39) + as.numeric(crt_race == 2) + as.numeric(crt_sheep == 8))/5,
  pk = as.numeric((pk1)==3) + as.numeric((pk2)==1)  + as.numeric((pk3)==3) + as.numeric((pk4)==1) + as.numeric((pk5)==3),
  ac1 = att1 == 47,
  ac2 = att2 == 35,
  attention= ac1 + ac2
) %>% filter(completed==1) %>% merge(item_level, all.x=T, by.x='item', by.y='index')

yourfeed <- long %>% filter(condition==1) %>% mutate(
  concordant = case_when(
    (lean == 'D-leaning') & (demrep_c<=3 ) ~ 1,
    (lean == 'R-leaning') & (demrep_c>3 ) ~ 1,
    T ~ 0
  ),
  discordant = case_when(
    (lean == 'D-leaning') & (demrep_c>3 ) ~ 1,
    (lean == 'R-leaning') & (demrep_c<=3 ) ~ 1,
    T ~ 0
  ),
  favors_party = case_when(
    demrep_c<=3 ~ -1*favors_r,
    demrep_c>3 ~ favors_r,
  ),
  benefits_party = case_when(
    demrep_c<=3 ~ -1*benefits_r,
    demrep_c>3 ~ benefits_r,
  ),
  wdwell = case_when(
    dwell > quantile(dwell, 0.95) ~ quantile(dwell, 0.95),
    T ~ dwell
  ),
  ldwell = log(1+dwell),
  attend =  as.numeric(dwell > 1330),
  prekink= as.numeric(order<6),
  crt=scale(crt),
  has_person = as.numeric(num_person > 0)
) %>% filter(order!=140)

yourfeed <- yourfeed %>%left_join((yourfeed %>% group_by(rid) %>% summarize(
  average_A = mean(ldwell),
  average_response = mean(response),
  std_A = sd(ldwell)
)), by='rid') %>%
  mutate(
    personal_ldwell = ldwell - average_A,
    personal_response = response - average_response
  )

