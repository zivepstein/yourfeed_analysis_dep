library(lme4)
library(lmerTest)
library(tidyverse)
library("readxl")
options("scipen"=100, "digits"=4)

pretest <- read_excel("/Users/ziv.e/github/yourfeed_analysis/data/20210129 pretest.xlsx")
long_raw <- read_csv("https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1")
#item_level <- read_csv("https://www.dropbox.com/s/asrmhf3upvhy1fq/aj_headlines.csv?dl=1")
item_level <-read_csv("/Users/ziv.e/github/yourfeed_analysis/data/yourfeed_item_level.csv")
item_level$nchar <- nchar(item_level$text)
res.pca <- prcomp(item_level[,121:2424])
item_level$deepmoji1 <-res.pca$x[,1]
item_level$deepmoji2 <-res.pca$x[,2]
item_level$deepmoji3 <-res.pca$x[,3]

torchmoji <- read_csv("/Users/ziv.e/github/torchmoji/data/out.csv", col_names=F)
item_level <- cbind(item_level, torchmoji)

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
long_raw$crt <- (as.numeric(long_raw$crt_ages == 4) + as.numeric(long_raw$crt_printer == 10) + as.numeric(long_raw$crt_bread == 39) + as.numeric(long_raw$crt_race == 2) + as.numeric(long_raw$crt_sheep == 8))/5
item_level$real = as.numeric(!grepl("f_", item_level$filename))-0.5
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

train_items <- sample(x=unique(yourfeed$item), size=length(unique(yourfeed$item))/2)
train_subj <- sample(x=unique(yourfeed$rid), size=length(unique(yourfeed$rid))/2)

yourfit <- yourfeed  %>% filter(average_A>log(50)) %>% mutate(
  is_train_item = item %in% train_items,
  is_train_subj = rid %in% train_subj,
  is_train = is_train_item |is_train_subj
)

yourfit <- yourfit %>% left_join(
  yourfit %>% filter(is_train_item) %>% group_by(rid) %>% summarize(
    train_subject_ldwell = mean(ldwell),
    train_subject_ldwell_var = var(ldwell),
    train_subject_response = mean(response),
    train_subject_response_var = var(response),
  ), by = 'rid') %>% left_join(
    yourfit %>% filter(is_train_subj) %>% group_by(item) %>% summarize(
      train_item_ldwell = mean(ldwell),
      train_item_ldwell_var = var(ldwell),
      train_item_response = mean(response),
      train_item_response_var = var(response)
    ), by = 'item')
yourfit$response <- as.factor(yourfit$response)
# write.csv(yourfit, "data/yourfit.csv")
# yourfit <- read.csv( "data/yourfit.csv")

#prediction task
#"crt","real","concordant","discordant","likely_true" ,"favors_party","benefits_party","important","funny","reputation_overall","reputation_partyloyalty","reputation_engage","likely_share")
library(randomForest)
library(ROCR)

eval_rf <- function(rf_classifier, outcome, name){
  if (outcome == 'ldwell'){
    p <- predict(rf_classifier, yourfit%>% filter(!is_train))
    out <- cor(p,yourfit[!yourfit$is_train,outcome])
    print(out)
  } else if (outcome == 'response'){
    p <- predict(rf_classifier, yourfit%>% filter(!is_train), type="prob")
    pred <- prediction(predictions = p[,2], labels=yourfit[!yourfit$is_train,outcome])
    auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
    out <- auc
    print(out)
  } else{
    print("outcome not supported")
  }
  save(rf_classifier, file=paste("models/", name , ".RData", sep=""))
  return(out)
}
#test train split - within person within item

#+train_item_ldwell+train_item_ldwell_var+train_subject_ldwell+train_subject_ldwell_var+

user_vars = c("crt","education",'media1','media2','attention', 'age', "demrep_c", "gender", "income","facebook_frequency" )
item_vars = c('funny','real','likely_true','favors_party','important','reputation_overall','reputation_partyloyalty', "nchar", "affect", 'num_person', 'trump', 'biden', "deepmoji1", "deepmoji2", "deepmoji3")
enviornment_vars = c('order','prekink', 'is_mobile')

generate_formula <- function(outcome, user_factors = "", item_factors = "", environment_factors = ""){
  u_f <- "1"
  i_f <- "1"
  e_f <- "1"
  if( user_factors == 'features'){
    u_f <- paste(user_vars , collapse=" + ")
  } else if ( user_factors == 'dummies'){
    u_f <- paste("train_subject_", outcome, "+train_subject_", outcome, "_var", sep="")
  }
  if( item_factors == 'features'){
    i_f <- paste(item_vars , collapse=" + ")
  } else if ( item_factors == 'dummies'){
    i_f <- paste("train_item_", outcome, "+train_item_", outcome, "_var", sep="")
  }
  if( environment_factors == 'features'){
    e_f <- paste(enviornment_vars , collapse=" + ")
  } 
  f <- paste(u_f, i_f, e_f, sep = "+")
  formula = paste(outcome, "~", f, sep=" ")
  return(formula)
}

do_rf <- function(outcome, user_factors = "", item_factors = "", environment_factors = "", name ="", mtry=2){
  f <- as.formula(generate_formula(outcome, user_factors =user_factors, item_factors =item_factors,environment_factors=environment_factors ))
  rf_classifier = randomForest(f,data=yourfit %>% filter(is_train), ntree=50, mtry=mtry, importance=TRUE)
  eval_rf(rf_classifier, outcome, name = name)
}
  
#out <- array(NA, dim=c(1, 4, 7))
out <- matrix(nrow=4, ncol=7)
out[1,1] <- do_rf("ldwell", user_factors = "dummies", name = "subject_dummies")
out[1,2] <- do_rf("ldwell", item_factors = "features", name = "headline_features")
out[1,3] <- do_rf("ldwell", environment_factors = "features", name = "enviornment_features")
out[1,4] <- do_rf("ldwell",  user_factors = "dummies", item_factors = "features", name = "subject_dummies_headline_features")
out[1,5] <- do_rf("ldwell", user_factors = "dummies",environment_factors = "features", name = "subject_dummies_environment_features")
out[1,6] <- do_rf("ldwell",  environment_factors = "features", item_factors = "features", name = "environment_features_headline_features")
out[1,7] <- do_rf("ldwell",  user_factors = "dummies",environment_factors = "features", item_factors = "features", name = "subject_dummies_headline_features_environment_features")

out[2,1] <- do_rf("ldwell", user_factors = "features", name = "subject_features")
out[2,2] <- do_rf("ldwell", item_factors = "dummies", name = "headline_dummies")
out[2,3] <- do_rf("ldwell", environment_factors = "features", name = "enviornment_features")
out[2,4] <- do_rf("ldwell",  user_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies")
out[2,5] <- do_rf("ldwell", user_factors = "features",environment_factors = "features", name = "subject_features_environment_features")
out[2,6] <- do_rf("ldwell",  environment_factors = "features", item_factors = "dummies", name = "environment_features_headline_dummies")
out[2,7] <- do_rf("ldwell",  user_factors = "features",environment_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies_environment_features")

out[3,1] <- do_rf("response", user_factors = "dummies", name = "subject_dummies")
out[3,2] <- do_rf("response", item_factors = "features", name = "headline_features")
out[3,3] <- do_rf("response", environment_factors = "features", name = "enviornment_features")
out[3,4] <- do_rf("response",  user_factors = "dummies", item_factors = "features", name = "subject_dummies_headline_features")
out[3,5] <- do_rf("response", user_factors = "dummies",environment_factors = "features", name = "subject_dummies_environment_features")
out[3,6] <- do_rf("response",  environment_factors = "features", item_factors = "features", name = "environment_features_headline_features")
out[3,7] <- do_rf("response",  user_factors = "dummies",environment_factors = "features", item_factors = "features", name = "subject_dummies_headline_features_environment_features")

out[4,1] <- do_rf("response", user_factors = "features", name = "subject_feartures")
out[4,2] <- do_rf("response", item_factors = "dummies", name = "headline_dummies")
out[4,3] <- do_rf("response", environment_factors = "features", name = "enviornment_features")
out[4,4] <- do_rf("response",  user_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies")
out[4,5] <- do_rf("response", user_factors = "features",environment_factors = "features", name = "subject_features_environment_features")
out[4,6] <- do_rf("response",  environment_factors = "features", item_factors = "dummies", name = "environment_features_headline_dummies")
out[4,7] <- do_rf("response",  user_factors = "features",environment_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies_environment_features")

par(mfrow=c(2,2))
barplot(out[1,],main ="Attention: subject dummies + headline features", ylab = "cor", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
barplot(out[2,],main ="Attention: subject features + headline dummies", ylab = "cor", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
barplot(out[3,],main ="Sharing: subject dummies + headline features", ylab = "AUC", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
barplot(out[4,],main ="Sharing: subject features + headline dummies",ylab = "AUC", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
