library(lme4)
library(lmerTest)
library(tidyverse)
library("readxl")
options("scipen"=100, "digits"=4)

print("loading data...")
# pretest <- read_excel("/Users/ziv.e/github/yourfeed_analysis/data/20210129 pretest.xlsx")
pretest <- read_csv("https://www.dropbox.com/s/lmnyerfkrzcgcxt/20210129pretest.csv?dl=1")
long_raw <- read_csv("https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1")
# item_level <-read_csv("/Users/ziv.e/github/yourfeed_analysis/data/yourfeed_item_level.csv")
item_level <-read_csv("https://www.dropbox.com/s/4pwk7gvgulbgo7o/yourfeed_item_level.csv?dl=1")
torchmoji <- read_csv("https://www.dropbox.com/s/mfeocp3rhvy78qm/yourfeed_torchmoji.csv?dl=1", col_names=F)
# torchmoji <- read_csv("/Users/ziv.e/github/torchmoji/data/yourfeed_torchmoji.csv", col_names=F)
item_level <- cbind(item_level, torchmoji)

item_level$nchar <- nchar(item_level$text)
res.pca <- prcomp(item_level[,122:2425])
item_level$deepmoji1 <-res.pca$x[,1]
item_level$deepmoji2 <-res.pca$x[,2]
item_level$deepmoji3 <-res.pca$x[,3]

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
library(glmnet)
library(ROCR)

eval_glmnet <- function(model, outcome,X_test, Y_test, name){
  if (outcome == 'ldwell'){
    p <- predict(model, X_test, s = "lambda.min")
    out <- cor(p,Y_test)
    print(out)
  } else if (outcome == 'response'){
    p <- predict(model, X_test, s = "lambda.min")
    pred <- prediction(predictions = p, labels=Y_test)
    auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
    out <- auc
    print(out)
  } else{
    print("outcome not supported")
  }
  # save(model, file=paste("models/", name , ".RData", sep=""))
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
    u_f <- "as.factor(rid)"
  }
  if( item_factors == 'features'){
    i_f <- paste(item_vars , collapse=" + ")
  } else if ( item_factors == 'dummies'){
    i_f <- "as.factor(item)"
  }
  if( environment_factors == 'features'){
    e_f <- paste(enviornment_vars , collapse=" + ")
  } 
  
  
  f <- paste(paste("(", u_f, ")", sep=""), paste("(", i_f, ")", sep=""), paste("(", e_f, ")", sep=""), sep = "*")
  f <- str_replace(str_replace(f, "\\*\\(1\\)", ""), "\\(1\\)\\*", "")
  formula = paste( "~", f, sep=" ")
  return(formula)
}

do_glmnet <- function(outcome, user_factors = "", item_factors = "", environment_factors = "", name =""){
  print(paste("starting ", name, "...", sep=""))
  f <- as.formula(generate_formula(outcome, user_factors =user_factors, item_factors =item_factors,environment_factors=environment_factors ))
  X <- model.matrix(f, data= yourfit)
  X_train <-X[yourfit$is_train,]
  X_test <-X[!yourfit$is_train,]
  Y_test <- yourfit %>% filter(!is_train) %>% select(outcome)
  Y_train <- yourfit %>% filter(is_train) %>% select(outcome)
  if (outcome == 'ldwell'){
    model = cv.glmnet(as.matrix(X_train),as.matrix(Y_train))  
  } else{
    model = cv.glmnet(as.matrix(X_train),as.matrix(Y_train), family = 'binomial')
  }
  out <- eval_glmnet(model, outcome, X_test, Y_test, name = name)
  saveRDS(model, paste("models/lasso_models/", name, ".RData", sep=""))
  print(paste(out, name, sep = ":"))
  return(out)
}
  
#out <- array(NA, dim=c(1, 4, 7))
out <- matrix(nrow=4, ncol=7)
out[1,1] <- do_glmnet("ldwell", user_factors = "dummies", name = "subject_dummies")
out[1,2] <- do_glmnet("ldwell", item_factors = "features", name = "headline_features")
out[1,3] <- do_glmnet("ldwell", environment_factors = "features", name = "enviornment_features")
out[1,4] <- do_glmnet("ldwell",  user_factors = "dummies", item_factors = "features", name = "subject_dummies_headline_features")
out[1,5] <- do_glmnet("ldwell", user_factors = "dummies",environment_factors = "features", name = "subject_dummies_environment_features")
out[1,6] <- do_glmnet("ldwell",  environment_factors = "features", item_factors = "features", name = "environment_features_headline_features")
out[1,7] <- do_glmnet("ldwell",  user_factors = "dummies",environment_factors = "features", item_factors = "features", name = "subject_dummies_headline_features_environment_features")

out[2,1] <- do_glmnet("ldwell", user_factors = "features", name = "subject_features")
out[2,2] <- do_glmnet("ldwell", item_factors = "dummies", name = "headline_dummies")
out[2,3] <- do_glmnet("ldwell", environment_factors = "features", name = "enviornment_features")
out[2,4] <- do_glmnet("ldwell",  user_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies")
out[2,5] <- do_glmnet("ldwell", user_factors = "features",environment_factors = "features", name = "subject_features_environment_features")
out[2,6] <- do_glmnet("ldwell",  environment_factors = "features", item_factors = "dummies", name = "environment_features_headline_dummies")
out[2,7] <- do_glmnet("ldwell",  user_factors = "features",environment_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies_environment_features")

out[3,1] <- do_glmnet("response", user_factors = "dummies", name = "subject_dummies")
out[3,2] <- do_glmnet("response", item_factors = "features", name = "headline_features")
out[3,3] <- do_glmnet("response", environment_factors = "features", name = "enviornment_features")
out[3,4] <- do_glmnet("response",  user_factors = "dummies", item_factors = "features", name = "subject_dummies_headline_features")
out[3,5] <- do_glmnet("response", user_factors = "dummies",environment_factors = "features", name = "subject_dummies_environment_features")
out[3,6] <- do_glmnet("response",  environment_factors = "features", item_factors = "features", name = "environment_features_headline_features")
out[3,7] <- do_glmnet("response",  user_factors = "dummies",environment_factors = "features", item_factors = "features", name = "subject_dummies_headline_features_environment_features")

out[4,1] <- do_glmnet("response", user_factors = "features", name = "subject_feartures")
out[4,2] <- do_glmnet("response", item_factors = "dummies", name = "headline_dummies")
out[4,3] <- do_glmnet("response", environment_factors = "features", name = "enviornment_features")
out[4,4] <- do_glmnet("response",  user_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies")
out[4,5] <- do_glmnet("response", user_factors = "features",environment_factors = "features", name = "subject_features_environment_features")
out[4,6] <- do_glmnet("response",  environment_factors = "features", item_factors = "dummies", name = "environment_features_headline_dummies")
out[4,7] <- do_glmnet("response",  user_factors = "features",environment_factors = "features", item_factors = "dummies", name = "subject_features_headline_dummies_environment_features")
# 
# write.csv(out, "models/lasso_results_interaction.csv")
# out <- as.matrix(read.csv("~/github/yourfeed_analysis/models/lasso_results_interaction.csv"))[,-1]
# par(mfrow=c(2,1))
# barplot(out[1,],main ="Attention: subject dummies + headline features", ylab = "cor", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
# barplot(out[2,],main ="Attention: subject features + headline dummies", ylab = "cor", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
# barplot(out[3,]-0.5,offset=0.5, main ="Sharing: subject dummies + headline features", ylab = "AUC", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))
# barplot(out[4,]-0.5, offset=0.5, main ="Sharing: subject features + headline dummies",ylab = "AUC", names.arg=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'))

m <- readRDS("models/lasso_models/subject_dummies_headline_features_environment_features.RData")
z <- coef(m)
mz <- data.frame(as.matrix(z))
mz$name <- rownames(mz)
mz$environment_var <- grepl( paste(enviornment_vars, collapse="|"), mz$name)
mz$item_vars <- grepl( paste(item_vars, collapse="|"), mz$name)
mz$subject_dummy <- grepl("as.factor.rid", mz$name)
s <- log(abs(z[which(!mz$environment_var & !mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
i <- log(abs(z[which(!mz$environment_var & mz$item_vars & !mz$subject_dummy & mz$s1 != 0),"s1"]))
e <- log(abs(z[which(mz$environment_var & !mz$item_vars & !mz$subject_dummy & mz$s1 != 0),"s1"]))
s_i <- log(abs(z[which(!mz$environment_var & mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
s_e <- log(abs(z[which(mz$environment_var & !mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
i_e <- log(abs(z[which(mz$environment_var & mz$item_vars & !mz$subject_dummy & mz$s1 != 0),"s1"]))
s_i_e <- log(abs(mz[which(mz$environment_var & mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
boxplot(s,i,e,s_i, s_e,i_e,  s_i_e, names=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'), ylab="log(abs(coef)")

m <- readRDS("models/lasso_models/subject_dummies_headline_features_environment_features.RData")
z <- coef(m)
mz <- data.frame(as.matrix(z))
mz$name <- rownames(mz)
mz$environment_var <- grepl( paste(enviornment_vars, collapse="|"), mz$name)
mz$item_vars <- grepl( paste(item_vars, collapse="|"), mz$name)
mz$subject_dummy <- grepl("as.factor.rid", mz$name)
s <- log(abs(z[which(!mz$environment_var & !mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
i <- log(abs(z[which(!mz$environment_var & mz$item_vars & !mz$subject_dummy & mz$s1 != 0),"s1"]))
e <- log(abs(z[which(mz$environment_var & !mz$item_vars & !mz$subject_dummy & mz$s1 != 0),"s1"]))
s_i <- log(abs(z[which(!mz$environment_var & mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
s_e <- log(abs(z[which(mz$environment_var & !mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
i_e <- log(abs(z[which(mz$environment_var & mz$item_vars & !mz$subject_dummy & mz$s1 != 0),"s1"]))
s_i_e <- log(abs(mz[which(mz$environment_var & mz$item_vars & mz$subject_dummy & mz$s1 != 0),"s1"]))
boxplot(s,i,e,s_i, s_e,i_e,  s_i_e, names=c("Subject","Items","Environment","Subject\nItems","Subject\nEnvironment","Items\nEnviornment","Subject+Items\nEnvironment"),col = c('#EECDCD','#FDF2D0','#D3E1F1', '#F8E6D0','#D8D2E7', '#DCE9D5', '#D9D9D9'), ylab="log(abs(coef)")
