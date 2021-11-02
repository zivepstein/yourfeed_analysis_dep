library(lme4)
library(lmerTest)
library(tidyverse)
library("readxl")
options("scipen"=100, "digits"=4)

pretest <- read_excel("/Users/ziv.e/github/yourfeed_analysis/data/20210129 pretest.xlsx")
long_raw <- read_csv("https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1")
#item_level <- read_csv("https://www.dropbox.com/s/asrmhf3upvhy1fq/aj_headlines.csv?dl=1")
item_level <-read_csv("/Users/ziv.e/github/yourfeed_analysis/data/yourfeed_item_level.csv")

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

plot(yourfeed$average_A, yourfeed$std_A)

vshare <- yourfeed %>% mutate(
  dwelltile = ntile(ldwell, n=10)  
) %>% group_by(dwelltile, real) %>% summarize(
  share = mean(response),
  share_dev = 1.96*sd(response) / sqrt(n()),
  a_dwell = mean(ldwell),
)
plot(vshare[vshare$real==0.5,]$a_dwell, vshare[vshare$real==0.5,]$share,col='green', pch=16, xlab = "log(Dwell)", ylab = "Fraction Shared", ylim = c(0,0.2))
arrows(vshare[vshare$real==0.5,]$a_dwell, 
       vshare[vshare$real==0.5,]$share - vshare[vshare$real==0.5,]$share_dev,
       vshare[vshare$real==0.5,]$a_dwell,
       vshare[vshare$real==0.5,]$share + vshare[vshare$real==-0.5,]$share_dev, length=0.05, angle=90, code=3)
arrows(vshare[vshare$real==-0.5,]$a_dwell, 
       vshare[vshare$real==-0.5,]$share - vshare[vshare$real==0.5,]$share_dev,
       vshare[vshare$real==-0.5,]$a_dwell,
       vshare[vshare$real==-0.5,]$share + vshare[vshare$real==-0.5,]$share_dev, length=0.05, angle=90, code=3)
points(vshare[vshare$real==-0.5,]$a_dwell,vshare[vshare$real==-0.5,]$share, col='red', pch=16)
lines(vshare[vshare$real==-0.5,]$a_dwell,vshare[vshare$real==-0.5,]$share, col='red', lwd=2)
lines(vshare[vshare$real==0.5,]$a_dwell,vshare[vshare$real==0.5,]$share, col='green', lwd=2)
legend("topleft", c("Real", "Fake"), fill= c( col="green","red"))


unique(yourfeed%>% select(rid)) %>% count()
unique(yourfeed%>% filter(average_A3!=0) %>% select(rid)) %>% count()
unique(yourfeed%>% filter(average_A3>log(50)) %>% select(rid)) %>% count()

d1 <- density(yourfeed[yourfeed$response==0,]$ldwell)
d2 <- density(yourfeed[yourfeed$response==1,]$ldwell)
d3 <- density(yourfeed$ldwell)
plot(d2, xlab=  "log(Dwell)", main = "Dwell by engagement")
polygon(d1, col=rgb(0,0,0.8,0.5))
polygon(d2, col=rgb(0.8,0,0,0.5))
legend("topleft", c("No share", "Share"), fill= c( col=rgb(0,0,0.8,0.5),rgb(0.8,0,0,0.5)))
abline(v=log(50), lty=3)
compute_threshold(d1,d2)

overall <- lm("ldwell ~ crt*(real+concordant+
            surprising+likely_true+ favors_party+ benefits_party +important+ funny+ 
            reputation_overall +reputation_partyloyalty+ reputation_engage+likely_share+
           order*prekink)", data =yourfeed); summary(overall)
options(max.print=5000)

summary(lm("response ~ order*prekink + scale(sad)",  data =yourfeed %>% filter(average_A>log(50))))
summary(lm("personal_response ~ order*prekink + scale(anx)",  data =yourfeed %>% filter(average_A>log(50))))
a
yourfeed
#mediating moderation
m5b <- lm("response ~ real*scale(crt) ", data =yourfeed%>% filter(average_A>log(50))); print(summary(m5b), digits=1)
m5b <- lm("response ~ real*scale(crt)+real*ldwell ", data =yourfeed%>% filter(average_A>log(50))); print(summary(m5b), digits=1)

restrict <- function(covar){
  print(coef(summary(lm(paste("ldwell ~ order*prekink + scale(", covar,")"),  data =yourfeed %>% filter(average_A>log(50)))))[4,])
  print(coef(summary(lm(paste("personal_ldwell ~ order*prekink + scale(", covar,")"),  data =yourfeed %>% filter(average_A>log(50)))))[4,])
}

restrict_share <- function(covar){
  print(coef(summary(lm(paste("response ~ order*prekink + scale(", covar,")"),  data =yourfeed %>% filter(average_A>log(50)))))[4,], digits=4)
  print(coef(summary(lm(paste("personal_response ~ order*prekink + scale(", covar,")"),  data =yourfeed %>% filter(average_A>log(50)))))[4,], digits=4)
}
covs <- c("crt","real","likely_true" ,"favors_party","benefits_party","important","funny","reputation_overall","reputation_partyloyalty","reputation_engage","likely_share", "num_person", "biden", "trump", "valence", "arousal", "posemo", "anger", "negemo", "sad")


summary(lm("ldwell ~ order*prekink + scale(concordant) + scale(discordant)",  data =yourfeed %>% filter(average_A>log(50))))
summary(lm("personal_ldwell ~ order*prekink + scale(concordant) + scale(discordant)",  data =yourfeed %>% filter(average_A>log(50))))

summary(lm("ldwell ~ order*prekink + scale(concordant) ",  data =yourfeed %>% filter(((concordant==1) | (discordant==1)) & average_A > log(50))))
summary(lm("personal_ldwell ~ order*prekink + scale(concordant) ",  data =yourfeed %>% filter(((concordant==1) | (discordant==1)) & average_A > log(50))))
for (cov in covs){
  print(cov)
  restrict(cov)
}

summary(lm("response ~ order*prekink + scale(concordant) + scale(discordant)",  data =yourfeed %>% filter(average_A>log(50))))
summary(lm("personal_response ~ order*prekink + scale(concordant) + scale(discordant)",  data =yourfeed %>% filter(average_A>log(50))))
for (cov in covs){
  print(cov)
  restrict_share(cov)
}

summary(lm("personal_ldwell ~ order*prekink + scale(real)*scale(attention)",  data =yourfeed))

incondi <- function(covar){
  print(coef(summary(lm(paste("response ~ order*prekink + scale(", covar,")"),  data =yourfeed %>% filter(ldwell > 7.2))))[4,])
}

for (cov in covs){
  print(cov)
  condi(cov)
}

print(coef(summary(lm("response ~ order*prekink + scale(concordant) + scale(discordant)",  data =yourfeed%>% filter(ldwell > 7.2)))))
m5b <- lm("ldwell ~ order*prekink + scale(concordant) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("ldwell ~ order*prekink + scale(concordant) ", data =yourfeed %>% filter(demrep_c>3)); summary(m5b)

m5b <- lm("personal_ldwell ~ order*prekink + scale(concordant) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("personal_ldwell ~ order*prekink + scale(concordant) ", data =yourfeed %>% filter(demrep_c>3)); summary(m5b)

m5b <- lm("ldwell ~ order*prekink + scale(has_person) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("ldwell ~ order*prekink + scale(biden) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("ldwell ~ order*prekink + scale(trump) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("ldwell ~ order*prekink + scale(valence) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("ldwell ~ order*prekink + scale(arousal) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("personal_ldwell ~ order*prekink + scale(has_person) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("personal_ldwell ~ order*prekink + scale(biden) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("personal_ldwell ~ order*prekink + scale(trump) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("personal_ldwell ~ order*prekink + scale(valence) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)
m5b <- lm("personal_ldwell ~ order*prekink + scale(arousal) ", data =yourfeed %>% filter(demrep_c<=3)); summary(m5b)


m5b <- lm("response ~ real*scale(crt)", data =yourfeed); summary(m5b)
m5bc <- lm("response ~real*ldwell", data =yourfeed); summary(m5b)
m5b <- lm("response ~ real*scale(crt) + real*ldwell", data =yourfeed); summary(m5b)
library(car)

linearHypothesis(m5bc, "5*real:ldwell+real=0") #for the people who dont dwell at all, they are more likely to share false than true, and for people who drwell >5, they are more likely to share tur
#round ldwell to 0.5 and psharetrue/false 

#prediction task
#"crt","real","concordant","discordant","likely_true" ,"favors_party","benefits_party","important","funny","reputation_overall","reputation_partyloyalty","reputation_engage","likely_share")
library(randomForest)
#test train split - within person within item
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

#+train_item_ldwell+train_item_ldwell_var+train_subject_ldwell+train_subject_ldwell_var+
rf_classifier = randomForest(ldwell ~ order+prekink+
                               crt+education+media1+media2+attention+
                               concordant+discordant+
                               funny+likely_true+favors_party+important+reputation_overall+reputation_partyloyalty,
                             data=yourfit %>% filter(is_train), ntree=50, mtry=5, importance=TRUE)
p <- predict(rf_classifier, yourfit%>% filter(!is_train))
plot(p,yourfit[!yourfit$is_train,"ldwell"])
cor(p,yourfit[!yourfit$is_train,"ldwell"])
importance(rf_classifier)
varImpPlot(rf_classifier)
#0.5542 ntree100 mtry 2
#0.6025 ntree100 mtry 5
#.6193 ntree100 mtry 8
#0.6218 ntree200 mtry 8
#0.6218 ntree50 mtry 5

library(ROCR)
#overall sharng
yourfit$response <- as.factor(yourfit$response)
rf_classifier = randomForest(response ~ order+prekink+
                               crt+education+media1+media2+attention+crt*real+
                               concordant+discordant+
                               train_item_response+train_item_response_var+train_subject_response+train_subject_response_var+
                               funny+likely_true+favors_party+important+reputation_overall+reputation_partyloyalty,
                             data=yourfit %>% filter(is_train), ntree=50, mtry=5, importance=TRUE)
p <- predict(rf_classifier, yourfit%>% filter(!is_train), type="prob")
pred <- prediction(predictions = p[,2], labels=yourfit[!yourfit$is_train,"response"])
auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
perf <- performance(pred,"tpr","fpr")
plot(perf)
importance(rf_classifier)
varImpPlot(rf_classifier)

rf_classifier = randomForest(response ~ order+prekink+
                               crt+education+media1+media2+attention+
                               concordant+discordant+
                               funny+likely_true+likely_share+favors_party+important+reputation_overall+reputation_partyloyalty+
                               train_item_response+train_item_response_var+train_subject_response+train_subject_response_var,
                             data=yourfit %>% filter(is_train), ntree=50, mtry=5, importance=TRUE)
p <- predict(rf_classifier, yourfit%>% filter(!is_train), type="prob")
pred <- prediction(predictions = p[,2], labels=yourfit[!yourfit$is_train,"response"])
auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
perf <- performance(pred,"tpr","fpr")
plot(perf)
importance(rf_classifier)
varImpPlot(rf_classifier)
