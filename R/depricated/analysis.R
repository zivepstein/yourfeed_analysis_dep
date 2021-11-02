library(lme4)
library(lmerTest)
library(robustHD)
library(tidyverse)
library("readxl")
options("scipen"=100, "digits"=4)

#Compliance (fig 1)
yourfeed <- c(923, 778, 747, 745, 710, 671, 600, 577, 566, 554, 543, 538, 532, 522, 494, 486)
qualtrics <- c(936, 780, 755, 755, 716)
qualtrics_x <- c(1, 2, 3, 4, 16)
plot(1-yourfeed/923, ylim=c(0,.6), type="n", ylab= "Dropout Proportion", xlab = "")
lines(qualtrics_x,1-qualtrics/936,  col='#4772db')
lines(1-yourfeed/923,  col='#5c7e69')
points(qualtrics_x,1-qualtrics/936,  col='#4772db', pch=16)
points(1-yourfeed/923,  col='#5c7e69', pch=16)

#make data
ul_raw <- read_csv("/Users/ziv.e/Documents/github/yourfeed_analysis/data/yvq_user_level.csv")
pretest <- read_excel("/Users/ziv.e/github/yourfeed_analysis/data/20210129 pretest.xlsx")
long_raw <- read_csv("https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1")
item_level <- read_csv("https://www.dropbox.com/s/asrmhf3upvhy1fq/aj_headlines.csv?dl=1")
pretest = as.data.frame(pretest)
pretest[,"reputation_partyloyalty(140)"]$`reputation_partyloyalty(140)`
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
  pk = as.numeric((pk1)==3) + as.numeric((pk2)==1)  + as.numeric((pk3)==3) + as.numeric((pk4)==1) + as.numeric((pk5)==3)
) %>% filter(completed==1) %>% merge(item_level, all.x=T, by.x='item', by.y='index')

ul <- ul_raw %>% merge(
  long_raw %>% group_by(rid) %>% dplyr::summarise(
    completed = mean(completed),
    crt = mean(crt),
    is_mobile = mean(is_mobile)
  ), by='rid', all.x=T)

ul <- ul %>% mutate(
  drop=1-coalesce(completed,0),
  diglit = as.numeric(case_when(
    Q259 ==3~ T,
    Q261 ==3~T,
    T ~F
  ))
) %>% filter(last_question != '7 social media' )

uld <- long %>% group_by(rid, real) %>% summarize(
  y = mean(response),
  condition = mean(condition),
  crt = mean(crt),
  pk= mean(pk)
) %>% group_by(rid) %>% dplyr::summarize(
  d = y[2] / (y[1] + y[2]),
  condition = mean(condition),
  crt = mean(crt),
  pk= mean(pk)
)

#differential dropout
summary(lm(drop ~ yourfeed* (scale(crt) + scale(is_mobile) + scale(diglit)), data=ul))
mean(ul[ul$drop==0,]$age)
table(ul[ul$drop==0,]$ethnicity)

 #long level analysis
main <- readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/main.rds")
print(summary(main), digits=3)
#m1 <- lmer("response ~ condition*real+ (1 + condition | item) + (1 + real | rid)", data =long) ; print(summary(m1), digits=3)
#moderation analysis
#m2 <- lmer("response ~ condition*real*crt+ (1 | item) + (1 | rid)", data =long) ; print(summary(m2), digits=3)
#m3 <- lmer("response ~ condition*real*pk+ (1 | item) + (1 | rid)", data =long) ; print(summary(m3), digits=3)
crt_mod <- readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/crt_mod.rds")
print(summary(crt_mod), digits=3)
pk_mod <- readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/pk_mod.rds")
print(summary(pk_mod), digits=3)

#novel discernment measure
summary(lm(d ~ condition, data=uld))
summary(lm(d ~ condition*crt, data=uld))
summary(lm(d ~ condition*pk, data=uld))

s1.long <- uld
s.1dtamean = uld %>% group_by(condition) %>% summarise(meanval = mean(d, na.rm=T), se = 1.96*sd(d, na.rm=T)/sqrt(n()))
s.1dtamean$condition <- as.factor(s.1dtamean$condition)
s.1dtamean$selow = s.1dtamean$meanval - s.1dtamean$se
s.1dtamean$sehi = s.1dtamean$meanval + s.1dtamean$se
uld$condition <- as.factor(uld$condition)
# Create plot

ggplot()+
  geom_bar(data = s.1dtamean, aes(x = condition, fill=condition, y = meanval),position ='dodge', stat = 'identity', with = .5, colour = "black") +
  geom_errorbar(data = s.1dtamean, aes(x = condition, fill = condition,ymin = selow, ymax = sehi, width = .4),position = 'dodge') +
  theme_classic()+  # Make graph background blank+
 # facet_grid(. ~ condition) +#for horizontal faceting, use: facet_wrap(~measure) 
  geom_jitter(data = uld, aes(color = condition, x = condition, group = condition,y = d), alpha = .3)+
  scale_color_manual(values=c( "#4772db", "#5c7e69"))+
  labs(title="", x="", y = "Relative discernment") +
scale_fill_manual(name = '',values=c('#ffffff', '#ffffff'))+ #makes bars filled with white (#f0f0f0 for off-white), but also adds the unnecessary legend  
  theme(legend.position = "none") + #gets rid of legends
  theme(axis.text.x = element_text(colour = "black"))+
  scale_x_discrete(labels=c("Qualtrics", "Yourfeed")) +
  theme(panel.spacing = unit(1, "lines")) # spaces out facet panels



#dwell 
#1 - understand item features for engagement
#2 - understand dwell measure
#3 - condition plots
#4 - 2SLS?

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
  crt=scale(crt)
) %>% filter(order!=140)

to_plot<- yourfeed %>%filter(attend==1) %>% group_by(concordant,real) %>% dplyr::summarize(
  y = mean(response, na.rm=T),
  ce = 1.96*sd(response, na.rm=T) / sqrt(n())
)
BarPlot <- barplot(matrix(to_plot$y, 2,3), beside=TRUE,ylab="Engage | Attend",main = "", ylim = c(0,0.15), names.arg=c("Discordant" , "Neutral", "Concordant"), col=c("purple","green"))
error.bar(BarPlot,matrix(to_plot$y, 2,3),matrix(to_plot$ce, 2,3))

A1 <- brm(data = yourfeed,family = gaussian,
               formula = attend~crt*(real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + order*prekink)+
                 (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt+order*prekink| item) +
                 (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt + order*prekink| rid),
      prior = prior, iter = 5000, warmup = 1000, chains = 4, cores = 4, seed = 42, control = list(adapt_delta = 0.92), 
      file = "/Users/ziv.e/Documents/github/yourfeed_analysis/models/A1")

m5 <- lmer("attend1 ~ real*concordant+ (1+ concordant| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m5)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m5.rds")), digits=3)

m6 <- lmer("attend1 ~ real*concordant*crt+ (1+ concordant+crt| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m6)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m6.rds")), digits=3)

m7 <- lmer("attend1 ~ real*concordant*pk+ (1+ concordant+pk| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m7)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m7.rds")), digits=3)

m8 <- lmer("attend2 ~ real*concordant+ (1+ concordant| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m8)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m8.rds")), digits=3)

m9 <- lmer("attend2 ~ real*concordant*crt+ (1+ concordant+crt| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m9)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m9.rds")), digits=3)

m10 <- lmer("attend2 ~ real*concordant*pk+ (1+ concordant+pk| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m10)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m10.rds")), digits=3)

m11 <- lmer("attend3 ~ real*concordant+ (1+ concordant | item) + (1 + real + concordant | rid)", data =yofo) ; print(summary(m11), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m11.rds")), digits=3)

m12 <- lmer("attend3 ~ real*concordant*crt+ (1+ concordant+crt| item) + (1 + real + concordant | rid)", data =yourfeed); summary(m12)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m12.rds")), digits=3)

m13 <- lmer("attend3 ~ real*concordant*pk+ (1+ concordant+pk| item) + (1 + real + concordant | rid)", data =yofo); summary(m13)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m13.rds")), digits=3)


###extra cov
m5b <- lmer("attend1 ~ real+concordant+surprising+likely_true+ favors_r+ benefits_r +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share+ (1| item) + (1 | rid)", data =yourfeed); summary(m5b)
m5b <- lmer("response ~ real+concordant+surprising+likely_true+ favors_r+ benefits_r +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share+ (1| item) + (1 | rid)", data = yourfeed%>% filter(attend1==1)); summary(m5b)

#we observe marginal baseline discernment, but gets destroyed by concordancy, which is strong
m14 <- lmer("response ~ real*concordant+ (1+ concordant | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend1==1)) ; print(summary(m14), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m14.rds")), digits=3)

m15 <- lmer("response ~ real*concordant*pk+ (1+ concordant+pk | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend1==1)) ; print(summary(m15), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m15.rds")), digits=3)

m16 <- lmer("response ~ real*concordant*crt+ (1+ concordant+crt | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend1==1)) ; print(summary(m16), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m16.rds")), digits=3)

m17 <- lmer("response ~ real*concordant+ (1+ concordant | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend2==1)) ; print(summary(m17), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m17.rds")), digits=3)

m18 <- lmer("response ~ real*concordant*pk+ (1+ concordant+pk | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend2==1)) ; print(summary(m18), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m18.rds")), digits=3)

m19 <- lmer("response ~ real*concordant*crt+ (1+ concordant+crt | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend2==1)) ; print(summary(m19), digits=3)
print(summary(readRDS("/Users/zive/GDrive/research/yourfeed_analysis/models/m19.rds")), digits=3)

hist(yourfeed[yourfeed$response==0,]$wdwell, freq=F, col=rgb(0,0,0.8,0.5), xlab = "Dwell Time", main = "Dwell by engagement")
hist(yourfeed[yourfeed$response==1,]$wdwell, freq = F, add=T,col=rgb(0.8,0,0,0.5))
d1 <- density(log(yourfeed[yourfeed$response==0,]$wdwell))
d2 <- density(log(yourfeed[yourfeed$response==1,]$wdwell))
plot(d2, xlab=  "log(Dwell)", main = "Dwell by engagement")
polygon(d1, col=rgb(0,0,0.8,0.5))
polygon(d2, col=rgb(0.8,0,0,0.5))
legend("topleft", c("No share", "Share"), fill= c( col=rgb(0,0,0.8,0.5),rgb(0.8,0,0,0.5)))
compute_threshold(d1,d2)



m6 <- lmer("response ~ real*concordant+ (1 | item) + (1 + real + concordant | rid)", data =yourfeed %>% filter(attend1==1)) ; print(summary(m6), digits=3)


d1 <- density(log(yourfeed[(yourfeed$response==0)& (yourfeed$rid ==yourfeed$rid[7]),]$wdwell))
#d2 <- density(log(yourfeed[(yourfeed$response==1)& (yourfeed$rid ==yourfeed$rid[4]),]$wdwell))
plot(d1, xlab=  "log(Dwell)", main = "Dwell by engagement")
polygon(d1, col=rgb(0,0,0.8,0.5))
polygon(d2, col=rgb(0.8,0,0,0.5))

ul <- yourfeed %>% group_by(rid, response) %>% dplyr::summarise(
  n_share = sum(response),
  avg_dwell = mean(dwell)
) %>% group_by(rid) %>% dplyr::summarise(
  dwell_coverage =avg_dwell[1]*avg_dwell[2]>0,
); ul
compute_threshold <- function(d1,d2){
  for (i in 1:length(d1$x)){
    if ((d2$y[i] - d1$y[i]) > 0){
      return(exp(d2$x[i]))
    } 
  }
  return(-1)#1330
}
engagers =ul%>% filter(dwell_coverage)
thresh <- c()
for (rid in engagers$rid){
  if (length(log(yourfeed[(yourfeed$response==1)& (yourfeed$rid ==rid),]$wdwell))>1){
    d1 <- density( log(exp(-1) +yourfeed[(yourfeed$response==0)& (yourfeed$rid ==rid),]$wdwell))
    d2 <- density( log(exp(-1) +yourfeed[(yourfeed$response==1)& (yourfeed$rid ==rid),]$wdwell))
    thresh <- c(thresh, compute_threshold(d1,d2))
  } else{
    thresh <- c(thresh, -1)
  }
}
thresh2 <- c()
for (rid in engagers$rid){
  if (length(log(yourfeed[(yourfeed$response==1)& (yourfeed$rid ==rid),]$wdwell))>0){
    d1 <- density( log(exp(-1) +yourfeed[(yourfeed$response==0)& (yourfeed$rid ==rid),]$wdwell))
    m <- min(yourfeed[(yourfeed$response==1)& (yourfeed$rid ==rid),]$wdwell)
    thresh2 <- c(thresh2,m)
  } else{
    thresh2 <- c(thresh2, -1)
  }
}
engagers$threshi <- thresh
engagers$threshi2 <- thresh2
engagers$is_engager <- 1

yourfeed <- yourfeed %>% merge(engagers, all.x=T, by='rid') %>% mutate(
  attend2 = as.numeric(dwell >= threshi2)
)
d4 <- density(log(engagers$threshi2[engagers$threshi2>0]))
plot(d4, xlab = "log(Threshold)", main="User level fixed threshold")
polygon(d4, col=rgb(0,0.8,0,0.5))

yf = yourfeed %>% merge(yofo[c("rid", 'item', 'attend3')], by=c('rid','item'))

########extra headline level covariates
inf_mean <- function(x){
  r <- x[!is.infinite(x)]
  return(mean(r))
}

order <- yourfeed  %>%group_by(order) %>% dplyr::summarize(
  y1 = mean(attend, na.rm=T), 
  ci1 = mean_error(attend),
  y2 = mean(wdwell, na.rm=T),
  ci2 = mean_error(wdwell),
  y3 = inf_mean(ldwell),
  ci3 = mean_error(ldwell)
)
p <- plot(order$order, order$y3, xlim=c(0,140),ylim=c(4,9), pch=21, bg='yellow', xlab = "Item order", ylab="log(Dwell)")
arrows(x0=order$order,y0=order$y3-order$ci3, y1=order$y3+order$ci3, angle=90, code=3, length=0.05)
points(order$order, order$y3, bg='yellow', pch=21)
abline(v=6, lty=2)

# arrows(x0=order$order,y0=(order$y2-order$ci2)/max(order$y2), y1=(order$y2+order$ci2)/max(order$y2), angle=90, code=3, length=0.05)
# points(order$order, order$y2/max(order$y2), bg='green', pch=21)
# 
# arrows(x0=order$order,y0=(order$y3-order$ci3)/max(order$y3), y1=(order$y3+order$ci3)/max(order$y3), angle=90, code=3, length=0.05)
# points(order$order, order$y3/max(order$y3), bg='skyblue', pch=21)
legend( x="topright", 
        legend=c("A1: Likelihood of attending", "A2: Fraction of max windsorized dwell (max=8.22s)","A3: Fraction of max log(1+dwell) (max=8.29)"),
        col=c("yellow","green", "skyblue"), 
        pch=16 )

order2 <- yourfeed  %>%group_by(order, concordant) %>% dplyr::summarize(
  y = inf_mean(ldwell),
  ci = mean_error(ldwell)
)
p <- plot(order2[order2$concordant==1,]$order, order2[order2$concordant==1,]$y, xlim=c(0,140),ylim=c(4,9), pch=21, bg='green', xlab = "Item order", ylab="log(Dwell)")
arrows(x0=order2[order2$concordant==1,]$order,y0=order2[order2$concordant==1,]$y-order2[order2$concordant==1,]$ci, y1=order2[order2$concordant==1,]$y+order2[order2$concordant==1,]$ci, angle=90, code=3, length=0.05)
points(order2[order2$concordant==1,]$order, order2[order2$concordant==1,]$y, bg='green', pch=21)

arrows(x0=order2[order2$concordant==0,]$order,y0=order2[order2$concordant==0,]$y-order2[order2$concordant==0,]$ci, y1=order2[order2$concordant==0,]$y+order2[order2$concordant==0,]$ci, angle=90, code=3, length=0.05)
points(order2[order2$concordant==0,]$order, order2[order2$concordant==0,]$y, bg='purple', pch=21)
