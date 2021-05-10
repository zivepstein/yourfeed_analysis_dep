library(lme4)
library(lmerTest)
options("scipen"=100, "digits"=4)

#Compliance (fig 1)
yourfeed <- c(923, 778, 747, 745, 710, 671, 600, 577, 566, 554, 543, 538, 532, 522, 494, 486)
qualtrics <- c(936, 780, 755, 716, 716)
qualtrics_x <- c(1, 2, 3, 4, 16)
plot(1-yourfeed/923, ylim=c(0,.6), type="n", ylab= "Dropout Proportion", xlab = "")
lines(qualtrics_x,1-qualtrics/936,  col='#4772db')
lines(1-yourfeed/923,  col='#5c7e69')
points(qualtrics_x,1-qualtrics/936,  col='#4772db', pch=16)
points(1-yourfeed/923,  col='#5c7e69', pch=16)

#make data
ul_raw <- read_csv("~/GDrive/research/yourfeed_analysis/data/yvq_user_level.csv")
long_raw <- read_csv("~/GDrive/research/yourfeed_analysis/data/soft_long.csv")
item_level <- read_csv("~/GDrive/research/yourfeed_analysis/data/aj_headlines.csv")
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
summary(lm(drop ~ yourfeed* (crt + is_mobile + diglit), data=ul))


#long level analysis
m1 <- lmer("response ~ condition*real+ (1 + condition | item) + (1 + real | rid)", data =long) ; print(summary(m1), digits=3)
#moderation analysis
m2 <- lmer("response ~ condition*real*crt+ (1 | item) + (1 | rid)", data =long) ; print(summary(m2), digits=3)
m3 <- lmer("response ~ condition*real*pk+ (1 | item) + (1 | rid)", data =long) ; print(summary(m3), digits=3)

#novel discernment measure
summary(lm(d ~ condition, data=uld))
summary(lm(d ~ condition*crt, data=uld))
summary(lm(d ~ condition*pk, data=uld))
