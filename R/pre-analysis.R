library(tidyverse)
mean_error <- function(data){
  n <- sum(!is.na(data))
  s <- sd(data,na.rm = T)
  return(1.96*s/sqrt(n))
}
long <- read_csv("~/GDrive/research/yourfeed_analysis/data/soft_long.csv")


#preprocessing (to pipe into python eventually)
long$veracity <- as.numeric(long$item > 70)
long$crt <- (as.numeric(long$crt_ages == 4) + as.numeric(long$crt_printer == 10) + as.numeric(long$crt_bread == 39) + as.numeric(long$crt_race == 2) + as.numeric(long$crt_sheep == 8))/5


order <- long %>% group_by(condition,order) %>% dplyr::summarize(y = mean(response), ci = mean_error(response))
p <- plot(order[order$condition==0,]$order, order[order$condition==0,]$y, xlim=c(0,140),ylim=c(0,0.6), pch=21, bg='yellow', xlab = "Item order", ylab="Percent share")
arrows(x0=order[order$condition==0,]$order,y0=order[order$condition==0,]$y-order[order$condition==0,]$ci, y1=order[order$condition==0,]$y+order[order$condition==0,]$ci, angle=90, code=3, length=0.05)
points(order[order$condition==0,]$order, order[order$condition==0,]$y, bg='yellow', pch=21)
arrows(x0=order[order$condition==1,]$order,y0=order[order$condition==1,]$y-order[order$condition==1,]$ci, y1=order[order$condition==1,]$y+order[order$condition==1,]$ci, angle=90, code=3, length=0.05)
points(order[order$condition==1,]$order, order[order$condition==1,]$y, bg='green', pch=21)

ds <- long %>% filter(condition==1) %>% mutate(
  has_dwell = 1-as.numeric(is.na(dwell))
) %>% group_by(rid) %>% dplyr::summarize(
  s = sum(has_dwell)
  )
table(ds$s) #66% the dwell thing doesnt work.... 

debug <- long %>% filter(condition==1 & is.na(dwell))
db <- debug %>% group_by(rid) %>% summarize(y=mean(response))
write.csv(db, "~/GDrive/research/yourfeed_analysis/data/debug")
long %>% group_by(rid) %>% summarize(y=mean(response))%>% select(y) %>% summary

d <- long %>% filter(condition ==1 & !is.na(dwell)) %>% filter(dwell <quantile(dwell,0.95) )
plot(d$response,d$dwell)
d %>% filter(response==1) %>% select(dwell) 
mean(d[d$response==1,]$dwell)
mean(d[d$response==0,]$dwell)

lmo <- lmer("response ~ veracity*condition + (1 | rid) + (1|item)",data=long); summary(lmo)
lmo <- lmer("response ~ veracity*condition*crt + (1 | rid) + (1|item)",data=long); summary(lmo)

bar<- long %>% group_by(condition, veracity) %>% dplyr::summarize(
  y = mean(response),
  ci=mean_error(response))
BarPlot <- barplot(t(matrix(bar$y,2 , 2, byrow=TRUE)), beside=TRUE, 
                   ylab="Sharing intentions", 
                   xlab="Condition", 
                   main="Sharing by condition", 
                   ylim=c(0, 1), 
                   names.arg=c("Qualtrics", "YourFeed"), 
                   col=colours)
arrows(BarPlot, t(matrix(bar$y,2 , 2, byrow=TRUE))+t(matrix(bar$ci,2 , 2, byrow=TRUE)), BarPlot, t(matrix(bar$y,2 , 2, byrow=TRUE))-t(matrix(bar$ci,2 , 2, byrow=TRUE)), angle=90, code=3, length=0.05)

ds


peeps <- long %>% group_by(rid) %>% dplyr::summarise(
  has_dwell = 1-is.na(sum(dwell))
)
table(peeps$condition, peeps$has_dwell)

sth <- long %>% merge(peeps, by ='rid') %>% group_by(rid) %>% dplyr::summarise(
  crt = max(crt),
  has_dwell = max(has_dwell),
  conditon = max(condition)
) %>% mutate(
  cohort = condition
)
table(sth$cohort)
ggplot(sth,aes(x=crt,group=cohort,fill=cohort))+
  geom_histogram(position="dodge",binwidth=0.25, stat="count")+theme_bw() +labs(x="Were any of these headlines tagged with a warning message?")


x <- long %>% group_by(rid) %>% dplyr::summarise(
  n= n(),
  y=mean(response),
  has_dwell = sum((dwell!=-1)),
  completed = mean(completed),
  finished_qualtrics = mean(is.na(debug_url)),
  condition = max(condition),
  experiment_id = max(experiment_id)
)# %>% group_by(experiment_id, condition) %>% dplyr::summarize(n=mean(d))
table(x$condition, x$completed==1)
table(x$condition, x$finished_qualtrics)
table(x$condition, x$completed==1, x$has_dwell)
table(x$completed, x$y>0)
y <- x %>% filter(condition==0) %>% mutate(
  did_share = y>0
  ) 
table( y$completed, y$experiment_id)

table(y$completed, y$did_share)

write.csv(y$rid[!y$completed], "~/GDrive/research/yourfeed_analysis/data/badboyz.csv")

y%>% filter(!did_share & completed==0)

y70 <- x %>% filter(experiment_id == 6667 & condition==1)
table(y70$completed, y70$y>0)


hist(log(long[(long$experiment_id==6674) & (long$condition==1)& (long$dwell!=-1),]$dwell))

x <- c()
for (i in 1:10000){
  x<- c(x,sum(rbinom(n=122, size=1, prob=0.5)))
}
2*sum(x>=77)/10000

difdrop <- matrix(c(27/66,49/69-27/66,1-49/69,
                    42/71,58/82-42/71,1-58/82,
                    31/69,51/81-31/69,1-51/81,
                    29/57,44/57-29/57,1-44/57,
                    40/77,37/45-40/77,1-37/45),nrow=3 )
barplot(difdrop, names.arg = c("MB 3", "MB 4", "MB 6", "MB8","MB10"), col=c("forestgreen","skyblue","white"))

table(x[!x$completed & x$condition==1,]$experiment_id)

noncompliers <- long %>% filter(condition==1 & !completed) %>% group_by(rid) %>% dplyr::summarize(
  crt= mean(crt),
  is_mobile= mean(is_mobile),
  age= mean(age),
  demrep_c= mean(demrep_c),
  time= mean(`Duration (in seconds)`)
)

compliers <- long %>% filter(condition==1 &completed) %>% group_by(rid) %>% dplyr::summarize(
  crt= mean(crt),
  is_mobile= mean(is_mobile),
  age= mean(age),
  demrep_c= mean(demrep_c),
  time= mean(`Duration (in seconds)`)
)
par(mfrow=c(3,2))
barplot(difdrop, names.arg = c("MB 3", "MB 4", "MB 6", "MB8","MB10"), col=c("forestgreen","skyblue","white"))
hist(noncompliers$crt,freq=F, col=adjustcolor("red", alpha.f = 0.2), xlab = "CRT", main = " ")
hist(compliers$crt,add=T,freq=F, col=adjustcolor("blue", alpha.f = 0.2))
abline(v=mean(noncompliers$crt, na.rm=T), col='red', lwd=2)
abline(v=mean(compliers$crt, na.rm=T), col='blue', lwd=2)

hist(noncompliers$is_mobile,freq=F, col=adjustcolor("red", alpha.f = 0.2), xlab = "is_mobile", main = " ")
hist(compliers$is_mobile,add=T,freq=F, col=adjustcolor("blue", alpha.f = 0.2))
abline(v=mean(noncompliers$is_mobile, na.rm=T), col='red', lwd=2)
abline(v=mean(compliers$is_mobile, na.rm=T), col='blue', lwd=2)

hist(compliers$age,freq=F, col=adjustcolor("blue", alpha.f = 0.2), xlab = "age", main = " ")
hist(noncompliers$age,add=T,freq=F, col=adjustcolor("red", alpha.f = 0.2))
abline(v=mean(noncompliers$age, na.rm=T), col='red', lwd=2)
abline(v=mean(compliers$age, na.rm=T), col='blue', lwd=2)

hist(compliers$demrep_c,freq=F, col=adjustcolor("blue", alpha.f = 0.2), xlab = "demrep_c", main = " ")
hist(noncompliers$demrep_c,add=T,freq=F, col=adjustcolor("red", alpha.f = 0.2))
abline(v=mean(noncompliers$demrep_c, na.rm=T), col='red', lwd=2)
abline(v=mean(compliers$demrep_c, na.rm=T), col='blue', lwd=2)

hist(log(compliers$time),freq=F, col=adjustcolor("blue", alpha.f = 0.2), xlab = "log(time in qualtrics)", main = " ")
hist(log(noncompliers$time),add=T,freq=F, col=adjustcolor("red", alpha.f = 0.2))
abline(v=mean(log(noncompliers$time), na.rm=T), col='red', lwd=2)
abline(v=mean(log(compliers$time), na.rm=T), col='blue', lwd=2)

