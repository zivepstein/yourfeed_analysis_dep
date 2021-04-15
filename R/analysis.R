library(dplyr)
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
p <- plot(order[order$condition==0,]$order, order[order$condition==0,]$y, xlim=c(0,140), pch=21, bg='yellow', xlab = "Item order", ylab="Percent share")
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
  has_dwell = sum(1-is.na(dwell)),
  completed = mean(completed),
  condition = max(condition),
  experiment_id = max(experiment_id)
)# %>% group_by(experiment_id, condition) %>% dplyr::summarize(n=mean(d))
table(x$condition, x$completed, x$has_dwell)
table(x$completed, x$y>0)
y <- x %>% filter(experiment_id == 6670 & condition ==0) %>% mutate(did_share = y>0)
table(y$did_share, y$has_dwell, y$completed)

y%>% filter(!did_share & completed==0)

y70 <- x %>% filter(experiment_id == 6667 & condition==1)
table(y70$completed, y70$y>0)
