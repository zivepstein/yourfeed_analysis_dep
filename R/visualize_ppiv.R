engage_ppiv <- read.csv("../data/generated/engage_ppiv.csv")
ep_long <- gather(engage_ppiv,  'model', 'pred', c('V1',"V2", "V3", "V4", "V5", "V6", "V7"),na.rm = T)
ep_long$condition2 <- ep_long$model
ep_long$variable <- ep_long$model
ep_long$value <- ep_long$pred
ep_long$measure <- factor(ep_long$model,  levels = c('V1',"V2", "V3", "V4", "V5", "V6", "V7"), labels = c("Subject","Items","Enviornment","Subject\nItems", "Subject\nEnviornment", "Items\nEnvironment", "Subject+Items\nEnviornmet"))
ep_long$target <-factor(ep_long$model,  levels = c('V1',"V2", "V3", "V4", "V5", "V6", "V7"), labels = c("Subject","Items","Enviornment","Subject\nItems", "Subject\nEnviornment", "Items\nEnvironment", "Subject+Items\nEnviornmet"))
ep_long <- ep_long %>% mutate(
  model_color =case_when(
    model == "V1" ~"red",
    model == "V2" ~"yellow",
    model == "V3" ~"blue",
    model == "V4" ~"orange",
    model == "V5" ~"purple",
    model == "V6" ~"green",
    model == "V7" ~"gray"
  )
)
ep_grouped = ep_long %>% group_by(target) %>% summarise(meanval = median(value), sehi = quantile(value, 0.975),  selow = quantile(value, 0.0275))
# Create plot

ggplot()+
  geom_bar(data = ep_grouped, aes(x = target, fill = target, y = meanval),position ='dodge', stat = 'identity', width = .5, colour = "black")+
  geom_errorbar(data = ep_grouped, aes(x = target, fill = target,ymin = selow, ymax = sehi, width = .4),position = 'dodge')+ 
  theme_classic() + # Make graph background blank
  geom_jitter(data = ep_long, aes(color = model_color, x = target, group = target,y = value), alpha = .3)+
  scale_color_identity()+
  labs(title="", x="", y = "AUC") + 
  scale_fill_manual(name = '',values=c('#ffffff', '#ffffff',"#ffffff","#ffffff","#ffffff","#ffffff","#ffffff"))+ #makes bars filled with white (#f0f0f0 for off-white), but also adds the unnecessary legend
  theme(legend.position = "none") + #gets rid of legends
  theme(axis.text.x = element_text(colour = "black"))+
  theme(panel.spacing = unit(1, "lines"))+ 
  coord_cartesian(ylim=c(0.5,0.85))

dwell_ppiv <- read.csv("../data/generated/dwell_ppiv.csv")
dp_long <- gather(dwell_ppiv,  'model', 'pred', c('V1',"V2", "V3", "V4", "V5", "V6", "V7"),na.rm = T)
s1.long <- dp_long
s1.long$condition2 <- s1.long$model
s1.long$variable <- s1.long$model
s1.long$value <- s1.long$pred
s1.long$measure <- factor(s1.long$model,  levels = c('V1',"V2", "V3", "V4", "V5", "V6", "V7"), labels = c("Subject","Items","Enviornment","Subject\nItems", "Subject\nEnviornment", "Items\nEnvironment", "Subject+Items\nEnviornmet"))
s1.long$target <-factor(s1.long$model,  levels = c('V1',"V2", "V3", "V4", "V5", "V6", "V7"), labels = c("Subject","Items","Enviornment","Subject\nItems", "Subject\nEnviornment", "Items\nEnvironment", "Subject+Items\nEnviornmet"))
s1.long <- s1.long %>% mutate(
  model_color =case_when(
    model == "V1" ~"red",
    model == "V2" ~"yellow",
    model == "V3" ~"blue",
    model == "V4" ~"orange",
    model == "V5" ~"purple",
    model == "V6" ~"green",
    model == "V7" ~"gray"
  )
)
s.1dtamean = s1.long %>% group_by(target) %>% summarise(meanval = median(value), sehi = quantile(value, 0.975),  selow = quantile(value, 0.0275))
# Create plot

ggplot()+
  geom_bar(data = s.1dtamean, aes(x = target, fill = target, y = meanval),position ='dodge', stat = 'identity', width = .5, colour = "black")+
  geom_errorbar(data = s.1dtamean, aes(x = target, fill = target,ymin = selow, ymax = sehi, width = .4),position = 'dodge')+ 
  theme_classic() + # Make graph background blank
  geom_jitter(data = s1.long, aes(color = model_color, x = target, group = target,y = value), alpha = .3)+
  scale_color_identity()+
  labs(title="", x="", y = "Correlation") + 
  scale_fill_manual(name = '',values=c('#ffffff', '#ffffff',"#ffffff","#ffffff","#ffffff","#ffffff","#ffffff"))+ #makes bars filled with white (#f0f0f0 for off-white), but also adds the unnecessary legend
  theme(legend.position = "none") + #gets rid of legends
  theme(axis.text.x = element_text(colour = "black"))+
  theme(panel.spacing = unit(1, "lines"))

pdf("/Users/zive/GDrive/research/ai_ethics/fig1.pdf",height = 4, width = 15)
print(plot.study1)
dev.off() 

imp_engage <- read.csv("../data/generated/imp_engage.csv")
user_vars = c("crt","education",'media1','media2','attention', 'age', "demrep_c", "gender", "income","facebook_frequency" )
item_vars = c('funny','real','likely_true','favors_party','important','reputation_overall','reputation_partyloyalty', "nchar", "affect", 'num_person', 'trump', 'biden', "deepmoji1", "deepmoji2", "deepmoji3")
environment_vars = c('order','prekink', 'is_mobile')
vars <- c(user_vars,item_vars, environment_vars)
ie_long <- gather(imp_engage,  'feature', 'imp', vars,na.rm = T)
ie_long$condition2 <- ie_long$feature
ie_long$variable <- ie_long$feature
ie_long$value <- ie_long$imp
ie_long$measure <- factor(ie_long$feature)
ie_long <- ie_long %>% mutate(
  model_color =case_when(
    feature %in% user_vars ~"red",
    feature %in% item_vars ~"yellow",
    feature %in% environment_vars ~"blue",
  )
)
ie_grouped = ie_long %>% group_by(measure) %>% summarise(meanval = median(value), sehi = quantile(value, 0.975),  selow = quantile(value, 0.0275)) %>% arrange(desc(meanval) ) 
ie_grouped$position <- as.factor(1:28)
# Create plot
ggplot()+
  geom_errorbar(data = ie_grouped, aes(x = reorder(measure, meanval), fill = measure,ymin = selow, ymax = sehi, width = .4),position = 'dodge')+ 
  theme_classic() + # Make graph background blank
  geom_jitter(data = ie_long, aes(color = model_color, x = measure, group = measure,y = value), alpha = .3)+
  scale_color_identity()+
  labs(title="", x="", y = "% Increase MSE") + 
  scale_fill_manual(name = '',values=rep('#ffffff',28))+ #makes bars filled with white (#f0f0f0 for off-white), but also adds the unnecessary legend
  theme(legend.position = "none") + #gets rid of legends
  theme(axis.text.x = element_text(colour = "black"))+
  theme(panel.spacing = unit(1, "lines")) + 
  coord_flip()

imp_dwell <- read.csv("../data/generated/imp_dwell.csv")
id_long <- gather(imp_dwell,  'feature', 'imp', vars,na.rm = T)
id_long$condition2 <- id_long$feature
id_long$variable <- id_long$feature
id_long$value <- id_long$imp
id_long$measure <- factor(id_long$feature)
id_long <- id_long %>% mutate(
  model_color =case_when(
    feature %in% user_vars ~"red",
    feature %in% item_vars ~"yellow",
    feature %in% environment_vars ~"blue",
  )
)
id_grouped = id_long %>% group_by(measure) %>% summarise(meanval = median(value), sehi = quantile(value, 0.975),  selow = quantile(value, 0.0275)) %>% arrange(desc(meanval) ) 
id_grouped$position <- as.factor(1:28)
id_grouped <- merge(id_grouped, ie_grouped[c("measure", 'meanval')], by ='measure')

# Create plot
ggplot()+
  geom_errorbar(data = id_grouped, aes(x = reorder(measure, meanval.y), fill = measure,ymin = selow, ymax = sehi, width = .4),position = 'dodge')+ 
  theme_classic() + # Make graph background blank
  geom_jitter(data = id_long, aes(color = model_color, x = measure, group = measure,y = value), alpha = .3)+
  scale_color_identity()+
  labs(title="", x="", y = "% Increase MSE") + 
  scale_fill_manual(name = '',values=rep('#ffffff',28))+ #makes bars filled with white (#f0f0f0 for off-white), but also adds the unnecessary legend
  theme(legend.position = "none") + #gets rid of legends
  theme(axis.text.x = element_text(colour = "black"))+
  theme(panel.spacing = unit(1, "lines")) + 
  coord_flip()

