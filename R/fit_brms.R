library(tidyverse)
library(brms)

prior = c(prior(normal(0.5, .25), class = Intercept), #start with vague and uninformative prior - 0.5. 
          prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
          prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
          prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
          prior(lkj(2),            class = cor))

long_raw <- read_csv("https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1")
item_level <- read_csv("https://www.dropbox.com/s/890rqvsdarl4ib7/item_level.csv?dl=1")

long_raw$crt <- (as.numeric(long_raw$crt_ages == 4) + as.numeric(long_raw$crt_printer == 10) + as.numeric(long_raw$crt_bread == 39) + as.numeric(long_raw$crt_race == 2) + as.numeric(long_raw$crt_sheep == 8))/5

long <- long_raw %>% mutate(
  crt = (as.numeric(crt_ages == 4) + as.numeric(crt_printer == 10) + as.numeric(crt_bread == 39) + as.numeric(crt_race == 2) + as.numeric(crt_sheep == 8))/5,
  pk = as.numeric((pk1)==3) + as.numeric((pk2)==1)  + as.numeric((pk3)==3) + as.numeric((pk4)==1) + as.numeric((pk5)==3)
) %>% filter(completed==1) %>% merge(item_level, all.x=T, by.x='item', by.y='index')

yourfeed <- long %>% filter(condition==1) %>% mutate(
  concordant = case_when(
    (lean == 'D-leaning') & (demrep_c>3 ) ~ -1,
    (lean == 'R-leaning') & (demrep_c<=3 ) ~ -1,
    (lean == 'D-leaning') & (demrep_c<=3 ) ~ 1,
    (lean == 'R-leaning') & (demrep_c>3 ) ~ 1,
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
  ldwell = log(dwell),
  attend =  as.numeric(dwell > 1330),
  prekink= as.numeric(order<6)
) %>% filter(order!=140)


A1 <- brm(data = yourfeed,family = gaussian,
          formula = attend~crt*(real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + order*prekink)+
            (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt+order*prekink| item) +
            (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt + order*prekink| rid),
          prior = prior, iter = 5000, warmup = 1000, chains = 4, cores = 4, seed = 42, control = list(adapt_delta = 0.92), 
          file = "../models/A1")

A2 <- brm(data = yourfeed,family = gaussian,
          formula = wdwell~crt*(real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + order*prekink)+
            (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt+order*prekink| item) +
            (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt + order*prekink| rid),
          prior = prior, iter = 5000, warmup = 1000, chains = 4, cores = 4, seed = 42, control = list(adapt_delta = 0.92), 
          file = "../models/A2")

A3 <- brm(data = yourfeed,family = gaussian,
          formula = ldwell~crt*(real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + order*prekink)+
            (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt+order*prekink| item) +
            (1 + real+concordant+surprising+likely_true+ favors_party+ benefits_party +important+ funny+ surprising +reputation_overall +reputation_partyloyalty+ reputation_engage+ likely_share + crt + order*prekink| rid),
          prior = prior, iter = 5000, warmup = 1000, chains = 4, cores = 4, seed = 42, control = list(adapt_delta = 0.92), 
          file = "../models/A3")
# main <- 
#   brm(data = long,
#       family = gaussian,
#       formula = response~real*condition+
#         (1 +condition   | item) +
#         (1 + real | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/main")

# crt_mod <- 
#   brm(data = long,
#       family = gaussian,
#       formula = response~real*condition*crt+
#         (1 +condition   | item) +
#         (1 + real | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 5000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/crt_mod")
# 
# pk_mod <- 
#   brm(data = long,
#       family = gaussian,
#       formula = response~real*condition*pk+
#         (1 +condition   | item) +
#         (1 + real | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/pk_mod")

#Predicting attentional capture as an outcome
# m5 <- 
#   brm(data = yourfeed,
#       family = gaussian,
#       formula = attend1~real*concordant+
#         (1 +concordant  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m5")
# 
# m6 <- 
#   brm(data = yourfeed,
#       family = gaussian,
#       formula = attend1~real*concordant*crt+
#         (1 +concordant +crt  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m6")
# 
# m7 <- 
#   brm(data = yourfeed,
#       family = gaussian,
#       formula = attend1~real*concordant*pk+
#         (1 +concordant +pk  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m7")
# 
# m8 <- 
#   brm(data = yourfeed,
#       family = gaussian,
#       formula = attend2~real*concordant+
#         (1 +concordant  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m8")
# 
# m9 <- 
#   brm(data = yourfeed,
#       family = gaussian,
#       formula = attend2~real*concordant*crt+
#         (1 +concordant +crt  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m9")
# 
# m10 <- 
#   brm(data = yourfeed,
#       family = gaussian,
#       formula = attend2~real*concordant*pk+
#         (1 +concordant +pk  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m10")
# 
# m11 <- 
#   brm(data = yofo,
#       family = gaussian,
#       formula = attend3~real*concordant+
#         (1 +concordant  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m11")
# 
# m12 <- 
#   brm(data = yofo,
#       family = gaussian,
#       formula = attend3~real*concordant*crt+
#         (1 +concordant +crt  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m12")
# 
# m13 <- 
#   brm(data = yofo,
#       family = gaussian,
#       formula = attend3~real*concordant*pk+
#         (1 +concordant +pk  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m13")
# 
# #Predicting engagement conditional on attention 
# m14 <- 
#   brm(data = yourfeed %>% filter(attend1==1),
#       family = gaussian,
#       formula = response~real*concordant+
#         (1 +concordant  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m14")
# 
# m15 <- 
#   brm(data = yourfeed %>% filter(attend1==1),
#       family = gaussian,
#       formula = response~real*concordant*pk+
#         (1 +concordant +pk  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m15")
# 
# m16 <- 
#   brm(data = yourfeed %>% filter(attend1==1),
#       family = gaussian,
#       formula = response~real*concordant*crt+
#         (1 +concordant +crt  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m16")
# 
# m17 <- 
#   brm(data = yourfeed %>% filter(attend2==1),
#       family = gaussian,
#       formula = response~real*concordant+
#         (1 +concordant  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m17")
# 
# m18 <- 
#   brm(data = yourfeed %>% filter(attend2==1),
#       family = gaussian,
#       formula = response~real*concordant*pk+
#         (1 +concordant +pk  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m18")
# 
# m19 <- 
#   brm(data = yourfeed %>% filter(attend2==1),
#       family = gaussian,
#       formula = response~real*concordant*crt+
#         (1 +concordant +crt  | item) +
#         (1 + real + concordant | rid),
#       prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
#                 prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
#                 prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
#                 prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
#                 prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
#       iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
#       control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
#       file = "../models/m19")
#   
# 
# 
# 
# 
