library(tidyverse)
library(brms)

long_raw <- read_csv("https://www.dropbox.com/s/xd0i4dse47nggmt/soft_long.csv?dl=1")
item_level <- read_csv("https://www.dropbox.com/s/asrmhf3upvhy1fq/aj_headlines.csv?dl=1")
long_raw$crt <- (as.numeric(long_raw$crt_ages == 4) + as.numeric(long_raw$crt_printer == 10) + as.numeric(long_raw$crt_bread == 39) + as.numeric(long_raw$crt_race == 2) + as.numeric(long_raw$crt_sheep == 8))/5
item_level$real = as.numeric(!grepl("f_", item_level$filename))-0.5

long <- long_raw %>% mutate(
  crt = (as.numeric(crt_ages == 4) + as.numeric(crt_printer == 10) + as.numeric(crt_bread == 39) + as.numeric(crt_race == 2) + as.numeric(crt_sheep == 8))/5,
  pk = as.numeric((pk1)==3) + as.numeric((pk2)==1)  + as.numeric((pk3)==3) + as.numeric((pk4)==1) + as.numeric((pk5)==3)
) %>% filter(completed==1) %>% merge(item_level, all.x=T, by.x='item', by.y='index')


fit <- 
  brm(data = long,
      family = gaussian,
      formula = response~real*condition+
        (1 +condition   | item) +
        (1 + real | rid),
      prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
                prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
                prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
                prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
                prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
      iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
      control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
      file = "../models/main")

fit <- 
  brm(data = long,
      family = gaussian,
      formula = response~real*condition*crt+
        (1 +condition   | item) +
        (1 + real | rid),
      prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
                prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
                prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
                prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
                prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
      iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
      control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
      file = "../models/crt_mod")

fit <- 
  brm(data = long,
      family = gaussian,
      formula = response~real*condition*pk+
        (1 +condition   | item) +
        (1 + real | rid),
      prior = c(prior(normal(0.5, 0.25), class = Intercept), #start with vague and uninformative prior - 0.5. 
                prior(normal(0, 0.25),    class = b),#b is all the slopes. we expect >0 (maybe 0.05, change variation) #class = b, coef="veracity" #remove priors on 25+27 (21+22)
                prior(exponential(14),   class = sd), #sd governs the SD of the fixed effects of the random cluster. can only be positive so exponential
                prior(exponential(1),    class = sigma),  #noise param, whats left over. fine as exp(1). if zed(continuous) then easy to set as exp(1)
                prior(lkj(2),            class = cor)), #prior on covariance structure. intercept and slope within cluster
      iter = 4000, warmup = 1000, chains = 4, cores = 4, seed = 42, #warmup=1000 is good. iter=3000 is chill too.  #1 chain per core 
      control = list(adapt_delta = 0.92), #not default, default = 0.8, if step size is too large,adjust 
      file = "../models/pk_mod")
