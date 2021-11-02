setwd("/home/groh/github/yourfeed_analysis/R")
print("generating data...")
source("load_data.R")
source("conduct_ppiv.R")

user_vars = c("crt","education",'media1','media2','attention', 'age', "demrep_c", "gender", "income","facebook_frequency" )
item_vars = c('funny','real','likely_true','favors_party','important','reputation_overall','reputation_partyloyalty', "nchar", "affect", 'num_person', 'trump', 'biden', "deepmoji1", "deepmoji2", "deepmoji3")
environment_vars = c('order','prekink', 'is_mobile')

vars <- c(user_vars,item_vars, environment_vars)

n_boot <- 1000
out_dwell <- matrix(nrow=n_boot, ncol=7)
out_engage <- matrix(nrow=n_boot, ncol=7)
imp_dwell <- matrix(ncol=length(vars), nrow=0)
imp_engage <- matrix(ncol=length(vars), nrow=0)
colnames(imp_engage) <- vars
colnames(imp_dwell) <- vars
print("starting bootstrap...")
for (i in 1:n_boot){
  yourfit_local <- generate_yourfit_fold()
  
  out_dwell[i,1] <- do_rf(yourfit_local, "ldwell", user_factors = "features", name = "subject_features")
  out_dwell[i,2] <- do_rf(yourfit_local, "ldwell", item_factors = "features", name = "headline_features")
  out_dwell[i,3] <- do_rf(yourfit_local, "ldwell", environment_factors = "features", name = "enviornment_features")
  out_dwell[i,4] <- do_rf(yourfit_local, "ldwell",  user_factors = "features", item_factors = "features", name = "subject_features_headline_features")
  out_dwell[i,5] <- do_rf(yourfit_local, "ldwell", user_factors = "features",environment_factors = "features", name = "subject_features_environment_features")
  out_dwell[i,6] <- do_rf(yourfit_local, "ldwell",  environment_factors = "features", item_factors = "features", name = "environment_features_headline_features")
  
  full_dwell_model <- do_rf(yourfit_local, "ldwell",  user_factors = "features",environment_factors = "features", item_factors = "features", name = "subject_features_headline_features_environment_features", return_model=T)
  out_dwell[i,7] <- eval_rf(yourfit_local, full_dwell_model, "ldwell", name = 'subject_features_headline_features_environment_features')
  model_dwell_imps <- importance(full_dwell_model)[,1]
  imp_dwell <- rbind(imp_dwell, model_dwell_imps)
  
  out_engage[i,1] <- do_rf(yourfit_local, "response", user_factors = "features", name = "subject_features")
  out_engage[i,2] <- do_rf(yourfit_local, "response", item_factors = "features", name = "headline_features")
  out_engage[i,3] <- do_rf(yourfit_local, "response", environment_factors = "features", name = "enviornment_features")
  out_engage[i,4] <- do_rf(yourfit_local, "response",  user_factors = "features", item_factors = "features", name = "subject_features_headline_features")
  out_engage[i,5] <- do_rf(yourfit_local, "response", user_factors = "features",environment_factors = "features", name = "subject_features_environment_features")
  out_engage[i,6] <- do_rf(yourfit_local, "response",  environment_factors = "features", item_factors = "features", name = "environment_features_headline_features")
  full_engage_model <-  do_rf(yourfit_local, "response",  user_factors = "features",environment_factors = "features", item_factors = "features", name = "subject_features_headline_features_environment_features", return_model=T)
  out_engage[i,7] <- eval_rf(yourfit_local, full_engage_model, "response", name = 'subject_features_headline_features_environment_features')
  model_engage_imps <- importance(full_engage_model)[,1]
  imp_engage <- rbind(imp_engage, model_engage_imps)
  
  if (i%%10 ==0){
    print(i)
    write.csv(imp_dwell, "../data/generated/imp_dwell.csv")
    write.csv(imp_engage, "../data/generated/imp_engage.csv")
    write.csv(out_engage, "../data/generated/engage_ppiv.csv")
    write.csv(out_dwell, "../data/generated/dwell_ppiv.csv")
  }
}
