library(randomForest)
library(ROCR)

user_vars = c("crt","education",'media1','media2','attention', 'age', "demrep_c", "gender", "income","facebook_frequency" )
item_vars = c('funny','real','likely_true','favors_party','important','reputation_overall','reputation_partyloyalty', "nchar", "affect", 'num_person', 'trump', 'biden', "deepmoji1", "deepmoji2", "deepmoji3")
environment_vars = c('order','prekink', 'is_mobile')


generate_yourfit_fold <- function(){
  
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
  return(yourfit)
}

eval_rf <- function(yourfit, rf_classifier, outcome, name){
  if (outcome == 'response'){
    p <- predict(rf_classifier, yourfit%>% filter(!is_train), type="prob")
    pred <- prediction(predictions = p[,2], labels=yourfit[!yourfit$is_train,outcome])
    auc.tmp <- performance(pred,"auc"); auc <- as.numeric(auc.tmp@y.values)
    out <- auc
  } else{
    p <- predict(rf_classifier, yourfit%>% filter(!is_train))
    out <- cor(p,yourfit[!yourfit$is_train,outcome])
  }
  # save(rf_classifier, file=paste("models/", name , ".RData", sep=""))
  return(out)
}


generate_formula <- function(outcome, user_factors = "", item_factors = "", environment_factors = ""){
  u_f <- "1"
  i_f <- "1"
  e_f <- "1"
  if( user_factors == 'features'){
    u_f <- paste(user_vars , collapse=" + ")
  } else if ( user_factors == 'dummies'){
    u_f <- paste("train_subject_", outcome, "+train_subject_", outcome, "_var", sep="")
  }
  if( item_factors == 'features'){
    i_f <- paste(item_vars , collapse=" + ")
  } else if ( item_factors == 'dummies'){
    i_f <- paste("train_item_", outcome, "+train_item_", outcome, "_var", sep="")
  }
  if( environment_factors == 'features'){
    e_f <- paste(environment_vars , collapse=" + ")
  } 
  f <- paste(u_f, i_f, e_f, sep = "+")
  formula = paste(outcome, "~", f, sep=" ")
  return(formula)
}

do_rf <- function(yourfit, outcome, user_factors = "", item_factors = "", environment_factors = "", name ="", mtry=2, return_model = FALSE){
  f <- as.formula(generate_formula(outcome, user_factors =user_factors, item_factors =item_factors,environment_factors=environment_factors ))
  rf_classifier = randomForest(f,data=yourfit %>% filter(is_train), ntree=50, mtry=mtry, importance=TRUE)
  if (return_model){
    return(rf_classifier)
  }
  eval_rf(yourfit, rf_classifier, outcome, name = name)
}

varImpPlot2 <- function (x, sort = TRUE, n.var = min(30, nrow(x$importance)), 
                         type = NULL, class = NULL, scale = TRUE, main = deparse(substitute(x)), 
                         ...) 
{
  if (!inherits(x, "randomForest")) 
    stop("This function only works for objects of class `randomForest'")
  imp <- importance(x, class = class, scale = scale, type = type, 
                    ...)
  if (ncol(imp) > 2) 
    imp <- imp[, -(1:(ncol(imp) - 2))]
  nmeas <- ncol(imp)
  if (nmeas > 1) {
    op <- par(mfrow = c(1, 2), mar = c(4, 5, 4, 1), mgp = c(2, 
                                                            0.8, 0), oma = c(0, 0, 2, 0), no.readonly = TRUE)
    on.exit(par(op))
  }
  for (i in 1:nmeas) {
    ord <- if (sort) 
      rev(order(imp[, i], decreasing = TRUE)[1:n.var])
    else 1:n.var
    xmin <- if (colnames(imp)[i] %in% c("IncNodePurity", 
                                        "MeanDecreaseGini")) 
      0
    else min(imp[ord, i])
    colz <- map(colorMap, rownames(imp[ord,]))
    dotchart(imp[ord, i], xlab = colnames(imp)[i], ylab = "", 
             main = if (nmeas == 1) 
               main
             else NULL, xlim = c(xmin, max(imp[, i])), ..., bg=colz)
  }
  if (nmeas > 1) 
    mtext(outer = TRUE, side = 3, text = main, cex = 1.2)
  invisible(imp)
}


colorMap <- function(x){
  if (x %in% user_vars){
    return("red")
  } else if (x %in% item_vars){
    return("yellow")
  } else if (x %in% enviornment_vars){
    return('blue')
  } else{
    return("black")
  }
}

map <- function(fn, vec){
  unlist(lapply(vec,fn))
}

