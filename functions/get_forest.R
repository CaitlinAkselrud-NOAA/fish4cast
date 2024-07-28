
get_forest <- function(mtry, ntrees, minn, splitrule, analy_data, assm_data)
{
  squid_rf <-
    parsnip::rand_forest(
      mode = "regression",
      mtry = mtry, # The number of predictors that will be randomly sampled at each split when creating the tree models.
      trees = ntrees, # The number of trees contained in the ensemble.
      min_n = minn     # The minimum number of data points in a node that are required for the node to be split further.
    ) %>%
    parsnip::set_engine("ranger",
                        splitrule = splitrule,
                        importance = "permutation",
                        seed=TRUE) %>%
    parsnip::fit(target ~ ., data = analy_data) #, na.action = na.exclude)

  pred_assm <- predict(squid_rf, new_data = assm_data) %>%
    bind_cols(target = assm_data$target, time = assm_data$time)

  pred_analysis <- predict(squid_rf, new_data = analy_data) %>%
    bind_cols(target = analy_data$target, time = analy_data$time)

  rmse <-  yardstick::rmse_vec(truth = pred_assm$target, estimate = pred_assm$.pred)
  squid_rf$rmse <- rmse

  return(list(model = squid_rf, assm_pred = pred_assm, analy_pred = pred_analysis, var_importance = squid_rf$fit$variable.importance))
}
