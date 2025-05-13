#' Training the random forest regression
#' includes: selecting best tuned model and hparam set
#' includes: variable importance of training data
#' @param train_slices
#' @param design_set
#' @param train_baked
#' @param user_hp_select
#' @param user_modelname
#'
#' @return
#' @export
#'
#' @examples

get_training <- function(train_slices,
                         design_set,
                         train_baked,
                         user_hp_select = 1,
                         user_modelname = "default")
{
  train_time <- system.time({
    print("starting hyperparam tuning-- please be patient")
    print(paste("start time: ", Sys.time()))
    squid_forests <- list()
    # parallel processing of the forests
    for(i in 1:(train_slices)) #for each k-fold
    { #in the following step, you are generating every hparam combo for a single k-fold
      squid_forests[[i]] <- furrr::future_pmap(list(mtry = (design_set$mtry),
                                                    ntrees = (design_set$trees),
                                                    minn = (design_set$min_n),
                                                    splitrule = (design_set$splitrule)),
                                               get_forest,
                                               analy_data = train_baked[[i]]$baked_squid,
                                               assm_data =  train_baked[[i]]$baked_assessment_squid,
                                               .progress = TRUE)

    }
    # plan(sequential)
    gc()
    # arrange forest results, and extract rmse
    print("almost done with hyperparam tuning....")
    print(paste("start saving time: ", Sys.time()))
    for(j in 1:dim(design_set)[1])
    {
      if(j == 1)
      {
        r_forest_all <- list()
        rmse_slice <- vector(length = dim(design_set)[1]) #rmse across all slices for each hyperparam combo
        mae_slice <- vector(length = dim(design_set)[1])
        rsq_slice <- vector(length = dim(design_set)[1])
        rpd_slice <- vector(length = dim(design_set)[1])
      }
      obsv_pred_assm <- NULL  #re-set for each hyperparam set
      for(i in 1:train_slices)
      {
        if(i ==1){r_forests <- list()}
        r_forests[[i]] <- squid_forests[[i]][[j]]
        obsv_pred_assm <- obsv_pred_assm %>% bind_rows(r_forests[[i]]$assm_pred)
      }
      r_forest_all[[j]] <- r_forests
      # rmse is across all k-folds for each unique hparam set
      rmse_slice[j] <-  yardstick::rmse_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      mae_slice[j] <-  yardstick::mae_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      rsq_slice[j] <-  yardstick::rsq_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      rpd_slice[j] <-  yardstick::rpd_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      # future dev: see yardstick package for more metric options: https://yardstick.tidymodels.org/articles/metric-types.html

    }
    print(paste("end hyperparam tuning time: ", Sys.time()))
  })

  # FIND THE BEST PARAMETER SET
  # 1 = rmse
  # 2 = mae
  # 3 = rsq
  # 4 = rpd

  best_metric <- dplyr::case_when(user_hp_select == 1 ~ min(rmse_slice),
                                  user_hp_select == 2 ~ min(mae_slice),
                                  user_hp_select == 3 ~ max(rsq_slice),
                                  user_hp_select == 4 ~ min(rpd_slice))

  best_hyperparam_set <- dplyr::case_when(user_hp_select == 1 ~ which.min(rmse_slice),
                                          user_hp_select == 2 ~ which.min(mae_slice),
                                          user_hp_select == 3 ~ which.max(rsq_slice),
                                          user_hp_select == 4 ~ which.min(rpd_slice))
  # SAVE BEST TUNED MODEL
  # cia: check using train_slices rather than 1
  best_ntrees <- r_forest_all[[best_hyperparam_set]][[1]]$model$fit$num.trees
  best_mtry <- r_forest_all[[best_hyperparam_set]][[1]]$model$fit$mtry
  best_minn <-r_forest_all[[best_hyperparam_set]][[1]]$model$fit$min.node.size
  best_splitrule <-r_forest_all[[best_hyperparam_set]][[1]]$model$fit$splitrule

  train_results <- bind_cols(ntrees = best_ntrees,
                             mtry = best_mtry,
                             min_n = best_minn,
                             splitrule = best_splitrule)

  # -> cia: go to explore_model_selex here for thinking about best rmse selex

  # SAVE VARIABLE IMPORTANCE FOR ALL FOLDS IN TUNED MODEL
  for(i in 1: train_slices)
  {
    if(i == 1){var_import_slices_train <- NULL}
    var_import <- r_forest_all[[best_hyperparam_set]][[i]]$var_importance %>%
      as_tibble() %>%
      bind_cols(names = names(r_forest_all[[best_hyperparam_set]][[i]]$var_importance)) %>%
      pivot_wider(values_from = value, names_from = names)
    var_import_slices_train <- bind_rows(var_import_slices_train, var_import)
  }

  # trained model info:
  write(paste("Time elapsed for model training (mins): ", round(train_time[3], digits = 2)),
        file = here::here("output", user_modelname, "info.txt"), append = TRUE)
  write(paste("Design set number of combos: ", dim(design_set)[1]),
        file = here::here("output", user_modelname, "info.txt"), append = TRUE)
  write_csv(train_results, path = here::here("output", user_modelname, "train_hparam_results.csv"))

  # save best model info
  dir_path <-  here::here("output", user_modelname, "tuned_model.txt")
  sink(dir_path)
  print(r_forest_all[[best_hyperparam_set]])
  sink()

  return(list(trained_model = r_forest_all[[best_hyperparam_set]],
              train_hparams = train_results,
              var_import = var_import))
}
