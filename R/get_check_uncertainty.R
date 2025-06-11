get_check_uncertainty <- function(train_mod,
                                  train_baked,
                                  train_slices,
                                  user_uncertainty,
                                  user_uncertainty_metric,
                                  user_uncertain_cutoff,
                                  user_selex_metric,
                                  user_modelname)
{
  best_hparams = train_mod$train_hparams

  check_grid <- bind_cols(mtry = rep(best_hparams$mtry, times = user_uncertainty),
                          ntrees = rep(best_hparams$ntrees, times = user_uncertainty),
                          minn = rep(best_hparams$min_n, times = user_uncertainty),
                          splitrule = rep(best_hparams$splitrule, times = user_uncertainty))

  ptime_check <- system.time({

    print("checking the forest for bears-- please be patient")
    print(paste("start time: ", Sys.time()))
    squid_forests_check <- list()
    # parallel processing of the forests
    for(i in 1:(train_slices))
    { squid_forests_check[[i]] <- furrr::future_pmap(list(mtry = (check_grid$mtry), ntrees = (check_grid$ntrees),
                                                          minn = (check_grid$minn), splitrule = (check_grid$splitrule)),
                                                     get_forest,
                                                     analy_data = train_baked[[i]]$baked_squid,
                                                     assm_data =  train_baked[[i]]$baked_assessment_squid,
                                                     .progress = TRUE)
    }
    # plan(sequential)
    gc()
    print("almost done checking for bears....")
    print(paste("start saving time: ", Sys.time()))
    # arrange forest results, and extract rmse
    for(j in 1:dim(check_grid)[1])
    {
      if(j == 1)
      {
        r_forest_all_checks <- list()
        rmse_slice_checks <- vector(length = dim(check_grid)[1]) #rmse across all slices for each check
        mae_slice_checks <- vector(length = dim(check_grid)[1])
        rsq_slice_checks <- vector(length = dim(check_grid)[1])
        rpd_slice_checks <- vector(length = dim(check_grid)[1])

        obsv_pred <- list()
        obsv_pred_assm_only  <- list()
        obsv_pred_analy_only <- list()
      }
      obsv_pred_assm_tmp <- NULL
      obsv_pred_analy_tmp <- NULL
      obsv_pred_tmp <- NULL   #re-set for each hyperparam set
      for(i in 1:train_slices)
      {
        if(i ==1){r_forests <- list()}
        r_forests[[i]] <- squid_forests_check[[i]][[j]]
        # obsv_pred_assm_checks <- obsv_pred_assm_checks %>% bind_rows(r_forests[[i]]$assm_pred)
        obsv_pred_assm_tmp <- obsv_pred_assm_tmp %>% bind_rows(r_forests[[i]]$assm_pred)
        obsv_pred_analy_tmp <- obsv_pred_analy_tmp %>% bind_rows(r_forests[[i]]$analy_pred)
        obsv_pred_tmp <- obsv_pred_tmp %>% bind_rows(obsv_pred_analy_tmp, obsv_pred_assm_tmp)
      }
      r_forest_all_checks[[j]] <- r_forests #note: r_forest_all_checks[[check number]][[slice]]
      rmse_slice_checks[j] <-  yardstick::rmse_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
      mae_slice_checks[j] <- yardstick::mae_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
      rsq_slice_checks[j] <- yardstick::rsq_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)
      rpd_slice_checks[j] <- yardstick::rpd_vec(truth = obsv_pred_assm_tmp$target, estimate = obsv_pred_assm_tmp$.pred)

      obsv_pred_assm_only <- obsv_pred_assm_only %>% bind_rows(obsv_pred_assm_tmp)
      obsv_pred_analy_only <- obsv_pred_analy_only %>% bind_rows(obsv_pred_analy_tmp)
      obsv_pred <- obsv_pred %>% bind_rows(obsv_pred_tmp)
    }
  })
  print(paste("end uncertainty time: ", Sys.time()))

  obsv_pred_assm_only <- obsv_pred_assm_only %>% rename(obsv_cdfg = target, pred = .pred)
  obsv_pred_analy_only <- obsv_pred_analy_only %>% rename(obsv_cdfg = target, pred = .pred)
  obsv_pred <- obsv_pred %>% rename(obsv_cdfg = target, pred = .pred)

  uncertainty_split <- case_when(user_uncertainty_metric == 1 ~ max(rmse_slice_checks) - min(rmse_slice_checks),
                                 user_uncertainty_metric == 2 ~ max(mae_slice_checks) - min(mae_slice_checks),
                                 user_uncertainty_metric == 3 ~ max(rsq_slice_checks) - min(rsq_slice_checks),
                                 user_uncertainty_metric == 4 ~ max(rpd_slice_checks) - min(rpd_slice_checks))

  write(paste("Uncertainty split: ", uncertainty_split),
        file = here::here("output", user_modelname,"info.txt"), append = TRUE)

  if(uncertainty_split > user_uncertain_cutoff)
  {
    write(paste("WARNING: The uncertainty split is > your assigned cutoff value"), file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
    write(paste("uncertainty split = ", uncertainty_split), file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
    write(paste("user assigned cutoff value = ", user_uncertain_cutoff), file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
    write(paste("Check that your assigned cutoff value is correct; if so, check features chosen,  feature sparseness, and model configurations; consider more hyperparameter combinations"),
          file = here::here("output", user_modelname, "warnings.txt"), append = TRUE)
  }

  # set best model based on best metric
  best_check_set <-   case_when( user_selex_metric == 1 ~ which.min(rmse_slice_checks),
                                 user_selex_metric == 2 ~ which.min(mae_slice_checks),
                                 user_selex_metric == 3 ~ which.max(rsq_slice_checks),
                                 user_selex_metric == 4 ~ which.min(rpd_slice_checks))

  # for(i in 1:train_slices)
  # {
  #   print(r_forest_all_checks[[best_check_set]][[i]]$model)
  # }
  # set fitted model to last fold of best check set
  best_squid_rf <- r_forest_all_checks[[best_check_set]][[train_slices]]$model

  # results of each slice prediction from the single best check set
  results <- NULL
  for(i in 1:train_slices)
  {
    result_each <- bind_rows(r_forest_all_checks[[best_check_set]][[i]]$analy_pred,
                             r_forest_all_checks[[best_check_set]][[i]]$assm_pred) %>%
      mutate(slice = i) %>%
      rename(pred = .pred) %>%
      mutate(diff = target-pred)

    results <- bind_rows(results, result_each)
  }

  # CIA: you are here-- anything else to output?
  return(list(best_rf = best_squid_rf,
              uncertainty = uncertainty_split,
              best_rf_results = results))
}
