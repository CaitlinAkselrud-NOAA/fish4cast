get_test_folds2 <- function(test_baked, train_baked, design_set, test_slices, train_slices)
{
  # test_slices
  options(future.rng.onMisuse="ignore")
  ptime_test <- system.time({
    for(i in 1:(test_slices))
    {
      if(i == 1){
        squid_forests <- list()
        new_tune <- bind_rows(train_baked[[train_slices]]$baked_squid,
                              train_baked[[train_slices]]$baked_assessment_squid)

        squid_forests[[i]] <- furrr::future_pmap(list(mtry = (design_set$mtry), ntrees = (design_set$trees),
                                                      minn = (design_set$min_n), splitrule = (design_set$splitrule)),
                                                 get_forest,
                                                 analy_data = new_tune,
                                                 assm_data =  test_baked[[i]]$baked_squid,
                                                 .progress = TRUE)
        }
      print(paste("re-tuning with fold step: ", i, "of", test_slices))
      print(paste("start time: ", Sys.time()))
      # bind the full training data set to the first fold of the testing set
      new_tune <- bind_rows(train_baked[[train_slices]]$baked_squid,
                            train_baked[[train_slices]]$baked_assessment_squid,
                            test_baked[[i]]$baked_squid)

      squid_forests[[i+1]] <- furrr::future_pmap(list(mtry = (design_set$mtry), ntrees = (design_set$trees),
                                                    minn = (design_set$min_n), splitrule = (design_set$splitrule)),
                                               get_forest,
                                               analy_data = new_tune,
                                               assm_data =  test_baked[[i]]$baked_assessment_squid,
                                               .progress = TRUE)
    }


    gc()
    # arrange forest results, and extract rmse
    print("almost done with testing analysis....")
    print(paste("start saving time: ", Sys.time()))

    # FIND THE BEST PARAMETER SET **FOR EACH SLICE**
    results <- NULL
    for(i in 1:(test_slices+1))
    {
      if(i == 1){best_rmse = vector(length = test_slices +1)
      best_hyperparam_set = vector(length = test_slices +1)}
      # within every slice, there is each hyperparam set
      for(j in 1:dim(design_set)[1])
      {
        if(j == 1) {rmse_each = vector(length = dim(design_set)[1])}
        rmse_each[j] = yardstick::rmse_vec(truth = squid_forests[[i]][[j]]$assm_pred$target,
                                           estimate = squid_forests[[i]][[j]]$assm_pred$.pred)
      }
      best_rmse[i] <- min(rmse_each)
      best_hyperparam_set[i] <- which.min(rmse_each)
      result_each <- bind_rows(squid_forests[[i]][[best_hyperparam_set[i]]]$analy_pred,
                               squid_forests[[i]][[best_hyperparam_set[i]]]$assm_pred) %>%
        mutate(slice = i)
      results <- bind_rows(results, result_each)

    }

    # SAVE THE BEST FIT FOR EACH SLICE (lowest rmse of any hyperparam set for each set of years predicted)
    best_hparams_slice <- NULL
    for(i in 1:(test_slices +1))
    {
      if(i == 1){best_ofeach_slice <- list()}
      # save info from each slice and best set
      best_ofeach_slice[[i]] <- squid_forests[[i]][[best_hyperparam_set[i]]]
      best_hparams_slice <- bind_rows(best_hparams_slice, c(ntrees = squid_forests[[i]][[best_hyperparam_set[i]]]$model$fit$num.trees,
                                                            minn = squid_forests[[i]][[best_hyperparam_set[i]]]$model$fit$min.node.size,
                                                            mtry = squid_forests[[i]][[best_hyperparam_set[i]]]$model$fit$mtry,
                                                            rmse = best_rmse[i],
                                                            slice = i))
    }

    # SAVE VARIABLE IMPORTANCE FOR ALL FOLDS IN TUNED MODEL
    var_import_slices_test <- NULL
    for(i in 1: test_slices +1)
    {
      # if(i == 1){}
      var_import <- squid_forests[[i]][[best_hyperparam_set[i]]]$var_importance %>%
        as_tibble() %>%
        bind_cols(names = names(squid_forests[[i]][[best_hyperparam_set[i]]]$var_importance)) %>%
        pivot_wider(values_from = value, names_from = names)
      var_import_slices_test <- bind_rows(var_import_slices_test, var_import)
    }
  })

  # write_csv(best_hparams_slice, path = here::here("output", user_modelname,"test_predictions_slice_hparms.csv"))
  # write_csv(results, path = here::here("output", user_modelname,"test_predictions_slice_all.csv"))

  return(list(results = results, var_import_slices_test = var_import_slices_test,
              models = squid_forests, best_set = best_hyperparam_set))

}
