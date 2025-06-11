get_check_trees <- function(train_mod,
                            train_baked,
                            train_slices,
                            user_treecheck,
                            user_modelname)
{
  best_hparams = train_mod$train_hparams
  ptime_trees <- system.time({
    print("planting trees in the forest-- please be patient")
    print(paste("start time: ", Sys.time()))
    squid_forests_trees <- list()
    # parallel processing of the forests
    for(i in 1:(train_slices))
    { squid_forests_trees[[i]] <- furrr::future_pmap(list(mtry = best_hparams$mtry, ntrees = (user_treecheck),
                                                          minn = best_hparams$min_n, splitrule = best_hparams$splitrule),
                                                     get_forest,
                                                     analy_data = train_baked[[i]]$baked_squid,
                                                     assm_data =  train_baked[[i]]$baked_assessment_squid,
                                                     .progress = TRUE)
    }
    # plan(sequential)
    gc()
    print("almost done planting trees....")
    print(paste("start saving time: ", Sys.time()))
    # arrange forest results, and extract rmse
    for(j in 1:length(user_treecheck))
    {
      if(j == 1)
      {
        r_forest_alltrees <- list()
        rmse_slice_trees <- vector(length = length(user_treecheck)) #rmse across all slices for each hyperparam combo
      }
      obsv_pred_assm_trees <- NULL  #re-set for each hyperparam set
      for(i in 1:train_slices)
      {
        if(i ==1){r_forests <- list()}
        r_forests[[i]] <- squid_forests_trees[[i]][[j]]
        obsv_pred_assm_trees <- obsv_pred_assm_trees %>% bind_rows(r_forests[[i]]$assm_pred)
      }
      r_forest_alltrees[[j]] <- r_forests #note: r_forest_alltrees[[tree number]][[slice]]
      rmse_slice_trees[j] <-  yardstick::rmse_vec(truth = obsv_pred_assm_trees$target, estimate = obsv_pred_assm_trees$.pred)
    }
  })
  print(paste("end trees time: ", Sys.time()))

  # tree check with best training model:
  tree_check_plot <- ggplot()+
    geom_line(aes(x = user_treecheck, y = rmse_slice_trees)) +
    geom_point(aes(x = user_treecheck, y = rmse_slice_trees)) +
    theme_classic()+
    xlab("Number of trees")+
    ylab("RMSE")

  print(tree_check_plot)

  get_plot_save(plot = tree_check_plot, plotname_png = "tree_effect.png", width = 6, height = 5, model_name = user_modelname)


  return(bind_cols(user_treecheck = user_treecheck, rmse = rmse_slice_trees))

}
