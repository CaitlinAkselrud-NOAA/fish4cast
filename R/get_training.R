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
    # print("almost done with hyperparam tuning....")
    # print(paste("start saving time: ", Sys.time()))
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
      rmse_slice[j] <- yardstick::rmse_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      mae_slice[j] <-  yardstick::mae_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      rsq_slice[j] <-  yardstick::rsq_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      rpd_slice[j] <-  yardstick::rpd_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      # future dev: see yardstick package for more metric options: https://yardstick.tidymodels.org/articles/metric-types.html

    }
    print(paste("end prior hyperparam tuning time: ", Sys.time()))

    print(paste("beginning posterior hyperparam tuning time: ", Sys.time()))
    # models within x% of the minimum rmse:

    check_percent <- 0.05 # 5%

    best_group <- quantile(rmse_slice, probs = check_percent, na.rm = TRUE)

    best_rmse_group <- rmse_slice[which(rmse_slice <= best_group)]
    num_best_group <- rmse_slice[which(rmse_slice <= best_group)] %>% length
    num_evals <- length(rmse_slice)
    ratio <- num_best_group/num_evals #proportion of results that are within x% of best rmse

    trees <- vector(length = num_best_group)
    mtries <- vector(length = num_best_group)
    min_ns <- vector(length = num_best_group)
    splitrules <- vector(length = num_best_group)
    combo <- vector(length = num_best_group)
    x <- 1
    for(i in which(rmse_slice <= best_group))
    {
      trees[x] <- (r_forest_all[[i]][[1]]$model$fit$num.trees)
      mtries[x] <- (r_forest_all[[i]][[1]]$model$fit$mtry)
      min_ns[x] <- (r_forest_all[[i]][[1]]$model$fit$min.node.size)
      splitrules[x] <- (r_forest_all[[i]][[1]]$model$fit$splitrule)
      combo[x] <- x
      x <- x+1
    }

    new_grid <- bind_cols(trees = trees, mtry = mtries,
                          min_n = min_ns, splitrule = splitrules,
                          rmse = best_rmse_group,
                          combo = combo)

    post_color <- "#0072B2" # "#CC79A7" "#D55E00"
    post_outline <- "#e9ecef"

    p_post_tree <- ggplot() +
      geom_histogram(aes(design_set$trees)) +
      geom_histogram(aes(new_grid$trees), fill = post_color, color = post_outline) +
      theme_classic()

    p_post_mtries <- ggplot() +
      geom_histogram(aes(design_set$mtry)) +
      geom_histogram(aes(new_grid$mtry), fill = post_color, color = post_outline) +
      theme_classic()

    p_post_minn <- ggplot() +
      geom_histogram(aes(design_set$min_n)) +
      geom_histogram(aes(new_grid$min_n), fill = post_color, color = post_outline) +
      theme_classic()

    p_post_rmse <- ggplot() +
      geom_histogram(aes(rmse_slice)) +
      geom_histogram(aes(best_rmse_group), fill = post_color, color = post_outline) +
      theme_classic()

    p_full_post <- p_post_rmse/(p_post_tree + p_post_mtries + p_post_minn)

    p_post_tree <- ggplot() +
      # geom_histogram(aes(design_set$trees)) +
      geom_histogram(aes(new_grid$trees), fill = post_color, color = post_outline) +
      theme_classic()

    p_post_mtries <- ggplot() +
      # geom_histogram(aes(design_set$mtry)) +
      geom_histogram(aes(new_grid$mtry), fill = post_color, color = post_outline) +
      theme_classic()

    p_post_minn <- ggplot() +
      # geom_histogram(aes(design_set$min_n)) +
      geom_histogram(aes(new_grid$min_n), fill = post_color, color = post_outline) +
      theme_classic()

    p_post_rmse <- ggplot() +
      # geom_histogram(aes(rmse_slice)) +
      geom_histogram(aes(best_rmse_group), fill = post_color, color = post_outline) +
      theme_classic()

    p_post <- p_post_rmse/(p_post_tree + p_post_mtries + p_post_minn)

    get_plot_save(plot = p_full_post, plotname_png = "p_training_priorandpost.png", model_name = user_modelname)
    get_plot_save(plot = p_post, plotname_png = "p_training_posterior.png", model_name = user_modelname)

    # new_grid %>% bind_rows(new_grid)
    new_grid <-bind_rows(replicate(10, bind_rows(new_grid, new_grid), simplify = FALSE))

# ---posterior tuning----------------------------------------------------------------------
    posterior_forests <- list()
    # parallel processing of the forests
    for(i in 1:(train_slices)) #for each k-fold
    { #in the following step, you are generating every hparam combo for a single k-fold
      posterior_forests[[i]] <- furrr::future_pmap(list(mtry = (new_grid$mtry),
                                                    ntrees = (new_grid$trees),
                                                    minn = (new_grid$min_n),
                                                    splitrule = (new_grid$splitrule)),
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

    # FIND THE BEST PARAMETER SET FOR THE WHOLE TRAINING SET
    for(j in 1:dim(new_grid)[1])
    {
      if(j == 1)
      {
        r_forest_all <- list()
        rmse_slice <- vector(length = dim(new_grid)[1]) #rmse across all slices for each hyperparam combo
        mae_slice <- vector(length = dim(new_grid)[1])
        rsq_slice <- vector(length = dim(new_grid)[1])
        rpd_slice <- vector(length = dim(new_grid)[1])
      }
      obsv_pred_assm <- NULL  #re-set for each hyperparam set
      for(i in 1:train_slices)
      {
        if(i ==1){r_forests <- list()}
        r_forests[[i]] <- posterior_forests[[i]][[j]]
        obsv_pred_assm <- obsv_pred_assm %>% bind_rows(r_forests[[i]]$assm_pred)
      }
      r_forest_all[[j]] <- r_forests
      # rmse is across all k-folds for each unique hparam set
      rmse_slice[j] <- yardstick::rmse_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      mae_slice[j] <-  yardstick::mae_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      rsq_slice[j] <-  yardstick::rsq_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      rpd_slice[j] <-  yardstick::rpd_vec(truth = obsv_pred_assm$target, estimate = obsv_pred_assm$.pred)
      # future dev: see yardstick package for more metric options: https://yardstick.tidymodels.org/articles/metric-types.html

    }

    check_percent <- 1#0.05 # 5%

    best_group <- quantile(rmse_slice, probs = check_percent, na.rm = TRUE)

    best_rmse_group <- rmse_slice[which(rmse_slice <= best_group)]
    num_best_group <- rmse_slice[which(rmse_slice <= best_group)] %>% length
    num_evals <- length(rmse_slice)
    ratio <- num_best_group/num_evals #proportion of results that are within x% of best rmse

    trees <- vector(length = num_best_group)
    mtries <- vector(length = num_best_group)
    min_ns <- vector(length = num_best_group)
    splitrules <- vector(length = num_best_group)
    x <- 1
    for(i in which(rmse_slice <= best_group))
    {
      trees[x] <- (r_forest_all[[i]][[1]]$model$fit$num.trees)
      mtries[x] <- (r_forest_all[[i]][[1]]$model$fit$mtry)
      min_ns[x] <- (r_forest_all[[i]][[1]]$model$fit$min.node.size)
      splitrules[x] <- (r_forest_all[[i]][[1]]$model$fit$splitrule)
      x <- x+1
    }

    new_grid <- bind_cols(trees = trees, mtry = mtries,
                          min_n = min_ns, splitrule = splitrules, rmse = best_rmse_group)

    print(paste("end posterior hyperparam tuning time: ", Sys.time()))
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
  # post post plots
  p_post_tree <- ggplot() +
    geom_histogram(aes(new_grid$trees), fill = post_color, color = post_outline) +
    geom_histogram(aes(train_results$ntrees), fill = "#D55E00") +
    theme_classic()

  p_post_mtries <- ggplot() +
    geom_histogram(aes(new_grid$mtry), fill = post_color, color = post_outline) +
    geom_histogram(aes(train_results$mtry), fill = "#D55E00") +
    theme_classic()

  p_post_minn <- ggplot() +
    geom_histogram(aes(new_grid$min_n), fill = post_color, color = post_outline) +
    geom_histogram(aes(train_results$min_n), fill = "#D55E00") +
    theme_classic()

  best_metric <- dplyr::case_when(user_hp_select == 1 ~ min(rmse_slice),
                                  user_hp_select == 2 ~ min(mae_slice),
                                  user_hp_select == 3 ~ max(rsq_slice),
                                  user_hp_select == 4 ~ min(rpd_slice))

  p_post_rmse <- ggplot() +
    geom_histogram(aes(best_rmse_group), fill = post_color, color = post_outline) +
    geom_histogram(aes(best_metric), fill = "#D55E00") +
    theme_classic()

  p_postpost <- p_post_rmse/(p_post_tree + p_post_mtries + p_post_minn)

  get_plot_save(plot = p_postpost, plotname_png = "p_hparam_best_selex.png", model_name = user_modelname)



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
  write(paste("Design set number of combos: ", dim(new_grid)[1]),
        file = here::here("output", user_modelname, "info.txt"), append = TRUE)
  write_csv(train_results, file = here::here("output", user_modelname, "train_hparam_results.csv"))

  # save best model info
  dir_path <-  here::here("output", user_modelname, "tuned_model.txt")
  sink(dir_path)
  print(r_forest_all[[best_hyperparam_set]])
  sink()

  # * * training diagnostic plots --------------------------------------------

  # save histogram
  # p_hist_rmse <- ggplot() +
  #   geom_histogram(aes(rmse_slice), fill = "black", color = "white") +
  #   theme_classic() +
  #   scale_y_continuous(expand = c(0,0))+
  #   labs(x = "RMSE",
  #        y = "Frequency")
  # p_hist_mae <- ggplot() +
  #   geom_histogram(aes(mae_slice), fill = "black", color = "white") +
  #   theme_classic() +
  #   scale_y_continuous(expand = c(0,0))+
  #   labs(x = "MAE",
  #        y = "Frequency")
  # p_hist_rsq <- ggplot() +
  #   geom_histogram(aes(rsq_slice), fill = "black", color = "white") +
  #   theme_classic() +
  #   scale_y_continuous(expand = c(0,0))+
  #   labs(x = "R^2",
  #        y = "Frequency")
  # p_hist_rpd <- ggplot() +
  #   geom_histogram(aes(rpd_slice), fill = "black", color = "white") +
  #   theme_classic() +
  #   scale_y_continuous(expand = c(0,0))+
  #   labs(x = "RPD",
  #        y = "Frequency")
  #
  # # save ggplots
  # p_hyperparams_trees <- ggplot() +
  #   geom_point(aes(x = design_set$trees, y = rmse_slice, color = as.factor(design_set$min_n)), size = 2) +
  #   theme_classic() +
  #   labs(x = "Number of trees",
  #        y = "RMSE",
  #        color = "Min num nodes") #+
  # # scale_color_manual(values=c("#762a83", "#1b7837"))
  #
  # p_hyperparams_trees2 <- ggplot() +
  #   geom_point(aes(x = design_set$trees, y = rmse_slice, color = as.factor(design_set$mtry)), size = 2) +
  #   theme_classic() +
  #   labs(x = "Number of trees",
  #        y = "RMSE",
  #        color = "Num predictors") #+
  # # scale_color_manual(values=c("#762a83", "#1b7837"))
  #
  #
  # p_hyperparams_minn <- ggplot() +
  #   geom_point(aes(x = design_set$min_n, y = rmse_slice, color = as.factor(design_set$trees)), size = 2) +
  #   theme_classic() +
  #   labs(x = "Minimum number of data points per node",
  #        y = "RMSE",
  #        color = "Number of trees") #+
  # # scale_color_manual(values=c("#762a83", "#1b7837"))
  #
  #
  # p_hyperparams_mtry <- ggplot() +
  #   geom_point(aes(x = design_set$mtry, y = rmse_slice, color = as.factor(design_set$trees)), size = 2) +
  #   theme_classic() +
  #   labs(x = "Number of predictors randomly sampled",
  #        y = "RMSE",
  #        color = "Number of trees") #+
  # # scale_color_manual(values=c("#762a83", "#1b7837"))
  #
  # # trees
  #
  # tree_plot <-
  #   bind_cols(design_set, rmse_slice=rmse_slice) %>%
  #   ggplot()+
  #   geom_line(aes(x = trees, y = rmse_slice)) +
  #   geom_point(aes(x = trees, y = rmse_slice)) +
  #   theme_classic()+
  #   xlab("Number of trees")+
  #   ylab("RMSE")+
  #   # facet_wrap(~combo)
  #   facet_wrap(vars(min_n, mtry), labeller = "label_both") +
  #   theme(axis.text.x=element_text(angle=90, hjust=1))
  # # tree_plot
  # if(test_mode == TRUE) {tree_plot}
  #
  # # SAVE hyperparameter plots
  #
  # get_plot_save(plot = p_hist_rmse, plotname_png = "p_hist_rmse.png", model_name = user_modelname)
  # get_plot_save(plot = p_hist_mae, plotname_png = "p_hist_mae.png", model_name = user_modelname)
  # get_plot_save(plot = p_hist_rsq, plotname_png = "p_hist_rsq.png", model_name = user_modelname)
  # get_plot_save(plot = p_hist_rpd, plotname_png = "p_hist_rpd.png", model_name = user_modelname)
  # get_plot_save(plot = p_hyperparams_trees, plotname_png = "p_hyperparams_trees.png", model_name = user_modelname)
  # get_plot_save(plot = p_hyperparams_trees2, plotname_png = "p_hyperparams_trees2.png", model_name = user_modelname)
  # get_plot_save(plot = p_hyperparams_minn, plotname_png = "p_hyperparams_minn.png", model_name = user_modelname)
  # get_plot_save(plot = p_hyperparams_mtry, plotname_png = "p_hyperparams_mtry.png", model_name = user_modelname)
  # get_plot_save(plot = tree_plot, plotname_png = "tree_plot.png", width = 8, height = 20, model_name = user_modelname)
  #

  # * * variable importance -------------------------------------------------

  # variable importance:
  write_csv(var_import_slices_train, path = here::here("output", user_modelname, "train_var_import.csv"))

  return(list(trained_model = r_forest_all[[best_hyperparam_set]],
              train_hparams = train_results,
              var_import = var_import_slices_train))
}
