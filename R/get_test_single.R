get_test_single <- function(uncertainty, folds, user_modelname)
{
  # * * single fit ----------------------------------------------------------
  # predict with best rf fit

  test_pred <- as_tibble(predict(object = uncertainty$best_rf, new_data = folds$test_simple)) %>%
    bind_cols(target = folds$test_simple$target) %>%
    rename(pred = .pred)

  # RESIDUALS OUT OF THE APPLIED RF MODEL TO GET RMSE
  test_results <- test_pred %>%
    summarise(rsquared = yardstick::rsq_vec(truth = target, estimate = pred),
              rmse = yardstick::rmse_vec(truth = target, estimate = pred))

  test_info_simple <- bind_cols(time = folds$test_simple$time, test_pred) %>%
    mutate(diff = target-pred)
  # plot(x = test_info_simple$pred, y = test_info_simple$target)

  # plot
  # test_simple <- ggplot()+
  #   geom_point(aes(x = test_info_simple$target, y = test_info_simple$pred)) +
  #   geom_abline()+
  #   theme_classic()+
  #   xlab("Observed")+
  #   ylab("Predicted")
  #
  # get_plot_save(plot = test_simple, plotname_png = "test_simple.png", width = 6, height = 5, model_name = user_modelname)


  # save:
  test_single_save <- list("test_results" = test_results, "test_predictions" = test_pred,
                           "test_data" = folds$test_simple)

  dir_path <-  here::here("output", user_modelname,"test_predictions_singlefit.txt")

  sink(dir_path)
  print(test_single_save)
  sink()

  return(test_simple_result = test_info_simple)

}
