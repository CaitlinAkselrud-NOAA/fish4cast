#' Get cross-validation folds for training and testing
#'
#' @param input_data
#' @param k_init_time_train
#' @param k_assess_train
#' @param k_cumulative_train
#' @param k_skip_train
#' @param k_lag_train
#' @param user_testmethod
#' @param k_init_time_test
#' @param k_assess_test
#' @param k_cumulative_test
#' @param k_skip_test
#' @param k_lag_test
#'
#' @return training cross-validation folds, full testing set,
#'          and cross-validation testing set (if turned on)
#' @export
#'
#' @examples
get_folds <- function(input_data,
                      k_init_time_train,
                      k_assess_train,
                      k_cumulative_train,
                      k_skip_train,
                      k_lag_train,
                      user_testmethod,
                      k_init_time_test,
                      k_assess_test,
                      k_cumulative_test,
                      k_skip_test,
                      k_lag_test)
{
  kfold_train <- get_kfold(k_data_in = all_input$training,
                           k_init_time = k_init_time_train,
                           k_assess = k_assess_train,
                           k_cumulative = k_cumulative_train,
                           k_skip = k_skip_train,
                           k_lag= k_lag_train)

  train_slices <- dim(kfold_train)[1]

  train_baked <- list()

  for(i in 1:(train_slices))
  {
    train_baked[[i]] <- get_baking(kfold = kfold_train,
                                   splitN=i)
  }

  # * * testing single fit -------------------------------------------------

  test_simple <- get_test_bake(all_input$testing)

  # ** optional testing cross-validation ---------------------------------------

  test_baked <- NA
  test_slices <- NA
  if(user_testmethod > 1)
  {
    kfold_test <- get_kfold(k_data_in = all_input$testing,
                            k_init_time = k_init_time_test,
                            k_assess = k_assess_test,
                            k_cumulative = k_cumulative_test,
                            k_skip = k_skip_test,
                            k_lag= k_lag_test)
    test_slices <- dim(kfold_test)[1]

    test_baked <- list()

    for(i in 1:(test_slices))
    {
      test_baked[[i]] <- get_baking(kfold = kfold_test,
                                    splitN=i)
    }

  }
  return(list(train_baked = train_baked,
              test_baked = test_baked,
              test_simple = test_simple,
              train_slices = train_slices,
              test_slices = test_slices))
}
