#' Get k-folds within a training or testing set
#'
#' @param k_data_in
#' @param k_init_time
#' @param k_assess
#' @param k_cumulative
#' @param k_skip
#' @param k_lag
#'
#' @return
#' @export
#'
#' @examples
#' @description
#' get k-folds for a data set; includes default settings:
#' using the training data,
#' 1/3 time of total rows of data are used as the initial step
#' 1 row of data is used for each assessment
#' each k-fold adds in the previews assessment data row, so increasing k-folds are cumulative
#' no rows of data are skipped
#' there are no lags added
#'

get_kfold <- function(k_data_in = training(dat_split), #data for cross-validation
                      k_init_time, #initial number of samples used; default setting = 1/3 training data
                      k_assess, #number of samples for each assessment resample; default = 1
                      k_cumulative, #continue to build years in each subsequent step
                      k_skip, #don't skip any resamples (thins thins data)
                      k_lag) #lag btw assessment and analysis sets...)
{
  if(is.na(k_init_time)){k_init_time = round(dim(k_data_in)[1]/3, digits = 0)}
  if(is.na(k_assess)){k_assess = 1}
  if(is.na(k_cumulative)){k_cumulative = T}
  if(is.na(k_skip)){k_skip = 0}
  if(is.na(k_lag)){k_lag = 0}
  k_fold <- rsample::rolling_origin(data = k_data_in,
                                    initial = k_init_time, #
                                    assess = k_assess,  #; -> assessment doing 2 seasons (1 year) at a time CIA: check on this
                                    cumulative = k_cumulative, #
                                    skip = k_skip, #don't skip any resamples (thins thins data)
                                    lag = k_lag) #lag btw assessment and analysis sets...)
  return(k_fold)
}
