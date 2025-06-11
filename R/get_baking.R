
#' get each k-fold
#'
#' @param k_dat data for k-folds
#' @param splitnum which fold you're pulling
#'
#' @return list of each fold, structured with analysis and assessment data sections
#' @export
#'
#' @examples
#'
get_k_breaks <- function(k_dat, splitnum)
{
  analysis_data <- rsample::analysis(k_dat$splits[[splitnum]])

  assessment_data <- rsample::assessment(k_dat$splits[[splitnum]])

  return(list(analysis_data=analysis_data, assessment_data=assessment_data))
}

#' recipe for regression
#'
#' @param analysis_data analysis data for a specific fold
#'
#' @return recipe (aka formula) for each fold for RF regression
#' @export
#'
#' @examples
#'
make_recipe <- function(analysis_data)
{
  na_foo <- function(x,na_replace=-999)
  {
    x[is.na(x)]<- na_replace
    return(x)
  }

  fish_recipe <- recipe(target~ ., data = analysis_data) #%>%
    # step_mutate(year=factor(time)) #%>% #CIA: can add this as a feature when > obsv per time step AND that time step is in both analy and assm sets
    # step_mutate_at(all_numeric(),-all_outcomes(), fn=na_foo) #UNCOMMENT HERE TO CHANGE NA -> -999

  return(fish_recipe)
}

#' Input for RF analysis with each k-fold, analysis and assessment, and regression formula
#'
#' @param kfold input data
#' @param splitN split number
#'
#' @return
#' @export
#'
#' @examples
#'
get_baking <- function(kfold, splitN, in_clean)
{
  d <- get_k_breaks(kfold, splitN)
  analy_data <- d$analysis_data
  assm_data <- d$assessment_data

  squid_recipe <- make_recipe(analysis_data = analy_data)

  clean_squid <- squid_recipe %>%
    step_rm(!any_of(names(in_clean)))

  prepped_squid<- prep(clean_squid, data= analy_data)

  juiced_squid<- juice(prepped_squid)

  baked_squid <- bake(prepped_squid, new_data = analy_data)

  # glimpse(baked_squid)

  ### do it again for assm data... otherwise you get NAs in the years
  squid_recipe <- make_recipe(assm_data)
  clean_squid <- squid_recipe %>%
    step_rm(!any_of(names(in_clean)))
  prepped_squid<- prep(clean_squid, data= assm_data)
  juiced_squid<- juice(prepped_squid)
  baked_assessment_squid <- bake(prepped_squid, new_data = assm_data)

  return(list(baked_squid=baked_squid, baked_assessment_squid=baked_assessment_squid))

}

#' Testing data for RF predictions in the single fit case
#'
#' @param testing_data input testing data
#'
#' @return single data set with features and target
#' @export
#'
#' @examples
#'
get_test_bake <- function(testing_data, in_clean)
{
  squid_recipe <- make_recipe(testing_data)
  clean_squid <- squid_recipe %>%
    step_rm(!any_of(names(in_clean)))
  prepped_squid<- prep(clean_squid, data= testing_data)
  juiced_squid<- juice(prepped_squid)
  baked_squid <- bake(prepped_squid, new_data = testing_data)
  return(baked_squid)
}
