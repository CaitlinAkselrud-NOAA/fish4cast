
get_k_breaks <- function(k_dat, splitnum)
{
  analysis_data <- rsample::analysis(k_dat$splits[[splitnum]])

  assessment_data <- rsample::assessment(k_dat$splits[[splitnum]])

  return(list(analysis_data=analysis_data, assessment_data=assessment_data))
}

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

get_baked <- function(kfold, splitN)
{
  d <- get_k_breaks(kfold, splitN)
  analy_data <- d$analysis_data
  assm_data <- d$assessment_data

  squid_recipe <- make_recipe(analysis_data = analy_data)

  prepped_squid<- prep(squid_recipe, data= analy_data)

  juiced_squid<- juice(prepped_squid)

  baked_squid <- bake(prepped_squid, new_data = analy_data)

  # glimpse(baked_squid)

  ### do it again for assm data... otherwise you get NAs in the years
  squid_recipe <- make_recipe(assm_data)
  prepped_squid<- prep(squid_recipe, data= assm_data)
  juiced_squid<- juice(prepped_squid)
  baked_assessment_squid <- bake(prepped_squid, new_data = assm_data)

  return(list(baked_squid=baked_squid, baked_assessment_squid=baked_assessment_squid))

}
