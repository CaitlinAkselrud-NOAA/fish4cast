#' Get regimes using multivariate hidden markov model
#'
#' @param dat all the features
#' @param dat_dist distribution type for each feature
#' @param n_states number of states to test (min = 1)
#' @param n_iters how many repeated simulations of hmm
#'
#' @return best model using aic selex and associated info
#' @export
#'
#' @examples see github zoer27/HMM-indicators
#'
get_regimes <- function(dat, dat_dist, n_states, n_iters = 200)
{
  dat_names <- names(dat)

  AICs <- 0
  best_aic <- 1.0e10
  best_model<-NULL

  for(i in n_iters)
  {

    #set up initial values
    init_list<-list()
    # create initial values for each data column using kmeans clustering for n regimes
    inits <- matrix(nrow = n_states, ncol = dim(dat)[2])
    for(i in 1:dim(dat)[2])
    {
      inits[,i] <- kmeans(na.omit(dat[,i]), n_states)$centers
    }
    colnames(inits) <- colnames(dat)
    for(j in 1:length(dat)){
      init_list[[dat_names[j]]]<-list(mean = inits[,j]+ runif(n_states,-0.05,0.05),
                                      sd= rep(runif(1,0.8,1.2),n_states))
    }

    #setting up model
    hidden <- MarkovChain$new(data = dat, n_states = n_states)
    obsv <- Observation$new(data = dat, n_states = n_states,
                            dists = dat_dist, par = init_list)

    hmm_new <- HMM$new(obs = obsv, hid = hidden)

    #fit model
    hmm_new$fit(silent = TRUE)
    out<-hmm_new$out()

    if(out$convcode < 1){
      AICs[i] <- hmm_new$AIC_conditional()

      if(AICs[i] < best_aic) {
        best_model <- hmm_new
        best_aic <- AICs[i]
        print(best_aic)
      }
    }
  }

  # model selection:
  hmm_best<-best_model
  hmm_out <- hmm_new$out()

  hmm_par <- hmm_new$par()
  hmm_states <- hmm_new$viterbi()


  hmm_aic <- hmm_new$AIC_conditional()

  # return: best model; number of regimes; regime states and time points; AICc; total data (n)
  return(list(best_mod = hmm_best,
              best_out = hmm_out,
              best_pars = hmm_par,
              best_states = hmm_states,
              best_aic = hmm_aic))
}
