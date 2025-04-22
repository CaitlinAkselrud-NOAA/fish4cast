get_surrogate_models <- function(iml_obj = predictor_squid,
                                 full_data = in_data,
                                 train_data = train_final,
                                 test_data = test_simple,
                                 test_features = features)
{
  # 1) global surrogate models, aka parsimony
  # A) GLM

  # B) Decision tree
  tree <- TreeSurrogate$new(iml_obj, maxdepth = 5)
  plot(tree)
  # predict with decision tree:
  tree_pred <- tree$predict(full_data)
  # CIA: something doesn't work right - plotting only NA...?

  # 2) LIME (also a parsimony method)
  # what features best predict a single row of data using the fitted rf model?
  lime.explain <- LocalModel$new(iml_obj,
                                 x.interest = test_features[1, ])
  lime.explain$results
  plot(lime.explain)

  for(i in 2:dim(test_features)[1])
  {
    lime.explain$explain(test_features[i, ])
    plot(lime.explain)
    # CIA: save output
  }



}
