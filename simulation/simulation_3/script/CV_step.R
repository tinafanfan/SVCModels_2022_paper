CV.step <- function(CV.func, data, effs, ids, extr.pars = NULL, cl, data_all_surface, sp.effs_surface) {
    
  # ids: TRUE = training, FALSE = testing
  method = names(CV.func)
  CV.func = CV.func[[1]]
    
  # fit model
  fit.time <- system.time({
    model <- CV.func$fit(dat  = data, ids  = ids)
  })
  
  if(method == "mrbs.svc"){
    M_hat = model$M_hat
  }else{
    M_hat = rep(0, p)
  }
    
  # predict betas
  pred.time <- system.time({
    beta.pred <- CV.func$pred(model = model,
                              locs  = data@coords[ids == "pred", ])
  })
  
  
  ## compute errors
  p <- ncol(beta.pred)
  
  # betas
  diffs <- beta.pred-effs@data[ids == "pred", 1:p]
  RISE.betas <- apply(diffs, 2, function(x) sqrt(sum(0.0001*x^2)))
  print(RISE.betas)

  # response y
  res <- apply((beta.pred*data@data[ids == "pred", 1:p]), 1, sum)-apply(data@data[ids == "pred", 1:p]*effs@data[ids == "pred", 1:p], 1, sum)
  RISE.y <- sqrt(sum(0.0001*res^2))
  print(RISE.y)
  
  hyper.pars <- if (is.null(extr.pars)) rep(NA, 3*p+1) else extr.pars(model)

  c(RISE.betas, RISE.y, summary(fit.time)[1], summary(pred.time)[1], M_hat)

}
