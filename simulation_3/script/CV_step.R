CV.step <- function(CV.func, data, effs, ids, extr.pars = NULL, cl, data_all_surface, sp.effs_surface) {
  # ids: TRUE = training, FALSE = testing
  method = names(CV.func)
  CV.func = CV.func[[1]]
  # fit model
  fit.time <- system.time({
    model <- CV.func$fit(dat  = data, ids  = ids)
  })
  
#   if(method == "mrbs.svc"){
#     M_hat = model$M_hat
#   }else{
#     M_hat = rep(0, p)
#   }
    
#   # predict betas
#   pred.time <- system.time({
#     beta.pred <- CV.func$pred(model = model,
#                               locs  = data@coords)
#   })
  
  
#   ## compute errors
#     print(class(beta.pred))
#   if(class(beta.pred) == "numeric"){
#       beta.pred = matrix(beta.pred, ncol = 1)
#   }
#   p <- ncol(beta.pred)
  
#   # betas
#   diffs <- beta.pred-effs@data[, 1:p]

  
  
#   MSE.betas <- lapply(unique(ids), function(type){
#     apply(diffs[ids == type, ], 2,
#           function(x) mean(x^2))
#   })
  
#   MSE.betas <- Reduce(c, MSE.betas)
#   names(MSE.betas) <- paste0(rep(unique(ids), each = p), ".", 
#                              names(MSE.betas))
      
#   # response y
#   sq.res <- (apply((beta.pred*data@data[, 1:p]), 1, sum)-apply(data@data[, 1:p]*effs@data[, 1:p], 1, sum))^2
      
#   MSE.y <- sapply(unique(ids), function(type){
#     mean(sq.res[ids == type])
#   })
#   names(MSE.y) <- paste0(names(MSE.y), ".y")
  
#   hyper.pars <- if (is.null(extr.pars)) rep(NA, 3*p+1) else extr.pars(model)

#   c(MSE.betas, MSE.y, summary(fit.time)[1], summary(pred.time)[1], M_hat)
      
      
      
      
  # MISE    
  pred.time <- system.time({
    beta.pred <- CV.func$pred(model = model,
                              locs  = data_all_surface)
  })  
  print(dim(sp.effs_surface))
  print(dim(beta.pred))
    print(class(sp.effs_surface))
    print(class(beta.pred))
  diffs <- beta.pred-sp.effs_surface@data # 10000x2
    print(class(diffs))
    print(dim(diffs))
  MISE <- apply(diffs, 2, function(x) mean(x^2))   
    print(MISE)
  c(MISE)    
}
