GWmodel.fit <- function(dat) {
    
    # formula
    form = as.formula(y ~ x0 + x1 + x2 - 1)
    # CV for bandwidth
    BW <- bw.gwr(formula = form, data = dat)
    
    GWR_model <- gwr.basic(formula = form, data = dat, bw = BW)
    # GWR.model <- gwr.predict(formula = form,
    #                          data = dat[ids == "train", ],
    #                          predictdata = dat,
    #                          bw = bw.sel, 
    #                          kernel = "exponential", 
    #                          dMat1 = dm.pred, 
    #                          dMat2 = dm.train)
    
    return(GWR_model)
}

GWmodel.pred <- function(model, locs = NULL) {
    model.out <- as.matrix(as.data.frame(model$SDF))
    # beta.pred <- model.out[, substr(colnames(model.out), start = 1, stop = 1) == "X"]
    beta_hat <- model.out[,c("x0", "x1", "x2")]
    y_hat <- model.out[,c("yhat")]
    
    return(list(beta_hat = beta_hat,
                y_hat = y_hat))
}


mrbs.svc.fit <- function(dat){
    # formula
    form = as.formula(y ~ x0 + x1 + x2 - 1)
    gwr.mrts_result <- mrbs.multi.svc(formula = form,
                                      data_attrb_train = dat@data,
                                      data_space_train = dat@coords,
                                      criterion_setting = "cv_mse", 
                                      M_up = 70,
                                      log_normal = FALSE)
}
mrbs.svc.pred <- function(model, locs = NULL){
    
    g.all  <- mrts(knot = model$data_space_train, k = max(max(model$M_hat),3), x = locs)
    beta.pred <- svc_est(g.all, model$M_hat, model$model$coefficients)
    
    return(list(beta_hat = beta.pred,
                y_hat = model$Y_hat))
}



SVC.MLE.profile_pc.fit <- function(dat, taper.range, init, p.dens = NULL, cl = NULL) {
    train.data <- dat@data
    y.col <- which(colnames(train.data) == "y")
    
    control <- SVC_mle_control(init       = init[1:(2*p+1)], 
                               tapering   = taper.range, 
                               profileLik = TRUE, 
                               lower      = c(rep(1e-10, 2*p+1)), 
                               upper      = c(rep(Inf, 2*p+1)),
                               pc.prior   = c(0.075, 0.05, sqrt(0.25), 0.05))

    model.fit <- SVC_mle(y       = as.numeric(train.data[, y.col]),
                         X       = as.matrix(train.data[, -y.col]),
                         locs    = as.matrix(dat@coords),
                         control = control)
    
}
SVC.MLE.pred <- function(model) {
    beta_hat = model$fitted[,1:3] + matrix(rep(coef(model), nrow(model$fitted)), ncol = p, byrow = TRUE)
    y_hat = apply(model$data$X*beta_hat,1,sum)

    return(list(beta_hat = beta_hat,
                y_hat = y_hat))
}