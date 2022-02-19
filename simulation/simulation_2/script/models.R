ggwr.fit <- function(dat) {
    
    # formula
    my.formula <- as.formula(y ~ x0 + x1 + x2+ x3  - 1)
    bw.ggwr <- bw.ggwr(my.formula, 
                       data = dat,
                       family = "binomial", 
                       approach = "CV", 
                       kernel = "exponential")
    model.ggwr <- gwr.generalised(my.formula, 
                                  data = dat, 
                                  family = "binomial",
                                  bw = bw.ggwr, 
                                  kernel = "exponential", 
                                  cv = F)
    
    
    return(model.ggwr)
}

ggwr.pred <- function(model, locs = NULL) {
    
    model.out <- as.matrix(as.data.frame(model$SDF))
    beta_hat <- model.out[,c("x0", "x1", "x2", "x3")]
    y_hat <- model.out[,c("yhat")]
    
    return(list(beta_hat = beta_hat,
                y_hat = y_hat))
}

mrbs.svc.fit_aic <- function(dat){
    # formula
    form = as.formula(y ~ x0 + x1 + x2 + x3 - 1)
    gwr.mrts_result <- mrbs.multi.svc(formula = form,
                                      data_attrb_train = dat@data,
                                      data_space_train = dat@coords,
                                      criterion_setting = "aic", #cv_ent
                                      M_up = 20,
                                      log_normal = FALSE)
}
mrbs.svc.fit_bic <- function(dat){
    # formula
    form = as.formula(y ~ x0 + x1 + x2 + x3 - 1)
    gwr.mrts_result <- mrbs.multi.svc(formula = form,
                                      data_attrb_train = dat@data,
                                      data_space_train = dat@coords,
                                      criterion_setting = "bic", #cv_ent
                                      M_up = 20,
                                      log_normal = FALSE)
}
mrbs.svc.fit_cv <- function(dat){
    # formula
    form = as.formula(y ~ x0 + x1 + x2 + x3 - 1)
    gwr.mrts_result <- mrbs.multi.svc(formula = form,
                                      data_attrb_train = dat@data,
                                      data_space_train = dat@coords,
                                      criterion_setting = "cv_ent", #cv_ent
                                      M_up = 20,
                                      log_normal = FALSE)
}
mrbs.svc.pred <- function(model, locs = NULL){
    
    g.all  <- mrts(knot = model$data_space_train, k = max(max(model$M_hat),3), x = locs)
    beta.pred <- svc_est(g.all, model$M_hat, model$model$coefficients)
    
    return(list(beta_hat = beta.pred,
                y_hat = model$Y_hat))
}


