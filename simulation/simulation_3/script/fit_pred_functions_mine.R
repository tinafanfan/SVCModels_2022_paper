# dat = sp.data
# ids = ids
setwd(paste0(mian_folder_path, "/application/script"))
source("inference.R")
source("models.R")
source("utils.R")

mrbs.svc.fit_aic <- function(dat, ids){
    data_attrb_train <- dat@data[ids == "train", ]
    data_space_train <- dat@coords[ids == "train", ]
    form <- as.formula(paste0("y~", paste(paste0("X", 1:p), collapse = "+"), "-1"))
    mrbs.svc.model <- mrbs.multi.svc(formula = form,
                                     data_attrb = data_attrb_train,
                                     data_space = data_space_train,
                                     criterion_setting = "aic", # c("aic", "bic", "cv_rmse", "cv_mse", "cv_mae")
                                     M_up = 50,
                                     log_normal = FALSE)
}
mrbs.svc.fit_bic <- function(dat, ids){
    data_attrb_train <- dat@data[ids == "train", ]
    data_space_train <- dat@coords[ids == "train", ]
    form <- as.formula(paste0("y~", paste(paste0("X", 1:p), collapse = "+"), "-1"))
    mrbs.svc.model <- mrbs.multi.svc(formula = form,
                                     data_attrb = data_attrb_train,
                                     data_space = data_space_train,
                                     criterion_setting = "bic", # c("aic", "bic", "cv_rmse", "cv_mse", "cv_mae")
                                     M_up = 50,
                                     log_normal = FALSE)
}
mrbs.svc.fit_cv <- function(dat, ids){
    data_attrb_train <- dat@data[ids == "train", ]
    data_space_train <- dat@coords[ids == "train", ]
    form <- as.formula(paste0("y~", paste(paste0("X", 1:p), collapse = "+"), "-1"))
    mrbs.svc.model <- mrbs.multi.svc(formula = form,
                                     data_attrb = data_attrb_train,
                                     data_space = data_space_train,
                                     criterion_setting = "cv_mse", # c("aic", "bic", "cv_rmse", "cv_mse", "cv_mae")
                                     M_up = 50,
                                     log_normal = FALSE)
}
mrbs.svc.pred <- function(model, locs = NULL){
    
    g.all  <- mrts(knot = model$data_space_train, k = max(max(model$M_hat),3), x = locs)
    beta.pred <- svc_est(g.all, model$M_hat, model$model$coefficients)
 
}

