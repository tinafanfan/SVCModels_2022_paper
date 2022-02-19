


## GWR
GWmodel.fit <- function(dat, ids) {
  # distance matrices
  dm.train <- gw.dist(dp.locat = coordinates(dat[ids == "train", ]))
  dm.pred <- gw.dist(dp.locat = coordinates(dat[ids == "train", ]),
                     rp.locat = coordinates(dat[ids == "pred", ]))
  # formula
  p <- ncol(dat)-1
  form <- as.formula(paste0("y~", paste(paste0("X", 1:p), collapse = "+"), "-1"))
  # CV for bandwidth
  bw.sel <- bw.gwr(formula = form, data = dat[ids == "train", ],
                   approach = "aic", kernel = "bisquare", adaptive = FALSE,
                   dMat = dm.train)
#   GWR_model <- gwr.basic(formula = form, data = dat[ids == "train", ], bw = bw.sel)
    print(dim(dat[ids == "train", ]))
    print(dim(dat[ids == "pred", ]))
  # for surface  
  GWR.model <- gwr.predict(formula = form,
                           data = dat[ids == "train", ],
                           predictdata = dat[ids == "pred", ],
                           bw = bw.sel,
                           kernel = "bisquare",
                           dMat1 = dm.pred, 
                           dMat2 = dm.train)
}

GWmodel.pred <- function(model, locs = NULL) {
  model.out <- as.matrix(as.data.frame(model$SDF))
  beta.pred <- model.out[, 1:10]
}


## ESF
spmoran.fit <- function(dat, ids) {
  train.data <- dat@data[ids == "train", ]
  y.col <- ncol(train.data)
  meig <- meigen(coords = dat@coords[ids == "train", ])
  rv_res <- resf_vc(y = as.numeric(train.data[, y.col]),
                    x = as.matrix(train.data[, -y.col]), meig = meig)
  list(fit = rv_res, meig = meig)
}

spmoran.beta.pred <- function(model, locs = NULL){
  meig.pred <- meigen0(meig = model$meig, coords = locs)
  beta_pred <- predict0_vc(mod = model$fit, meig0 = meig.pred)

  beta_pred$b_vc
}


## SVC MLE model
SVC.MLE.fit <- function(dat, ids, taper.range, init, p.dens = NULL, cl = NULL) {
  train.data <- dat@data[ids == "train", ]
  y.col <- ncol(train.data)
  
  control <- SVC_mle_control(init      = init, 
                             tapering  = taper.range, 
                             lower     = c(rep(1e-10, 2*p+1), rep(-Inf, p)),
                             parallel = if (is.null(cl)) {
                               NULL
                             } else {
                               list(
                                 cl = cl, 
                                 forward = FALSE,
                                 loginfo = TRUE
                               )
                             })
  
  model.fit <- SVC_mle(y       = as.numeric(train.data[, y.col]),
                       X       = as.matrix(train.data[, -y.col]),
                       locs    = as.matrix(dat@coords[ids == "train", ]),
                       control = control)
                      
}

SVC.MLE.profile.fit <- function(dat, ids, taper.range, init, p.dens = NULL, cl = NULL) {
  train.data <- dat@data[ids == "train", ]
  y.col <- ncol(train.data)
  
  control <- SVC_mle_control(init       = init[1:(2*p+1)], 
                             tapering   = taper.range, 
                             profileLik = TRUE, 
                             lower      = c(rep(1e-10, 2*p+1)), 
                             upper      = c(rep(Inf, 2*p+1)),
                             parallel = if (is.null(cl)) {
                               NULL
                             } else {
                               list(
                                 cl = cl, 
                                 forward = FALSE,
                                 loginfo = TRUE
                               )
                             })
  
  model.fit <- SVC_mle(y       = as.numeric(train.data[, y.col]),
                       X       = as.matrix(train.data[, -y.col]),
                       locs    = as.matrix(dat@coords[ids == "train", ]),
                       control = control)
  
}

SVC.MLE.profile_pc.fit <- function(dat, ids, taper.range, init, p.dens = NULL, cl = NULL) {
  train.data <- dat@data[ids == "train", ]
  y.col <- ncol(train.data)
  
  control <- SVC_mle_control(init       = init[1:(2*p+1)], 
                             tapering   = taper.range, 
                             profileLik = TRUE, 
                             lower      = c(rep(1e-10, 2*p+1)), 
                             upper      = c(rep(Inf, 2*p+1)),
                             pc.prior   = c(0.075, 0.05, sqrt(0.25), 0.05),
                             parallel   = list(cl = cl, forward = FALSE, loginfo = TRUE))
#     control <- SVC_mle_control()
  
  model.fit <- SVC_mle(y       = as.numeric(train.data[, y.col]),
                       X       = as.matrix(train.data[, -y.col]),
                       locs    = as.matrix(dat@coords[ids == "train", ]),
                       control = control)
  
}

SVC.MLE.profile_OLS_pc.fit <- function(dat, ids, taper.range, init, p.dens = NULL) {
  train.data <- dat@data[ids == "train", ]
  y.col <- ncol(train.data)
  
  control <- SVC_mle_control(init       = init[1:(2*p+1)], 
                             tapering   = taper.range, 
                             profileLik = TRUE, 
                             lower      = c(rep(1e-10, 2*p+1)), 
                             upper      = c(rep(Inf, 2*p+1)),
                             pc.prior   = c(0.075, 0.05, sqrt(0.25), 0.05),
                             mean.est   = "OLS")
  
  
  model.fit <- SVC_mle(y       = as.numeric(train.data[, y.col]),
                       X       = as.matrix(train.data[, -y.col]),
                       locs    = as.matrix(dat@coords[ids == "train", ]),
                       control = control)
  
}

SVC.MLE.pred <- function(model, locs) {
  SVC_nomean <- predict(object = model, newlocs = locs)[, 1:p]
  
  SVC_nomean + matrix(rep(coef(model), nrow(locs)), 
                      ncol = p, byrow = TRUE)
}

SVC.pars <- function(model) {
  c(cov_par(model), coef(model))
}

