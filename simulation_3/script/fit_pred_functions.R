


## GWR
GWmodel.fit <- function(dat, ids) {
  # distance matrices
  dm.train <- gw.dist(dp.locat = coordinates(dat[ids == "train", ]))
  dm.pred <- gw.dist(dp.locat = coordinates(dat[ids == "train", ]),
                     rp.locat = coordinates(dat))
  
  # formula
  p <- ncol(dat)-1
  form <- as.formula(paste0("y~", paste(paste0("X", 1:p), collapse = "+"), "-1"))
  # CV for bandwidth
  bw.sel <- bw.gwr(formula = form, data = dat[ids == "train", ],
                   approach = "aic", kernel = "bisquare", adaptive = FALSE,
                   dMat = dm.train)
  GWR_model <- gwr.basic(formula = form, data = dat[ids == "train", ], bw = bw.sel)
    
  # for surface  
#   GWR.model <- gwr.predict(formula = form,
#                    data = dat[ids == "train", ],
#                    predictdata = dat,
#                    bw = bw.sel, 
#                    kernel = "bisquare", 
#                    dMat1 = dm.pred, 
#                    dMat2 = dm.train)
}

GWmodel.pred <- function(model, locs = NULL) {
  model.out <- as.matrix(as.data.frame(model$SDF))
  beta.pred <- model.out[, 1:10]
    
# GWR.model <- gwr.predict(formula = formula(model$lm$terms),
#                data = model$lm$x,
#                predictdata = dat,
#                bw = bw.sel, 
#                kernel = "bisquare", 
#                dMat1 = dm.pred, 
#                dMat2 = dm.train)  
    
}

spgwr.fit <- function(dat, ids) {
  # formula
  p <- ncol(dat)-1
  form <- as.formula(paste0("y~", paste(paste0("X", 1:p), collapse = "+"), "-1"))
  
  # CV for bandwidth
  bw.sel <- gwr.sel(form, data = dat[ids == "train", ])
  
  GWR.model <- gwr(form,
                   data = dat[ids == "train", ],
                   fit.points = coordinates(dat),
                   bandwidth = bw.sel)
  
}

spgwr.pred <- function(model, locs = NULL) {
  model.out <- as.matrix(as.data.frame(model$SDF))
  beta.pred <- model.out[, substr(colnames(model.out), start = 1, stop = 1) == "X"]
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

SPDE.pars <- function(model) {
  model$parameters
}

SPDE.fit <- function(dat, ids) {

  train.data <- dat@data[ids == "train", ]
  train.locs <- dat@coords[ids == "train", ]

  y.col <- ncol(train.data)


  # create mesh
  # domain <- inla.nonconvex.hull(as.matrix(dat@coords))
  # mesh <- inla.mesh.2d(boundary = domain, max.edge = c(4/sqrt(nrow(dat@coords))))
  #
  max.edge = c(4/sqrt(nrow(dat@coords)))

  ## suggested by Haakon Bakka
  mesh <- inla.mesh.2d(
    loc=dat@coords,
    offset = c(0.02, 0.3),
    max.edge=c(max.edge, max.edge*3),
    # discretization accuracy
    cutoff=max.edge/5)

  ## model
  # prepare Matern class model
  spde <- inla.spde2.pcmatern(mesh = mesh,
                              alpha = 3/2,                            # for exponential covariance function
                              prior.range = c(2*0.075, 0.05),         # gives same pc priors (times two due to practical range)
                              prior.sigma = c(sqrt(0.25), 0.05)) # -"-

  A.SVC <- lapply(1:ncol(train.data[, -y.col]), function(j){
    inla.spde.make.A(mesh    = mesh,
                     loc     = as.matrix(train.locs),
                     weights = as.numeric(train.data[, j]))
  })
  names(A.SVC) <- paste0("A.", colnames(train.data[, -y.col]))

  idx.SVC <- lapply(1:ncol(dat[, -y.col]), function(j){
    inla.spde.make.index( paste0("idx.", colnames(train.data)[j]), spde$n.spde)
  })
  names(idx.SVC) <- paste0("idx.", colnames(train.data[, -y.col]))

  idx.SVC[[y.col]] <- train.data[, -y.col]

  y <- as.numeric(train.data[, y.col])



  # combining data into INLA stack
  stk.dat <- inla.stack(data = list(y = y),
                        A    = c(A.SVC, 1), tag = "est",
                        effects = idx.SVC)
  model.formula <- formula(paste0("y~", paste(paste0("f(idx.",
                                                     colnames(train.data[, -y.col]),
                                                     ", model = spde)"),
                                              collapse = " + "), " -1"))

  # fitting
  spde_mod <- inla(formula           = model.formula,
                   data              = inla.stack.data(stk.dat),
                   verbose           = TRUE,
                   control.predictor = list(A       = inla.stack.A(stk.dat),
                                            compute = TRUE))



  # calcuate hyper parameters
  f.hyper <- function(x) {
    xx <- c((x[-1])^rep(c(1, 2), 3), 1/x[1])
    # divide by two due to "practical range"
    xx[c(1, 3, 5)] <- xx[c(1, 3, 5)]/2
    
    return(xx)
  }


  SPDE.pars <- c(f.hyper(spde_mod$summary.hyperpar$`0.5quant`), 
                 sapply(spde_mod$summary.random, function(x) median(x$mean)))
    
  list(model = spde_mod,
       mesh = mesh,
       parameters = SPDE.pars)
}

SPDE.pred <- function(model, locs) {

  # posterior fields
  proj <- inla.mesh.projector(model$mesh, loc = as.matrix(locs))
  beta_pred <- lapply(model$model$summary.random, function(sum_rand){
    inla.mesh.project(proj, sum_rand$mean)
  })
  Reduce(cbind, beta_pred)
}

#
#
#
# ## INLA
# INLA.time <- system.time({
#   Nrow <- Ncol <- sqrt(n)
#   grd <- GridTopology(cellcentre.offset = c(1/(Nrow*2), 1/(Ncol*2)),
#                       cellsize = c(1/Nrow, 1/Ncol), cells.dim = c(Nrow, Ncol))
#
#   polygrd <- as(grd, "SpatialPolygons")
#   idxpp <- over(SpatialPoints(locs$s), polygrd)
#
#   nb <- poly2nb(polygrd, queen = FALSE, row.names = 1:n)
#   adj <- nb2mat(nb, style = "B")
#   adj <- as(adj, "dgTMatrix")
#
#   ##Create neighborhood matrix from shape file and create INLA graph
#   nb_mat <- nb2mat(nb, style="B",zero.policy=TRUE)
#   inla_gr = inla.read.graph(nb_mat)
#
#   ##Auxiliary variables for use in inla function
#   d.data <- sp.data@data
#   d.data[-even.ids, ] <- NA
#   d.data$ind_X1=1:n
#   d.data$ind_X2=1:n
#   d.data$ind_X3=1:n
#
#   ##Fit models
#   formula = y ~ f(ind_X1 , X1,model="besag",graph=adj)+
#     f(ind_X2 , X2,model="besag",graph=adj)+
#     f(ind_X3 , X3,model="besag",graph=adj)-1
#
#   SVC_inla_mod = inla(formula,family="gaussian",
#                       data=d.data,
#                       control.compute=list(graph=T, dic=T))
# })
#
# ##The posterior fields (means and also other quantities) can be accessed as follows
# INLA.fit <- cbind(
#   SVC_inla_mod$summary.random[['ind_X1']][['mean']],##Intercept
#   SVC_inla_mod$summary.random[['ind_X2']][['mean']],
#   SVC_inla_mod$summary.random[['ind_X3']][['mean']]
#   )
#
#
#
#
#
# ## compute MSE of methods
#
# # OLS
# MSE.OLS <- apply((OLS.beta[even.ids,]-sp.effs@data[even.ids, -(p+1)])^2, 2, mean)
# PMSE.OLS <- apply((OLS.beta[even.ids,]-sp.effs@data[-even.ids, -(p+1)])^2, 2, mean)
#
# # spmoran
# MSE.moran <- apply((rv_res$b_vc-sp.effs@data[even.ids, -(p+1)])^2, 2, mean)
# PMSE.moran <- apply((beta_pred$b_vc-sp.effs@data[-even.ids, -(p+1)])^2, 2, mean)
#
# # INLA
# SE.INLA <- (INLA.fit-sp.effs@data[, -(p+1)])^2
# MSE.INLA <- apply(SE.INLA[even.ids, ], 2, mean)
# PMSE.INLA <- apply(SE.INLA[-even.ids, ], 2, mean)
#
# # SVCModels
# SE.SVCModels <- (effs.fit2-sp.effs@data[, -(p+1)])^2
# MSE.SVCModels <- apply(SE.SVCModels[even.ids, ], 2, mean)
# PMSE.SVCModels <- apply(SE.SVCModels[-even.ids, ], 2, mean)
#
# # SVCModels tapered
# SE.taper.SVCModels <- (effs.taper-sp.effs@data[, -(p+1)])^2
# MSE.taper.SVCModels <- apply(SE.taper.SVCModels[even.ids, ], 2, mean)
# PMSE.taper.SVCModels <- apply(SE.taper.SVCModels[-even.ids, ], 2, mean)
#
#
# c(MSE.OLS              = MSE.OLS,
#   PMSE.OLS             = PMSE.OLS,
#   MSE.moran            = MSE.moran,
#   PMSE.moran           = PMSE.moran,
#   MSE.INLA             = MSE.INLA,
#   PMSE.INLA            = PMSE.INLA,
#   MSE.SVCModels        = MSE.SVCModels,
#   PMSE.SVCModels       = PMSE.SVCModels,
#   MSE.taper.SVCModels  = MSE.taper.SVCModels,
#   PMSE.taper.SVCModels = PMSE.taper.SVCModels,
#   OLS.time             = OLS.fit.time["user.self"],
#   spmoran.fit.time     = spmoran.fit.time["user.self"],
#   spmoran.pred.time    = spmoran.pred.time["user.self"],
#   INLA.time            = INLA.time["user.self"],
#   SVCModels.fit.time   = fit.time["user.self"],
#   SVCModels.pred.time  = pred.time["user.self"],
#   SVCModels.fit.taper.time  = fit.taper.time["user.self"],
#   SVCModels.pred.taper.time = pred.taper.time["user.self"],
#   SVC.pars             = model.fit$optim.output$par,
#   SVC.taper.pars       = model.taper.fit$optim.output$par)
#
#
#
# ## results ####
# res <- data.frame(t(res.par))
#
# save(res, file = "../pred_test_outcome/comp.RData")
#
# n.methods <- 5
#
# # MSE error
# error.df <- data.frame(MSE    = as.numeric(as.matrix(res[, 1:(2*p*n.methods)])),
#                        coef   = as.factor(rep(rep(1:p, each = n.rep), 2*n.methods)),
#                        method = as.factor(rep(c("OLS", "spmoran", "INLA", "SVC", "SVC.taper"), each = 2*p*n.rep)),
#                        type   = as.factor(rep(rep(c("fit", "pred"), each = p*n.rep), 5)))
#
# ggplot(error.df, aes(x  = method, y = MSE)) + geom_boxplot() + facet_grid(coef~type, scales = "free")
#
# ggsave(file = "../pred_test_outcome/MSE.png")
#
# # time
# time.df <- data.frame(time = as.numeric(as.matrix(res[, (2*p*n.methods) + 1:8])),
#                       type = as.factor(rep(c("OLS.time", "spmoran.fit", "spmoran.pred", "INLA", "SVC.fit", "SVC.pred", "SVC.taper.fit", "SVC.taper.pred"), each = n.rep)))
#
# ggplot(time.df, aes(x  = type, y = time)) + geom_boxplot() + scale_y_log10()
#
# ggsave(file = "../pred_test_outcome/runtime.png")
#
# # parameter estimates
# par.names <- c(paste0(rep(c("scale", "var"), p),
#                       rep(1:p, each = 2)),
#                "nugget", paste0("mu", 1:p))
#
# df <- data.frame(value     = c(as.vector(t(res[, (2*p*n.methods+8) + 1:(2*(3*p+1))])), hyper.init, rep(0, p)),
#                  parameter = par.names,
#                  type      = c(rep("fitted", 2*n.rep*(3*p+1)),
#                                rep("true", 3*p+1)),
#                  method    = c(rep(c("full", "tapered"), each = n.rep*(3*p+1)),
#                                rep(NA, 3*p+1)))
#
# ggplot(df, aes(x = parameter, y = value, fill = method)) +
#   geom_boxplot(data = df[df$type == "fitted",]) +
#   geom_point(data = df[df$type == "true",], aes(color = 3), show.legend = FALSE) +
#   facet_wrap(.~parameter, scales = "free") +
#   stat_summary(data = df[df$type == "fitted",], fun.y=mean, colour="black", geom="point",shape=18, size=3,position = position_dodge(width = 0.75), show.legend = FALSE)
#   #ylim(lower = -0.6, upper = 1.2) +
#
# ggsave(file = "../pred_test_outcome/parameters.png")
