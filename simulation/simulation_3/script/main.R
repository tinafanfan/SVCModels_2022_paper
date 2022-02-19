rm(list = ls())
mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_3")
setwd(paste0(folder_path, "/script"))

## =================================
## Parameters to change ####
## =================================

# Which simulation? (definition below)
simulation <- 3

library(RandomFields)
library(parallel)

library(spmoran)
library(varycoef)
library(GWmodel)

source("help_functions.R")
source("sample_functions.R")
source("fit_pred_functions.R")
source("CV_step.R")
source("fit_pred_functions_mine.R")

## =================================
## Set CV-Parameters ####
## =================================
# get number of covariates and observations
p <- 10
n <- 625

# other parameters for simulations
delta          <- 0.5 # parameter for perturbated grid
n.rep          <- 100 # number of runs 
tapering.range <- if (n>2500) 0.2 else NULL
init.par       <- c(scale = 0.075, var = 0.3, mu = 0) # initial value for SVC-MLE

# sample seeds for each simulation in run
seeds <- c(328, 649, 730, 308, 352, 560, 368, 534, 103, 167, 425, 789, 703, 644, 142, 905, 815, 762, 889, 235, 535,
                 646, 121, 490, 809,  48, 804, 323, 965, 658, 553, 428, 596,  89, 508, 686, 744, 507, 263, 354, 729,  25,
                 784,  65, 214, 749, 314, 318, 492, 853, 857, 362,  45, 321, 823, 993, 209,  22,  55, 461, 851, 429,  71,
                 668,  19, 622, 928, 773, 435,  57, 718, 451, 436, 798, 232, 879, 438, 210, 238, 244,  12, 661,  69, 606,
                 446, 687, 607, 557, 107, 246, 175, 119, 638, 675, 586, 685, 714, 715, 797, 457)

## =================================
## Setting up CV ####
## =================================

# initial values for MLE
init.pars <- unname(c(rep(init.par[1:2], p), 
                      init.par["var"], rep(init.par[3], p)))

# actual functions for methods fitting and predicting
CV.functions <- list(
  GWR.GWmod = list(fit  = GWmodel.fit,
                   pred = GWmodel.pred,
                   extr = NULL)
  ,
  MLE.profile.pc = list(fit  =
                     function(dat, ids){
                       SVC.MLE.profile_pc.fit(dat = dat, ids = ids, tapering.range, init = init.pars, p.dens = NULL, cl = cl)},
                   pred = SVC.MLE.pred,
                   extr = SVC.pars)
  ,
  ESF       = list(fit  = spmoran.fit,
                   pred = spmoran.beta.pred,
                   extr = NULL)
  ,
  mrbs.svc_aic = list(fit  = mrbs.svc.fit_aic,
                   pred = mrbs.svc.pred,
                   extr = NULL)
  ,
  mrbs.svc_bic = list(fit  = mrbs.svc.fit_bic,
                   pred = mrbs.svc.pred,
                   extr = NULL)
  ,
  mrbs.svc_cv = list(fit  = mrbs.svc.fit_cv,
                   pred = mrbs.svc.pred,
                   extr = NULL)    
  )


## result data frame
result <- expand.grid(sim = 1:n.rep,
                      method = names(CV.functions),
                      loc.fun = c("random"),
                      covar.fun = c("independent"))
result$seed <- seeds

## =================================
## Starting up CV ####
## =================================

env <- ls()

n.cores <- detectCores() - 1
cl <- makeCluster(n.cores)
clusterEvalQ(cl, {
  library(sp)
  library(RandomFields)
  library(spam)
  library(spmoran)
  library(varycoef)
  library(spdep)
  library(spgwr)
  library(GWmodel)
})
clusterExport(cl, varlist = env)
setDefaultCluster(cl = cl)

res.par <- sapply(1:nrow(result), function(i) {
    
  print(paste0("---------- Start Simulation ", i, " ----------"))
  ## LOCATIONS
  
  # which function to sample locations?
  locs <- switch(as.character(result$loc.fun[i]),
                 perGrid = locations.perGrid(n, delta, result$seed[i]),
                 random  = locations.random(n, result$seed[i]))
  
  colnames(locs$s) <- c("x", "y")
    
  ids <- rep("train", times = nrow(locs$s))
    
  ##### for MISE #####  
  data_all_surface <- expand.grid(seq(0,1,length.out = 100),seq(0,1,length.out = 100)) 
  data_all_surface <- as.matrix(data_all_surface)
  colnames(data_all_surface) <- c("x", "y")
  locs$s <- rbind(locs$s, data_all_surface)
  ids <- c(ids, rep("pred", times = nrow(data_all_surface)))
  ####################    
    
    
    
  ## EFFECTS
  
  # sample effects from given model
  sp.effs <- samp.effects(locs, seed = result$seed[i]+1)
  ## DATA
  
  # which function to sample data?
  sp.data <- switch(as.character(result$covar.fun[i]),
                    independent = {
                      temp <- samp.Z.covars(nrow(locs$s), p, result$seed[i]+2)
                      c.sp.data(locs, temp)})
  
  sp.data <- effs.times.data(sp.effs, sp.data)

  output = CV.step(CV.func   = CV.functions[result$method[i]], 
          data      = sp.data,
          effs      = sp.effs, 
          ids       = ids, 
          cl = cl,
          data_all_surface = data_all_surface$s,
          sp.effs_surface = sp.effs_surface         
          )
    return(output)
  
})

stopCluster(cl)


result <- cbind(result, as.data.frame(t(res.par)))

setwd(paste0(folder_path, "/figure/figure7"))
save(result, n, file = paste0("surface_100sim_10range.RData"))
