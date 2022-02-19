

# function that combines locations and data to spatial data
c.sp.data <- function(locs, data) {
  coords <- SpatialPoints(locs$s) 
  sp.data <- SpatialPointsDataFrame(coords, data$df) 
  
  sp.data
}

effs.times.data <- function(sp.effs, sp.data) {
  
  # check that multiplication makes sense
  stopifnot(identical(
    coordinates(sp.effs),
    coordinates(sp.data)))
  
  # do multiplication and add up

#   y <- apply(cbind(sp.data@data, 1)*sp.effs@data, 1, sum)
  y <- apply(sp.data@data*sp.effs@data + 
             matrix(rnorm(nrow(sp.effs@data)*ncol(sp.effs@data), 0, 0.25), ncol = ncol(sp.effs@data)), 1, sum)
  
  # bind y to data as last column and return
  sp.data <- cbind(sp.data, y)
  colnames(sp.data@data) <- c(paste0("X", 1:(dim(sp.data)[2]-1)), "y")
  
  #return
  sp.data
}


# run optimization
prepare.optim <- function(sp.data, tapering = NULL, hyper.init = NULL) {
  
  # get dimensions
  n <- dim(sp.data)[1]
  p <- dim(sp.data)[2]-1
  
  # get tapering cut-off distance
  if (is.null(tapering)) {
    # no tapering
    d.co <- max(dist(coordinates(sp.data))) + 0.001
  } else {
    # with tapering
    d.co <- tapering
  }
  
  # get distances
  d <- nearest.dist(coordinates(sp.data),
                    coordinates(sp.data),
                    delta = d.co)
  
  # get starting values for beta
  dat <- as.data.frame(sp.data)[, 1:(p+1)]
  beta.init <- coefficients(lm(y~.-1, data = dat))
  
  # get starting values for hyper-parameters
  # if not given, set them to 1
  if (is.null(hyper.init))
    hyper.init <- rep(1, 2*p+1)
  
  # compute outer-product
  XX <- lapply(paste0("X", 1:p), 
               function(x) {
                 dat[, x] %o% dat[, x]
               })
  
  # return all 
  list(n = n, p = p, d.co = d.co, d = d, 
       init.x = c(hyper.init, beta.init),
       XX = XX, dat = dat)
}


# negative log-likelihood function for specific pars
nLL.for.pars <- function(pars, cov_func) {
  
  function(x) {
    # compute covariance matrix
    Sigma <- lapply(1:pars$p, function(j) {
      if (j == 1) {
        hyper.par <- x[1:3]
      } else {
        hyper.par <- c(x[3 + (j-2)*2 + 1:2], 0)
      }
      do.call(cov_func, list(hyper.par)) * pars$XX[[j]]
    })
    
    Sigma <- Reduce('+', Sigma)
    
    # calculate Cholesky-Decompisition
    cholS <- spam::chol.spam(Sigma)
    
    # get beta
    beta <- x[1 + 2*pars$p + 1:pars$p]
    
    resid <- pars$dat$y - (as.matrix(pars$dat[, -(pars$p+1)]) %*% beta)
    return(pars$n * log(2 * pi) + 2 * c(spam::determinant.spam.chol.NgPeyton(cholS)$modulus) + 
             sum(resid * spam::solve.spam(cholS, resid)))
  }
}


optim.SVC <- function(pars, nLL, optim.pars) {
  time <- system.time(
    {
      result <- optim(pars$init.x, fn = nLL, pars = pars, 
                      method = optim.pars$method,
                      control = optim.pars$control,
                      lower = optim.pars$lower,
                      upper = optim.pars$upper)
    })
  
  list(time = time, result = result)
}



extr.hyper.pars <- function(model) {
  unlist(sapply(model, function(RF) {
    c(RF@par.general$scale, RF@par.general$var)
  }))
}
