

locations.random <- function(n, seed, side.length = 1) {
  # setting seed
  set.seed(seed)
  # sample locations on a (unit) square
  x <- as.data.frame(matrix(runif(2*n, min = 0, max = side.length),
                            ncol = 2))
  colnames(x) <- c("x", "y")

  # return
  list(s    = x,
       type = "random.sq",
       vars = c(n = n, seed = seed, side.length = side.length))
}


# computes a perturbated grid (PG), that is a grid constisting of
# sqrt(n)*sqrt(n) unit squares. in each subgrid there one observation
#   first, of side length sqrt(n), where delta (in [0, 0.5]) equals the
#          restriction of the perturbation, i.e.
#            delta =   0: point can lie anywhere in unit square
#            delta = 0.5: no perturbation
#   then, the (PG) is scaled on the given side length
#
# In order to make this approach work, one has to give an integer square number.
# Otherwise, it will be set closest, smallest square number.
locations.perGrid <- function(n, delta, seed, side.length = 1) {
  # setting seed
  set.seed(seed)
  # checking for sqaured values
  n.sqrt <- as.integer(sqrt(n))
  if (n.sqrt*n.sqrt != n) {
    warning(paste0("n = ", n, " is not a square. It has been set to ", n.sqrt*n.sqrt, "!"))
  }

  # compute basic lattice
  mids <- 1:n.sqrt-0.5
  mids <- expand.grid(mids, mids)

  # sample perturbations
  stopifnot((delta>=0)&(delta<=0.5))
  pert <- runif(2*n.sqrt*n.sqrt, min = delta-0.5, max = 0.5-delta)

  # add together
  x <- mids + matrix(pert, ncol = 2)

  # scaling
  x <- (side.length/n.sqrt)*x
  colnames(x) <- c("x", "y")

  # return
  list(s    = x,
       type = "PG",
       vars = c(n = n.sqrt*n.sqrt, delta = delta, seed = seed, side.length = side.length))

}


samp.Z.covars <- function(n, p, seed) {
  # setting seed
  set.seed(seed)

  # return
  list(df   = as.data.frame(matrix(c(rep(1, n),
                                     rnorm(n*(p-1), 0, 1)),
                                   ncol = p)),
       vars = list(n = n, p = p, seed = seed))
    
#   list(df   = as.data.frame(matrix(rnorm(n*p, 0, 1),
#                                    ncol = p)),
#        vars = list(n = n, p = p, seed = seed))   
   
}


samp.effects <- function(locs, seed){
    # setting seed
    set.seed(seed)

#     # sample effects at corresponding locations
#     effs <- lapply(model, RandomFields::RFsimulate,
#                    x = locs$s[, 1],
#                    y = locs$s[, 2])
#     # make spatial points DF
#     effs <- as.data.frame(Reduce(cbind, effs))
#     colnames(effs) <- c(paste0("effect", 1:length(model)), "x", "y")
#     coords <- SpatialPoints(effs[, c("x", "y")])
#     sp.effs <- SpatialPointsDataFrame(coords, effs[, 1:length(model)])
    ###################

    
    
#     effs$effect5  <- rep(1, nrow(effs))
#     effs$effect6  <- rep(1, nrow(effs))
#     effs$effect7  <- rep(1, nrow(effs))
#     effs$effect8  <- rep(0, nrow(effs))
#     effs$effect9  <- rep(0, nrow(effs))
#     effs$effect10 <- rep(0, nrow(effs))    
#     sp.effs <- SpatialPointsDataFrame(coords, effs[, c("effect1","effect2","effect3","effect4",
#                                                        "effect5","effect6","effect7","effect8",
#                                                       "effect9","effect10")])
    ###################    
    surface <- function(x, y){
        a <- runif(1, -5, 5)
        b <- runif(1, -5, 5)
        c <- runif(1, -5, 5)
        d <- runif(1, -5, 5)
        e <- runif(1, -5, 5)
        a*x + b*y + c*x^2 + d*y^2 + e*x*y
    }    
    loc = as.matrix(locs$s)
    sp.effs <- SpatialPointsDataFrame(coords = loc, 
                                      data = data.frame(effect1 = surface(loc[,1], loc[,2]),
                                                        effect2 = surface(loc[,1], loc[,2]),
                                                        effect3 = surface(loc[,1], loc[,2]),
                                                        effect4 = surface(loc[,1], loc[,2]),
                                                        effect5 = surface(loc[,1], loc[,2]),
                                                        effect6 = rep(0, nrow(loc)),
                                                        effect7 = rep(0, nrow(loc)),
                                                        effect8 = rep(0, nrow(loc)),
                                                        effect9 = rep(0, nrow(loc)),
                                                        effect10 = rep(0, nrow(loc))))
    sp.effs
}

