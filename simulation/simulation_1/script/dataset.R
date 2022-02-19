generate_data <- function(grid_n,
                          grid_min,
                          grid_max,
                          grid_length){

    ##########
    # regular
    x <- seq(grid_min,grid_max, length.out = grid_length)
    y <- seq(grid_min,grid_max, length.out = grid_length)
    beta_1 <- matrix(NA, length(x), length(y))
    beta_2 <- matrix(NA, length(x), length(y))
    coord_X <- c();coord_Y <- c()
    for(i in 1:length(x)){
        for(j in 1:length(y)){
            beta_1[i,j] <- 1 + (1/12) * (x[i] + y[j])
            beta_2[i,j] <- 1 + (1/324)*(36-(6-x[i]/2)^2)*(36-(6-y[j]/2)^2)
            # f <- runif(5, -1, 1)
            # g <- runif(5, -1, 1)
            # beta_1[i,j] <- f[1]*x[i] + f[2]*y[i] + f[3]*x[i]^2 + f[4]*y[i]^2 + f[5]*x[i]*y[i]
            # beta_2[i,j] <- g[1]*x[i] + g[2]*y[i] + g[3]*x[i]^2 + g[4]*y[i]^2 + g[5]*x[i]*y[i]
            coord_X <- c(coord_X,x[i])
            coord_Y <- c(coord_Y,y[j])
        }
    }



    beta_0 <-  matrix(3, length(x), length(y))

    x1 <- matrix(rnorm(grid_n, 0, 1), grid_length, grid_length)
    x2 <- matrix(rnorm(grid_n, 0, 1), grid_length, grid_length)
    error <- matrix(rnorm(grid_n, 0, 0.5^2), grid_length, grid_length)



    true_y <- beta_0 + x1*beta_1 + x2*beta_2
    true_y_vt <- matrix(true_y, ncol=1, byrow = F)
    var_y <- beta_0 + x1*beta_1 + x2*beta_2 + error
    coef <- data.frame(beta_0 = matrix(beta_0, ncol=1, byrow = F),
                       beta_1 = matrix(beta_1, ncol=1, byrow = F),
                       beta_2 = matrix(beta_2, ncol=1, byrow = F))
    data <- data.frame(y = matrix(var_y, ncol = 1, byrow = F),
                       x0 = matrix(1, ncol = 1, byrow = F),
                       x1 = matrix(x1, ncol = 1, byrow = F),
                       x2 = matrix(x2, ncol = 1, byrow = F))
    space <- data.frame(coord_X = coord_X,
                        coord_Y = coord_Y)
    data_sp <- SpatialPointsDataFrame(coords = space,
                                      data = data)
    
    #####
    # irregular
    # xy <- cbind(runif(625, 0, 25), runif(625, 0, 25))
    # beta_1 <- c()
    # beta_2 <- c()
    # for(i in 1:nrow(xy)){
    #     beta_1[i] <- 1 + (1/12) * (xy[i,1] + xy[i,2])
    #     beta_2[i] <- 1 + (1/324)*(36-(6-xy[i,1]/2)^2)*(36-(6-xy[i,2]/2)^2)
    # }
    # coord_X <- xy[,1]
    # coord_Y <- xy[,2]
    # 
    # 
    # beta_0 <-  rep(3, grid_n)
    # 
    # x1 <- rnorm(grid_n, 0, 1)
    # x2 <- rnorm(grid_n, 0, 1)
    # error <- rnorm(grid_n, 0, 0.5^2)
    # 
    # 
    # 
    # true_y <- beta_0 + x1*beta_1 + x2*beta_2
    # var_y <- beta_0 + x1*beta_1 + x2*beta_2 + error
    # coef <- data.frame(beta_0 = beta_0,
    #                    beta_1 = beta_1,
    #                    beta_2 = beta_2)
    # data <- data.frame(y = var_y,
    #                    x0 = matrix(1, ncol = 1, byrow = F),
    #                    x1 = x1,
    #                    x2 = x2)
    # space <- data.frame(coord_X = coord_X,
    #                     coord_Y = coord_Y)
    # data_sp <- SpatialPointsDataFrame(coords = space, 
    #                                   data = data)
    #####
    return(list(data_sp = data_sp,
                coef = coef,
                true_y_vt = true_y_vt))
    
}

