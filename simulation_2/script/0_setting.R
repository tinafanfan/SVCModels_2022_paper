
beta1.function <- function(s1,s2){
    r <- 3 + 2*sin(2*pi*s1)*sin(2*pi*s2) + 
        exp(-10*s1^2-15*(s2-1)^2)+exp(-15*(s1-1)^2-15*(s2-1)^2)
    return(r)
}
beta2.function <- function(s1,s2){
    # g1 <- 0.3; g2 <- 0.4
    # r <- 2/(pi*g1*g2)*exp(-(s1-0.2)^2/g1^2-(s2-0.3)^2/g2^2)+
    #     -2/(pi*g1*g2)*exp(-(s1-0.7)^2/g1^2-(s2-0.8)^2/g2^2)
    r <- - 2 * (s1 + s2) + 2
    return(r)
}
beta3.function <- function(s1,s2){
    # r <- 1.9*(1.45 + exp(s1)*sin(13*(s1-0.6)^2))*exp(-s2)*sin(7*s2)
    r <- - 2 * (-s1 + s2) 
    # r <- sin(s1) + cos(s2)
    return(r)
}

grid_max <- 1
grid_length <- 50
grid_n <- grid_length^2
y <- x <- seq(0, grid_max, length.out = grid_length)

coord_X <- c();coord_Y <- c()
for(i in 1:length(x)){
    for(j in 1:length(y)){
        coord_X <- c(coord_X,x[i]); coord_Y <- c(coord_Y,y[j])
    }
}

beta_0_mt <- -0.5
beta_0_vt <- as.vector(beta_0_mt)

beta_1_vt <- beta1.function(coord_X, coord_Y)
beta_1_mt <- matrix(beta_1_vt, length(x), length(y))
image.plot(x,y,beta_1_mt)

beta_2_vt <- beta2.function(coord_X, coord_Y)
beta_2_mt <- matrix(beta_2_vt, length(x), length(y))
image.plot(x,y,beta_2_mt)

beta_3_vt <- beta3.function(coord_X, coord_Y)
beta_3_mt <- matrix(beta_3_vt, length(x), length(y))
image.plot(x,y,beta_3_mt)


sim1 <- geoR::grf(grid_n, mean = 0, grid = "reg",
                   nx = grid_length, ny = grid_length, 
                   xlims = c(0,1),  ylims = c(0,1),
                   cov.model = "exponential", cov.pars=c(1, 0.02), nug=0)
x1_vt <- sim1$data
x1_mt <- matrix(x1_vt, length(x), length(y))
image.plot(x,y,x1_mt)


sim2 <- geoR::grf(grid_n, mean = 0, grid = "reg",
                   nx = grid_length, ny = grid_length, 
                   xlims = c(0,1), ylims = c(0,1),
                   cov.model = "exponential", cov.pars=c(1, 0.02), nug=0)
x2_vt <- sim2$data
x2_mt <- matrix(x2_vt, length(x), length(y))
image.plot(x,y,x2_mt)

sim3 <- geoR::grf(grid_n, mean = 0, grid = "reg",
                   nx = grid_length, ny = grid_length, 
                   xlims = c(0,1), ylims = c(0,1),
                   cov.model = "exponential", cov.pars=c(1, 0.02), nug=0)
x3_vt <- sim3$data
x3_mt <- matrix(x3_vt, length(x), length(y))
image.plot(x,y,x3_mt)


# sim4 <- geoR::grf(grid_n, mean = 0, grid = "reg",
#                   nx = grid_length, ny = grid_length, 
#                   xlims = c(0,1), ylims = c(0,1),
#                   cov.model = "exponential", cov.pars=c(1, 0.02), nug=0)
# x4_vt <- sim4$data
# x4_mt <- matrix(x4_vt, length(x), length(y))
# 
# 
# sim5 <- geoR::grf(grid_n, mean = 0, grid = "reg",
#                   nx = grid_length, ny = grid_length, 
#                   xlims = c(0,1), ylims = c(0,1),
#                   cov.model = "exponential", cov.pars=c(1, 0.02), nug=0)
# x5_vt <- sim3$data
# x5_mt <- matrix(x5_vt, length(x), length(y))
# 
# 
# sim6 <- geoR::grf(grid_n, mean = 0, grid = "reg",
#                   nx = grid_length, ny = grid_length, 
#                   xlims = c(0,1), ylims = c(0,1),
#                   cov.model = "exponential", cov.pars=c(1, 0.02), nug=0)
# x6_vt <- sim6$data
# x6_mt <- matrix(x6_vt, length(x), length(y))


mu_mt <- beta_0_mt + x1_mt*beta_1_mt + x2_mt*beta_2_mt + x3_mt*beta_3_mt
p_mt <- 1/(1 + exp(-mu_mt)) 
p_vt <- matrix(p_mt, ncol = 1)
image.plot(x,y,mu_mt)
image.plot(x,y,p_mt)

var_y_mt <- matrix(NA, nrow = nrow(p_mt), ncol = ncol(p_mt))
true_y_mt <- matrix(NA, nrow = nrow(p_mt), ncol = ncol(p_mt))

for(i in 1:length(x)){
    for(j in 1:length(y)){
        var_y_mt[i,j] <- rbinom(n=1, size = 1, p_mt[i,j])
        true_y_mt[i,j] <- ifelse(p_mt[i,j] > 0.5, 1, 0)
        
    }
}
# image.plot(x,y,var_y_mt)
# image.plot(x,y,true_y_mt)
true_y_vt <- matrix(true_y_mt, ncol = 1, byrow = T)

coef <- data.frame(beta_0 = matrix(beta_0_mt, ncol=1, byrow = F),
                   beta_1 = matrix(beta_1_mt, ncol=1, byrow = F),
                   beta_2 = matrix(beta_2_mt, ncol=1, byrow = F),
                   beta_3 = matrix(beta_3_mt, ncol=1, byrow = F))
data <- data.frame(var_y  = matrix(var_y_mt, ncol = 1, byrow = F),
                   var_x1 = matrix(x1_mt, ncol = 1, byrow = F),
                   var_x2 = matrix(x2_mt, ncol = 1, byrow = F),
                   var_x3 = matrix(x3_mt, ncol = 1, byrow = F)
                   # ,
                   # var_x4 = matrix(x4_mt, ncol = 1, byrow = F),
                   # var_x5 = matrix(x5_mt, ncol = 1, byrow = F),
                   # var_x6 = matrix(x6_mt, ncol = 1, byrow = F)
                   )
space <- data.frame(coord_X = coord_X, coord_Y = coord_Y)
data_sp <- SpatialPointsDataFrame(coords = space, data = data)






