# vis for simulation_3_smoothfunction

library(ggplot2)
library(colorRamps)

surface <- function(x, y){
    a <- runif(1, -3, 3)
    b <- runif(1, -3, 3)
    c <- runif(1, -3, 3)
    d <- runif(1, -3, 3)
    e <- runif(1, -3, 3)
    a*x + b*y + c*x^2 + d*y^2 + e*x*y
}    
loc <- expand.grid(seq(0,1, length.out = 100), seq(0,1, length.out = 100))

data <- cbind(surface(loc[,1], loc[,2]), loc)
colnames(data) <- c("value", "x", "y")



df <- data.frame(x = data[, "x"],
                 y = data[, "y"],
                 coef = data[, "value"])
ggplot(df, aes(x, y)) +
    geom_point(aes(color = coef)) +
    scale_colour_gradientn(colors = blue2green2red(400))


sp.effs <- SpatialPointsDataFrame(coords = locs, 
                                  data = data.frame(effect1 = surface(locs[,1], locs[,2]),
                                                    effect2 = surface(locs[,1], locs[,2]),
                                                    effect3 = surface(locs[,1], locs[,2]),
                                                    effect4 = surface(locs[,1], locs[,2]),
                                                    effect5 = rep(1, nrow(locs)),
                                                    effect6 = rep(1, nrow(locs)),
                                                    effect7 = rep(1, nrow(locs)),
                                                    effect8 = rep(0, nrow(locs)),
                                                    effect9 = rep(0, nrow(locs)),
                                                    effect10 = rep(0, nrow(locs))))
