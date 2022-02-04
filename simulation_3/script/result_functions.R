rm(list = ls())

## WHICH SIMULATION? 
sim <- 3
## AT WHICH POINT IS THE CORRELTAION TOO HIGHT?
too.high.corr <- 0.5

setwd()

load(file = paste0("outcome/sim", sim, ".RData"))
summary(result)

result <- droplevels(result)
result$method <- factor(as.character(result$method))

p <- length(model)-1

library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(dplyr)




all.methods <- c("MLE", "MLE.r", "ED-GRF", "ESF", "GWR", "OLS", "SPDE")
myColors <- brewer.pal(all.methods,"Dark2")
names(myColors) <- all.methods

colScale <- scale_colour_manual(name = "method",values = myColors)
fillScale <- scale_fill_manual(name = "method", values = myColors)



## MSE ####
MSE.long <- tidyr::gather(result[, c(1:2, 5+1:(3*(p+1)))], 
                          key = "type", value = "MSE", 
                          train.effect1:test.extrapolate.y)
MSE.long$variable <- sub('.*\\.', '', MSE.long$type)
MSE.long$type <- sub('.[^.]*$', '', MSE.long$type)

MSE.long$type <- factor(MSE.long$type, 
                        levels = c("train", "test.interpolate", "test.extrapolate"),
                        labels = c(paste0(c("train", "test.interpolate", "test.extrapolate"), 
                                          " (n = ", c(0.5, 0.25, 0.25)*n, ")")))
MSE.long$variable <- factor(MSE.long$variable, 
                            levels = c(paste0("effect", 1:p), "y"))



MSE.long$method <- as.character(MSE.long$method)
MSE.long <- 
  MSE.long[MSE.long$method %in%  
             c("MLE.profile", "MLE.profile.pc", "SPDE.INLA", "ESF", "GWR") ,]
MSE.long$method <- factor(MSE.long$method, 
                          levels = c("MLE.profile", "MLE.profile.pc", "SPDE.INLA", "ESF", "GWR"),
                          labels = c("MLE", "MLE.r", "SPDE", "ESF", "GWR"))

MSE.long$method <- droplevels(MSE.long$method)


# without OLS or GWR
ggplot(MSE.long[!(MSE.long$method %in% c("OLS", "GWR")), ], 
       aes(x = method, y = MSE, color = method)) +
  geom_boxplot(width = 0.5) +
  facet_wrap(variable~type, ncol = 3, scales = "free_y") + 
  colScale + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE) +
  ggtitle(paste0("MSE of SVC Model with ", p, " covariables"), 
          subtitle = paste0("simulations: 100")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom", 
         axis.text.x.bottom = element_blank()) + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_MSE_wo_OLS_GWR.png"), 
       height = 10*p/3, width = 10)



# only y without OLS or GWR
ggplot(MSE.long[!(MSE.long$method %in% c("OLS", "GWR")) & 
                  (MSE.long$variable == "y"), ], 
       aes(x = method, y = MSE, color = method)) +
  facet_grid(~type, scales = "free") + 
  colScale + 
  geom_boxplot()  +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE) +
  ggtitle(paste0("MSE of SVC Model with ", p, " covariables"), 
          subtitle = paste0("simulations: 100")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom", 
        axis.text.x.bottom = element_blank()) + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_y_MSE_wo_OLS_GWR.png"), 
       height = 10, width = 10)


## RMSE ####

RMSE.long <- MSE.long

RMSE.long$RMSE <- sqrt(RMSE.long$MSE)


if (sim == 3) {
  
  RMSE.long1 <- RMSE.long %>% 
    filter(variable %in% paste0("effect", 1:4)) %>%
    droplevels()
  
  RMSE.long2 <- RMSE.long %>% 
    filter(variable %in% paste0("effect", 5:8)) %>%
    droplevels()
  
  RMSE.long3 <- RMSE.long %>% 
    filter(!(variable %in% paste0("effect", 1:8))) %>%
    droplevels()
  
  ggplot(RMSE.long1, aes(x = method, y = RMSE, color = method)) +
    facet_wrap(variable~type, scales = "free", ncol = 3) + 
    colScale + 
    geom_boxplot() + 
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
                 show.legend = TRUE) +
    ggtitle(paste0("RMSE of SVC Model with ", p, " covariables"), 
            subtitle = paste0("simulations: 100")) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          legend.position="bottom") + 
    theme_bw()
  
  ggsave(filename = paste0("plots/sim", sim, "_RMSE_all_1.png"), 
         height = 10*4/3, width = 10)
  
  ggplot(RMSE.long2, aes(x = method, y = RMSE, color = method)) +
    facet_wrap(variable~type, scales = "free", ncol = 3) + 
    colScale + 
    geom_boxplot() + 
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
                 show.legend = TRUE) +
    ggtitle(paste0("RMSE of SVC Model with ", p, " covariables"), 
            subtitle = paste0("simulations: 100")) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          legend.position="bottom") + 
    theme_bw()
  
  ggsave(filename = paste0("plots/sim", sim, "_RMSE_all_2.png"), 
         height = 10*4/3, width = 10)
  
  ggplot(RMSE.long3, aes(x = method, y = RMSE, color = method)) +
    facet_wrap(variable~type, scales = "free", ncol = 3) + 
    colScale + 
    geom_boxplot() + 
    stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
                 show.legend = TRUE) +
    ggtitle(paste0("RMSE of SVC Model with ", p, " covariables"), 
            subtitle = paste0("simulations: 100")) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          legend.position="bottom") + 
    theme_bw()
  
  ggsave(filename = paste0("plots/sim", sim, "_RMSE_all_3.png"), 
         height = 10*3/3, width = 10)
} 


ggplot(RMSE.long, aes(x = method, y = RMSE, color = method)) +
  facet_wrap(variable~type, ncol = 3, scales = "free_y") + 
  geom_boxplot(aes(color = method), width = 0.5) +
  colScale + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE) +
  ggtitle(paste0("RMSE of SVC Model with ", p, " covariables"), 
          subtitle = paste0("simulations: 100")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom") + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_RMSE_all.png"), 
       height = 10*p/3, width = 10)




# without OLS or GWR
ggplot(RMSE.long[!(RMSE.long$method %in% c("OLS", "GWR")), ], 
       aes(x = method, y = RMSE)) +
  facet_wrap(variable~type, ncol = 3, scales = "free_y") + 
  geom_boxplot(aes(color = method), width = 0.5) +
  colScale + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE, aes(color = method)) +
  facet_wrap(variable~type, ncol = 3, scales = "free_y") + 
  ggtitle(paste0("RMSE of SVC Model with ", p, " covariables"), 
          subtitle = paste0("simulations: 100")) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom") + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_RMSE_wo_OLS_GWR.png"), 
       height = 10*p/3, width = 10)



## parameters ####

para.sel <- rbind(result[, c(1:2, ncol(result)-(3*p):0)], 
                  c(NA, NA, init.true))
para.sel$method <- as.character(para.sel$method)
para.sel$method[is.na(para.sel$method)] <- "true"
para.sel <- para.sel[!is.na(para.sel$range.1), ]
para.sel <- para.sel[
  para.sel$method %in%
    c("MLE.profile", "MLE.profile.pc", "SPDE.INLA", "ESF", "GWR", "true"), ]

para.sel$method <- factor(para.sel$method, 
                          levels = c("MLE.profile", "MLE.profile.pc", "SPDE.INLA", "ESF", "GWR", "true"),
                          labels = c("MLE", "MLE.r", "SPDE", "ESF", "GWR", "true"))
para.sel$method <- droplevels(para.sel$method)


if (sim ==3) {
  parameter <- tidyr::gather(para.sel, 
                             key = "parameter", value = "value", 
                             range.1:mean.10)
} else {
  parameter <- tidyr::gather(para.sel, 
                             key = "parameter", value = "value", 
                             range.1:mean.3)
}

parameter$covariable <- sub('.*\\.', '', parameter$parameter)
parameter$parameter <- sub('.[^.]*$', '', parameter$parameter)
parameter$parameter[parameter$parameter == ""] <- "var"

parameter$parameter[parameter$parameter == "var"] <- "variance"
parameter$parameter <- factor(parameter$parameter, 
                              levels = c("range", "variance", "mean"))
parameter$covariable <- factor(parameter$covariable, 
                              levels = c(1:p, "nugget"))


ggplot(parameter[parameter$method != "MLE.rand.pars", ], 
       aes(x = method, y = value)) +
  facet_grid(parameter~covariable, scales = "free_y") + 
  colScale + 
  geom_boxplot(data = parameter[parameter$method != "true" &
                                  parameter$method != "MLE.rand.pars", ],
               aes(color = method)) + 
  geom_hline(data = parameter[parameter$method == "true", ], aes(yintercept = value)) +
  stat_summary(data = parameter[parameter$method != "true", ], 
               fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE, aes(color = method)) + 
  ggtitle("Estimated parameters", subtitle = "simulations: 100, black line is the true value") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom") + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_parameters.png"), 
       height = 10, width = 10)


temp <- parameter[parameter$method %in% c("SPDE", "MLE.r", "true") ,]
temp <- droplevels(temp)


ggplot(temp, aes(x = method, y = value)) +
  facet_grid(parameter~covariable, scales = "free_y") + 
  colScale + 
  geom_boxplot(data = parameter[parameter$method %in% c("SPDE", "MLE.r"), ],
               aes(color = method)) + 
  geom_hline(data = parameter[parameter$method == "true", ], aes(yintercept = value)) +
  stat_summary(data = parameter[parameter$method %in% c("SPDE", "MLE.r"), ], 
               fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE, aes(color = method)) + 
  ggtitle("Estimated parameters", subtitle = "simulations: 100, black line is the true value") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom") + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_parameters_wo_MLE.png"), 
       height = 10, width = 10)

ggplot(parameter[parameter$method != "MLE.prof", ], aes(x = method, y = value, color = method)) +
  geom_boxplot(data = parameter[parameter$method != "true" &
                                  parameter$method != "MLE.prof", ] %>% droplevels()) + 
  facet_grid(parameter~covariable, scales = "free_y") + 
  colScale + 
  geom_hline(data = parameter[parameter$method == "true", ], aes(yintercept = value)) +
  stat_summary(data = parameter[parameter$method != "true" &
                                  parameter$method != "MLE.prof", ] %>% droplevels(), 
               fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE) + 
  ggtitle("Estimated parameters", subtitle = "simulations: 100, black line is the true value") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom") + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_parameters_only_with_pc.png"), 
       height = 10, width = 10)



parameter2 <- parameter %>% 
  filter(method != "MLE.rand.pars", 
         covariable == 2) %>%
  droplevels()

ggplot(parameter2, aes(x = method, y = value, color = method)) +
  geom_boxplot(data = parameter2[parameter2$method != "true" , ]) + 
  facet_wrap(.~parameter, scales = "free_y") + 
  colScale + 
  geom_hline(data = parameter2[parameter2$method == "true", ], aes(yintercept = value)) +
  stat_summary(data = parameter2[parameter2$method != "true", ], 
               fun.y = mean, geom = "point", shape = 18, size = 3,
               show.legend = TRUE) + 
  ggtitle("Estimated parameters", subtitle = "simulations: 100, black line is the true value") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.position="bottom") + 
  theme_bw()

ggsave(filename = paste0("plots/sim", sim, "_SVC2_parameters.png"), 
       height = 10/p, width = 10)




