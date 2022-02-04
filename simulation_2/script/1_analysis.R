Packages <- c("matrixcalc", "GWmodel","matrixcalc","fields","geoR")
install.packages(Packages)
invisible(lapply(Packages, library, character.only = TRUE))


setwd("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_2")
source(file = "function_updated.R")
total.times = 5
beta0_rmse <- matrix(NA, total.times, 2)
beta1_rmse <- matrix(NA, total.times, 2)
beta2_rmse <- matrix(NA, total.times, 2)
beta3_rmse <- matrix(NA, total.times, 2)
c.entropy_p  <- matrix(NA, total.times, 2)
c.entropy_b  <- matrix(NA, total.times, 2)
running.time <- matrix(NA, total.times, 2)
correct.model <- matrix(0, total.times, 3)

# ---- analysis----
for(times in 1:total.times){
    set.seed(times)
    # data generate ----
    source(file = "0_setting.R")
    my.formula <- as.formula(var_y ~ var_x1 + var_x2 + var_x3)
    a <- Sys.time()
    # 1. mbs ---- 
    model.mbs <- mrbs.multi.svc.glm(formula = my.formula, 
                                    data.train = data,
                                    space.train = coordinates(data_sp),
                                    criterion_setting = "bic", 
                                    M.up = 20)
    b <- Sys.time()
    running.time[times, 1] <- b-a
    image.plot(x,y,matrix(model.mbs$coefficients.alpha[,2],length(x), length(y)))
    # image.plot(x,y,matrix(model.mbs$coefficients.alpha[,2],length(x), length(y)))
    # image.plot(x,y,matrix(p.hat,length(x), length(y)))

    
    beta0_rmse[times, 1] <- RMSE(model.mbs$coefficients.alpha[,1], beta_0_vt)
    beta1_rmse[times, 1] <- RMSE(model.mbs$coefficients.alpha[,2], beta_1_vt)
    beta2_rmse[times, 1] <- RMSE(model.mbs$coefficients.alpha[,3], beta_2_vt)
    beta3_rmse[times, 1] <- RMSE(model.mbs$coefficients.alpha[,4], beta_3_vt)
    
    p.hat <- model.mbs$fitted.values
    c.entropy_p [times, 1] <- -mean(p_vt*log(p.hat) + (1-p_vt)*log(1-p.hat))
    c.entropy_b [times, 1] <- -mean(true_y_vt*log(p.hat) + (1-true_y_vt)*log(1-p.hat))
    

    # 2. ggwr ----
    my.formula <- as.formula(var_y ~ var_x1 + var_x2 + var_x3)
    a <- Sys.time()
    bw.ggwr <- bw.ggwr(my.formula, data = data_sp,
                       family = "binomial", approach = "CV", kernel = "exponential")
    model.ggwr <- gwr.generalised(my.formula, data = data_sp, family = "binomial",
                                  bw = bw.ggwr, kernel = "gaussian", cv = F)
    b <- Sys.time()
    running.time[times, 2] <- b-a
    
    beta0_rmse[times, 2] <- RMSE(model.ggwr$SDF$Intercept, beta_0_vt)
    beta1_rmse[times, 2] <- RMSE(model.ggwr$SDF$var_x1, beta_1_vt)
    beta2_rmse[times, 2] <- RMSE(model.ggwr$SDF$var_x2, beta_2_vt)
    beta3_rmse[times, 2] <- RMSE(model.ggwr$SDF$var_x3, beta_3_vt)

    # image.plot(x,y,matrix(model.ggwr$SDF$var_x1,length(x), length(y)))
    p.hat <- model.ggwr$SDF$yhat
    c.entropy_p [times, 2] <- -mean(p_vt*log(p.hat) + (1-p_vt)*log(1-p.hat))
    c.entropy_b [times, 2] <- -mean(true_y_vt*log(p.hat) + (1-true_y_vt)*log(1-p.hat))
    
    # 3. glm ----
    # modle.glm <- glm(my.formula, data=data, family = "binomial")
    # p.hat <- modle.glm$fitted.values
    # RMSE(beta_0_vt, modle.glm$coefficients[1])
    # RMSE(beta_1_vt, modle.glm$coefficients[2])
    # RMSE(beta_2_vt, modle.glm$coefficients[3])
    # RMSE(beta_3_vt, modle.glm$coefficients[4])
    # beta0_rmse[times, 3] <- RMSE(beta_0_vt, modle$coefficients[1])
    # beta1_rmse[times, 3] <- RMSE(beta_1_vt, modle$coefficients[2])
    # c.entropy [times, 3] <- -mean(p_vt*log(p.hat) + (1-p_vt)*log(1-p.hat))
}
# ---- result----
apply(beta0_rmse, 2, mean)
apply(beta1_rmse, 2, mean)
apply(c.entropy, 2, mean)
apply(beta0_rmse, 2, sd)
apply(beta1_rmse, 2, sd)
apply(c.entropy, 2, sd)
save.image(file = "sim_glm_2.RData")
