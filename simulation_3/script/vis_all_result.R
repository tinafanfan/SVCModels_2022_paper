
setwd("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_3_smoothfunction/fine_setup_10range")
load("all_100sim_10range_aic.RData")
result_aic <- result
result_aic$method <- "Ours(AIC)"
load("all_100sim_10range_bic.RData")
result_bic <- result
result_bic$method <- "Ours(BIC)"
setwd("~/Documents/3_Research/201810_GWR/script/1_simulation/simulation_3_smoothfunction/fine_setup_10range")
load("all_100sim_10range.RData")
result$method <- as.character(result$method)
result$method[which(result$method == "mrbs.svc")] <- "Ours(CV)"
result$method[which(result$method == "GWR.GWmod")] <- "GWR"
result$method[which(result$method == "MLE.profile.pc")] <- "MLE"
result <- rbind(result, result_aic, result_bic)
result$method <- factor(result$method, levels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "GWR" ,"ESF", "MLE"))


library(ggplot2)

df = data.frame(method = result$method, RMSE = sqrt(result[,"train.y"]))

p <-  ggplot(df, aes(x=method, y=RMSE, fill = method)) + 
    geom_boxplot() +
    xlab("") +
    ylab("") +
    coord_cartesian(ylim=c(0,0.85)) +
    scale_x_discrete(labels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "GWR", "ESF", "MLE")) +
    scale_fill_manual(name = "",values=c("white","white","white", "grey50", "grey30", "black")) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          panel.border = element_rect(colour = "black", fill=NA), 
          legend.position = "none",
          axis.text.x=element_text(size=14), 
          axis.text.y=element_text(size=14), 
          axis.title=element_text(size=14), 
          plot.margin = margin(0.05,0.05,-0.35,-0.3, "cm"),
          legend.key = element_blank()
    )

png(paste0("MSE_y.png"), width = 180, height = 150, units = "mm", res = 300)
plot.new()
print(p)
dev.off()

for(i in 1:10){

    nm <- paste0("train.effect",i)
    df = data.frame(method = result$method, RMSE = sqrt(result[,nm]))
    
    box_MSE <- list()
    box_MSE[[i]] <- ggplot(df, aes(x=method, y=RMSE, fill = method)) + 
        geom_boxplot() +
        xlab("") +
        ylab("") +
        coord_cartesian(ylim=c(0,0.85)) +
        scale_x_discrete(labels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "GWR", "ESF", "MLE")) +
        scale_fill_manual(name = "",values=c("white","white","white", "grey50", "grey30", "black")) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              panel.border = element_rect(colour = "black", fill=NA), 
              legend.position = "none",
              axis.text.x=element_text(size=14), 
              axis.text.y=element_text(size=14), 
              axis.title=element_text(size=14), 
              plot.margin = margin(0.05,0.05,-0.35,-0.3, "cm"),
              legend.key = element_blank()
        )
    
    png(paste0("MSE_SVC_", i, ".png"), width = 180, height = 150, units = "mm", res = 300)
    plot.new()
    print(box_MSE[[i]])
    dev.off()
}
sum(result[301:400,"6"] == 0)
sum(result[301:400,"7"] == 0)
sum(result[301:400,"8"] == 0)
sum(result[301:400,"9"] == 0)
sum(result[301:400,"10"] == 0)

sum(result[401:500,"6"] == 0)
sum(result[401:500,"7"] == 0)
sum(result[401:500,"8"] == 0)
sum(result[401:500,"9"] == 0)
sum(result[401:500,"10"] == 0)

sum(result[501:600,"6"] == 0)
sum(result[501:600,"7"] == 0)
sum(result[501:600,"8"] == 0)
sum(result[501:600,"9"] == 0)
sum(result[501:600,"10"] == 0)
