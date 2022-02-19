
mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_3")

setwd(paste0(folder_path, "/figure/figure7"))
load("surface_100sim_10range.rdata")

result$method <- as.character(result$method)
result$method[which(result$method == "GWR.GWmod")] <- "GWR"
result$method[which(result$method == "MLE.profile.pc")] <- "MLE"
result$method[which(result$method == "mrbs.svc_aic")] <- "Ours(AIC)"
result$method[which(result$method == "mrbs.svc_bic")] <- "Ours(BIC)"
result$method[which(result$method == "mrbs.svc_cv")] <- "Ours(CV)"
result$method <- factor(result$method, levels = c("Ours(AIC)", "Ours(BIC)", "Ours(CV)", "GWR", "MLE" ,"ESF"))

# visualization ----
setwd(paste0(folder_path, "/figure/figure7"))
library(ggplot2)
colnames(result)[16] <- "train.y"
df = data.frame(method = result$method, RISE = result[,"train.y"])

p <- ggplot(df, aes(x=method, y=RISE, fill = method)) + 
    geom_boxplot()+
    xlab("") +
    ylab("") +
    coord_cartesian(ylim=c(0,1.5)) +
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
          legend.key = element_blank())

png(paste0("RISE_y.png"), width = 180, height = 150, units = "mm", res = 300)
plot.new()
print(p)
dev.off()

y_lim_ls <- list(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1),
                 c(0,0.35),c(0,0.35),c(0,0.35),c(0,0.35),c(0,0.35))

for(i in 1:10){

    nm <- paste0("effect",i)
    df = data.frame(method = result$method, RISE = result[,nm])
    
    box_MSE <- list()
    box_MSE[[i]] <- ggplot(df, aes(x=method, y=RISE, fill = method)) + 
        geom_boxplot() +
        xlab("") +
        ylab("") +
        coord_cartesian(ylim=y_lim_ls[[i]]) +
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
              legend.key = element_blank())
    
    png(paste0("RISE_SVC_", i, ".png"), width = 180, height = 150, units = "mm", res = 300)
    plot.new()
    print(box_MSE[[i]])
    dev.off()
}




