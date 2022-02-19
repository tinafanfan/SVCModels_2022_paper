mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/simulation/simulation_3")

setwd(paste0(folder_path,"/table/table1"))

load(file = "controlpts_100sim_10range.rdata")
apply(result[401:500,24:28],2,function(x) 1- length(which(x == 0))/100) # aic
apply(result[501:600,24:28],2,function(x) 1- length(which(x == 0))/100) # aic
apply(result[301:400,24:28],2,function(x) 1- length(which(x == 0))/100) # aic
