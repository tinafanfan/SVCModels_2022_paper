mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/application/table/table4")

setwd(folder_path)

result_gwr <- matrix(NA, 20, 6)
load("GWR_result_5_times_sample.RData")
result_gwr[5,] <- result[5,]
load("GWR_result_2_times_sample.RData")
result_gwr[2,] <- result[2,]
load("GWR_result_6_times_sample.RData")
result_gwr[6,] <- result[6,]
load("GWR_result_8_times_sample.RData")
result_gwr[8,] <- result[8,]
load("GWR_result_10_times_sample.RData")
result_gwr[10,] <- result[10,]
load("GWR_result_11_times_sample.RData")
result_gwr[11,] <- result[11,]
load("GWR_result_14_times_sample.RData")
result_gwr[14,] <- result[14,]
load("GWR_result_15_times_sample.RData")
result_gwr[15,] <- result[15,]
load("GWR_result_18_times_sample.RData")
result_gwr[18,] <- result[18,]
load("GWR_result_19_times_sample.RData")
result_gwr[19,] <- result[19,]

load("ESF_result_10_times_sample.rdata")
result_esf <- result
load("OURS_result_10_times_sample_all.rdata")
result_ours <- result
