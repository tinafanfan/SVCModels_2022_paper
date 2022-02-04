

# read the csv data and select a range
load_data <- function(folder_path){
    
    setwd(paste0(folder_path, "/data"))
    # read the csv file export from TGOS (after cleaning)
    import.data <- read.csv(file = "108_greatertaipericity_utf8.csv", header = T)
    
    # select transactions
    raw.data <- import.data[which(import.data$縣市 == "臺北市" | import.data$縣市 == "新北市"),]
    raw.data <- raw.data[which(raw.data$移轉層次_ADD > 0),]
    
    ### location range
    loc.domain <- which(raw.data$X > 282000 & raw.data$X < 320000 & raw.data$Y > 2755000 & raw.data$Y < 2790000)
    raw.data <- raw.data[loc.domain,]
    
    # select variables
    raw.data <- raw.data[, c(7,9,10,11,12,13)]
    colnames(raw.data) <- c("price_unit","build_area","age","X","Y","floor_th")
    
    # adjust values
    raw.data$price_unit <- raw.data$price_unit/10000
    
    # attribute variables + space variables
    raw.data <- raw.data[,c("price_unit","build_area","age","floor_th","X","Y")]
    
    return(raw.data)
}





