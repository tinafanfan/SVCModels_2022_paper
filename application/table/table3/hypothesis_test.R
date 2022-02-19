mian_folder_path = "~/Documents/3_Research/201810_GWR/script"
folder_path = paste0(mian_folder_path, "/application")

setwd(paste0(folder_path,"/script"))
source("datasets.R")
source("utils.R")
# data ----
data <- load_data(folder_path)
data.train <- data[, c(1,2,3,4)]
space.train <- data[, c(5,6)]

# prepare for model training ----
M.up = 189
p. <- 4
formula = price_unit ~ build_area + age + floor_th
formula <- as.formula(formula)
y_name <- all.vars(formula)[1]
y_ind <- which(colnames(data.train) == y_name)
Y.train <- data.train[,y_ind]
formula.modelmatrix <- as.formula(paste0("~ 1 + ", substr(paste(formula,collapse=" "), start = nchar(y_name) + 4, stop = nchar(paste(formula,collapse=" ")))))
X.train <- model.matrix(formula.modelmatrix, data.train)
Z.train <- log(Y.train)

# null model ----
M.hat <- c(189,9,21,0)
phi.train <- compute.datamatrix(X = X.train, 
                                k_can = M.hat, 
                                mrts_knot = space.train, 
                                mrts_x = space.train)
model_reduce <- lm(Z.train ~ phi.train[,-1]) 

# alternative model ----
sq = c(1,2,3,4,5,50,100)
for (i in 1:length(sq)){
    M.hat <- c(189,9,21,sq[i])
    phi.train <- compute.datamatrix(X.train, M.hat, space.train, space.train)
    model_full <- lm(Z.train ~ phi.train[,-1]) 
    
    f <- anova(model_reduce, model_full)
    print(f$`Pr(>F)`)
}


