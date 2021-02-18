library(dplyr)
library(ggplot2)
library(gridExtra)
library(Hmisc)
library(ggcorrplot)
library(randomForest)

# read in data
data_y <- readRDS("ng20012016.rds") %>% 
    subset(select = c(Date, evi))
data_x <- readRDS("cache/met.ng20012016.rds") %>% 
    subset(select = -c(lat, lon))
data_evi <- left_join(data_y, data_x, by= "Date")

#initial check #################################################################
describe(data_evi)

var_scatter_ls <- list()
var_box_ls <- list()
var_ls <- c("evi", names(data_x)[2:6])

for (var in var_ls){
    # var <- "rain"
    # var <- "tmax"
    
    y <- data_evi[[var]]
    x <- data_evi$Date
    df <- data.frame(x,y)
    
    var_scatter_ls[[var]] <- ggplot(df, aes(x,y)) +
        geom_point(color = "coral", shape = 20) +
        xlab("Date") +
        ylab(var)
    
    var_box_ls[[var]] <- ggplot(df, aes("", y))+
        geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
        geom_point(shape = 20, color = "coral", position = "jitter", size = 0.5) +
        # scale_y_continuous(limits = c(0, ifelse(var == "TotalTransfers", 10, 5))) +
        # quantile(df_ggplot$x, c(0.1, 0.95)) +
        xlab("") +
        ylab(var)
}

do.call(grid.arrange, var_scatter_ls)
do.call(grid.arrange, var_box_ls)



# relation to evi###############################################################
scatter_ls <- list()
for (var in names(data_x)[2:6]){
    # var <- "rain"
    
    x <- data_evi[[var]]
    y <- data_evi$evi
    df <- data.frame(x,y)
    scatter_ls[[var]] <- ggplot(df, aes(x, y)) +
        geom_point(color = "coral", shape = 20) +
        xlab(var)
}
do.call(grid.arrange, scatter_ls)


x_ls <- names(data_x)[2:6]
y <- "evi"
corr <- cor(cbind(data_evi[x_ls], data_evi[y]), method = "pearson",
            use = "pairwise.complete.obs")

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, 
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


# model fitting(random forest regression)#######################################
set.seed(2019)
df_evi <- na.omit(data_evi) # remove na rows
# describe(df_evi)

train_index <- sample(1:nrow(df_evi),300)
df_train <- df_evi[train_index, ]
df_test <- df_evi[-train_index, ]

# # find best parameters
# evi_rf=randomForest(x = df_train[3:7], #returns a dataframe
#                     y = df_train$evi, #returns a vector
#                     ntree = 500, mtry = 3, nPerm = 4, nodesize = 2)
# plot(evi_rf)
# 
# oob.err=double(5)
# test.err=double(5)
# 
# 
# for (mtry in 1:5){
#     # mtry <- 1
#     
#     rf=randomForest(evi ~ . , data = df_train[2:7, ],
#                     mtry=mtry, ntree=200, nPerm = 4, nodesize = 2) 
#     oob.err[mtry] = rf$mse[200] #Error of all Trees fitted
#     
#     pred<-predict(rf,df_test) #Predictions on Test Set for each Tree
#     test.err[mtry]= with(df_test, mean( (evi - pred)^2)) #Mean Squared Test Error
#     
#     cat(mtry," ") #printing the output to the console
#     
# }
# matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"), type="b", 
#         ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
# legend("center",legend=c("Out of Bag Error","Test Error"),
#        pch=19, col=c("red","blue"))


evi_rf=randomForest(x = df_train[3:7], 
                    y = df_train$evi, 
                    ntree = 200, mtry = 2, nPerm = 4, nodesize = 2)
evi_pred<-predict(evi_rf,df_test) 
df_pred <- data.frame(Prediction = evi_pred,
                      TrueValue = df_test$evi)
# mean square error
test_err= with(df_pred, mean( (TrueValue - Prediction)^2))
test_err

plot(TrueValue~Prediction,data = df_pred)
summary(lm(TrueValue~Prediction,data = df_pred))
