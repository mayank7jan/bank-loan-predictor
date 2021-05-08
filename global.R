# libraries used
library(shiny) # 1.3.1
library(shinydashboard) #0.7.1
library(dplyr) #glm function from here #0.8.0.1
library(tidyr) #0.8.3
library(RMySQL)  #DB connection # 0.10.17
library(dbConnect) #DB connection #1.0
library(ggplot2) # plotting #3.1.1
library(pROC)    # For ROC Curve #1.14.0
library(reshape2) # For melting data # 1.4.3
library(corrplot) # Corelation plot #0.84
library(pander) # For knitting in Pandoc Markdown #0.6.3
library(caret)  #general train model for various model building #6.0-8.2
library(e1071) # specific package for KNN model building, under caret #1.7-1
library(kableExtra) # for producing nice tables in markdown, decision matrix #1.1.0
library(rpart) # buidling r-tree #4.1-13
library(rpart.plot) # for plotting rtree #3.0.7
library(shinycssloaders) #for loading animations #0.2.0
library(DT) # For displaying dataframe in shiny ui #0.5
library(rsconnect) #0.8.13
library(iterators) #1.0.10
library(foreach) # 1.4.4
library(packrat)

################################################################################################
#------------------------------- Custom project functions -------------------------------------#
################################################################################################
# FUNCTION TO LOAD DATA FROM DATABASE

## fetch data from local SQL database
# load.data <- function(USER = 'root',
#                       PASSWORD = 'root',
#                       HOST = 'localhost',
#                       DBNAME = 'bank_tm') {
#   
#   statement <- "Select * from tele_marketing;"
#   # statement <- "Select * from BankMarketing;"
#   
#   db <- dbConnect(MySQL(), user = USER, password = PASSWORD, host = HOST, dbname = DBNAME) #port=3306)
#   result <- dbGetQuery(db, statement = statement)
#   dbDisconnect(db)
#   colnames(result) <- gsub(pattern = "\\.",replacement = "_",toupper(colnames(result)))
#   colnames(result)[colnames(result) == "DEFAULT"] <- "DEFAULTCREDIT"
#   return(result)
# }

## fetch data from .zip file URL
load.data <- function() {
  
  # original file link - zip
  # 'https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip'
  
  # Data fetch
  # data <- read.csv(file = "https://github.com/mayank7jan/bank-loan-predictor/raw/main/data/bank-additional-full.csv", sep = ";", header = TRUE)
  data <- read.csv(file = "data/bank-additional-full.csv", sep = ";", header = TRUE)
  
  # Data manipulation
  colnames(data) <- gsub(pattern = "\\.",replacement = "_",toupper(colnames(data)))
  colnames(data)[colnames(data) == "DEFAULT"] <- "DEFAULTCREDIT"
  return(data)
}

# CLEAN DATA FOR DECISION TREE AND LOGISTIC REGRESSION

clean.data.lr <- function(df){
  # Changing the class for each column
  df[,c(1,11:14,16:20)] <- lapply(df[,c(1,11:14,16:20)], as.numeric)
  df[,c(2:10,15,21)] <- lapply(df[,c(2:10,15,21)], as.factor)
  
  df <- subset(df, select = -c(DURATION,LOAN,HOUSING,PDAYS)) 
  return(df)
}

# CLEAN DATA FOR KNN

clean.data.knn <- function(df){
  # Changing the class for each column
  df[,c(1,11:14,16:20)] <- lapply(df[,c(1,11:14,16:20)], as.numeric)
  df[,c(2:10,15,21)] <- lapply(df[,c(2:10,15,21)], as.factor)
  df[,c(2:10,15)] <- lapply(df[,c(2:10,15)], as.numeric)
  
  df <- subset(df, select = -c(DURATION,LOAN,HOUSING,PDAYS)) 
  return(df)
}

# BUILD R TREE

rtree_info <- function(raw_data, dependency = Y~., method_use = "class" ) {
  
  rtree <- rpart(formula = dependency, 
                 data = raw_data,
                 method = method_use,
                 model = TRUE
  )
  
  return(rtree)
}

# FUNCTION TO PLOT RTREE
plot_rtree <- function(tree_str = rtree_info){
  rpart.plot(tree_str, type = 2, fallen.leaves = F, cex = 1, extra = 104, main = "Decision Tree for success of Bank Telemarketing Call") 
}

# PREDICTION FUNCTION for Known DATA
predict_tree <- function(tree_info, new_data, type_class = 'class') {
  
  pred_tree_predict <- predict(object = tree_info, newdata = new_data, type = type_class)
  
  confusion_matrix <- table(Actual = new_data$Y, Predicted = pred_tree_predict)
  
  confidence <- mean(pred_tree_predict == new_data$Y)
  recall_cal <- confusion_matrix[2,2] / sum(confusion_matrix[2,])  #TP/(TP+FN)
  precision_cal <- confusion_matrix[2,2] / sum(confusion_matrix[,2])  #TP/(TP+FP)
  
  decision_matrix <- data.frame("TYPE" = c("Model_Accuracy","Model_Error","Recall","Precision"),
                                "VALUE" = c(round(confidence,4), round(1-confidence,4), round(recall_cal,4), round(precision_cal,4) )
  )
  
  return(decision_matrix)
}

# FUNCTION FOR PLOT INFO of TREE ROC
tree_roc <- function(tree_info, data_pass) {
  
  TreeProb <- predict(object = tree_info, newdata = data_pass, type = "prob")
  TreeROC <- roc(response = data_pass$Y, predictor = TreeProb[,2])  
  return(TreeROC)
}

# FUNCTION FOR LOGISTIC REGRESSION BUILD
glm_build <- function(dependency = Y ~., data_pass, family_bin = binomial) {
  
  glm_model <- glm(formula = dependency, data = data_pass, family = family_bin)
  return(glm_model)
}

# FUNCTION FOR LOGISTIC REGRESSION ACCURACY METRICES
lr_predict <- function(glm_info, new_data, type_class = "response") {
  
  new_data$Predictions <- predict(object = glm_info, newdata = new_data, type = type_class)
  new_data$Ypredict <- ifelse(new_data$Predictions > 0.5, "yes", "no")
  
  confusion_matrix <- table(Actual = new_data$Y, Predicted = new_data$Ypredict)
  
  confidence <- mean(new_data$Ypredict == new_data$Y) 
  recall_cal <- confusion_matrix[2,2] / sum(confusion_matrix[2,])  #TP/(TP+FN)
  precision_cal <- confusion_matrix[2,2] / sum(confusion_matrix[,2])  #TP/(TP+FP)
  
  decision_matrix <- data.frame("TYPE" = c("Model_Accuracy","Model_Error","Recall","Precision"),
                                "VALUE" = c(round(confidence,4), round(1-confidence,4), round(recall_cal,4), round(precision_cal,4) )
  )
  
  return(decision_matrix)
  
}

# FUNCTION FOR PLOT INFO of LOGISTIC REGRESSION ROC
lr_roc <- function(lr_info, data_pass) {
  
  GlmProb <- predict(object = lr_info, newdata = data_pass, type = "response")
  GlmROC <- roc(response = data_pass$Y, predictor = GlmProb)  
  return(GlmROC)
}

# FUNCTION FOR KNN MODEL BUILD
knn_build_it <- function(n = 10, dataframe, formula = Y~.){
  
  KnnModel <- train(
    form = formula,                      
    data = dataframe,                            
    method = 'knn',                         # knn model
    tuneGrid=expand.grid(.k= n),          # For specified number 'n' of neighbours
    metric='Accuracy',                      # Use accuracy as the performance measure
    trControl=trainControl(                 # Tell the function how to do training:
      method='repeatedcv',                     # Use repeatedCV
      number=10,                               # with 10 folds
      repeats=15 )
  )                                       # and 15 repeats
  return (KnnModel)
  
}

# FUNCTION FOR KNN ACCURACY METRICES
knn_predict <- function(obj, new_data) {
  
  KnnProbs <- predict(object = obj, newdata = new_data, type = "prob")[,2]
  
  # Calculating Precision, recall and accuracy
  new_data$Predictions <- KnnProbs
  
  new_data$Ypredict <- ifelse(new_data$Predictions > 0.5, "yes", "no")
  
  confusion_matrix <- table(Actual = new_data$Y, Predicted = new_data$Ypredict)
  
  confidence <- mean(new_data$Ypredict == new_data$Y) 
  recall_cal <- confusion_matrix[2,2] / sum(confusion_matrix[2,])  #TP/(TP+FN)
  precision_cal <- confusion_matrix[2,2] / sum(confusion_matrix[,2])  #TP/(TP+FP)
  
  decision_matrix <- data.frame("TYPE" = c("Model_Accuracy","Model_Error","Recall","Precision"),
                                "VALUE" = c(round(confidence,4), round(1-confidence,4), round(recall_cal,4), round(precision_cal,4) )
  )
  return(decision_matrix)
  
}

# FUNCTION FOR KNN ROC INFO
knn_roc <- function(obj, new_data) {
  
  KnnProbs <- predict(object = obj, newdata = new_data, type = "prob")[,2]
  KnnROC <- roc(response = new_data$Y, predictor = KnnProbs)
  return(KnnROC)
}

