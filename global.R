# libraries used
library(shiny)
library(shinydashboard)
library(dplyr) #glm function from here
library(tidyr)
library(RMySQL)  #DB connection
library(dbConnect) #DB connection
library(ggplot2) # plotting
library(pROC)    # For ROC Curve
library(reshape2) # For melting data
library(corrplot) # Corelation plot
library(pander) # For knitting in Pandoc Markdown
library(caret)  #general train model for various model building
library(e1071) # specific package for KNN model building, under caret
library(kableExtra) # for producing nice tables in markdown, decision matrix
library(rpart) # buidling r-tree
library(rpart.plot) # for plotting rtree
library(shinycssloaders) #for loading animations
library(DT) # For displaying dataframe in shiny ui
library(rsconnect)
library(iterators)
library(foreach)
