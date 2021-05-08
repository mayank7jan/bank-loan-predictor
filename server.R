# Define server logic
server <- function(input, output, session) {
  
  # Loading and Cleaning Data for different models
  bank_data <- load.data()
  
  bank_data_lr <- clean.data.lr(bank_data)
  bank_data_knn <- clean.data.knn(bank_data)
  
  # Splitting for DT and LR into train and test
  set.seed(123)
  SplitIndex <- sample(x = c("Train", "Test"), replace = TRUE, prob = c(0.7,0.3))
  Train_bank_data_lr <- bank_data_lr[SplitIndex == "Train", ]
  Test_bank_data_lr <- bank_data_lr[SplitIndex == "Test", ]
  
  # Decision Tree calc
  rtree_build <- rtree_info(raw_data = Train_bank_data_lr)
  tree_accuracy_train <-  predict_tree(tree_info = rtree_build, new_data = Train_bank_data_lr, type_class = "class")
  tree_accuracy_test <-  predict_tree(tree_info = rtree_build, new_data = Test_bank_data_lr, type_class = "class")
  
  # Logistic Regression calc
  lr_build <- glm_build(data_pass = Train_bank_data_lr)
  lr_accuracy_train <- lr_predict(glm_info = lr_build, new_data = Train_bank_data_lr)
  lr_accuracy_test <- lr_predict(glm_info = lr_build, new_data = Test_bank_data_lr)
  
  # Sample data for KNN
  TrainData_knn_full <- bank_data_knn[SplitIndex == "Train", ]
  TestData_knn_full <- bank_data_knn[SplitIndex == "Test", ]
  knn_train_data <- sample_n(tbl = TrainData_knn_full, replace = TRUE, size = 1000)
  knn_test_data <- sample_n(tbl = TestData_knn_full, replace = TRUE, size = 1000)
  # KNN calc
  knn_build <- knn_build_it(n = 10, dataframe = knn_train_data)
  knn_accuracy_train <- knn_predict(obj = knn_build, new_data = knn_train_data)
  knn_accuracy_test <- knn_predict(obj = knn_build, new_data = knn_test_data)
  
  # LOGISTIC REGRESSION CUSTOMER PREDICT 
  NumericCols <- c("AGE", "PREVIOUS", "EMP_VAR_RATE", "CONS_PRICE_IDX","CONS_CONF_IDX", "EURIBOR3M", "NR_EMPLOYED")
  
  FactorCols <- c("JOB","DEFAULTCREDIT", "CONTACT","MONTH", "POUTCOME", "Y")
  
  
  # Decision Tree Output mapping
  output$Tree_Fig <- renderPlot({
    
    plot_rtree(tree_str = rtree_build)    
    
  })
  
  output$Tree_roc <- renderPlot({
    
    train_roc <- tree_roc(tree_info = rtree_build, data_pass = Train_bank_data_lr)
    test_roc <- tree_roc(tree_info = rtree_build, data_pass = Test_bank_data_lr)
    
    plot.roc(train_roc, col = "blue", print.auc = T)
    plot.roc(test_roc, col = "red", print.auc = T, add = TRUE, print.auc.x = .5, print.auc.y = .45)
    legend("bottomright", legend = c("Training Data ROC", "Testing Data ROC"), fill = c("blue", "red"), bty = "n")
  })
  
  output$dt_acc_train <- renderPrint({
    
    print(tree_accuracy_train, row.names = FALSE)
    
  })
  
  output$dt_acc_test <- renderPrint({
    
    print(tree_accuracy_test, row.names = FALSE)
    
  })
  
  # Logistic Regression Output mapping
  output$lr_roc <- renderPlot({
    
    train_roc <- lr_roc(lr_info = lr_build, data_pass = Train_bank_data_lr)
    test_roc <- lr_roc(lr_info = lr_build, data_pass = Test_bank_data_lr)
    
    plot.roc(train_roc, col = "blue", print.auc = T)
    plot.roc(test_roc, col = "red", print.auc = T, add = TRUE, print.auc.x = .5, print.auc.y = .45)
    legend("bottomright", legend = c("Training Data ROC", "Testing Data ROC"), fill = c("blue", "red"), bty = "n")   
    
  })
  
  output$lr_acc_train <- renderPrint({
    
    print(lr_accuracy_train, row.names = FALSE)
    
  })
  
  output$lr_acc_test <- renderPrint({
    
    print(lr_accuracy_test, row.names = FALSE)
    
  })
  
  # KNN Classification Output mapping
  output$knn_roc <- renderPlot({
    
    train_roc <- knn_roc(obj = knn_build, new_data = knn_train_data)
    test_roc <- knn_roc(obj = knn_build, new_data = knn_test_data)
    #test_roc <- knn_roc(obj = knn_build_it(n = 10, dataframe = knn_test_data), new_data = knn_test_data)
    
    plot.roc(train_roc, col = "blue", print.auc = T)
    plot.roc(test_roc, col = "red", print.auc = T, add = TRUE, print.auc.x = .5, print.auc.y = .45)
    legend("bottomright", legend = c("Training Data ROC", "Testing Data ROC"), fill = c("blue", "red"), bty = "n")   
    
  })
  
  output$knn_acc_train <- renderPrint({
    
    print(knn_accuracy_train, row.names = FALSE)
    
  })
  
  output$knn_acc_test <- renderPrint({
    
    print(knn_accuracy_test, row.names = FALSE)
    
  })
  
  
  
  update_bt_dt <- reactiveValues(predict = FALSE)
  
  observeEvent(input$update_dt_button, {
    update_bt_dt$predict <- input$update_dt_button
  })
  
  # continue_bt <- reactiveValues(press = FALSE)
  # 
  # observeEvent(input$copy_button, {
  #   continue_bt$press <- input$copy_button
  # })
  
  
  
  output$dt_cust_pred_ui <- renderValueBox({
    
    if (update_bt_dt$predict == FALSE) return(valueBox(
      value = "ATTENTION!", icon = icon("info-circle"),
      color = "yellow", subtitle = "Please click PREDICT button for computation."
    ))
    
    # if (continue_bt$press != FALSE) {
    #   
    #   input$lr_nr_employed <- input$dt_nr_employed
    #   input$knn_nr_employed <- input$dt_nr_employed
    #   
    # }
    
    isolate({
      
      validate(
        need(input$dt_nr_employed != "" , "Please enter values for all the attributes")
      )
      
      Train_data_dt <- subset(Train_bank_data_lr, select = c(NumericCols, FactorCols) )
      rtree_build_cust <- rtree_info(raw_data = Train_data_dt)
      
      # Predicting for single customer
      dt_cust_predict_data <- function(nr_employed = as.numeric(input$dt_nr_employed) , 
                                       poutcome = factor(input$dt_poutcome, levels = levels(Train_data_dt$POUTCOME)) ) {
        
        sample_data <- data.frame( "NR_EMPLOYED" = nr_employed,
                                   "POUTCOME" = poutcome,
                                   "AGE" = as.numeric(NA), "PREVIOUS" = as.numeric(NA),
                                   "EMP_VAR_RATE" = as.numeric(NA), "CONS_PRICE_IDX" = as.numeric(NA),
                                   "CONS_CONF_IDX" = as.numeric(NA), "EURIBOR3M" = as.numeric(NA), "Y" = as.factor(NA),
                                   "JOB" = as.factor(NA), "DEFAULTCREDIT" = as.factor(NA), 
                                   "CONTACT" = as.factor(NA), "MONTH" = as.factor(NA)
        )
        return(sample_data)
      }
      
      dt_cust_predict <- function(tree_info = rtree_build_cust, data_pass = dt_cust_predict_data()) {
        
        TreeProb <- predict(object = tree_info, newdata = data_pass, type = "prob")[,2]
        return(TreeProb)
      }
      
      dt_single <- round(dt_cust_predict(), 4)
      
      
      if (dt_single >= 0.5) {
        valueBox(
          paste0(dt_single*100 , "%"), "Probability of accepting long term bank deposit", icon = icon("thumbs-up"),
          color = "purple"
        )
      } else {
        valueBox(
          paste0(dt_single*100 , "%"), "Probability of accepting long term bank deposit", icon = icon("thumbs-down"),
          color = "red"
        )
      }
      
    })
    
  })
  
  output$lr_cust_pred_ui <- renderValueBox({
    
    if (update_bt_dt$predict == FALSE) return(valueBox(
      value = "ATTENTION!", icon = icon("info-circle"),
      color = "yellow", subtitle = "Please click PREDICT button for computation."
    ))
    
    isolate({
      
      validate(
        need(input$lr_age != "" , "Please enter values for all the attributes"),
        need(input$lr_previous != "" , "Please enter values for all the attributes"),
        need(input$lr_emp_var_rate != "" , "Please enter values for all the attributes"),
        need(input$lr_cons_price_idx != "" , "Please enter values for all the attributes"),
        need(input$lr_cons_conf_idx != "" , "Please enter values for all the attributes"),
        need(input$lr_euribor != "" , "Please enter values for all the attributes"),
        need(input$lr_nr_employed != "" , "Please enter values for all the attributes")
      )
      
      Train_data_lr <- subset(Train_bank_data_lr, select = c(NumericCols, FactorCols) )
      
      sample_data <- data.frame( "AGE" = as.numeric(input$lr_age), 
                                 "JOB" = factor(input$lr_job, levels = levels(Train_data_lr$JOB)), 
                                 "DEFAULTCREDIT" = factor(input$lr_dcredit, levels = levels(Train_data_lr$DEFAULTCREDIT)), 
                                 "CONTACT" = factor(input$lr_contact, levels = levels(Train_data_lr$CONTACT)), 
                                 "MONTH" = factor(input$lr_month, levels = levels(Train_data_lr$MONTH)),
                                 "PREVIOUS" = as.numeric(input$lr_previous),
                                 "POUTCOME" = factor(input$lr_poutcome, levels = levels(Train_data_lr$POUTCOME)), 
                                 "EMP_VAR_RATE" = as.numeric(input$lr_emp_var_rate), 
                                 "CONS_PRICE_IDX" = as.numeric(input$lr_cons_price_idx), 
                                 "CONS_CONF_IDX" = as.numeric(input$lr_cons_conf_idx), 
                                 "EURIBOR3M" = as.numeric(input$lr_euribor),
                                 "NR_EMPLOYED" = as.numeric(input$lr_nr_employed)
      )
      
      lr_build_cust <- glm(formula = Y~., data = Train_data_lr, family = binomial)
      lr_accuracy_cust <- predict(object = lr_build_cust, newdata = sample_data, type = "response")
      
      lr_single <- round(lr_accuracy_cust, 4)
      
      
      if (lr_single >= 0.5) {
        valueBox(
          paste0(lr_single*100 , "%"), "Probability of accepting long term bank deposit", icon = icon("thumbs-up"),
          color = "purple"
        )
      } else {
        valueBox(
          paste0(lr_single*100 , "%"), "Probability of accepting long term bank deposit", icon = icon("thumbs-down"),
          color = "red"
        )
      }
    })
  })
  
  
  # Keeping some calculations for KNN customer predict outside output function for faster prediction, since model needs to be build only once.
  Train_data_knn <- subset(bank_data_lr, select = c(NumericCols, FactorCols) )
  Train_data_knn[,c("JOB","DEFAULTCREDIT", "CONTACT","MONTH", "POUTCOME")] <- 
    lapply(Train_data_knn[,c("JOB","DEFAULTCREDIT", "CONTACT","MONTH", "POUTCOME")], as.numeric)
  Train_data_knn <- sample_n(tbl = Train_data_knn, replace = TRUE, size = 1000)
  knn_build_cust <- knn_build_it(n = 10, dataframe = Train_data_knn)
  
  
  output$knn_cust_pred_ui <- renderValueBox({
    
    
    if (update_bt_dt$predict == FALSE) return(valueBox(
      value = "ATTENTION!", icon = icon("info-circle"),
      color = "yellow", subtitle = "Please click PREDICT button for computation."
    ))
    
    isolate({
      
      # no_input_msg <- valueBox(
      #     value = "ERROR!", icon = icon("exclamation-triangle"),
      #            color = "red", subtitle = "Please enter values for all the attributes"
      # )
      
      validate(
        
        need(input$knn_age != "" , "Please enter values for all the attributes"),
        need(input$knn_previous != "" , "Please enter values for all the attributes"),
        need(input$knn_emp_var_rate != "" , "Please enter values for all the attributes"),
        need(input$knn_cons_price_idx != "" , "Please enter values for all the attributes"),
        need(input$knn_cons_conf_idx != "" , "Please enter values for all the attributes"),
        need(input$knn_euribor != "" , "Please enter values for all the attributes"),
        need(input$knn_nr_employed != "" , "Please enter values for all the attributes")
        
      )
      
      
      Train_data_knn <- subset(bank_data_lr, select = c(NumericCols, FactorCols) )
      
      sample_data <- data.frame( "AGE" = as.numeric(input$knn_age), 
                                 "JOB" = factor(input$knn_job, levels = levels(Train_data_knn$JOB)), 
                                 "DEFAULTCREDIT" = factor(input$knn_dcredit, levels = levels(Train_data_knn$DEFAULTCREDIT)), 
                                 "CONTACT" = factor(input$knn_contact, levels = levels(Train_data_knn$CONTACT)), 
                                 "MONTH" = factor(input$knn_month, levels = levels(Train_data_knn$MONTH)),
                                 "PREVIOUS" = as.numeric(input$knn_previous),
                                 "POUTCOME" = factor(input$knn_poutcome, levels = levels(Train_data_knn$POUTCOME)), 
                                 "EMP_VAR_RATE" = as.numeric(input$knn_emp_var_rate), 
                                 "CONS_PRICE_IDX" = as.numeric(input$knn_cons_price_idx), 
                                 "CONS_CONF_IDX" = as.numeric(input$knn_cons_conf_idx), 
                                 "EURIBOR3M" = as.numeric(input$knn_euribor),
                                 "NR_EMPLOYED" = as.numeric(input$knn_nr_employed)
      )
      
      sample_data[,c("JOB","DEFAULTCREDIT", "CONTACT","MONTH", "POUTCOME")] <- 
        lapply(sample_data[,c("JOB","DEFAULTCREDIT", "CONTACT","MONTH", "POUTCOME")], as.numeric)
      
      knn_accuracy_cust <- predict(object = knn_build_cust, newdata = sample_data, type = "prob")[,2]
      
      knn_single <- round(knn_accuracy_cust, 4)
      
      
      if (knn_single >= 0.5) {
        valueBox(
          paste0(knn_single*100 , "%"), "Probability of accepting long term bank deposit", icon = icon("thumbs-up"),
          color = "purple"
        )
      } else {
        valueBox(
          paste0(knn_single*100 , "%"), "Probability of accepting long term bank deposit", icon = icon("thumbs-down"),
          color = "red"
        )
      }
    })
  })
  
  
  output$best_10_lr <- DT::renderDataTable({
    
    Train_bank_data_lr <- sample_n(tbl = Train_bank_data_lr, size = 2000, replace = TRUE)
    Train_data_lr <- subset(Train_bank_data_lr, select = c(NumericCols, FactorCols) )
    lr_build_cust <- glm(formula = Y~., data = Train_data_lr, family = binomial)
    Train_data_lr$Probability <- round(predict(object = lr_build_cust, newdata = Train_data_lr, type = "response"), 3)
    Train_data_lr <- Train_data_lr[Train_data_lr$Probability >= 0.5,]
    Train_data_lr <- select(Train_data_lr, -c(Y, DEFAULTCREDIT))
    
    # DT::datatable(Train_data_lr)
    DT::datatable(
      data = Train_data_lr,
      options = list(
        pageLength = 20,
        paging = TRUE,
        searching = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    )
    
  })
  
  output$worst_10_lr <- DT::renderDataTable({
    
    Train_bank_data_lr <- sample_n(tbl = Train_bank_data_lr, size = 2000, replace = TRUE)
    Train_data_lr <- subset(Train_bank_data_lr, select = c(NumericCols, FactorCols) )
    lr_build_cust <- glm(formula = Y~., data = Train_data_lr, family = binomial)
    Train_data_lr$Probability <- round(predict(object = lr_build_cust, newdata = Train_data_lr, type = "response"), 3)
    Train_data_lr <- Train_data_lr[Train_data_lr$Probability < 0.5,]
    Train_data_lr <- select(Train_data_lr, -c(Y, DEFAULTCREDIT))
    
    # DT::datatable(Train_data_lr)
    DT::datatable(
      data = Train_data_lr,
      options = list(
        pageLength = 20,
        paging = TRUE,
        searching = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        scrollX = TRUE,
        scrollY = TRUE
      )
    )
  })
  
}
