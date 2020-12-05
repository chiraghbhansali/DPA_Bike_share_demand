# ====================== Part 5 : Model Builing starts here ======================
    # 5a. Split data into test and train set
    # 5b. Linear Regression
    # 5c. Random Forest
    # 5d. Gradient Boosting


    # 5a. Split data into test and train set
        sample_size = floor(0.8 * nrow(bike))
        set.seed(1)
        train_index = sample(nrow(bike), size = sample_size)
        train <- bike[train_index, ]
        test <- bike[-train_index, ]    


    # 5b. Linear Regression
        # Fit Linear Model
        # drop atemp, registered, casual and date
        train_subset = train[-c(6,9:10, 12)]
        test_subset = test[-c(6,9:10, 12)]

        lm_fit = lm(count ~ ., data = train_subset)
        summary(lm_fit)
        
        # Choosing the best model by AIC in a Stepwise Algorithm
        # The step() function iteratively removes insignificant features from the model.
        step(lm_fit)
        summary(lm_fit)

        # Calculate Train RMSLE
        y_act_train <- abs(train_subset$count)
        y_pred_train <- abs(predict(lm_fit, train_subset))
        lm_train_RMSLE = rmsle(y_act_train, y_pred_train)
        
        # Calculate Test RMSLE
        y_act_test <- abs(test_subset$count)
        y_pred_test <- abs(predict(lm_fit, test_subset))
        lm_test_RMSLE = rmsle(y_act_test, y_pred_test)
        
        # Save the results
        lm_results = predict(lm_fit, bike_test)
        hist(lm_results)


plot(lm_fit)

mse(y_act_train,y_pred_train)
rmse(y_act_train,y_pred_train)
rmsle(y_act_train,y_pred_train)

mse(y_act_test,y_pred_test)
rmse(y_act_test,y_pred_test)
rmsle(y_act_test,y_pred_test)

    # 5b. Random Forest
        Ntree=500
        Mtry = 5
        myImportance = TRUE
        
        # Predict Casual Counts
        set.seed(1)
        CasualData <- subset(train, select = -c(count, registered, date, atemp))
        CasualFit <- randomForest(casual ~ ., data=CasualData, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)
        

        # Predict Registered Counts
        RegisteredData <- subset(train, select = -c(count, casual, date, atemp))
        RegisteredFit <- randomForest(registered ~ ., data=RegisteredData, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)

        varImpPlot(CasualFit) 
        varImp(CasualFit) 

        varImpPlot(RegisteredFit)
        varImp(RegisteredFit) 

    #Inference - Casual Fit: season, holiday, windspeed and weather are not much significant here.
    #Inference - Registered Fit: season, holiday, windspeed and weekday are not much significant here.


        casualFitFinal <- randomForest(casual ~ hour + year + humidity + month + temp + workingday + wkday, 
                               data=CasualData, ntree=Ntree, mtry=Mtry,importance=myImportance)
        RegisteredFitFinal <- randomForest(registered ~ hour + year + month + weather + workingday + humidity + temp, 
                                        data=RegisteredData, ntree=Ntree, mtry=Mtry,importance=myImportance)


        # Prediction on train data
        
            # Prediction on train data - casual users
            PredTrainCasual = round(predict(CasualFit, train),0)
            PredTrainCasualFinal = round(predict(casualFitFinal, train),0)

            # Prediction on train data - Registered users
            PredTrainRegistered = round(predict(RegisteredFit, train),0)
            PredTrainRegisteredFinal = round(predict(RegisteredFitFinal, train),0)
            
            # Sum up Casual and Registered to get Total Count
            PredTrainCount = PredTrainCasual+PredTrainRegistered
            PredTrainCountFinal = PredTrainCasualFinal+PredTrainRegisteredFinal

            # Calculate Train RMSLE
            rf_train_rmsle_full = rmsle(train$count, PredTrainCount)
            rf_train_rmsle2_reduced = rmsle(train$count, PredTrainCountFinal)
    

        # Prediction on test data
            # Prediction on test data - casual users
            PredTestCasual = round(predict(CasualFit, test),0)
            PredTestCasualFinal = round(predict(casualFitFinal, test),0)

            # Prediction on test data - registered users
            PredTestRegistered = round(predict(RegisteredFit, test),0)
            PredTestRegisteredFinal = round(predict(RegisteredFitFinal, test),0)

            # Sum up Casual and Registered to get Total Count
            PredTestCount = PredTestCasual+PredTestRegistered
            PredTestCountFinal = PredTestCasualFinal+PredTestRegisteredFinal

            # Calculate Train RMSLE
            rf_test_rmsle_full = rmsle(test$count, PredTestCount)
            rf_test_rmsle2_reduced = rmsle(test$count, PredTestCountFinal)



cat("Training RMSLE - Linear Regression: ", lm_train_RMSLE)
cat("\nTraining RMSLE - Random Forest (Full Model): ", rf_train_rmsle_full)
cat("\nTraining RMSLE - Random Forest (Reduced Model): : ", rf_train_rmsle2_reduced)

cat("\n\nTest RMSLE - Linear Regression: ", lm_test_RMSLE)
cat("\nTest RMSLE - Random Forest (Full Model): ", rf_test_rmsle_full)
cat("\nTest RMSLE - Random Forest (Reduced Model): ", rf_test_rmsle2_reduced)


        hist(bike$count, main="Training Data")
        hist(lm_results, main="Linear Regression Fit")
        hist(rf_results, main="Random Forest Fit")

        # Inference: The distribution of predicted count looks similar to that of train data. 

        # Save the RF results
        rf_test_casual = round(predict(casualFitFinal, bike_test),0)
        rf_test_registered = round(predict(RegisteredFitFinal, bike_test),)
        rf_results = rf_test_casual + rf_test_registered

gbmtree=4000
iDepth = 3
set.seed(1) 

# Predict Casual Counts
CasualData <- subset(train, select = -c(count, registered, atemp, date))
gbm.Casual <- gbm(log1p(casual)~.,data=CasualData,distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


# Predict Registered Counts
RegisteredData <- subset(train, select = -c(count, casual, atemp, date))
gbm.Registered <- gbm(log1p(registered)~.,data=RegisteredData,distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


summary(gbm.Casual)
summary(gbm.Registered)

##Inference - gbm Casual: season, holiday, year, windspeed are not much significant here.
##Inference - gbm Registered: holiday, windspeed, season, weather are not much significant here.

gbm.CasualFinal <- gbm(log1p(casual) ~ hour + workingday + temp + month  +  wkday + humidity + weather, 
                               data=CasualData, distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)
gbm.RegisteredFinal <- gbm(log1p(registered) ~ hour + year + workingday + month + wkday + humidity + temp, 
                           data=RegisteredData, distribution= "gaussian",n.trees=gbmtree,interaction.depth=iDepth)


# Prediction on train data
  # Prediction on train data - casual users

gbm.PredTrainCasual <- predict(gbm.Casual, train, n.trees=gbmtree)
gbm.PredTrainCasualFinal <- predict(gbm.CasualFinal, train, n.trees=gbmtree)

# Prediction on train data - Registered users
gbm.PredTrainRegistered <- predict(gbm.Registered, train, n.trees=gbmtree)
gbm.PredTrainRegisteredFinal <- predict(gbm.RegisteredFinal, train, n.trees=gbmtree)

# Sum up Casual and Registered to get Total Count
gbm.PredTrainCount <- round(exp(gbm.PredTrainCasual) - 1, 0) + round(exp(gbm.PredTrainRegistered) - 1, 0)
gbm.PredTrainCountFinal <- round(exp(gbm.PredTrainCasualFinal) - 1, 0) + round(exp(gbm.PredTrainRegisteredFinal) - 1, 0)

# Calculate Train RMSLE
gbm.rf_train_rmsle_full <- rmsle(train$count, gbm.PredTrainCount)
gbm.rf_train_rmsle2_reduced <- rmsle(train$count, gbm.PredTrainCountFinal)

# Prediction on test data
# Prediction on test data - casual users
gbm.PredTestCasual = predict(gbm.Casual, test, n.trees=gbmtree)
gbm.PredTestCasualFinal = predict(gbm.CasualFinal, test, n.trees=gbmtree)

# Prediction on test data - registered users
gbm.PredTestRegistered = predict(gbm.Registered, test, n.trees=gbmtree)
gbm.PredTestRegisteredFinal = predict(gbm.RegisteredFinal, test, n.trees=gbmtree)

# Sum up Casual and Registered to get Total Count
gbm.PredTestCount = round(exp(gbm.PredTestCasual) - 1, 0) + round(exp(gbm.PredTestRegistered) - 1, 0)
gbm.PredTestCountFinal = round(exp(gbm.PredTestCasualFinal) - 1, 0) + round(exp(gbm.PredTestRegisteredFinal) - 1, 0)

# Calculate Test RMSLE
gbm.rf_test_rmsle_full = rmsle(test$count, gbm.PredTestCount)
gbm.rf_test_rmsle2_reduced = rmsle(test$count, gbm.PredTestCountFinal)

gbm.rf_train_rmsle_full
gbm.rf_train_rmsle2_reduced

gbm.rf_test_rmsle_full
gbm.rf_test_rmsle2_reduced

par(mfrow=c(3,2))
plot(y_act_train, y_pred_train, main="Linear Regression - Train Dataset", xlab="actual", ylab="predicted")
plot(y_act_test, y_pred_test, main="Linear Regression - Test Dataset", xlab="actual", ylab="predicted")

plot(train$count, PredTrainCount, main="Random Forest - Train Dataset", xlab="actual", ylab="predicted")
plot(test$count, PredTestCount, main="Random Forest - Test Dataset", xlab="actual", ylab="predicted")

plot(train$count, gbm.PredTrainCountFinal, main="Gradient Boosting - Train Dataset", xlab="actual", ylab="predicted")
plot(test$count, gbm.PredTestCountFinal, main="Gradient Boosting - Test Dataset", xlab="actual", ylab="predicted")


cor(y_act_train, y_pred_train)
cor(y_act_test, y_pred_test)

cor(train$count, PredTrainCountFinal)
cor(test$count, PredTestCountFinal)

cor(train$count, gbm.PredTrainCountFinal)
cor(test$count, gbm.PredTestCountFinal)


par(mfrow=c(2,2))
plot(test_subset$count, main = "Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(lm_fit, newdata = test), col = "red", pch = 20)

plot(test_subset$count, main = "Random Forest", ylab = "Test Set Rental Count", pch = 20)
points(PredTestCountFinal, col = "red", pch = 20)

plot(test_subset$count, main = "Gradient Boosting", ylab = "Test Set Rental Count", pch = 20)
points(gbm.PredTestCountFinal, col = "red", pch = 20)

 # Save the RF results
        gbm_test_casual = round(predict(gbm.CasualFinal, bike_test, n.trees=gbmtree),0)
        gbm_test_registered = round(predict(gbm.RegisteredFinal, bike_test, n.trees=gbmtree),0)
        gbm_results = gbm_test_casual + gbm_test_registered

par(mfrow=c(2,2))
hist(bike$count, main="Training Data")
hist(lm_results, main="Linear Regression Fit")
hist(rf_results, main="Random Forest Fit")
hist(rf_results, main="Gradient Boosting Fit")

par(mfrow=c(2,2))
hist(bike$count, main="Training Data")
hist(lm_results, main="Linear Regression Fit")
hist(rf_results, main="Random Forest Fit")
hist(rf_results, main="Gradient Boosting Fit")


