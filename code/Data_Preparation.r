# ====================== Step 3: Data Preparation ======================
  # 3a. Analyze Attributes: Check properties of data
  # 3b. Complete Data Perform missing value analysis and Impute if needed
  # 3c. Correct Data: Check for any invalid data points
  # 3d. Create Derived Attributes - Feature Extraction
  # 3e. Convert - Converting data to proper formats


  # 3a. Analyze Attributes: Check properties of data
      dim(bike)
      str(bike)
      head(bike, 10)
  # 3a -> Inference: 
        #i. The dataset has 10,886 observations (n=10886) and 12 columns of type int, num and factor.
        #ii. Season, Holiday, Working day and weather are categorical variables.
        #ii. temp, atemp, humidity, windspeed, casual, registered and count are continuous numerical variables.


  # 3b. Complete Data Perform missing value analysis and Impute if needed
      table(is.na(bike))
  # 3b -> Inference: There are no null values in the dataset. If it had, then either the rows/columns had to be
    # dropped or the null values be imputed based on the % of null values


  # 3c. Correct Data: Check for any invalid data points
    # From above observations data doesnot seem to have any invalid datatypes to be handled.
    # Let's check for the outliers in EDA step


  # 3d. Create Derived Attributes - Feature Extraction
      # Lets extract 'date','month','weekday' and 'year' from 'datetime' column as we will be needing it for analysis
      bike$date=as.factor(day(bike$datetime))
      bike$year = as.factor(year(bike$datetime))
      bike$month = as.factor(month(bike$datetime))
      bike$hour = as.factor(hour(bike$datetime))
      bike$wkday = as.factor(wday(bike$datetime))
      
      bike_test$date=as.factor(day(bike_test$datetime))
      bike_test$year = as.factor(year(bike_test$datetime))
      bike_test$month = as.factor(month(bike_test$datetime))
      bike_test$hour = as.factor(hour(bike_test$datetime))
      bike_test$wkday = as.factor(wday(bike_test$datetime))

      # Drop datetime as we have extracted all the above needed information from it
      bike = bike[-c(1)]
      bike_test = bike_test[-c(1)]

      head(bike, 5)
      head(bike_test, 5)

  # 3d -> Inference: There are no null values in the dataset. If it had, then either the rows/columns had to be 
                    #dropped or the null values be imputed based on the % of null values.


  # 3e. Convert - Converting data to proper formats
    # We can clearly see that "season", "holiday", "workingday" and"weather" are categories rather than continous variable.
    # Let's convert them to categories
      names = c("season", "holiday", "workingday", "weather")
      bike[,names] = lapply(bike[,names], factor)
      bike_test[,names] = lapply(bike_test[,names], factor)

      str(bike) 
      str(bike_test)

# ====================== Step 3: Data Preparation ends here ======================
