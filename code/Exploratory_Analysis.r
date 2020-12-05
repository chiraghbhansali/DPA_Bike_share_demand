# ====================== Step 4: Exploratory Data Analysis ======================
    # 4a. Outlier Analysis

    # 4a(1). Visualize continuos variables
          par(mfrow=c(1,5))
          boxplot(bike$count, main="Count", col="Gray", border = "black")
          boxplot(bike$temp, main="Temperature", col="blue", border = "black")
          boxplot(bike$atemp, main="Feels Like Temp", col="purple", border = "black")
          boxplot(bike$humidity, main="Humidity", col="green", border = "black")
          boxplot(bike$windspeed, main="Windspeed", col="orange", border = "black")

    # 4a(2). Visualize categorical variables wrt target variable
    par(mfrow=c(3,4))
    ggplot(data = bike, aes(x=season, y=count, fill=as.factor(season))) + geom_boxplot() + labs(title="Season vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=weather, y=count, fill=as.factor(weather))) + geom_boxplot() + labs(title="weather vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=holiday, y=count, fill=as.factor(holiday))) + geom_boxplot() + labs(title="holiday vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=workingday, y=count, fill=as.factor(workingday))) + geom_boxplot() + labs(title="workingday vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=year, y=count, fill=as.factor(year))) + geom_boxplot() + labs(title="year vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=month, y=count, fill=as.factor(month))) + geom_boxplot() + labs(title="month vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=wkday, y=count, fill=as.factor(wkday))) + geom_boxplot() + labs(title="weekday vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=hour, y=count, fill=as.factor(hour))) + geom_boxplot() + labs(title="hour vs Count") + theme(legend.title = element_blank())
    ggplot(data = bike, aes(x=date, y=count, fill=as.factor(day(date)))) + geom_boxplot() + labs(title="date vs Count") + theme(legend.title = element_blank())



# 4b. Correlation Analysis

# ====================== Explore Continuous Variables ======================
    # 4b(1). Explore continous features
        # i. Check distribution of target variable
        # ii. Explore correlation between independent continuous variables with target variable
        # iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        # iv. Visualize the relationship among all continuous variables using pairplots
        # v. Explore relationship between independent continuous variables and dependent variables using Joint Plot


   # 4b(1) i. Check distribution of target variable
          hist(bike$count, col="blue") 
          plot(bike$count)
   # Inference: Target variable "count" is almost normally distributed.


    # 4b(1) ii. Explore correlation between independent continuous variables with target variable      
        plot(bike$temp,bike$count)
        plot(bike$atemp,bike$count)
        plot(bike$windspeed,bike$count)
        plot(bike$humidity,bike$count)


    # 4b(1) iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        corr <- as.data.frame(lapply(bike[c(5:8, 11)], as.numeric))
        corrplot(cor(corr), method = "color", type='lower')
    # Inference: 
        # i. temp and atemp are highly correlated, we would need to drop one of them to remove multicolinearity.
        # ii. We can also drop Registered and Casual from our analysis as Counts are categorized as Registered and Casual 
             # and we will be predicting "Count" variable only.


    # 4b(1) iv. Visualize the relationship among all continuous variables using pairplots
        ggpairs(bike[c(5:8)], lower=list(continuous=wrap("smooth", colour="orange")) )


    # 4b(1) v. Explore relationship between independent continuous variables and dependent variables using Joint Plot       
        
        # 1. temp vs Count
        plot_center = ggplot(bike, aes(x=temp,y=count)) + geom_point(colour="blue") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="blue")
        # Inference: temp has good correlation with count.


        # 4b(1).v.2. atemp vs Count
        plot_center = ggplot(bike, aes(x=atemp,y=count)) + geom_point(colour="red") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="red")
        # Inference: atemp has good correlation with count.


        # 4b(1).v.3. humidity vs Count
        plot_center = ggplot(bike, aes(x=humidity,y=count)) + geom_point(colour="green") + geom_smooth(method="lm") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="green")
        # Inference: Humidity has low correlation with count.


        # 4b(1).v.4. windspeed vs Count
        plot_center = ggplot(bike, aes(x=windspeed,y=count)) + geom_point(colour="orange") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="orange")


    # 4b(1) Inferences Summary - Analysis of continous variables
        # 1. Target variable 'count' is almost normally distributed.
        # 2. From correlation with dependent variable "count", we can see that 'casual','registered' are very 
             # highly correlated to cnt. Needs to be dropped from the dataset.
        # 3. 'humidity' has low correlation with 'count'. For now, lets keep it.
        # 4. atemp and temp has good correlation with 'count'
        # 5. From heatmap, we can see that atemp and temp are highly correlated. So we need to drop 1 to remove multicollinearity.
        # 6. Since, as seen from jointplot, p(atemp) < p(temp), we can drop 'temp' and retain 'atemp' in the dataset.


# ====================== Explore Catogorical Variables ======================
    # 4b(2) Explore categorical features
          # i. Check distribution of categorical variables
            ggplot(bike, aes(x=" ",fill=year))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "year")+theme_void()

            ggplot(bike, aes(x=" ",fill=month))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "month")+theme_void()
            bike$season = factor(bike$season)

            ggplot(bike, aes(x=" ",fill=season))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "Season")+theme_void()
            bike$holiday = factor(bike$holiday)

            ggplot(bike, aes(x=" ",fill=holiday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "holiday")+theme_void()

            ggplot(bike, aes(x=" ",fill=wkday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "weekday")+theme_void()
            bike$workingday = factor(bike$workingday)

            ggplot(bike, aes(x=" ",fill=workingday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "workingday")+theme_void()
            bike$weather = factor(bike$weather)

            ggplot(bike, aes(x=" ",fill=weather))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "weather")+theme_void()


          # ii. Check how individual categorical features affects the target variable
        ggplot(bike, aes(x=season, y=count, fill=year)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
            ) + labs(title="Histogram for Seasons") +  labs(x="Season", y="Count")

        ggplot(bike, aes(x=year, y=count, fill=year)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
          ) + 
          labs(title="Histogram for year") +  labs(x="year", y="Count")

        ggplot(bike, aes(x=month, y=count, fill=month)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
          ) + 
          labs(title="Histogram for month") +  labs(x="month", y="Count")

        ggplot(bike, aes(x=holiday, y=count, fill=holiday)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
          ) +   labs(title="Histogram for holiday") +labs(x="holiday", y="Count")

        ggplot(bike, aes(x=wkday, y=count, fill=wkday)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
          ) +    labs(title="Histogram for weekday") +labs(x="weekday", y="Count")

        ggplot(bike, aes(x=workingday, y=count, fill=workingday)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
          ) +    labs(title="Histogram for working day") +labs(x="working day", y="Count")

        ggplot(bike, aes(x=weather, y=count, fill=weather)) + 
          stat_summary(
            fun.y=median, 
            geom='bar', 
            position=position_dodge(),
          ) +  labs(title="Histogram for weather") +labs(x="weather", y="Count")



        # iii. Explore trends over time ---- exploring some more pairplots

            ggplot(bike, aes(x=season, y=count, group=year, color=year)) + 
              stat_summary(
                fun.y=mean, 
                geom='line'
              ) + 
              stat_summary(
                fun.y=mean, 
                geom='point'
              ) + 
              labs(title="Average Count by Month Across Season") +
              labs(x="Season", y="Count")


            ggplot(bike, aes(x=bike$hour, y=count, group=season, color=season)) + 
              stat_summary(
                fun.y=mean, 
                geom='line'
              ) + 
              stat_summary(
                fun.y=mean, 
                geom='point'
              )+ 
              labs(title="Average Count By Hour Of The Day Across Season") +
              labs(x="Hour of the Day", y="Count")

            ggplot(bike, aes(x=bike$hour, y=count, group=wkday, color=wkday)) + 
              stat_summary(
                fun.y=mean, 
                geom='line'
              ) + 
              stat_summary(
                fun.y=mean, 
                geom='point'
              )+ 
              labs(title="Average Count By Hour Of The Day Across Weekdays") +
              labs(x="Hour of the Day", y="Count")


        ggplot(bike, aes(x=bike$day, y=count, group=day, color=day)) + 
              stat_summary(
                fun.y=mean, 
                geom='line'
              ) + 
              stat_summary(
                fun.y=mean, 
                geom='point'
              )+ 
              labs(title="Average Count By Day") +
              labs(x="HDay", y="Count")

# 4c. Drop some variables from the dataset based on the analysis so far 
        # drop temp, casual, registered and date
        bike_subset = bike[-c(5,9:10, 12)] 
        head(bike_subset,5)


# ====================== Step 4: Exploratory Data Analysis ENDS Here ======================
# Final observations:
#1.) 'atemp' and 'temp' are very strongly correlated . Drop 'atemp' from the dataset (since it has higher p-value 
        #than 'temp')
#2.) 'date' does not seem to have any affect on count of bikes, it can be dropped from the dataset
#========================================================================================