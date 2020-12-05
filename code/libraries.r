
# ====================== Step 1a:Problem Definition and Categorization  ======================

# The problem statement is to "Predict the daily bike rental count based on the environmental and seasonal settings"
# This is clearly a 'Supervised machine learning regression problem' to predict a number based on the input features.

#  ====================== Step 1a ends here  ====================== 


#  ====================== Step 1b: Import all the required libraries  ======================

# ==== for data transformations ====
    #install.packages("lubridate")
    library(lubridate)

# ==== for EDA Visualizations =====
    #install.packages("corrplot")
    library(corrplot)
    #install.packages("ggplot2")
    library(ggplot2)
    #install.packages("GGally")
    library(GGally)
    #install.packages("ggExtra")
    library(ggExtra)

# ==== for model building =====
    library(caret)
    #install.packages("Metrics")
    library(Metrics)
    #install.packages("randomForest")
    library(randomForest)
    #install.packages(gbm)
    library (gbm)


#  ====================== Step 1b ends here  ======================