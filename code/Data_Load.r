#  ====================== Step 2: Gather the data  ======================

  # Data is provided as .csv file and already split into Test and Train.
  # The training set is comprised of the first 19 days of each month, 
  # while the test set is the 20th to the end of the month.
  # Data Import
    bike= read.csv("C:/Users/chira/Documents/IIT/CourseWork/Fall2020/CSP571-DataPreparationAndAnalysis/Project/DPA_Bike_share_demand/Data/train.csv", header=TRUE)
    bike_test = read.csv("C:/Users/chira/Documents/IIT/CourseWork/Fall2020/CSP571-DataPreparationAndAnalysis/Project/DPA_Bike_share_demand/Data/test.csv", header=TRUE)

#  ====================== Step 2 ends here  ======================