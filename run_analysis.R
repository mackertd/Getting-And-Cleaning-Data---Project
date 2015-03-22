## Conduct an initial assessment of the data files for the project

## Set up the environment

require(plyr)

## Load the data file function

## Let the user know the application is starting

      print("The application is starting")

loadfiles <- function(directoryPath, filePath) {
      
      ## Form the path
      
      path <- file.path(directoryPath, filePath)
      
      ## Get the data as data.frame
      
      data <- read.table(path, header = FALSE)
      
      return(data)
      
} ## end loadfiles function


## Set the file names and directories for each of the data files

      baseDirectory <- "UCI_HAR_Dataset"

      features <- "features.txt"

      activities <- "activity_labels.txt"

      testDirectory <- "UCI_HAR_Dataset/test" 

      testSubjects <- "subject_test.txt"

      testData <- "X_test.txt"

      testLabels <- "y_test.txt"

      trainDirectory <- "UCI_HAR_Dataset/train"

      trainSubjects <- "subject_train.txt"

      trainData <- "X_train.txt"

      trainLabels <- "y_train.txt"

## Load the oringal data files

      testSubjects <- loadfiles(testDirectory, testSubjects)

      testData <- loadfiles(testDirectory, testData)

      testLabels <- loadfiles(testDirectory, testLabels)

      trainSubjects <- loadfiles(trainDirectory, trainSubjects)

      trainData <- loadfiles(trainDirectory, trainData)

      trainLabels <- loadfiles(trainDirectory, trainLabels)

      featuresList <- loadfiles(baseDirectory, features)
      
      activityList <- loadfiles(baseDirectory, activities)

## Evaluate the number of rows in each file; for each set of data to ensure the same lenth before merging

## Test Data Set

numTestSubjects <- nrow(testSubjects)
numTestData <- nrow(testData) 
numTestLabels <- nrow(testLabels) 

if( numTestSubjects == numTestData && numTestData == numTestLabels && numTestSubjects == numTestLabels ) {
      
      print("Test data rows are equal")
      
} else {
      
      print("Test data rows are not equal")
      
}

## Train Data Set

numTrainSubjects <- nrow(trainSubjects)
numTrainData <- nrow(trainData) 
numTrainLabels <- nrow(trainLabels) 

if( numTrainSubjects == numTrainData && numTrainData == numTrainLabels && numTrainSubjects == numTrainLabels ) {
      
      print("Train data rows are equal")
      
} else {
      
      print("Train data rows are not equal")
      
}

## Combine the test and training data sets

      ## Combine the observation files
      
      combinedData <- rbind(testData, trainData)
      
      ## Combine the label files for the observations
      
      combinedLabels <- rbind(testLabels, trainLabels)
      
      ## Combine the label files for the observations
      
      combinedSubjects <- rbind(testSubjects, trainSubjects)

## Combine the three data sets together

## Order is the subject, activity label, and observations

      mergedDataSet <- cbind(combinedSubjects, combinedLabels, combinedData)

## Define and set the column names

## Set the column names on the activity types

      colnames(activityList) <- c("activityID", "Activity")

## Get the feature Labels

      featureColumnNames <- as.character(featuresList[,2])

## Apply the column names to the merged data set

      colnames(mergedDataSet) <- c("Subject", "activityID", featureColumnNames)

## Extract only the columns that are the measurement of the meand and standard deviaiton

      ## Adding ignore.case = TRUE to grepl adds an additional nine columns where Mean is upper case
      ## This is currently excluded from the data set

      ## grepl returns a logical vector

      meanStdDevDataOnly <- mergedDataSet[,grepl("Subject|activity|mean|std", colnames(mergedDataSet))]

## Make the names of the columns descriptive

      ## Add the Activity Descriptions

      meanStdDevDataOnly = merge(meanStdDevDataOnly, activityList, by='activityID', all.x=TRUE);

## Appropriately label the data columns to make them more descriptive

      ## Make the names syntatically Valid

      colnames(meanStdDevDataOnly) <- make.names(colnames(meanStdDevDataOnly))
head
      ## Remove Parentheses 

      colnames(meanStdDevDataOnly) <- gsub("\\()", "", colnames(meanStdDevDataOnly))

      ## Clean up the Frequency ahead of the Frequency domain

      colnames(meanStdDevDataOnly) <- gsub('Freq', "Frequency - ", colnames(meanStdDevDataOnly))

      ## Clean the time and frequency domains

      colnames(meanStdDevDataOnly) <- gsub('^t', "Time Domain - ", colnames(meanStdDevDataOnly))
      colnames(meanStdDevDataOnly) <- gsub('^f', "Frequency Domain - ", colnames(meanStdDevDataOnly))

      ## Clean up the standard deviation and the mean
      ## Need to account for the . that make.names changed the - to

      colnames(meanStdDevDataOnly) <- gsub('.std', " Standard Deviation - ", colnames(meanStdDevDataOnly))
      colnames(meanStdDevDataOnly) <- gsub('.mean', " Mean - ", colnames(meanStdDevDataOnly))

      ## Clean up the X, Y, and Z axis

      colnames(meanStdDevDataOnly) <- gsub('...X', "X Axis", colnames(meanStdDevDataOnly))
      colnames(meanStdDevDataOnly) <- gsub('...Y', "Y Axis", colnames(meanStdDevDataOnly))
      colnames(meanStdDevDataOnly) <- gsub('...Z', "Z Axis", colnames(meanStdDevDataOnly))

## Export a tidy data set with the means of the columns
      
      averagedData <- ddply(meanStdDevDataOnly, c("Subject","Activity"), numcolwise(mean))
      write.table(averagedData, file = "averagedDataSummary.csv", sep = ",", row.names=FALSE)

## Post a message that it complete

      print('Data file has been written')