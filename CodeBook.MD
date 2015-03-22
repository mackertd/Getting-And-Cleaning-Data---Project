# Code Book for the Getting and Cleaning Data Course

This file describes how the data was ingested, cleaned, ogranized, and labelled to meet the course objectives.

Note: Detailed information can be found on the data sets in the README.txt and features_info.txt include the data set. The majority of the data set descriptions are taken from the README.txt file

# General Comments

* Each line of code was tpyically developed in the console and then shifted to the source file when operating as expected
* This approach allows the code to remain in an operational state the majority of the time

# Overview Of The Data Set

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The data sets used from the include files are:

- features_info.txt - Shows information about the variables used on the feature vector

- features.txt - List of all features

- activity_labels.txt - Links the class labels with their activity name

- train/X_train.txt - Training set

- train/y_train.txt -Training labels
 
- trian/subject_test.txt - Subjects who performed the trianing set

- test/X_test.txt Test set

- test/y_test.txt Test labels
 
- trian/subject_test.txt - Subjects who performed the test set

# Data Ingest Set Up Procedures

* Define the directories and file names into variables
* Load each of the files into a variable utlizing the read.table function
   * Subjects, Data Set, and Test Activity labels
   * These files included the activities and activity labels and the labels for the feature labels
* Test to see if each of the files in the test and training data sets have the same length before merging them

# Combine The Data Sets

* Combined the test and training subjects, data sets, and acitivty lables utilizing row bind function with the test data on top
* Data is the merged together from left to right - subjects, activitie, and features using the column bind function 
* Output of this section is the mergedDataSet

# Create The Column Labels
 
* Set the activityId, and Activity labels on the Activity Type Data
* Create a character vector of the data set feature names
* Merge the two column data labels with the features
* Apply the columns to the merged data set

# Extract Only The Columns For Means and Standard Deviations

* Utilize the grepl function to only retain the columns with subject, activity, mean, and std into a new data frame
* Output of this section is the meanStdDevDataOnly

# Replace The Activity Numbers With The Actual Label

* Utilize the merge function to replace the Activity Numbers with the Activity Labels

# Clean The Readability Of The Feature Columns

Note: Steps here are the order performed in the code. The majority of the steps utilize the gsub function with regular expressions to repalace the text

* Remove the parentheses
* Utilze the make.names function to make the column names more syntactically readable 
* Replace Freq With Frequency
* Replace t with Time Domain -
* Replace f with Frequency Domain -
* Replace std with Standard Deviation -
* Replace mean with Mean - 
* Replace ...X with X - Axis
* Replace ...Y with Y - Axis
* Replace ...Z with Z - Axis

# Calculate the mean of each activity by each subject

* Utilize the plyr library ddply function to calculate the mean of each column grouped by the Subject and the Activity

# Export the file to CSV

* Utilize the write.table function to export the file to csv

Note: Utilized the row.name = FALSE parameter to eliminate an issue with the column labels being off by one in the exported data

