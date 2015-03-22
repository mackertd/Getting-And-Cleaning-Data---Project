# Code Book for the Getting and Cleaning Data Course

This file describes how the data was ingested, cleaned, ogranized, and labelled to meet the course objectives.

Note: Detailed information can be found on the data sets in the README.txt and features_info.txt include the data set. The majority of the data set descriptions are taken from the README.txt file

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

 


