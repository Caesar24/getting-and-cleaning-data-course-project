# Coursera Getting and Cleaning Data Course Project
# run_analysis.R
#
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of 
#    each variable for each activity and each subject.

# Clean up workspace
rm(list = ls())
setwd("C:/dev/R/getting-and-cleaning-data-course-project/")

# Create the data directory if it does not exist
if(!file.exists("./data")) 
{
    dir.create("./data")
}      

# only download zip file if we haven't yet (speed optimization)
localZipFileName = "./data/dataset.zip"
if( !file.exists(localZipFileName) )
{
    # Fetch the data and unzip file locally
    urlData <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    download.file(urlData, destfile = localZipFileName)  
} else
{
    print("Skipping file download")
}

# unzip file
print("Unzipping file")
unzip(zipfile = './data/dataset.zip', exdir = './data') 

# load necessary libraries
print("Loading Libraries")
library(dplyr)
library(tidyr)

# load datasets to variables
print("Loading Table: Features")
dsFeatures <- read.table('./data/UCI HAR Dataset/features.txt')

print("Loading Table: Activity Types")
dsActivityTypes <- read.table('./data/UCI HAR Dataset/activity_labels.txt')

print("Loading Table: Subject Train")
dsSubjectTrain <- read.table('./data/UCI HAR Dataset/train/subject_train.txt') 

print("Loading Table: XTrain")
dsXTrain <- read.table('./data/UCI HAR Dataset/train/x_train.txt')

print("Loading Table: YTrain")
dsYTrain <- read.table('./data/UCI HAR Dataset/train/y_train.txt')

print("Loading Table: Subject Test")
dsSubjectTest <- read.table('./data/UCI HAR Dataset/test/subject_test.txt')

print("Loading Table: XTest")
dsXTest <- read.table('./data/UCI HAR Dataset/test/x_test.txt')

print("Loading Table: YTest")
dsYTest <- read.table('./data/UCI HAR Dataset/test/y_test.txt')

dsTrainingData = cbind(dsYTrain,dsSubjectTrain,dsXTrain)

# Assiging column names to the data imported above
colnames(dsXTrain) = dsFeatures[,2]
dsXTrain <- dsXTrain[, !duplicated(colnames(dsXTrain))]
colnames(dsXTest) <- dsFeatures[,2]
dsXTest <- dsXTest[, !duplicated(colnames(dsXTest))]
dsMainDataset  <- rbind(cbind(dsSubjectTrain, dsYTrain, dsXTrain), cbind(dsSubjectTest, dsYTest, dsXTest)) 
colnames(dsMainDataset)[2] <- 'Activity'  
dsMeanStd <- select(dsMainDataset, V1, Activity, contains('-mean()-'), contains('-std()-'))
dsMeanStd$Activity <- factor(dsMeanStd$Activity, levels = c(1,2,3,4,5,6), labels = dsActivityTypes[,2]) 
colnames(dsMeanStd)[1] <- 'Subject' 

# process data and write it
print("Writing tidyData.txt") 
dsMeanStd %>% gather(Features, Measurement, -(Subject:Activity)) %>%  
    group_by(Subject, Activity, Features) %>%  summarise(Average = mean(Measurement)) %>%  
    spread(Features, Average) %>% print %>%
    write.table('tidyData.txt', row.names = FALSE)

