# Load libraries
library(dplyr)

# Set WD 1
setwd("D:/Coursera/Getting and Cleaning Data/Proyecto")


# URL data for the project:
url.data <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


# Download the data
download.file(url.data, destfile = "getdata_projectfiles_UCI HAR Dataset.zip")


# Unzip
unzip("getdata_projectfiles_UCI HAR Dataset.zip")


# Read files
features <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activity.labels <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

x.test <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y.test <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject.test <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

x.train <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y.train <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/train/y_train.txt", col.names = "code")
subject_train <- read.table("D:/Coursera/Getting and Cleaning Data/Proyecto/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")


# 1. Merges the training and the test sets to create one data set
x.data <- rbind(x.train, x.test)
y.data <- rbind(y.train, y.test)

subjects <- rbind(subject_train, subject.test)

all.data <- cbind(x.data, y.data, subjects)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement
tidy.data <- all.data %>% select(subject, code, contains("mean"), contains("std"))


# 3. Uses descriptive activity names to name the activities in the data set
tidy.data$code <- activity.labels[tidy.data$code, 2]


# 4. Appropriately labels the data set with descriptive variable names.
names(tidy.data)[2] = "activity"
names(tidy.data)<-gsub("Acc", "Accelerometer", names(tidy.data))
names(tidy.data)<-gsub("Gyro", "Gyroscope", names(tidy.data))
names(tidy.data)<-gsub("BodyBody", "Body", names(tidy.data))
names(tidy.data)<-gsub("Mag", "Magnitude", names(tidy.data))
names(tidy.data)<-gsub("^t", "Time", names(tidy.data))
names(tidy.data)<-gsub("^f", "Frequency", names(tidy.data))
names(tidy.data)<-gsub("tBody", "TimeBody", names(tidy.data))
names(tidy.data)<-gsub("-mean()", "Mean", names(tidy.data), ignore.case = T)
names(tidy.data)<-gsub("-std()", "STD", names(tidy.data), ignore.case = T)
names(tidy.data)<-gsub("-freq()", "Frequency", names(tidy.data), ignore.case = T)
names(tidy.data)<-gsub("angle", "Angle", names(tidy.data))
names(tidy.data)<-gsub("gravity", "Gravity", names(tidy.data))


# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

final.tidy.data <- tidy.data %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

write.table(final.tidy.data, "Final_Tidy_Data.txt", row.name=F)