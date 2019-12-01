# Coursera Project 
# Camila A. Medeiros

library(dplyr)
library(plyr)
getData <- function(){
    dir=getwd()    
    if(!file.exists("./data")){dir.create("./data")}
    fileUrl1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl1, destfile = "./data/humanactivity.zip")
    unzip("./data/humanactivity.zip",exdir = "./data")
}

run_analysis <- function(){
#1. Merges the training and the test sets to create one data set.

    getData()
    
    #  test dataset
    test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
  
    #  train dataset
    train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
 
    
   # merge test and train
    mergeData <- merge(test,train,all=TRUE)
    dsLabels <-read.table("./data/UCI HAR Dataset/features.txt")
    names(mergeData) <- dsLabels[,2]
    
    
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
    selectCols<-dsLabels[,2][grep("mean\\(\\)|std\\(\\)", dsLabels[,2])]
    selectCols<-c(as.character(selectCols))
    selectData<-subset(mergeData,select=selectCols)

# 3. Uses descriptive activity names to name the activities in the data set

    #  merge activity dataset
    ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
    ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
    activity <- rbind(ytest,ytrain)
    names(activity) <- c("activity")
    
   
   # activity labels
   actLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
   actLabels[,2] <- tolower(actLabels[,2])
   activity[,1] = actLabels[activity[,1],2]

# 4. Appropriately labels the data set with descriptive variable names.
   names(selectData) <- gsub("[-()]", "", names(selectData))
   names(selectData) <- gsub(",", "_", names(selectData))
   names(selectData)<-gsub("-mean|mean", "Mean", names(selectData))
   names(selectData)<-gsub("-std|std", "Std", names(selectData))
   names(selectData)<-gsub("Acc", "Accelerometer", names(selectData))
   names(selectData)<-gsub("Gyro", "Gyroscope", names(selectData))
   names(selectData)<-gsub("Mag", "Magnitude", names(selectData))
   names(selectData)<-gsub("BodyBody", "Body", names(selectData))
   
   
   # merge subject dataset
   
   subtrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
   subtest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
   subject <- rbind(subtest,subtrain)
   names(subject) <- c("subject")
   
   data <- cbind(activity, subject) 
   selectData <- cbind(selectData,data) 
   selectData
   
   
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
   
   tidy<-aggregate(. ~subject + activity, selectData, mean)
   tidy<-tidy[order(selectData$subject,selectData$activity),]
   write.table(selectData, file = "tidydata.txt",row.name=FALSE)
    
    
}


