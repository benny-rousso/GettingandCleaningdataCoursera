## This script was developed by the student Benny Rousso to 
## perform the required activites to complete the Course Project 
## for the 'Getting and Cleaning Data' Course

## Download .zip file from
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## unzip it in your working directory

setwd("~/Australia Project/Data Scientist/03. Getting and Cleaning Data/Week 4/Project")

#1. Merge the training and the test sets to create one data set
        #1.1 Load training data
        X.train <- read.table("./train/X_train.txt")
        Y.train <- read.table("./train/Y_train.txt")
        S.train <- read.table("./train/subject_train.txt")
        
        #1.2 Load testing data
        X.test <- read.table("./test/X_test.txt")
        Y.test <- read.table("./test/Y_test.txt")
        S.test <- read.table("./test/subject_test.txt")

        #1.3 Load metadata (features, activitylabels and subjects for training 
        #    and testing)
        features <- read.table("./features.txt")
        
        activityLabels <- read.table("./activity_labels.txt") 
        colnames(activityLabels) <- c("activityId","activityType")
        
        #1.4 Name training, testing and subject sets
        colnames(X.train) <- features[,2]
        colnames(X.test) <- features[,2]
        
        colnames(Y.train) <- "activityId"
        colnames(Y.test) <- "activityId"
        
        colnames(S.train) <- "subjectId"
        colnames(S.test) <- "subjectId"
        
        #1.5 Merge complete training and testing sets
        Train.set <- cbind(Y.train,S.train,X.train)
        Test.set <- cbind(Y.test,S.test,X.test)
        
        #1.6 Merge training and testing sets into complete set
        Complete.set <- rbind(Train.set,Test.set)
        
#2. Extract only the measurements on the mean and standard deviation 
#   for each measurement
        
        #2.1 Get variable names from complete set
        var.complete.set <- colnames(Complete.set)
        
        #2.2 Create logical vector to identify mean and standard deviation variables
        logical <- (grepl("[Mm]ean",var.complete.set) | grepl("std",var.complete.set))
        
        #2.3 Extract Id variables (activity and subject)
        id.var <- Complete.set[1:2]
        
        #2.4 Filter variables of Complete.set according to 2.2
        Complete.set <- Complete.set[logical==TRUE]
        
        #2.5 Add id variables to Complete.set
        Complete.set <- cbind(id.var,Complete.set)
        
#3. Use descriptive activity names to name the activities in the data set
        
        #3.1 Merge Complete.set and activityLabels by "activityId"
        Complete.set <- merge(activityLabels,Complete.set,by="activityId")
        
#4. Appropriately label the data set with descriptive variable names
        
        #This was already done during item 1 using colnames() function:
        # each variable has its descriptive (feature) name. However, the names
        # can be improved, so some characters will be replaced using gsub() function
        
        #4.1 Rename complete.set variables
        names(Complete.set) <- gsub("Acc", "Accelerometer", names(Complete.set))
        names(Complete.set)<-gsub("Gyro", "Gyroscope", names(Complete.set))
        names(Complete.set)<-gsub("BodyBody", "Body", names(Complete.set))
        names(Complete.set)<-gsub("Mag", "Magnitude", names(Complete.set))
        names(Complete.set)<-gsub("^t", "Time", names(Complete.set))
        names(Complete.set)<-gsub("^f", "Frequency", names(Complete.set))
        names(Complete.set)<-gsub("tBody", "TimeBody", names(Complete.set))
        names(Complete.set)<-gsub("angle", "Angle", names(Complete.set))
        names(Complete.set)<-gsub("gravity", "Gravity", names(Complete.set))
        
        #4.2 Check variables names
        str(Complete.set)

#5.From the data set in step 4, creates a second, independent tidy data set with 
#  the average of each variable for each activity and each subject
        
        #5.1 Aggregate Complete.set by subjectId and activityType using the mean()
        #function
        Mean.set <- aggregate(.~subjectId + activityType, Complete.set, mean)
        
        