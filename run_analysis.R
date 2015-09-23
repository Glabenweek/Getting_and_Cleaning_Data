###############################################################################
#
# 22-09-2015
#
# COURSERA - Getting and Cleaning Data course
#
# Course project
#
# BACKGROUND
#
# One of the most exciting areas in all of data science right now is wearable computing.
# Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms
# to attract new users. The data linked to from the course website represent data collected
# from the accelerometers from the Samsung Galaxy S smartphone.
# 
# Aim of this script:
# 
# - Merges the training and the test sets to create one data set.
# - Extracts only the measurements on the mean and standard deviation for each measurement. 
# - Uses descriptive activity names to name the activities in the data set
# - Appropriately labels the data set with descriptive variable names. 
# - From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.
#
###############################################################################



# Install library required



library(dplyr)


# Download the data of the project



if (!file.exists("data project")) {
        dir.create("data project")
}

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile="./data project/data.zip")
unzip("data project/data.zip")



###############################################################################
# STEP 1 - Merges the training and the test sets to create one data set 
###############################################################################



# 1.1 - Read the training and the test sets 



trai_set <- read.table("UCI HAR Dataset/train/X_train.txt")          # training set
trai_sub <- read.table("UCI HAR Dataset/train/subject_train.txt")    # subject in training set
trai_lab <- read.table("UCI HAR Dataset/train/y_train.txt")          # activity label in training set

test_set <- read.table("UCI HAR Dataset/test/X_test.txt")            # test set
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt")      # subject in test set
test_lab <- read.table("UCI HAR Dataset/test/y_test.txt")            # activity label in test set

features <- read.table("UCI HAR Dataset/features.txt")               # list of all features

colnames(trai_set) <- colnames(test_set) <- features [,2]            # add the features name in each set

trai_set2 <- cbind(trai_sub,trai_set)                                # add the subject identification in training set
test_set2 <- cbind(test_sub,test_set)                                # add the subject identification in test set
colnames(trai_set2)[1] <- colnames(test_set2)[1] <- "Subject_id"     # rename the first column

trai_set3 <- cbind(trai_lab,trai_set2)                               # add the activity label in training set
test_set3 <- cbind(test_lab,test_set2)                               # add the activity label in test set
colnames(trai_set3)[1] <- colnames(test_set3)[1] <- "activity"       # rename the first column



# 1.2 - Merge the two datasets



dataset   <- rbind(trai_set3,test_set3)                              # merge training and test set in on data-set

rm(trai_set, trai_set2, trai_set3,                                   # remove useless objects from working environment
   test_set, test_set2, test_set3, 
   trai_sub, test_sub, trai_lab, test_lab, features)

###############################################################################
# STEP 2 - Extracts only the measurements on the mean and standard deviation for each measurement 
###############################################################################

# According to the "features_info.txt" file, mean and standard deviation values are stored under the labels:
# - mean(): Mean value
# - std(): Standard deviation
# So we create a filter for each measurement required

mean_filter <- grep("mean()",colnames(dataset))
std_filter  <- grep("std()",colnames(dataset))

dataset_sub <- dataset[,c(1,2,mean_filter,std_filter)]                 # extraction of activity name (1), 
                                                                       # the subject name (2), 
                                                                       # the mean (mean_filter) and 
                                                                       # the standard deviation variables (std_filter)

rm(dataset, mean_filter, std_filter)                                   # remove useless objects from working environment



###############################################################################
# STEP 3 - Uses descriptive activity names to name the activities in the data set
###############################################################################



# 3.1 Read the "activity_label".txt file



act_label <- read.table("UCI HAR Dataset/activity_labels.txt")



# 3.2 Replace variable values in "activity" column by descriptive activity names  



dataset2 <- merge(act_label, dataset_sub, by.x = "V1", by.y = "activity")
                                                                     # dataset2 created has the generic activity 
                                                                     # names in the first column, and the descriptive
                                                                     # activity names in the second column

dataset3 <- dataset2[,-1]                                            # remove the first useless column
colnames(dataset3)[1] <- "Activity_name"                            # edit the name of the first column 


rm(act_label, dataset_sub, dataset2)                                 # remove useless objects from working environment



###############################################################################
# STEP 4 - Appropriately labels the data set with descriptive variable names
###############################################################################



# 4.1 - Convert the dataset3 in dataframe suitable for dplyr functions



dataset4 <- tbl_df(dataset3)   



# 4.2 - Rename the labels of the column with descriptive variable names


  
# expand the first "t" and"f" letters to "Time_" and "Frequency_" repsectively

for (i in 1:81){
        char <- colnames(dataset4)[i]
        if (substring(char,1,1) =="t") {char <- paste("Time_",substring(char,2),sep="") }
        if (substring(char,1,1) =="f") {char <- paste("Frequency_",substring(char,2),sep="") }
        colnames(dataset4)[i] <- char
}

# replace the "BodyBody" apparent tipo error to "Body"

colnames(dataset4) <- gsub("BodyBody", "Body", colnames(dataset4))

# replace short variable names by descriptive variable names, separated by "_"

colnames(dataset4) <- gsub("Body", "Body_", colnames(dataset4))
colnames(dataset4) <- gsub("Acc", "Acceleration_", colnames(dataset4))
colnames(dataset4) <- gsub("Gyro", "Angular_velocity_", colnames(dataset4))
colnames(dataset4) <- gsub("Gravity", "Gravity_", colnames(dataset4))
colnames(dataset4) <- gsub("Mag", "Magnitude_", colnames(dataset4))
colnames(dataset4) <- gsub("Jerk", "Jerk_", colnames(dataset4))
colnames(dataset4) <- gsub("-mean", "Mean", colnames(dataset4))
colnames(dataset4) <- gsub("-std", "Std", colnames(dataset4))

# replace remaining "-" by "_" and remove "()" to create valid variable names as defined by the R language

colnames(dataset4) <- gsub("-", "_", colnames(dataset4))
colnames(dataset4) <- gsub("\\(", "", colnames(dataset4))
colnames(dataset4) <- gsub("\\)", "", colnames(dataset4))

rm(dataset3)                                                         # remove useless objects from working environment

###############################################################################
# STEP 5 - Create tidy data set with the average of each variable for each activity and each subject
###############################################################################



dataset5 <- group_by(dataset4, Activity_name, Subject_id) %>% summarise_each(funs(mean))
                                                                     # create groups by activity and subject
                                                                     # and calculate the average for each group


write.table(dataset5, file = "Tidy_Data.txt", row.name=FALSE)        # save the tidy data created with space separator string


rm(dataset4)                                                         # remove useless objects from working environment


