#  run_analysis.R
#  Coursera_Data_Science
#
#  Created by Chad Junkermeier on 9/24/15.
#  Copyright (c) 2015 Chad Junkermeier. All rights reserved.


#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

#Here are the data for the project: 

#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# You should create one R script called run_analysis.R that does the following:
#    1. Merges the training and the test sets to create one data set.
#    2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#    3. Uses descriptive activity names to name the activities in the data set
#    4. Appropriately labels the data set with descriptive variable names. 
#    5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################




# First I will note that I think that these guys have done an absolutely aweful job of writing up what is what in their data.  The readme was almost no help.  After lot of hair pulling later, and looking at the different files numerous times, I finally think I know what is going on with their files.  BAH!


# To do this assignment I am going to load the reshape2, stringr, and dplyr libraries.  I only really need the reshape2 library, but the dplyr and stringr library allows me to use the pipe symbol, %>%, which really speaks to me because of my functional programming background.
library(reshape2)
library(dplyr)
library(stringr)



# Step 1. Merges the training and the test sets to create one data set.

# I am going to first load the features.txt file in first because I will need it when loading the X_*.txt files.

features_table <- read.table("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/features.txt", colClasses=c("numeric", "character"),col.names=c("ColumnNumber", "Measurement"))

# I am going to use this to supply the measurement names in the col.names argument when I load the X_*.txt files.  I am using the names that are already provided, with a slight two slight changes, as part of my tidy data because then there isn't a question as to what values are being transfered from the original dataset into my new data frame.  The changes are needed to make R play nicely with them.  The changes are to substitute "()" with "" and "-" with "_".

features_names <- features_table$Measurement %>% str_replace_all("\\(\\)","") %>% str_replace_all("-","_")

# I want a way to programmitacly determine which columns in X_test.txt and X_trial.txt that I want to keep.  Also, because these are moderately large files, I don't really want to load into memory anything that I don't need.  Thus the following function, Find_Col_Classes, will create a list that will be used in colClasses argument of read.table.

Find_Col_Classes <- function(n){
    if (1 == length(grep("mean", n))){
         "numeric"
    } else if (1 == length(grep("std", n))){
         "numeric"
    } else {
         "NULL"
    }
}

# I am going to use this to supply the data types in the colClasses argument when I load the X_*.txt files.
features_Col_Classes <- unlist(lapply(features_table$Measurement, Find_Col_Classes))




#I also need a function that will substitute the numeric values in the y_*.txt files for more descriptive names.  Again, I think the best names to use are what is already used in the original data.

change_to_activity_label <- function(n){
    if (1 ==  n){
         "WALKING"
    } else if (n == 2){
         "WALKING_UPSTAIRS"
    } else if (n == 3){
         "WALKING_DOWNSTAIRS"
    } else if (n == 4){
         "SITTING"
    } else if (n == 5){
         "STANDING"
    } else if (n == 6){
         "LAYING"
    }
}



# Now we can read in all of the TEST data and create a TEST data frame.
subject_test <- read.table("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/test/subject_test.txt",col.names=c("Subject_ID"))


y <- unlist(lapply(unlist(scan("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/test/y_test.txt", list(0))), change_to_activity_label))

y_test <- data.frame(activity_labels = y)

X_test <- read.table("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/test/X_test.txt",col.names=features_names,colClasses=features_Col_Classes)

TEST_DF <- cbind(subject_test, y_test,X_test) # This is the combined and named data frame for the TEST data.




# Now we will read in all of the TRAIN data and create a TRAIN data frame.
subject_train <- read.table("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/train/subject_train.txt",col.names=c("Subject_ID"))


yy <- unlist(lapply(unlist(scan("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/train/y_train.txt", list(0))), change_to_activity_label))

y_train <- data.frame(activity_labels = yy)

X_train <- read.table("/Users/chadjunkermeier/Downloads/UCI_HAR_Dataset/train/X_train.txt",col.names=features_names,colClasses=features_Col_Classes)

TRAIN_DF <- cbind(subject_test, y_test,X_test) # This is the combined and named data frame for the TRAIN data.

#########################################################
# Now I will combine TRAIN_DF and TEST_DF which satisfies all of the requirements of steps 1-4.
Combined_TEST_TRAIN_DF <- rbind(TEST_DF, TRAIN_DF)
#########################################################

# This last part is the only thing that is challenging in this assignment-after figuring out what the heck is going on with the original data files.  I would have had no clue what to try if I hadn't worked through all of the Swirl programming assignments.  



# The melt function will take the Combined_TEST_TRAIN_DF data frame and will turn it into a long skinny data frame with four columns: Subject_ID,  activity_labels, variable, and value.  The elements of the variable column are the names of the measurements that act as column names in Combined_TEST_TRAIN_DF.  The values in the melted DF are all of the elements of all of the columns-other than Subject_ID and activity_labels.  We now pipe this into dcast (which is the complement function of melt), which will now apply the mean function to all of the elements of value that have the same elements in the other three columns.


TIDY_DATA <- Combined_TEST_TRAIN_DF       %>%     melt(id.var = c("Subject_ID",  "activity_labels"))    %>%     dcast(Subject_ID + activity_labels ~ variable, mean)


# Write the output to a file
write.table(TIDY_DATA, file = "/Users/chadjunkermeier/Desktop/TIDY_DATA.txt",row.name=FALSE)










