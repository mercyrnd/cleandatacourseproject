## Note: Throughout the code I've used the rm() command to clean up the environment.  I am not sure if this 
## is stylistically discouraged, I just don't like all of these dataframes etc. cluttering the space!

setwd("C:/Users/Telles/R/data/UCI HAR Dataset/")

library(readr)
library(dplyr)

## Read in the activity labels
activity_labels <- read.table("activity_labels.txt")

## Read in the features information
features <- read_delim("features.txt"," ", escape_double = FALSE, col_names = FALSE,trim_ws = TRUE)
## Convert the features names to a vector, so it can be used to rename the columns of the x_data.          
        ll <- as.vector(features$X2)
        
## Read in the subject information for the test group
subject_test <- read.table("test/subject_test.txt")
       
## Read in the activity information for the test group
        y_test <- read.table("test/y_test.txt")
                
## Read in the data for the test group           
        x_test <- read.table("test/X_test.txt")
       
######################################################################################################           
 ## This (and the corresponding code for the train group) is the response to QUESTION 4
        colnames(subject_test) <- "subject"  
        colnames(y_test) <- "activity"
        colnames(x_test) <- ll
######################################################################################################           

        
## Merge the tables, adding subject and activity to the collected data table. 
        test <- cbind(subject_test, y_test, x_test)
        
        rm(subject_test, y_test, x_test)

## Now do the same for the train data. 

        ## Read in the subject information for the train group
        subject_train <- read.table("train/subject_train.txt")
       
        ## Read in the activity information for the train group
        y_train <- read.table("train/y_train.txt")
        
        ## Read in the data for the train group              
        x_train <- read.table("train/X_train.txt")
        
######################################################################################################          
## This (and the corresponding code for the test group) is the response to QUESTION 4
        colnames(subject_train) <- "subject"
        colnames(y_train) <- "activity"
        colnames(x_train) <- ll 
######################################################################################################   
        
## Merge the tables, adding subject and activity to the collected data table. 
        train <- cbind(subject_train, y_train, x_train)
        rm(subject_train, y_train, x_train)

## Next, combine the training and test data sets.

        all <- rbind(test, train)
        rm(test, train)
        
######################################################################################################        
## This is the response for QUESTION 1 of the assignment
View(all)        
######################################################################################################

## Now, we need to select only the columns for means and standard deviations(stds)
## Use grepl to match the column names (from the ll vector)

sel <- (grepl("mean", ll) | grepl("std", ll)) & !grepl("Freq", ll)

## BUT, a two-value vector must be added to the "left"/front of the sel vector, so we will capture the
## subject and activity as well. 

add <- c(TRUE, TRUE); sel <- c(add, sel)

## Use the vector sel to select only the columns that are means and std measurements.

###################################################################################################### 
##This is the response for QUESTION 2 of the assignment
small <- all[,sel]
###################################################################################################### 

rm(add, sel)

## Merge the activity_labels table with the narrowed-down data, so that each entry has a named activity
## then rename the column to "activity" and get rid of the column with the "numbered" activity.

###################################################################################################### 
## This is the response for QUESTION 3 of the assignment
mall <- merge(activity_labels, small, by.x = "V1", by.y = "activity")
mall <- rename(mall, "activity" = "V2"); mall <- mall[,-1]
###################################################################################################### 

rm(small)

## Now to output the 
meann <- mall %>%
        group_by(subject, activity) %>%
        summarize_all(funs(mean))

###################################################################################################### 
## This is the response for QUESTION 5 of the assignment
View(meann)
###################################################################################################### 
