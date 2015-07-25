# load library
library(dplyr)
library(reshape2)

## load the data, assume path was set as working directory
# load feature and activity label, change feature name to valid variable name
features_raw <- read.table("./UCI HAR Dataset/features.txt")
features[,2] <- make.names(features_raw[,2])
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("label","activity")
# load test and train data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# add feature for each column and test label for each row and rename label
names(X_test) <- features[,2]
names(X_train) <- features[,2]
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_test$label <- y_test[,1]
X_train$label <- y_train[,1]
X_test$set <- rep(factor("test"),times=nrow(X_test))
X_train$set <- rep(factor("train"),times=nrow(X_train))
X_test$subject <- subject_test[,1]
X_train$subject <- subject_train[,1]

## Step1: merge two data frames. 
# It turns out there are duplicated columns. I will use make.unique to change the column
# names
step_1 <- rbind(X_test,X_train)
names(step_1) <- make.unique(names(step_1))

## Step2: Extracts only the measurements on the mean and standard deviation 
## for each measurement. 
step_2 <- select(step_1, set, label, subject, contains("mean"), contains("std") )

## Step3: Uses descriptive activity names to name the activities in the data set
# Obviously the following code doesn't work. I need to return back to solve this issue
# funAct <- function(x) {
#         return(filter(activity_labels,code == x)$activity)
# }
# step_3 <- mutate(step_2, set = funAct(set))
# The best way I found is to use merge() function. Column activity now contains the 
# information of the activity. Arrange the column order.

step_3_pre <- merge(step_2, activity_labels,by="label",all.y = FALSE,sort = FALSE)
step_3 <- step_3_pre[,c(3,ncol(step_3_pre),4:(ncol(step_3_pre)-1))]

## step_5 From the data set in step 4, creates a second, independent tidy data 
## set with the average of each variable for each activity and each subject.

step_5 <- aggregate(select(step_3, 5:ncol(step_3)),list(subject = step_3$subject, 
                                                       activity = step_3$activity), mean)

## Save the file into txt file
write.table(step_5, file = "./step_5.txt",row.names = FALSE)
