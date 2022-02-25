library(dplyr)

#load data
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt") 
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt") 
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt") 
f_labels <- read.table("./UCI HAR Dataset/features.txt") # features
a_labels <- read.table("./UCI HAR Dataset/activity_labels.txt") # activities

#1. merge train and test data
X_merged <- rbind(X_train, X_test)
y_merged <- rbind(y_train, y_test)
subject_merged <- rbind(subject_train, subject_test)

#2. remove features that don't refer to means or standard deviations (the num variable is used to index the variables to keep)
names(f_labels) <- c("num", "feature")
f_labels$mean_sd <- grepl("mean|std", f_labels$feature)
f_labels <- f_labels %>% filter(mean_sd==T)
X_merged <- X_merged[, f_labels$num]

#4. give names to the feature columns
names(X_merged) <- f_labels$feature

X_merged <- cbind(X_merged, subject_merged)
X_merged <- rename(X_merged, subject=V1)

#3. use factor labels to give names to the activity numbers
y_factor <- data_frame(y=factor(as.character(y_merged$V1), levels=as.character(a_labels$V1), labels = a_labels$V2))

#clean up environment
rm(X_train, X_test, y_train, y_test, subject_train, subject_test)

X_y <- cbind(X_merged, y_merged) %>% rename(activity=V1)

# 5. finally, calculate the average of each variable for each combination of subject and activity
means <-  X_y %>% group_by(activity, subject) %>% summarise_all(mean)