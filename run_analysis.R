library(dplyr)
##colnames
col <- read.table("./UCI HAR Dataset/features.txt")
col_frame <- as.data.frame(col)

## creating train dataset
user_id <- read.table("./UCI HAR Dataset/train/subject_train.txt")
user_frame <- as.data.frame(user_id)
train_table <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_frame <- as.data.frame(train_table)
colnames(train_frame) <- col[,2]
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
y_train_frame <- as.data.frame(y_train)

dataset_train <- data.frame(user_id = user_frame[,1], activity = y_train_frame[,1], train_frame)

## creating test dataset with labels (4.)
user_id_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
user_test_frame <- as.data.frame(user_id_test)
test_table <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_frame <- as.data.frame(test_table)
colnames(test_frame) <- col[,2]
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_test_frame <- as.data.frame(y_test)

dataset_test <- data.frame(user_id = user_test_frame[,1], activity = y_test_frame[,1], test_frame)

## 1. Merging data
merge_dataset <- merge(dataset_train, dataset_test, all=TRUE)

## 2. Extracting only the mean and standard deviation
std_vector <- grep("std", col_frame$V2)+2
mean_vector <- grep("mean", col_frame$V2)+2
user_activity_vector <- c(1,2)
dataset_vector <- c(user_activity_vector, std_vector, mean_vector)
dataset_vector < sort(dataset_vector)

extracted_dataset <- merge_dataset[,dataset_vector]

## 3. Using descriptive activity names
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity_labels$V1 <- as.character(activity_labels$V1)
class(activity_labels$V1)
extracted_dataset$activity <- as.character(extracted_dataset$activity)
class(extracted_dataset$activity)
i <- 1
for (lev in activity_labels[,1]) {
  extracted_dataset$activity<-gsub(lev, activity_labels[i,2], extracted_dataset$activity)
  i <- i+1
}

## 4. label the data set - done on row 22

## 5. Data set with the average of each variable for each activity and each subject
independent_tidy_dataset <- extracted_dataset %>% 
          group_by(user_id, activity) %>% 
          summarize(across(everything(), list(mean)))
tidy_colnames_vector<-c("user_id", "activity")
for (col in colnames(independent_tidy_dataset[3:81])) {
  name <- paste("AVG", col, sep="_")
  tidy_colnames_vector <- c(tidy_colnames_vector,name)
}
colnames(independent_tidy_dataset) <- tidy_colnames_vector
## create table
write.table(independent_tidy_dataset, file = "./tidy_dataset.txt", row.name = FALSE)
