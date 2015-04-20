# Date : 20 April 2015
# Author : V.Kolipakam

#1. Code to merge the train and test data and make a new dataset.

A1 <- read.table("train/X_train.txt")
A2 <- read.table("test/X_test.txt")
X <- rbind(A1, A2)

A1 <- read.table("train/subject_train.txt")
A2<- read.table("test/subject_test.txt")
S <- rbind(A1, A2)

A1 <- read.table("train/y_train.txt")
A2 <- read.table("test/y_test.txt")
Y <- rbind(A1, A2)

#2.Code to extract only mean and standard deviation .

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3.Code for descriptive activity names in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4.Code for appropriate labels of the data set from the descriptive names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Code for the second tidy dataset with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")

