# R script run_analysis

# 1. Merges the training and the test sets to create one data set.

data.trainX <- read.table("train/X_train.txt")
data.testX <- read.table("test/X_test.txt")
merge.X <- rbind(data.trainX, data.testX)

subject.train <- read.table("train/subject_train.txt")
subject.test <- read.table("test/subject_test.txt")
merge.S <- rbind(subject.train, subject.test)

data.trainY <- read.table("train/y_train.txt")
data.testY <- read.table("test/y_test.txt")
merge.Y <- rbind(data.trainY, data.testY)

# 2. Extracts measurements on the mean and standard deviation for each measurement.

data.features <- read.table("features.txt")
measures.features <- grep("-mean\\(\\)|-std\\(\\)", data.features[, 2])
merge.X <- merge.X[, measures.features]
names(merge.X) <- data.features[measures.features, 2]
names(merge.X) <- gsub("\\(|\\)", "", names(merge.X))
names(merge.X) <- tolower(names(merge.X))

# 3. Uses descriptive activity names to name the activities in the data set.

name.activities <- read.table("activity_labels.txt")
name.activities[, 2] = gsub("_", "", tolower(as.character(name.activities[, 2])))
merge.Y[,1] = name.activities[merge.Y[,1], 2]
names(merge.Y) <- "activity"

# 4. Labels the data set with descriptive activity names.

names(merge.S) <- "subject"
descriptive <- cbind(merge.S, merge.Y, merge.X)
write.table(descriptive, "tidy_data.txt")

# 5. Creates a independent tidy data set with the average of each variable for each activity and each subject.

Subject.Elem = unique(merge.S)[,1]
Subject.Length = length(unique(merge.S)[,1])
Activity.Length = length(name.activities[,1])
numCols = dim(descriptive)[2]
average = descriptive[1:(Subject.Length*Activity.Length), ]

row = 1
for (s in 1:Subject.Length) {
        for (a in 1:Activity.Length) {
                average[row, 1] = Subject.Elem[s]
                average[row, 2] = name.activities[a, 2]
                tmp <- descriptive[descriptive$subject == s & descriptive$activity == name.activities[a, 2], ]
                average[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(average, "data_set_averages.txt")