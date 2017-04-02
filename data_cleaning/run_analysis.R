
# Get dataset
wdir <- 'UCI HAR Dataset'
if(!dir.exists(wdir)){
  zip_file <- 'UCI_HAR_dataset.zip'
  if(!file.exists(zip_file))
    download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', zip_file)
  unzip(zip_file)
  file.remove(zip_file)
}

# Load activity labels
activity <- read.table(file.path(wdir, 'activity_labels.txt'))
# convert the activity label columns into a factor column
activity[,2] <- factor(activity[,1], labels = activity[,2])

# Load features
features<-read.table(file.path(wdir, 'features.txt'))
# convert the features label columns into a factor column
features[,2]<-factor(features[,1], labels = features[,2])

# Load test data
x_test <- read.table(file.path(wdir, 'test/x_test.txt'))
# the features columns labels
colnames(x_test) <- features[,2]
y_test <- read.table(file.path(wdir, 'test/y_test.txt'))
colnames(y_test) <- 'activity'
subject_test <- read.table(file.path(wdir, 'test/subject_test.txt'))
# convert the subjects ids columns into a factor column
subject_test[,1] <- factor(subject_test[,1])
colnames(subject_test) <- 'subject'

# merge test columns into a single dataset
test <- cbind(subject_test, y_test, x_test)

# Load training data
x_train <- read.table(file.path(wdir, 'train/x_train.txt'))
# the features columns labels
colnames(x_train) <- features[,2]
y_train <- read.table(file.path(wdir, 'train/y_train.txt'))
colnames(y_train) <- 'activity'
subject_train <- read.table(file.path(wdir, 'train/subject_train.txt'))
# convert the subjects ids columns into a factor column
subject_train[,1] <- factor(subject_train[,1])
colnames(subject_train) <- 'subject'

# merge training columns into a single dataset
train <- cbind(subject_train, y_train, x_train)

# 1. Merges the training and the test sets to create one data set.
data <- rbind(test, train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_data <- data[, grep('subject|activity|-mean\\(|-std\\(', colnames(data))]

# 3. Uses descriptive activity names to name the activities in the data set
mean_std_data[, 'activity'] <- factor(mean_std_data[, 'activity'], labels = activity[,2])

# 4. Appropriately labels the data set with descriptive variable names.
colnames(mean_std_data) <- sub('(\\w+)-(\\w)(\\w+)\\W+', '\\1\\U\\2\\L\\3', colnames(mean_std_data), perl = TRUE)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- aggregate(. ~ subject + activity, mean_std_data, mean)

# export the tidy_data data frame into a text file
write.table(tidy_data, "tidy_data.txt", row.name=FALSE)
