# Get dataset
wdir <- 'UCI HAR Dataset'
zip_file <- 'UCI_HAR_dataset.zip'
if(!file.exists(zip_file))
    download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', zip_file)

# Load activity labels
activity <- read.table(unz(zip_file, file.path(wdir, 'activity_labels.txt')))
activity[, 2] <- factor(activity[, 1], labels = activity[, 2]) # convert the activity label columns into a factor column

# Load features
features <- read.table(unz(zip_file, file.path(wdir, 'features.txt')))
# convert the features label columns into a factor column
# features[, 2] <- factor(features[, 1], labels = features[, 2])

extract_data <- function(unit){
    # Load data
    x <- read.table(unz(zip_file, file.path(wdir, unit, paste0('X_', unit, '.txt'))))
    colnames(x) <- features[, 2] # the features columns labels
    y <- read.table(unz(zip_file, file.path(wdir, unit, paste0('y_', unit, '.txt'))), col.names = 'activity')
    subject <- read.table(unz(zip_file, file.path(wdir, unit, paste0('subject_', unit, '.txt'))), col.names = 'subject')

    # merge columns into a single dataset
    cbind(subject, y, x)
}

# 1. Merges the training and the test sets to create one data set.
data <- do.call(rbind, lapply(c('test', 'train'), extract_data))
data[, 'subject'] <- factor(data[, 'subject']) # convert the subjects ids columns into a factor column

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_data <- data[, grep('subject|activity|-mean\\(|-std\\(', colnames(data))]

# 3. Uses descriptive activity names to name the activities in the data set
mean_std_data[, 'activity'] <- factor(mean_std_data[, 'activity'], labels = activity[, 2])

# 4. Appropriately labels the data set with descriptive variable names.
colnames(mean_std_data) <- sub('(\\w+)-(\\w)(\\w+)\\W+', '\\1\\U\\2\\L\\3', colnames(mean_std_data), perl = TRUE)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy_data <- aggregate(. ~ subject + activity, mean_std_data, mean)
write.table(tidy_data, "tidy_data.txt", row.name = FALSE) # export the tidy_data data frame into a text file

# remove zip file
file.remove(zip_file)
