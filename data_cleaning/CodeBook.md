# Getting and Cleaning Data Course Project

## Input data
The input dataset
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The input dataset includes the following files:

- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
1. WALKING
2. WALKING_UPSTAIRS
3. WALKING_DOWNSTAIRS
4. SITTING
5. STANDING
6. LAYING

- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

Those data are merged together in a single data frame `mean_std_data` containing the `mean` and `standard deviation` measurements all the subjects.

## Output data
### Categorical variables
subject
activity

### Output Measures columns
- tBodyAccMeanX
- tBodyAccMeanY
- tBodyAccMeanZ
- tBodyAccStdX
- tBodyAccStdY
- tBodyAccStdZ
- tGravityAccMeanX
- tGravityAccMeanY
- tGravityAccMeanZ
- tGravityAccStdX
- tGravityAccStdY
- tGravityAccStdZ
- tBodyAccJerkMeanX
- tBodyAccJerkMeanY
- tBodyAccJerkMeanZ
- tBodyAccJerkStdX
- tBodyAccJerkStdY
- tBodyAccJerkStdZ
- tBodyGyroMeanX
- tBodyGyroMeanY
- tBodyGyroMeanZ
- tBodyGyroStdX
- tBodyGyroStdY
- tBodyGyroStdZ
- tBodyGyroJerkMeanX
- tBodyGyroJerkMeanY
- tBodyGyroJerkMeanZ
- tBodyGyroJerkStdX
- tBodyGyroJerkStdY
- tBodyGyroJerkStdZ
- tBodyAccMagMean
- tBodyAccMagStd
- tGravityAccMagMean
- tGravityAccMagStd
- tBodyAccJerkMagMean
- tBodyAccJerkMagStd
- tBodyGyroMagMean
- tBodyGyroMagStd
- tBodyGyroJerkMagMean
- tBodyGyroJerkMagStd
- fBodyAccMeanX
- fBodyAccMeanY
- fBodyAccMeanZ
- fBodyAccStdX
- fBodyAccStdY
- fBodyAccStdZ
- fBodyAccJerkMeanX
- fBodyAccJerkMeanY
- fBodyAccJerkMeanZ
- fBodyAccJerkStdX
- fBodyAccJerkStdY
- fBodyAccJerkStdZ
- fBodyGyroMeanX
- fBodyGyroMeanY
- fBodyGyroMeanZ
- fBodyGyroStdX
- fBodyGyroStdY
- fBodyGyroStdZ
- fBodyAccMagMean
- fBodyAccMagStd
- fBodyBodyAccJerkMagMean
- fBodyBodyAccJerkMagStd
- fBodyBodyGyroMagMean
- fBodyBodyGyroMagStd
- fBodyBodyGyroJerkMagMean
- fBodyBodyGyroJerkMagStd

The following measures names are a concatenation of the signals and the calculations (Mean or Std, for standard deviation)
- tBodyAcc-XYZ
- tGravityAcc-XYZ
- tBodyAccJerk-XYZ
- tBodyGyro-XYZ
- tBodyGyroJerk-XYZ
- tBodyAccMag
- tGravityAccMag
- tBodyAccJerkMag
- tBodyGyroMag
- tBodyGyroJerkMag
- fBodyAcc-XYZ
- fBodyAccJerk-XYZ
- fBodyGyro-XYZ
- fBodyAccMag
- fBodyAccJerkMag
- fBodyGyroMag
- fBodyGyroJerkMag
- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean

Unit used:
* `g` : standard Gravity unit for the acceleration signal
* `radians/seconds` : angular velocity

The signal is captured at a constant rate of 50 Hz

From the input document, we also know that
* prefix 't' to denote time
* prefix 'f' to frequency domain signals (a Fast Fourier Transform (FFT) was applied to some of these signals)

Then
* 'Acc' for Accelerometer
* 'Gyro' for gyroscope

* 'Gravity' for gravity acceleration
* 'Body' for body acceleration

* 'Jerk' the body linear acceleration and angular velocity were derived in time to obtain Jerk signals
* 'Mag' the magnitude of these three-dimensional signals were calculated using the Euclidean norm

* 'XYZ' is used to denote 3-axial signals in the X, Y and Z directions

The same structure is also used for `tidy_data.txt` export containing the mean for each measures per subjects and activites

This final data frame result of the following steps, processed by the `run_analysis.R` script :

- download the dataset
- unzip the dataset
- load the differents files into _R_ data frames
- the subjects and activities are converted into _factor_
- merge the subjects, activities and measures into a single data set for each test and training data
- merge the test and training data together
- slice the measurement columns to keep only the _mean_ and _standard deviation_ related column
- rename the measures column in more descriptive variable names
- build _tidy data_ data frame which contains the mean value of the above measures per subjects and activities
- export the _tidy data_ data frame in a _tidy_data.txt_ flat file.

