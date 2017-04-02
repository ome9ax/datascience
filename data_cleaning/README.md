# Data cleaning

This repo contains a single script :
1 script `run_analysis.R`. This script :
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

2 Documents:
- README : the present document
- CodeBook : the generated dataset content description

1 dataset export for the question 5.
