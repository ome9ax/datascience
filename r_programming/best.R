
# Download the data file
wdir <- 'data'
if(!dir.exists(wdir)){
  zip_file <- 'rprog_data_ProgAssignment3-data.zip'
  if(!file.exists(zip_file))
    download.file('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip', zip_file)
  # Unzip the dataset
  unzip(zip_file, exdir = wdir)
  file.remove(zip_file)
}

best <- function(state, outcome){
  # Load the file into a data frame
  data <- read.csv(file.path('data','outcome-of-care-measures.csv'), colClasses = "character")
  # Convert the state codes to a factor column
  data[, 7] <- factor(data[, 7])
  data[, 2] <- factor(data[, 2])
  if(!(state %in% levels(data[, 7]))) stop('invalid state')
  # Retrieve the outcome column index
  index <- switch(outcome, 'heart attack' = 11, 'heart failure' = 17, 'pneumonia' = 23)
  if(is.null(index)) stop('invalid outcome')
  # Convert the index column to numeric values
  data[, index] <- as.numeric(data[, index])
  # Filter the state with the non null values for the selected outcome column
  data <- data[!is.na(data[, index]) & !is.na(data[, 2]) & data$State == state, ]
  # List the hospitals with the minimum death rate for the selected outcome
  min_data <- data[which(data[, index] == min(data[, index])), 'Hospital.Name']
  # Return the first one alphabeticaly sorted
  min_data[order(min_data)][1]
}
