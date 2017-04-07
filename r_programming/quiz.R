
# Download the data file
wdir <- 'data'
if(!dir.exists(wdir)){
  zip_file <- 'quiz_data.zip'
  # if(!file.exists(zip_file))
  #   download.file('https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip', zip_file)
  # Unzip the dataset
  unzip(file.path(wdir, zip_file), exdir = wdir)
  # file.remove(zip_file)
}

mean(data[data$Parameter.Name == 'Bromine PM2.5 LC' & data$State.Name == 'Wisconsin', 'Arithmetic.Mean'], na.rm = TRUE)
with(data, tapply(Arithmetic.Mean, Parameter.Name, mean, na.rm = TRUE))
