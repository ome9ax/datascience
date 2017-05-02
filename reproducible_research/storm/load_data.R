# Get the archive containing the datasets
get_file <- function(file_url = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', data_dir = 'data'){
  if(!dir.exists(data_dir)) dir.create(data_dir)
  file_name <- basename(file_url)
  file_path <- file.path(data_dir, file_name)
    if(!file.exists(file_path))
    download.file(file_url, file_path, quiet = TRUE)
  if(!file.exists(file_path))
    stop("Unable to download file ", file_url)
# return the archive file name
  file_name
}
# if (!file.exists('load_data.R')) setwd('~/projects/training/datascience/reproducible_research/storm')
# source('load_data.R')

# Load data from files
file_archive <- get_file()
if(!exists('storm')) storm <- read.csv(file.path('data', file_archive), sep = ',')
for(col in c('fips', 'SCC', 'Pollutant', 'type')) NEI[, col] <- as.factor(NEI[, col]) # change the fips, SCC, Pollutant, and type column type to factor
# cols <- c('fips', 'SCC', 'Pollutant', 'type')
# NEI[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols = cols]

if(!dir.exists('pics')) dir.create('pics')
if(!exists('old.par')) old.par <- par() # mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0)
