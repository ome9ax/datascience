# Get the archive containing the datasets
get_file <- function(file_url = 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip', data_dir = 'data'){
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

# Read the file from the archive
read_file <- function(file_name, file_archive, data_dir = 'data'){
  file_path <- file.path(data_dir, file_name)
  if(!file.exists(file_path))
    unzip(file.path(data_dir, file_archive), file = file_name, exdir = data_dir)
# data <- readRDS(gzcon(unz(file_archive, file_path)))
  data <- readRDS(file_path)
  file.remove(file_path)
# return the loaded data frame
  data
}

# Load data from files
file_archive <- get_file()
if(!exists('SCC')) SCC <- read_file('Source_Classification_Code.rds', file_archive)
if(!exists('NEI')) NEI <- read_file('summarySCC_PM25.rds', file_archive)
for(col in c('fips', 'SCC', 'Pollutant', 'type')) NEI[, col] <- as.factor(NEI[, col]) # change the fips, SCC, Pollutant, and type column type to factor
# cols <- c('fips', 'SCC', 'Pollutant', 'type')
# NEI[, (cols) := lapply(.SD, function(x) as.factor(x)), .SDcols = cols]

if(!dir.exists('pics')) dir.create('pics')
# if (!file.exists('load_data.R')) setwd('~/projects/training/datascience/exploratory_analysis')
# source('load_data.R')

# subset the emisions data on motor emission types for 'Baltimode City'
motor_baltimore <- merge(NEI[NEI$fips == '24510', ], SCC[grep('motor', SCC[, 'Short.Name'], ignore.case = T), c('SCC', 'Short.Name')], by = 'SCC')
# Sum the Baltimore City (fips == '24510') emissions for each year
emissions_year_motor_baltimore <- aggregate(Emissions ~ year, motor_baltimore, sum, na.rm = T)
png('pics/plot5.png')# , width = 480, height = 480, units = 'px'
# Plot the chart
plot(emissions_year_motor_baltimore, type = 'o', main = 'Total motor vehicle sources emission in Baltimore City', xlab = 'Year of emissions recorded', ylab = 'Amount of motor vehicle sources emitted, in tons')
dev.off()
