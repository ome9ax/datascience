# Get dataset
file_name <- 'payments.csv'
if(!file.exists(file_name))
    download.file('https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1491868800&Signature=hX2Z3EXE9DPh8k0FfsR~vT8WU8SetSBxzvmI~-XYboa8nHP9RJsVZfLH5TbQY7JndFPgHE3daPn-ALqm3N~BVt~whlaoPyD5lFmXP4-ri-xA-t6VqcH3yHS3~8YNj5MsioZG0VlsXn3I3gyLndmayNk~sW3w9XlO9KgRORKBbec_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A', file_name)

data <- read.csv(file_name)
ny_data <- subset(data, Provider.State == 'NY')[, c('Average.Covered.Charges', 'Average.Total.Payments')]
with(ny_data, plot(log(Average.Covered.Charges), log(Average.Total.Payments), main = 'NY'))

dev.copy(pdf, 'plot1.pdf')
dev.off()
