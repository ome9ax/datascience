# Get dataset
file_name <- 'payments.csv'
if(!file.exists(file_name))
    download.file('https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1491868800&Signature=hX2Z3EXE9DPh8k0FfsR~vT8WU8SetSBxzvmI~-XYboa8nHP9RJsVZfLH5TbQY7JndFPgHE3daPn-ALqm3N~BVt~whlaoPyD5lFmXP4-ri-xA-t6VqcH3yHS3~8YNj5MsioZG0VlsXn3I3gyLndmayNk~sW3w9XlO9KgRORKBbec_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A', file_name)

data <- read.csv(file_name)
sp_data <- split(data, data$Provider.State)

# old.par <- par() # mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0)
par(mfrow = c(3, ceiling(length(sp_data) / 2)), mar = c(4.1, 4.1, 2.1, 1.1), oma = c(1, 1, 1, 1))
get_plot <- function(i)
    with(sp_data[[i]], plot(log(Average.Covered.Charges), log(Average.Total.Payments), pch = '.', main = names(df_data[i]), col = DRG.Definition))

lapply(seq(length(sp_data)), get_plot)

# par(oma = c(4, 1, 1, 1))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(1, 0, 8, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
with(data, legend("bottom", legend = levels(DRG.Definition), pch = 19, col = seq_along(levels(DRG.Definition)), xpd = TRUE))

dev.copy(pdf, 'plot2.pdf')
dev.off()
