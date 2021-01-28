# | Congratulations on writing your first function. By writing functions, you can gain serious insight into how R
# | works. As John Chambers, the creator of R once said:
#   |
#   | To understand computations in R, two slogans are helpful: 1. Everything that exists is an object. 2. Everything
# | that happens is a function call.

# install.packages(c("dplyr", "tidyr", "readr", "readxl"))
# install.packages("swirl")
# packageVersion("swirl")
# library(swirl)
# install_course("The R Programming Environment")
# install_from_swirl("R Programming")
# install_from_swirl("Getting and Cleaning Data")
# swirl()
# library(dplyr)
# library(tidyr)
# library(readr)
# library(ggplot2)
# library(tidyverse)

#1 Auck ?
#3 Logic int, comp, num, char
#4 integer()
#5 num
#6 m[2,3]
#7 == class
#8 character vector lenght 1 containing a
#9 3456
#10 x<6 || x<=5
#11 hw1<-read.csv(pipe(paste("zcat", 'data/quiz1_data.zip')), header = T)
#   colnames(hw1)
#12 hw1[1:2,]
#13 nrow(hw1)
#14 tail(hw1,2)
#15 hw1[47,'Ozone']
#16 length(hw1$Ozone[is.na(hw1$Ozone)])
#17 mean(hw1$Ozone[!is.na(hw1$Ozone)])
#18 mean(hw1$Solar.R[!is.na(hw1$Solar.R) & !is.na(hw1$Ozone) & hw1$Ozone > 31 & !is.na(hw1$Temp) & hw1$Temp > 90])
#19 mean(hw1$Temp[!is.na(hw1$Temp) & !is.na(hw1$Month) & hw1$Month == 6])
#20 max(hw1$Ozone[!is.na(hw1$Ozone) & hw1$Month == 5])

# 1 q1<-read_csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv')
# download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx', 'data/clean_q1.xlsx')
# dat<-read.xls('data/clean_q1.xlsx', sheet = 1, skip = 17, nrows = 5, header = T, blank.lines.skip = F) %>% select(7:15)
# xmldataframe <- xmlToDataFrame(myurl)

# library(XML)
# library(RCurl)
# myurl<-getURL('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml')
# rootNode<-xmlRoot(xmlTreeParse(myurl, useInternal=TRUE))
# test <- xmlSApply(rootNode[[1]], function(x) xmlSApply(x, xmlValue))
# test_df <- data.frame(t(test),row.names=NULL)
# or tbl_df(x)
# length(test_df$name[test_df$zipcode == 21231])

# download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv', 'data/clean_q5.csv')
# install.packages('data.table')
# library(data.table)
# DT<-fread('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv')
# tapply(DT$pwgtp15,DT$SEX,mean)
# sapply(split(DT$pwgtp15,DT$SEX),mean)
# mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
# rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
# mean(DT$pwgtp15,by=DT$SEX)
# DT[,mean(pwgtp15),by=SEX]

# https://github.com/hadley/httr/blob/master/demo/oauth2-github.r
#
# f_df <- function(x, directory = 'specdata'){
#   file.path('data', directory, paste(gsub(' ', '0', formatC(x, width = 3)), '.csv', sep = '')) %>%
#     read.csv(header = T)
# }
#
# pollutantmean <- function(directory, polluant, id = 1:332) {
#   # bind_rows(lapply(id, f_df))
#   do.call(rbind, lapply(id, f_df, directory))[polluant] %>%
#     as.matrix %>%
#     mean(na.rm = TRUE) %>%
#     round(3)
# }
#
# complete <- function(directory, id = 1:332) {
#   do.call(rbind, lapply(id, function(x) data.frame(id = x, nobs = nrow(filter(f_df(x, directory), !is.na(sulfate) & !is.na(nitrate))))))
# }
#
# corr <- function(directory, threshold = 0){
#   f_length <- function(x, directory = 'specdata', threshold = 0){
#     z <- filter(f_df(x, directory), !is.na(sulfate) & !is.na(nitrate))
#     z <- z[nrow(z) > threshold]
#     if(ncol(z) > 0) cor(z$sulfate, z$nitrate)
#   }
#   z <- c()
#   for(i in 1:332) z <- c(z, f_length(i, directory, threshold))
#   # do.call(c, lapply(1:332, f_length, directory, threshold))
#   z
# }
#
# titanic_3 <- titanic %>%
#   select(Survived, Pclass, Age, Sex) %>%
#   filter(!is.na(Age)) %>%
#   mutate(agecat = cut(Age, breaks = c(0, 14.99, 49, 100), include.lowest = T, labels = c('Under 15', '15 and 50', 'Over 50'), right = T))

# sapply(split(mtcars$mpg, mtcars$cyl), mean)
# tapply(mtcars$mpg,mtcars$cyl,mean)
# with(mtcars, tapply(mpg, cyl, mean))

# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
#
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
#
# qx<-quantile(x$X.1, probs = seq(0,1,0.2), names = T)
# xqx<-cbind(x, qx = cut(x$X.1, breaks = qx, include.lowest = T, labels = names(qx)[2:6]))
xyplot(price~carat | color*cut, data = diamonds, strip = FALSE, pch = 20, xlab = myxlab, ylab = myylab, main = mymain)
qplot(drv, hwy, data = mpg, geom = 'boxplot', color = manufacturer)


