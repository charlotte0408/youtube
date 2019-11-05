library(lubridate)
library(dplyr)
library(readr)

df <- read.csv('USvideos.csv')
data <- mutate(df, formatted_trending_date = ydm(trending_date))
data_frame <- mutate(data, trending_month = month(formatted_trending_date), 
                     trending_day = day(formatted_trending_date), 
                     trending_year = year(formatted_trending_date))
head(sort(table(data_frame$video_id), decreasing = TRUE), n = 50)
hist(table(data_frame$video_id))

