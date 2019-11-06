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
fivenum(table(data_frame$video_id)) ## top 25% stay on the trending list for more than 8 days

pop_list = levels(data_frame$video_id)[table(data_frame$video_id) > 8]
df_trend = data_frame[data_frame$video_id %in% pop_list, ]
dim(df_trend)
category_count <- df_trend %>% count(category = category_id) %>% data.frame()
order <- order(category_count$n, decreasing = TRUE)
category_count$category[order] ## same as the general case, top categories are still top

df_trend$LikeRatio = df_trend$likes / df_trend$views

df_growth = data.frame()

for (id in pop_list){
  id_df = df_trend[df_trend$video_id == id, ]
  for (j in seq(2, dim(id_df)[1])){
    df_growth <- rbind(df_growth, data.frame(id, id_df[j,'category_id'], table(data_frame$video_id)[[id]], 
                              (id_df[j, 'likes'] - id_df[j-1, 'likes']) / id_df[j-1, 'likes'], 
                              (id_df[j, 'comment_count'] - id_df[j-1, 'comment_count']) / id_df[j-1, 'comment_count'], 
                              (id_df[j, 'views'] - id_df[j-1, 'views']) / id_df[j-1, 'views'],
                              (id_df[j, 'LikeRatio'] - id_df[j-1, 'LikeRatio']) / id_df[j-1, 'LikeRatio']))
  }
}
colnames(df_growth) <- c('video_id', 'category','trending_days', 'Glikes', 'Gcomments', 'Gviews', 'GLikeRatio')

df_24 <- filter(df_growth, category == 24)
d <- aggregate(df_24[, 3:6], list(df_24$video_id), min)
mean(d$Gviews, na.rm = TRUE)
cor(d$trending_days, d$GlikeRatio, use = "complete.obs")

df_23 <- filter(df_growth, category == 23)
d <- aggregate(df_23[, 3:6], list(df_23$video_id), min)
mean(d$Gviews, na.rm = TRUE)
mean(d$Glikes, na.rm = TRUE)

df_10 <- filter(df_growth, category == 10)
d <- aggregate(df_10[, 3:6], list(df_10$video_id), min)
mean(d$Gviews, na.rm = TRUE)
mean(d$Glikes, na.rm = TRUE)

df_26 <- filter(df_growth, category == 26)
d <- aggregate(df_26[, 3:6], list(df_26$video_id), min)
mean(d$Gviews, na.rm = TRUE)
mean(d$Glikes, na.rm = TRUE)

plot(seq(1:13), df_g$Glikes, type="l")
lines(seq(1:13), df_c$Gviews, col = 'blue')
lines(seq(1:9),df_g$Gviews, col="red")
lines(seq(1:9),df_g$Gcomments, col="green")
lines(seq(1:13), df_c$Glikes, type="o")
lines(seq(1:13), df_c$Gviews, col = 'black')
lines(seq(1:13),df_c$Gcomments, col="purple")
