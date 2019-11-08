library(lubridate)
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(wesanderson)

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
d_24 <- aggregate(df_24[, 3:6], list(df_24$video_id), min)
Gviews24 <- mean(d_24$Gviews, na.rm = TRUE)
Glikes24 <- mean(d_24$Glikes, na.rm = TRUE)

df_10 <- filter(df_growth, category == 10)
d_10 <- aggregate(df_10[, 3:6], list(df_10$video_id), min)
Gviews10 <- mean(d_10$Gviews, na.rm = TRUE)
Glikes10 <- mean(d_10$Glikes, na.rm = TRUE)

df_26 <- filter(df_growth, category == 26)
d_26 <- aggregate(df_26[, 3:6], list(df_26$video_id), min)
Gviews26 <- mean(d_26$Gviews, na.rm = TRUE)
Glikes26 <- mean(d_26$Glikes, na.rm = TRUE)

final_num = melt(data.frame('Growth in Views'= round(c(Gviews24, Gviews10, Gviews26)*100,2), 'Growth in Likes'= round(c(Glikes24, Glikes10, Glikes26)*100,2),
                     Category=c('Entertainment', 'Music', 'Howto&Style')),
                     variable.name="Growth")
ggplot(final_num, aes(Category, value, fill=Growth)) + 
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label=value), position = position_dodge(0.9), vjust=-0.5, size=3.5) + scale_fill_brewer(palette="OrRd") +
  theme_minimal()

