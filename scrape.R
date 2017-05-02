#install.packages(c("Rfacebook", "dplyr", "syuzhet","ggplot2", "outliers"))
library(Rfacebook)
library(ggplot2)
library(dplyr)

# token generated here: https://developers.facebook.com/tools/explorer 
token <- 'EAACEdEose0cBAKgAA14sukObKXWlSassZBx3DRc3xL7oiwNQuyK9I1L3OoGRnGmc9AfjHDFgOWClsLCOZBkHM0zj25vBYjXIVa2Pv1DW4Cvg83mKux3HBhE4t5XZA1DFwnN7m7e2NhF9D7xZAiVaVWAxk0DHdrEZC3EG9LPIrZBfShNqDfjIc6HXvWER9nTxgZD'

loadComments <- function(page, token, since, until){
  all_comments <- data.frame(row.names = c("from_id", "from_name", "message"))
  
  ## load posts
  page <- getPage(page, token, n = 5000, since, until)
  
  for(id in page$id){
    post_comments <- getPost(id, token, n = 1000, likes = FALSE, comments = TRUE)$comments
    all_comments <- rbind(all_comments, post_comments)
  }
  
  all_comments
}

#   oehwu
comments_raw <- loadComments("BallderWU", token, '2016/01/01', '2017/04/01')

comments <- comments_raw %>% transmute(
  # remove special chars
  message = iconv(message, sub = "byte"),
  datetime = as.POSIXct(created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT"),
  sentiment = sapply(message, syuzhet::get_sentiment),
  weighted_sentiment = sentiment * likes_count,
  likes_count = likes_count,
  outlier = outliers::scores(weighted_sentiment, type="chisq", prob=0.9)
) %>% filter(likes_count > 0)


ggplot(comments, aes(datetime, weighted_sentiment)) + 
  geom_line(alpha=0.2) +
  geom_point(aes(color=outlier, size=likes_count)) + scale_x_datetime(date_labels = "%b %d")

