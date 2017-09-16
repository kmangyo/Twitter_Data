library(twitteR)
library(dplyr)
library(cronR)

consumer_key='XXXX'
consumer_secret='XXXX'
access_token='XXXX'
access_secret='XXXX'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

sema_data <-read.csv("sema_data.csv")
sema_rand<-sample_n(sema_data, 3)

for (i in 1:3){
  myurl <- sema_rand[i,]$main_image
  sub <- with(sema_rand[i,],paste0(작가명,
                                      " (",제작년도,"), "
                                      ,"'",작품명.국문.,"', "
                                      ,"'",작품분류명,"', "
                                      ,"'", 재료.기법,"', "
                                      ,"'",소장처분류명,"', "))
  download.file(as.character(myurl),'a.jpg',mode="wb")
  tweet(sub, mediaPath = "a.jpg")
}
