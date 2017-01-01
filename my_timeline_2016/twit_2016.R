#devtools::install_github('haven-jeon/KoNLP') 

library(readr)
library(dplyr)
library(reshape2)
library(wordcloud2)
library(KoNLP)
library(ggplot2)
library(stringr)

time_line <- read_csv(file.choose())
time_line <- data.frame(time_line)
time_line <- time_line[!complete.cases(time_line$retweeted_status_user_id),]
time_line <- time_line[!complete.cases(time_line$in_reply_to_status_id),]
time_line$timestamp <- as.POSIXct(time_line$timestamp)

time_line$sec <- as.numeric(time_line$timestamp)
time_line_2016 <- subset(time_line, sec>=as.numeric(as.POSIXct('2016-01-01')) & sec<as.numeric(as.POSIXct('2017-01-01')))

# wordcloud

time_line_2016$text<- gsub("ㅋ", " ", time_line_2016$text)
time_line_2016$text<- gsub("ㅎ", " ", time_line_2016$text)
time_line_2016$text<- gsub("ㅜ", " ", time_line_2016$text)
time_line_2016$text<- gsub("ㄷ", " ", time_line_2016$text)
time_line_2016$text<- gsub("ㅠ", " ", time_line_2016$text)

time_line_2016$text<- gsub("[[:punct:]]", " ", time_line_2016$text)
time_line_2016$text<- gsub("\\w", " ", time_line_2016$text)
time_line_2016$text<- gsub("\\s+", " ", time_line_2016$text)

time_line_2016$text<- toupper(time_line_2016$text)

text.noun<- list()
for (i in 1:nrow(time_line_2016) ) {
  text.noun[i]<-melt(extractNoun(time_line_2016[i,6]))
}

text.noun<-melt(text.noun)
text.noun<-subset(text.noun, nchar(as.character(text.noun$value))>1)
text.noun.table<-table(text.noun$value)
text.noun.table<-subset(data.frame(text.noun.table), Freq > 1)

wordcloud2(data = text.noun.table, size = .5, fontFamily='NanumGothic', figPath = 'twit_pic.png',color = "skyblue",backgroundColor = "darkblue")

# freq by day
time_line_2016$day<-as.Date(time_line_2016$timestamp)
time_line_2016_day<-time_line_2016 %>% group_by(day) %>% summarise(freq=n())
ggplot(time_line_2016_day, aes(x= factor(day), y = freq)) + geom_bar(stat = "identity")
write.table(time_line_2016_day, "time_line_2016_day.csv", quote = T, sep = ",", row.name=F)

# freq by hour
time_line_2016$hour<-as.POSIXlt(time_line_2016$timestamp)
time_line_2016$hour<-time_line_2016$hour$hour+9
time_line_2016_hour<-time_line_2016 %>% group_by(hour) %>% summarise(freq=n())
time_line_2016_hour$hour<-with(time_line_2016_hour, ifelse(hour>=24, hour-24, hour))
time_line_2016_hour <-rbind(time_line_2016_hour, data.frame(hour=c(5,6),freq=c(0,0)))
ggplot(time_line_2016_hour, aes(x= factor(hour), y = freq)) + geom_bar(stat = "identity")

# pic freq by day
time_line_2016$pic<-ifelse(str_count(time_line_2016$expanded_urls, "photo|video")>0,1,0)
time_line_2016$pic[is.na(time_line_2016$pic)]<-0
time_line_2016_day.pic<-time_line_2016 %>% group_by(day) %>% summarise(freq=sum(pic))
ggplot(time_line_2016_day.pic, aes(x= factor(day), y = freq)) + geom_bar(stat = "identity")
time_line_2016_day.pic<-subset(time_line_2016_day.pic, freq>0)
write.table(time_line_2016_day.pic, "time_line_2016_day.pic.csv", quote = T, sep = ",", row.name=F)
