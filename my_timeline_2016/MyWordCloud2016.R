#devtools::install_github('haven-jeon/KoNLP') 

library(readr)
library(dplyr)
library(reshape2)
library(wordcloud2)
library(KoNLP)
library(ggplot2)

time_line <- read_csv(file.choose())
time_line <- data.frame(time_line)
time_line <- time_line[!complete.cases(time_line$retweeted_status_user_id),]
time_line <- time_line[!complete.cases(time_line$in_reply_to_status_id),]
time_line$timestamp <- as.POSIXct(time_line$timestamp)

time_line$sec <- as.numeric(time_line$timestamp)
time_line_2016 <- subset(time_line, sec>=as.numeric(as.POSIXct('2016-01-01')))

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
