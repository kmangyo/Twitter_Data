library(tidyr)
library(readr)
library(dplyr)
library(reshape2)
library(wordcloud2)
library(KoNLP)
useNIADic()
library(ggplot2)
theme_set(theme_gray(base_family='NanumGothic'))
library(stringr)

time_line <- read_csv(file.choose())
time_line <- data.frame(time_line)
time_line <- time_line[!complete.cases(time_line$retweeted_status_user_id),]
time_line <- time_line[!complete.cases(time_line$in_reply_to_status_id),]
time_line$timestamp <- as.POSIXct(time_line$timestamp)

# as.POSIXct(1514757238+32400,origin='1970-01-01')
time_line$sec <- as.numeric(time_line$timestamp+32400)
time_line_16_17 <- subset(time_line, sec>=as.numeric(as.POSIXct('2016-01-01')) & sec<as.numeric(as.POSIXct('2018-01-01')))
time_line_16_17$time <- as.POSIXct(time_line_16_17$sec,origin='1970-01-01')

# by year & month
time_line_16_17$time <- as.POSIXlt(time_line_16_17$time)
time_line_16_17$year <- time_line_16_17$time$year+1900
table(time_line_16_17$year)

time_line_16_17$mon <- time_line_16_17$time$mon+1
twt_yearnmon<-table(time_line_16_17$year, time_line_16_17$mon) %>% data.frame()
names(twt_yearnmon)[1:2] <- c('Year','Mon')
ggplot(data= twt_yearnmon, aes(x=Mon, y=Freq, group=Year)) + geom_line(aes(linetype=Year)) + geom_point()

# by year & hour
time_line_16_17$hour<- time_line_16_17$time$hour
twt_yearnhour<-table(time_line_16_17$year, time_line_16_17$hour) %>% data.frame()
names(twt_yearnhour)[1:2] <- c('Year','Hour')
ggplot(data= twt_yearnhour, aes(x=Hour, y=Freq, group=Year)) + geom_line(aes(linetype=Year)) + geom_point()

# by year & wday
time_line_16_17$wday<- time_line_16_17$time$wday
twt_yearnwday<-table(time_line_16_17$year, time_line_16_17$wday) %>% data.frame()
names(twt_yearnwday)[1:2] <- c('Year','wDay')
ggplot(data= twt_yearnwday, aes(x=wDay, y=Freq, group=Year)) + geom_line(aes(linetype=Year)) + geom_point()

# pre-processing
time_line_16_17$text<- gsub("ㅋ", " ", time_line_16_17$text)
time_line_16_17$text<- gsub("ㅎ", " ", time_line_16_17$text)
time_line_16_17$text<- gsub("ㅜ", " ", time_line_16_17$text)
time_line_16_17$text<- gsub("ㄷ", " ", time_line_16_17$text)
time_line_16_17$text<- gsub("ㅠ", " ", time_line_16_17$text)

time_line_16_17$text<- gsub("[[:punct:]]", " ", time_line_16_17$text)
time_line_16_17$text<- gsub("\\w", " ", time_line_16_17$text)
time_line_16_17$text<- gsub("\\s+", " ", time_line_16_17$text)

# extract noun using KonLP
time_line_16<- subset(time_line_16_17, year==2016)
text.noun_16<- list()
for (i in 1:nrow(time_line_16) ) {
  text.noun_16[i]<-melt(extractNoun(time_line_16[i,6]))
}

time_line_17<- subset(time_line_16_17, year==2017)
text.noun_17<- list()
for (i in 1:nrow(time_line_17) ) {
  text.noun_17[i]<-melt(extractNoun(time_line_17[i,6]))
}

text.noun_16_df <-melt(text.noun_16)
text.noun_17_df <-melt(text.noun_17)

text.noun_16_df$group<-'Y2016'
text.noun_17_df$group<-'Y2017'

# 2017 twt
text.noun_17_df_table<- table(text.noun_17_df$value) %>% data.frame()
text.noun_17_df_table$nchar <- nchar(as.character(text.noun_17_df_table$Var1))
text.noun_17_df_table<-subset(text.noun_17_df_table, nchar>1)
wordcloud2(data = text.noun_17_df_table, size = 1, fontFamily='NanumGothic', figPath = '/Users/kims/Library/Mobile Documents/com~apple~CloudDocs/twitter_kor_year/twit.png',color = "skyblue",backgroundColor = "darkblue")

text.noun_df<-rbind(text.noun_16_df,text.noun_17_df)
text.noun_df$nchar <- nchar(as.character(text.noun_df$value))
text.noun_df <- subset(text.noun_df, nchar>1)

# top10 by year
text.noun_df_hist_year <- text.noun_df %>% count(group, value) %>% arrange(-n)
ggplot(head(subset(text.noun_df_hist_year,group==c('Y2017')), 10),aes(x=reorder(value, n), y=n)) + geom_bar(stat = "identity") + coord_flip() + facet_grid(.~group) + xlab('Words')
ggplot(head(subset(text.noun_df_hist_year,group==c('Y2016')), 10),aes(x=reorder(value, n), y=n)) + geom_bar(stat = "identity") + coord_flip() + facet_grid(.~group) + xlab('Words')

text.noun_df_hist<- text.noun_df %>% count(value)
text.noun_df_hist<- text.noun_df_hist %>% arrange(-n)

text.noun_df<- left_join(text.noun_df, text.noun_df_hist, c('value'))
names(text.noun_df)[5] <-'cnt'

# compare nouns 16/17
text.noun_df_count <- text.noun_df %>% filter(cnt>=20) %>% 
  count(group, value) %>%
  spread(group, n, fill = 0) %>%
  mutate(total = Y2016 + Y2017,
         Y2016 = (Y2016 + 1) / sum(Y2016 + 1),
         Y2017 = (Y2017 + 1) / sum(Y2017 + 1),
         log_ratio = log2(Y2017 / Y2016),
         abs_ratio = abs(log_ratio)) %>%
  arrange(desc(log_ratio))

text.noun_df_count$group<-ifelse(text.noun_df_count$log_ratio>0,c('Y2017'),c('Y2016'))
ggplot(rbind(head(text.noun_df_count, 10),tail(text.noun_df_count, 10)), aes(x=reorder(value, log_ratio), y=log_ratio)) + geom_bar(aes(fill = group), stat = "identity") + coord_flip() + xlab('Words')
