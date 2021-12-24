reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
api_key <- "317Wwl5q3yO9rPPiHDxzPMhIf"
api_secret <- "9gEaL5Lzb5RAYneEQXwv0Pv9f0HyildogcXMjAI7B73Kz30EEZ"
access_token <- "906570111821258752-Yuv0vpNMFLL4fCHl7EsrxC7v9jRAP8V"
access_token_secret <- "444rjKV34f9Rg46D0PWlVErp1lOb18ecA3kbyppc34b1y"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results

posText <- read.delim("C:/Users/PC/Desktop/positive-words.txt", header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim("C:/Users/PC/Desktop/negative-words.txt", header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))
pos.words = c(posText, 'upgrade')
neg.words = c(negText, 'wtf', 'wait', 'waiting','epicfail', 'mechanical')

delta_tweets = searchTwitter('@delta', n=5000)
jetblue_tweets = searchTwitter('@jetblue', n=5000)
united_tweets = searchTwitter('@united', n=5000)

tweets.df=ldply(delta_tweets,function(t) t$toDataFrame())
summary(tweets.df)
setwd("C:/Users/PC/Documents")
getwd()
write.csv(tweets.df,file="deltatweets.csv")

tweets1.df=ldply(jetblue_tweets,function(t) t$toDataFrame())
summary(tweets1.df)
setwd("C:/Users/PC/Documents")
getwd()
write.csv(tweets1.df,file="jetbluetweets.csv")

tweets2.df=ldply(united_tweets,function(t) t$toDataFrame())
summary(tweets2.df)
setwd("C:/Users/PC/Documents")
getwd()
write.csv(tweets2.df,file="united.csv")

delta_txt = sapply(delta_tweets, function(t) t$getText() )
jetblue_txt = sapply(jetblue_tweets, function(t) t$getText() )
united_txt = sapply(united_tweets, function(t) t$getText() )

noof_tweets = c(length(delta_txt), length(jetblue_txt),length(united_txt))

airline<- c(delta_txt,jetblue_txt,united_txt)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
# Parameters
# sentences: vector of text to score
# pos.words: vector of words of positive sentiment
# neg.words: vector of words of negative sentiment
# .progress: passed to laply() to control of progress bar
# create a simple array of scores with laply
scores = laply(sentences,
function(sentence, pos.words, neg.words)
{
# remove punctuation
sentence = gsub("[[:punct:]]", "", sentence)
# remove control characters
sentence = gsub("[[:cntrl:]]", "", sentence)
# remove digits?
sentence = gsub('\\d+', '', sentence)
# define error handling function when trying tolower
tryTolower = function(x)
{
# create missing value
y = NA
# tryCatch error
try_error = tryCatch(tolower(x), error=function(e) e)
# if not an error
if (!inherits(try_error, "error"))
y = tolower(x)
# result
return(y)
}
# use tryTolower with sapply 
sentence = sapply(sentence, tryTolower)
# split sentence into words with str_split (stringr package)
word.list = str_split(sentence, "\\s+")
words = unlist(word.list)
# compare words to the dictionaries of positive & negative terms
pos.matches = match(words, pos.words)
neg.matches = match(words, neg.words)
# get the position of the matched term or NA
# we just want a TRUE/FALSE
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)
# final score
score = sum(pos.matches) - sum(neg.matches)
return(score)
}, pos.words, neg.words, .progress=.progress )
# data frame with scores for each sentence
scores.df = data.frame(text=sentences, score=scores)
return(scores.df)
}

scores = score.sentiment(airline, pos.words,neg.words , .progress='text')

scores$airline = factor(rep(c("Delta", "JetBlue","United"), noof_tweets))

scores$positive <- as.numeric(scores$score >0)
scores$negative <- as.numeric(scores$score >0)
scores$neutral <- as.numeric(scores$score==0)

delta_airline <- subset(scores, scores$airline=="Delta")
jetblue_airline <- subset(scores,scores$airline=="JetBlue")
united_airline <- subset(scores,scores$airline=="United")

delta_airline$polarity <- ifelse(delta_airline$score >0,"positive",ifelse(delta_airline$score < 0,"negative",ifelse(delta_airline$score==0,"Neutral",0)))

jetblue_airline$polarity <- ifelse(jetblue_airline$score >0,"positive",ifelse(jetblue_airline$score < 0,"negative",ifelse(jetblue_airline$score==0,"Neutral",0)))

united_airline$polarity <- ifelse(united_airline$score >0,"positive",ifelse(united_airline$score < 0,"negative",ifelse(united_airline$score==0,"Neutral",0)))

qplot(factor(polarity), data=delta_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - Delta Airlines")

qplot(factor(score), data=delta_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - Delta Airlines")

qplot(factor(polarity), data=jetblue_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle(" Customer Sentiments - JetBlue Airlines ")

qplot(factor(score), data=jetblue_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - JetBlue Airlines")

qplot(factor(polarity), data=united_airline, geom="bar", fill=factor(polarity))+xlab("Polarity Categories") + ylab("Frequency") + ggtitle("Customer Sentiments - United Airlines")

qplot(factor(score), data=united_airline, geom="bar", fill=factor(score))+xlab("Sentiment Score") + ylab("Frequency") + ggtitle("Customer Sentiment Scores - United Airlines ")

df = ddply(scores, c("airline"), summarise,
pos_count=sum( positive ),
neg_count=sum( negative ),
neu_count=sum(neutral))

df$total_count = df$pos_count +df$neg_count + df$neu_count

df$pos_prcnt_score = round( 100 * df$pos_count / df$total_count )
df$neg_prcnt_score = round( 100 * df$neg_count / df$total_count )
df$neu_prcnt_score = round( 100 * df$neu_count / df$total_count )

attach(df)
lbls <-paste(df$airline,df$pos_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(pos_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Positive Comparative Analysis - Airlines")

lbls <-paste(df$airline,df$neg_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neg_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = " Negative Comparative Analysis - Airlines")

lbls <-paste(df$airline,df$neu_prcnt_score)
lbls <- paste(lbls,"%",sep="")
pie(neu_prcnt_score, labels = lbls, col = rainbow(length(lbls)), main = "Neutral Comparative Analysis - Airlines")
