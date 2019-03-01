library(readxl)
library("ggpubr")
library(stringr)

#########################################################
# Stats about words
#########################################################

words_all= read_csv("words.csv")

mean =mean(words_all$tf)
sd =sd(words_all$tf)
availablePackages <- available.packages()[,1]
words<- words_all[words_all$offered>-5,]
features<- words_all[words_all$offered>5 & words_all$tf>0, c("word","tf","idf","success_rate","lemmaTF","bucket")]


#features$len = nchar(words_all$word)

#hist(table(head),main="TF histogram for the words", col='gray', breaks = 20)
#hist(features$bucket,main="Sucees rate of words recognition ", col='gray')


ggscatter(features, x = "tf", y = "success_rate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Words TF in children corpus", ylab = "Answer Success Rate")

cor.test(features$tf, features$success_rate,  method="pearson")

total_offered =sum(words_all$offered)
mean =mean(words$tf)
sd =sd(words$tf)

#barplot(table(words$pos),main = "POS Tag Histogram")
head=words[words$tf>=40,"tf"]
tail=words[words$tf<40,"tf"]


#hist(features$bucket,main="Sucees rate of words recognition ", col='gray')

#########################################################
# Stats about flights
#########################################################

sessionLength = function(x) {
  x1 =sort(x, decreasing = TRUE)[1]
  x2= sort(x, decreasing = FALSE)[1]
  d1=strptime(x1,"%m/%d/%Y %H:%M")
  d2=strptime(x2,"%m/%d/%Y %H:%M")
  dif =abs(as.numeric(difftime(d1,d2), units='mins'))
  return (dif)
} 

count = function(x) {
    
    return (length(x))
} 

flightdata= read_csv("flightdata.csv")

flightdata_b = flightdata[flightdata$flight == "languagerank" & flightdata$language !="ru" & flightdata$word !="apple", c("date", "userid", "success","word")]
flightdata_a = flightdata[flightdata$flight == "noflight" & flightdata$language !="ru" & flightdata$word !="apple", c("date", "userid", "success","word")]

unique_users_total = unique(flightdata$userid)
unique_users_a = unique(flightdata_a$userid)
unique_users_b = unique(flightdata_b$userid)

unique_words_a = unique(flightdata_a$word)
unique_words_b = unique(flightdata_b$word)

#group_a_correct = nrow(flightdata_a[flightdata_a$success == TRUE,]) 
#group_b_correct = nrow(flightdata_b[flightdata_b$success == TRUE,]) 

pval = fisher.test(data.frame(A=c(group_a_correct,nrow(flightdata_a)-group_a_correct), B=c(group_b_correct,nrow(flightdata_b)-group_b_correct)))


agg_a = aggregate(flightdata_a, by=list(flightdata_a$userid), FUN = function(x) c( time=count(x)))
agg_b = aggregate(flightdata_b, by=list(flightdata_b$userid), FUN = function(x) c( time=count(x)))

mean_a=length(agg_a[agg_a$date=1,c("date")])
mean_b=length(agg_b[agg_b$date=1,c("date")])


plot(agg_a$date)


