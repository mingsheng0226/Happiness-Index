library(dplyr)
library(wordcloud)
setwd('/Users/riyueyoutu/Desktop/')
happy = read.csv('Happiness.csv')
str(happy)
text = happy %>% select(Q17...Your.gender.,
                        Q12...Please.give.us.ONE.word..about.ONE.thing.that.makes.you.happy.)
colnames(text)=c('gender','word')
text = text %>% filter(gender %in% c('Female','Male') & !(word %in% c('',' ')))
text$gender = as.character(text$gender)
text$word = as.character(text$word)
text$gender = as.factor(text$gender)
str(text)
library(tm)
library(qdap)
male_text = text %>% filter(gender=='Male')
female_text = text %>% filter(gender=='Female')
male_text = male_text[,2]
female_text = female_text[,2]
qdap_clean = function(x){
  x = replace_abbreviation(x)
  x = replace_contraction(x)
  x = removeNumbers(x)
  x = replace_ordinal(x)
  x = replace_symbol(x)
  x = tolower(x)
}
tm_clean = function(x){
  x = tm_map(x, removePunctuation)
  x = tm_map(x, stripWhitespace)
  x = tm_map(x, removeWords, c('with','and','i'))
}
male = qdap_clean(male_text)
female = qdap_clean(female_text)
male = VCorpus(VectorSource(male))
female = VCorpus(VectorSource(female))
male = tm_clean(male)
female = tm_clean(female)
male = TermDocumentMatrix(male)
male_m = as.matrix(male)
female = TermDocumentMatrix(female)
female_m = as.matrix(female)
male_freq = rowSums(male_m)
wordcloud(names(male_freq),male_freq,color='blue')
female_freq = rowSums(female_m)
wordcloud(names(female_freq),female_freq,color='red')

male_hc = removeSparseTerms(male,sparse=0.98)
male_hc
male_hc = hclust(dist(male_hc,method='euclidean'),method='complete')
plot(male_hc,main='Male Frequency Dendrogram',ylab='',xlab='')

female_hc = removeSparseTerms(female,sparse=0.99)
female_hc
female_hc = hclust(dist(female_hc,method='euclidean'),method='complete')
plot(female_hc,main='Female Frequency Dendrogram',ylab='',xlab='')

all_male = paste(male_text, collapse=' ')
all_female = paste(female_text, collapse=' ')
fm_text = c(all_male,all_female)
fm_text = qdap_clean(fm_text)
fm = VCorpus(VectorSource(fm_text))
fm = tm_clean(fm)
fm = TermDocumentMatrix(fm)
colnames(fm) = c('Male','Female')
fm_m = as.matrix(fm)
comparison.cloud(fm_m,colors=c('blue','red'),max.words=50)

common = subset(fm_m,fm_m[,1]>0 & fm_m[,2]>0)
diff = abs(common[,1]-common[,2])
com = cbind(common,diff)
com = com[order(com[,3],decreasing=T),]
comm <- data.frame(Male = com[,1], 
                   Female = com[, 2], 
                   labels = rownames(com))
pyramid.plot(comm$Male, comm$Female,
             labels = comm$labels, gap = 2,
             top.labels = c("Male", "Words", "Female"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)