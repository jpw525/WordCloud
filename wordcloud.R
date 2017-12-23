##If you have panel data (multiple entries per case) you will need to transform you data to be one column per case. See my column merger script to combine all text entries to one column (can be useful if you are analyzing multiple entries on the same topic and are interested in overall responses, especially if there are few words per entry)


##packages you will need
#install.package("SnowballC")
#install.packages("tm")
#install.packages("ggplot2")
#install.packages("wordcloud")

cname <- file.path("~", "Desktop", "texts")   
cname   
dir(cname)
library(tm)   
docs <- Corpus(DirSource(cname))   

summary(docs)

##remove punctuation and numbers
inspect(docs)
docs <- tm_map(docs, removePunctuation)
inspect(docs)
docs <- tm_map(docs, removeNumbers) 
for(j in seq(docs))   
{   
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}  
docs <- tm_map(docs, tolower) 

##remove stop words (words that do not contain important meaning; e.g., a, an...)
docs <- tm_map(docs, removeWords, stopwords("english"))  

##remove additional words of your choosing
docs <- tm_map(docs, removeWords, c("can","cant","didnt","something")) 

##combine words, or create categories (example below from bullying study)
for (j in seq(docs))
{
  docs[[j]] <- gsub("dont know", "dont_know", docs[[j]])
  docs[[j]] <- gsub("gained weight", "weight", docs[[j]])
  docs[[j]] <- gsub("lose weight", "weight", docs[[j]])
  docs[[j]] <- gsub("chunky", "weight", docs[[j]])
  docs[[j]] <- gsub("dont have a clue", "dont_know", docs[[j]])
}

library(SnowballC)   

docs <- tm_map(docs, stripWhitespace) 
docs <- tm_map(docs, PlainTextDocument) 
dtm <- DocumentTermMatrix(docs)   
dtm   
tdm <- TermDocumentMatrix(docs)   
tdm  
freq <- colSums(as.matrix(dtm))   
length(freq)   

ord <- order(freq) 
dtms <- removeSparseTerms(dtm, 0.2)
inspect(dtms) 
freq[head(ord)] 
freq[tail(ord)]   
head(table(freq), 20)   
freq <- colSums(as.matrix(dtms))   
freq  


wf <- data.frame(word=names(freq), freq=freq)   
head(wf)

##Histogram of word frequency
library(ggplot2)   
p <- ggplot(subset(wf, freq>10), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   



##Your wordcloud!
library(wordcloud)   
set.seed(142)   
wordcloud(names(freq), freq, min.freq=10)

##play around with colors
#wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

