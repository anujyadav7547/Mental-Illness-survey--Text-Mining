setwd("E:/Tutorial/Mental Health in Tech Survey/Text Mining in R/Mental Illness survey -Text Mining")
install.packages("tm")
install.packages("sas7bdat")
install.packages("wordcloud") # word-cloud generator 
#library(wordnet)
install.packages("xlsx")
library(xlsx)
#library(tidyr)
library(tm)
library(sas7bdat)
#library(quanteda)
library(wordcloud)
survey2016 <- read.sas7bdat("survey2016.sas7bdat")
## Drop relevant columns
col <- c(40,48,64)
survey2016 <- survey2016[,col]
survey2016$Row <- 1:nrow(survey2016)
survey2016<-subset(survey2016,GEN != "")
survey2016<-subset(survey2016, MentalDisorderNow == "Yes")
survey2016<-subset(survey2016, ReasonRevealMentalInInterview != "")



## now merging all the records to form a single row datasource
Reasons <- sapply(unique(survey2016$GEN),
                  function(x){
 
                        paste(survey2016[survey2016$GEN==x,"ReasonRevealMentalInInterview"]
                              , collapse = " " )
               
                    
                  })

ReasonTxt<- trimws(ReasonTxt, "both")



GEN <- as.list(as.character(unique(survey2016$GEN)))

ReasonTxt <- as.list(ReasonTxt)
Collapsed<- cbind(GEN,ReasonTxt)

maleReason <- as.character(subset(Collapsed,GEN == "Male")[,2])
femaleReason <- as.character(subset(Collapsed,GEN == "Female")[,2])
OtherReason <- as.character(subset(Collapsed,GEN == "Other")[,2])

maleDocs <- Corpus(VectorSource(maleReason))
femaleDocs <- Corpus(VectorSource(femaleReason))
otherDocs <- Corpus(VectorSource(OtherReason))
inspect(maleDocs)
inspect(femaleDocs)
inspect(otherDocs)



toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
maleDocs <- tm_map(maleDocs, toSpace, "/")
maleDocs <- tm_map(maleDocs, toSpace, "@")
maleDocs <- tm_map(maleDocs, toSpace, "\\|")

femaleDocs <- tm_map(femaleDocs, toSpace, "/")
femaleDocs <- tm_map(femaleDocs, toSpace, "@")
femaleDocs <- tm_map(femaleDocs, toSpace, "\\|")

otherDocs <- tm_map(otherDocs, toSpace, "/")
otherDocs <- tm_map(otherDocs, toSpace, "@")
otherDocs <- tm_map(otherDocs, toSpace, "\\|")


# Convert the text to lower case
maleDocs <- tm_map(maleDocs, content_transformer(tolower))
# Remove numbers
maleDocs <- tm_map(maleDocs, removeNumbers)
# Remove english common stopwords
maleDocs <- tm_map(maleDocs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
maleDocs <- tm_map(maleDocs, removeWords, c("issue", "issues", "mental", "health")) 
# Remove punctuations
maleDocs <- tm_map(maleDocs, removePunctuation)
# Eliminate extra white spaces
maleDocs <- tm_map(maleDocs, stripWhitespace)
# Text stemming
#maleDocs <- tm_map(maleDocs, stemDocument)

dtm <- TermDocumentMatrix(maleDocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
MaleRows <- data.frame(word = names(v),freq=v)
head(MaleRows, 10)

wordcloud(words = MaleRows$word, freq = MaleRows$freq, min.freq = 1,
           random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

MaleRows <-cbind(MaleRows , "Male")


# Convert the text to lower case
femaleDocs <- tm_map(femaleDocs, content_transformer(tolower))
# Remove numbers
femaleDocs <- tm_map(femaleDocs, removeNumbers)
# Remove english common stopwords
femaleDocs <- tm_map(femaleDocs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
femaleDocs <- tm_map(femaleDocs, removeWords, c("issue", "issues", "mental", "health")) 
# Remove punctuations
femaleDocs <- tm_map(femaleDocs, removePunctuation)
# Eliminate extra white spaces
femaleDocs <- tm_map(femaleDocs, stripWhitespace)
# Text stemming
#femaleDocs <- tm_map(femaleDocs, stemDocument)

dtm <- TermDocumentMatrix(femaleDocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
femaleRows <- data.frame(word = names(v),freq=v)
head(femaleRows, 10)

wordcloud(words = femaleRows$word, freq = femaleRows$freq, min.freq = 1,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

femaleRows <-cbind(femaleRows , "female")

# Convert the text to lower case
otherDocs <- tm_map(otherDocs, content_transformer(tolower))
# Remove numbers
otherDocs <- tm_map(otherDocs, removeNumbers)
# Remove english common stopwords
otherDocs <- tm_map(otherDocs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
otherDocs <- tm_map(otherDocs, removeWords, c("issue", "issues", "mental", "health")) 
# Remove punctuations
otherDocs <- tm_map(otherDocs, removePunctuation)
# Eliminate extra white spaces
otherDocs <- tm_map(otherDocs, stripWhitespace)
# Text stemming
#otherDocs <- tm_map(otherDocs, stemDocument)

dtm <- TermDocumentMatrix(otherDocs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
otherRows <- data.frame(word = names(v),freq=v)
head(otherRows, 10)

wordcloud(words = otherRows$word, freq = otherRows$freq, min.freq = 1,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

otherRows <-cbind(otherRows , "other")

colnames(MaleRows) <- c("Word", "Count", "Gender")
colnames(femaleRows) <- c( "Word", "Count", "Gender")
colnames(otherRows) <- c("Word", "Count", "Gender")

WordCloudDTM <- rbind(MaleRows, femaleRows, otherRows)

write.table(WordCloudDTM, "WordCloudDTM.csv", sep=",", row.names = FALSE)

write.xlsx(WordCloudDTM, "WordCloudDTM.xlsx")



