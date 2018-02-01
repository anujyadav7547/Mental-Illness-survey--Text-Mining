
install.packages("tm")
install.packages("sas7bdat")
install.packages("wordcloud") # word-cloud generator 
install.packages("xlsx")

setwd("E:/Tutorial/Mental Health in Tech Survey/Text Mining in R/Mental Illness survey -Text Mining")
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
DocToDTM <- function(docs, GEN){
  
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("issue", "issues", "mental", "health")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  DocRows <- data.frame(word = names(v),freq=v)
  DocRows <-cbind(DocRows , GEN)
  colnames(DocRows) <- c("Word", "Count", GEN)
  DocRows
}

MaleRows <- DocToDTM(maleDocs, "Male")
femaleRows <- DocToDTM(femaleDocs, "female")
otherRows <- DocToDTM(otherDocs, "Other")


WordCloudDTM <- rbind(MaleRows, femaleRows, otherRows)

write.table(WordCloudDTM, "WordCloudDTM.csv", sep=",", row.names = FALSE)

write.xlsx(WordCloudDTM, "WordCloudDTM.xlsx")



