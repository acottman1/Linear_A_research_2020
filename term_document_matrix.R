### Preprocessing for NLP pprocesses
library(proxy)
library(tm)
library(readODS)
library(tidyverse)

path <- "/Users/Aron/omniGit/000-LinearAInscriptions.ods"
dat1 <- read_ods(path = path, sheet = 1)

# 1) create document term matrix
#select the document names and symbols columns
dat<- dat1
dat[2:3] <- lapply(dat[2:3], as.factor)
df <- dat[3:4]
colnames(df) <- c("doc_id", "text")

# remove the NA lines.  These are lines in the tablets that do not have any text on them.  
df<- na.omit(df)
df$text <- strsplit(df$text, NULL)
df <- group_by(df, doc_id) %>%
  mutate(text = paste0(text, collapse = " "))

#add space inbetween each character


#create corpus
corp <- VCorpus(DataframeSource(df))

corp <- tm_map(corp,stripWhitespace)


#dtm <- DocumentTermMatrix(corp)
tdm <- TermDocumentMatrix(corp)
dim(tdm)
inspect(tdm)



    