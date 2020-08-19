library(readODS)
library(tidyverse)
library(DescTools)
library(ngram)
library(janitor)
library(topicmodels)
library(tm)
library(readODS)

path <- "/Users/Aron/omniGit/000-LinearAInscriptions.ods"

lin_A_data <- read_ods(path = path, sheet = 1, skip = 3)
#View(lin_A_data)
#str(lin_A_data) 

lin_A_data[2:3] <- lapply(lin_A_data[2:3], as.factor)

#summary(lin_A_data)

# get frequecy of characters for whole corpus

df <- lin_A_data$`normal copy`

#split each character
df1 <- strsplit(df, "")

#head(df1)

#flatten, create a frequency table and coerce into df
df2<-  as.data.frame(table(unlist(df1, recursive = FALSE)))

#sort the data
df2 <- arrange(df2, desc(Freq))

#includes undeciphered characters.
Entropy((df2$Freq))


#write_csv( df3,"linear_a_freq")
#slice off the first few rows.  The first row is unreadable glyphs, 
#the second is a separator glyph.( im pretty sure, but  I should confirm)

df3 <- slice(df2, 3:373)

Entropy(df3$Freq)

ggplot(df3)+
  geom_point(aes(reorder(Var1, -Freq), y = Freq))


###addded n-gram package

df4 <- concatenate(unlist(df1, recursive = FALSE), collapse = " ")
#string.summary(df4)

ng_2 <- ngram(df4, n =2)

#print(ng_2, output = "truncated")



# create document term / term document matrix

lin_A <- clean_names(lin_A_data)

lin_A <- group_by(lin_A, name) %>%
  summarize(symbols = paste(normal_copy, collapse = " ") )

# format column names to use the "datframesource() 
colnames(lin_A) <- c('doc_id', 'text')
lin_A$text <- gsub("", " ", lin_A$text)



#make sure that the data frame is corrected properly.  
dd1 <- data.frame(
  doc_id=lin_A$doc_id,
  text=lin_A$text,
  stringsAsFactors=F
)

dd1[is.na(dd1$text)] <- ""


lina_corp <- Corpus(DataframeSource (dd1))
lina_corp <- tm_map(lina_corp,stripWhitespace)

dtm <- DocumentTermMatrix(lina_corp) 
tdm<- TermDocumentMatrix(lina_corp)

#Look at the dtm/tdm
inspect(dtm)
inspect(tdm)

#check frequent terms.  
findFreqTerms(dtm, 150)


##
inspect(lina_corp[[10]])


