#preprocessing
library(readODS)
library(DescTools)
library(tidyverse)

#locatiion of the ODS file
path <- "/Users/Aron/omniGit/000-LinearAInscriptions.ods"
#read in the data
dat<- read_ods(path = path, sheet = 1, skip = 3)
rm(path)
#set the characters and the names as factors.
#dat[2:3] <- lapply(dat[2:3], as.factor)

### Get frequency of characters for corpus

df <- dat$`normal copy`
#remove the NA values.
df<-df[!is.na(df)]

#split each character
df <- strsplit(df, "")

#head(df1)

#flatten, create a frequency table and coerce into df
df<-  as.data.frame(table(unlist(df, recursive = FALSE)))

#view the caracters
cat(as.character(df$Var1))

#sort the data
df <- arrange(df, desc(Freq))

#includes undeciphered characters.
ent1 <- Entropy((df$Freq))

#remove undeciphred characters
df <- slice(df, 2:length(df[,1]))
ent2 <- Entropy((df$Freq))

#remove separator "dot"
df <- slice(df, 2:length(df[,1]))
ent3 <- Entropy((df$Freq))

#create table of each entropy.  
entropy_1_gram <- data.frame(ent1, ent2,ent3)
entropy_1_gram
rm(ent1)
rm(ent2)
rm(ent3)
