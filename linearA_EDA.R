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
str(lin_A_data) 

lin_A_data[2:3] <- lapply(lin_A_data[2:3], as.factor)

#summary(lin_A_data)
sum(is.na(df))
# get frequecy of characters for whole corpus

df <- data.frame(doc_id = lin_A_data$name,
                 text = lin_A_data$`normal copy`,
                 stringsAsFactors = FALSE
                 )
df<-na.omit(df)
df1 <- group_by(df, doc_id) %>%
  summarize(text = paste(text , collapse = "") )


#remove the NA values.

#split each character
df1 <- mutate(df1, text_split = strsplit(df1$text, ""))


charcounts <- unlist(strsplit(df1$text, ""))
plot(as.factor(charcounts))
tab<- data.frame(table(as.factor(charcounts)))
max(tab)
tab <- tab[order(-tab$Freq),]

# percent of the corpus that is unreadable.
tab$Freq[1]/sum(tab$Freq)


#Estimate the probability of each symbol occuring within the corpus. 
#Later do this for all the pairs and trios to see if you can make a conditional probability table for everything.
#remove the round() before you do this.
est_prob <- tab %>% mutate(estimate_probability = round(Freq/sum(tab$Freq),digits = 5))
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

lin_A <- lin_A_data[3:4]
lin_A<- na.omit(lin_A)

colnames(lin_A) <- c('doc_id', 'text')
lin_A <- group_by(lin_A, doc_id) %>%
  summarize(text = paste(text , collapse = "") )

# format column names to use the "datframesource() 

lin_A$text <- gsub("", " ", lin_A$text)

#Document and raw text.
#write_excel_csv2(lin_A, 'linear_A_docs_text.csv', delim = ',')

#document and numbers to .csv
df <- data.frame(name = lin_A_data$name,
                 integers = lin_A_data$integers) %>%
  na.omit() %>% 
  group_by(name) %>%
  summarise(integers = paste(integers, collapse = " "))
#write_excel_csv2(df, 'linear_A_docs_integers.csv', delim = ',')

#document and ideograms
df <- data.frame(name = lin_A_data$name,
                 ideograms = lin_A_data$ideograms) %>%
  na.omit() %>% 
  group_by(name) %>%
  summarise(ideograms = paste(ideograms, collapse = " "))
write_excel_csv2(df, 'linear_A_docs_ideograms.csv', delim = ',')

#document and probable words
df <- data.frame(name = lin_A_data$name,
                 prob_word = lin_A_data$`probable “words”`) %>%
  na.omit() %>% 
  group_by(name) %>%
  summarise(prob_word = paste(prob_word, collapse = " "))
write_excel_csv2(df, 'linear_A_docs_prob_word.csv', delim = ',')

#document and fractions
df <- data.frame(name = lin_A_data$name,
                 fractions = lin_A_data$fractions) %>%
  na.omit() %>% 
  group_by(name) %>%
  summarise(fractions = paste(fractions, collapse = " "))
write_excel_csv2(df, 'linear_A_docs_fractions.csv', delim = ',')

#make sure that the data frame is corrected properly.  
dd1 <- data.frame(
  doc_id=lin_A$doc_id,
  text=lin_A$text,
  stringsAsFactors=F
)

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
inspect(lina_corp[10])

###


tdf_matrix <- as.matrix(removeSparseTerms(tdm, .999))

#install.packages("proxy")
library(proxy)
lina_dist <- dist(tdf_matrix, method = 'cosine')

dim(tdf_matrix)
dim(lina_dist)

lina_kmeans <- kmeans(lina_dist, centers = 5 ) 
lina_hclust <- hclust(lina_dist, method = "ward.D2") 

lina_dbscan <- hdbscan(lina_dist, minPts =3 )

### now visualize to see what the above lines actiually mean.
plot(tdm)


plot(lina_hclust,
     xlab = "hierarchical clustering of Linear A"
     )


plot(lina_dbscan$hc)
plot(lina_dbscan, scale = 1.5)

print(lina_dbscan$cluster_scores)
print(head(lina_dbscan$membership_prob))
plot()


######topic modeling

#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
freq[ord]
write.csv(freq[ord],"word_freq.csv")

#load topic models library
library(topicmodels)
library(stringi)

#used https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
#for gettig the below code. more exploration needs to happen.


#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE


#Number of topics
k <- 15

lda_out_15 <-LDA(dtm,k, 
             method="Gibbs", 
             control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin)
             )

lda_out_topics <- data.frame(topics = topics(lda_out_15))

topic_probs <- data.frame(lda_out_15@gamma)

topic_compare <-  lapply(1:nrow(dtm),function(x)
  sort(topic_probs[x,])[k]/sort(topic_probs[x,])[k-1])

colnames(lda_out_topics) <- "topics"
ggplot(lda_out_topics)+
  geom_bar(aes(topics))

library(readxl)
linA_groupings <- read_excel("Linear A/montecci_linearA_groupings.xlsx")


linA_groupings$Classes <- as.factor(linA_groupings$Classes)
groupings <- group_by(linA_groupings)
str(groupings)
summary(groupings)

HT_topics <- data.frame(doc_id = row.names(lda_out_topics),
                           topics = lda_out_topics$topics)

HT_topics <- HT_topics%>%
  filter(str_detect(doc_id, "^HT \\d"))
HT_topics$topics<- as.factor(HT_topics$topics)

library(forcats)
library(gridExtra)
#barchar of academic classes for HT tablets
academic_plot_15 <- ggplot(mutate(linA_groupings, Classes = fct_infreq(as.factor(Classes))))+
  geom_bar(aes(x = Classes))

#barchart of LDA topic models. for HT tablets
tm_plot_15 <- ggplot(mutate(HT_topics, Topics = fct_infreq(topics)))+
  geom_bar(aes(x = Topics))
grid.arrange(academic_plot_15, tm_plot_15, ncol = 2)

# now regroup into smaller classes to see if that helps.  
linA_regrouped <- linA_groupings
#group unidentifieable and not classifiable
linA_regrouped$Classes <-  gsub("U", "X", linA_regrouped$Classes)
#group personnel and agriculural commodoties and misc commodoties
linA_regrouped$Classes <-  gsub("B|M", "A", linA_regrouped$Classes)
#group livestock, cloth and wool
linA_regrouped$Classes <-  gsub("D|L|O", "C", linA_regrouped$Classes)
#regroup foods
linA_regrouped$Classes <-  gsub("F|G|K|N|P", "E", linA_regrouped$Classes)

 
summary(as.factor(linA_regrouped$Classes))
summary(as.factor(linA_groupings$Classes))

#rerun the LDA for 5 categotries.

k <- 5

lda_out_5 <-LDA(dtm,k, 
              method="Gibbs", 
              control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin)
)

lda_out_topics_5 <- data.frame(topics = topics(ldaOut))

topic_probs <- data.frame(lda_out_5@gamma)

HT_topics_5 <- data.frame(doc_id = row.names(lda_out_topics_5),
                        topics = lda_out_topics_5$topics,
                        stringsAsFactors = FALSE) 

#filter the HT documents out.
HT_topics_5 <- HT_topics_5%>%
  filter(str_detect(doc_id, "^HT \\d"))
#convert the numerical topics to factors. character might work too.  IDK?
HT_topics_5$topics<- as.factor(HT_topics_5$topics)
#plot stuff
tm_plots_5 <- ggplot(mutate(HT_topics_5, as.factor(Topics = fct_infreq(topics))))+
  geom_bar(aes(x = Topics))

#plot more stuff
academic_plots_5 <- ggplot(mutate(linA_regrouped, Classes = fct_infreq(as.factor(Classes))))+
  geom_bar(aes(x = Classes))
grid.arrange(academic_plots_5, tm_plots_5, ncol = 2)

#It looks like the distribution is approximatly the same.  
#now verify how accurate it is by visualizzing which documents are in
#each category.  This is cool!!!!!

grp_A <- linA_regrouped %>% filter(Classes == "A") %>%
  select(doc_id) %>% unlist()
grp_C <- linA_regrouped %>% filter(Classes == "C")  %>%
  select(doc_id) %>% unlist()
grp_E <- linA_regrouped %>% filter(Classes == "E") %>%
  select(doc_id) %>% unlist()
grp_V <- linA_regrouped %>% filter(Classes == "V") %>%
  select(doc_id) %>% unlist() 
grp_X <- linA_regrouped %>% filter(Classes == "X") %>%
  select(doc_id) %>% unlist() 

topic_1 <- HT_topics_5 %>% filter(topics == 1) %>%
  select(doc_id) %>% as_tibble  %>% unlist()
topic_2 <- HT_topics_5 %>% filter(topics == 2) %>%
  select(doc_id) %>% as_tibble %>% unlist()
topic_3 <- HT_topics_5 %>% filter(topics == 3) %>%
  select(doc_id) %>% as_tibble  %>% unlist()
topic_4 <- HT_topics_5 %>% filter(topics == 4) %>%
  select(doc_id) %>% as_tibble  %>% unlist()
topic_5 <-filter(HT_topics_5, topics == 5) %>%
  select(doc_id) %>% as_tibble  %>% unlist()

sum(grp_C %in% topic_2)/
sum(grp_A %in% topic_1)

str(grp_C
    )
str(topic_5
    )

topics_1_5 <- list(topic_1, topic_2, topic_3, topic_4, topic_5)

topics_1_5 <- data.frame(lapply(topics_1_5, "length<-", max(lengths(topics_1_5))))

colnames(topics_1_5)<- c("topic_1", "topic_2", "topic_3", "topic_4", "topic_5")
rownames(topics_1_5)<- NULL
topics_1_5 <- lapply(topics_1_5, as.character)

classes1 <- list(grp_A, grp_C, grp_E, grp_V,grp_X)
classes <- data.frame(lapply(classes1, "length<-", max(lengths(classes1))))
colnames(classes) <- c("class_A", "class_C", "class_E", "class_V","class_X")
rownames(classes)<- NULL
classes <- lapply(classes, as.character)

sum( na.omit(classes$class_A) %in% na.omit(topics_1_5$topic_1)) / max(c(length(na.omit( classes$class_A)),length(na.omit(topics_1_5$topic_1))))
sum( na.omit(classes$class_A) %in% na.omit(topics_1_5$topic_2)) / max(c(length(na.omit( classes$class_A)),length(na.omit(topics_1_5$topic_2))))
sum( na.omit(classes$class_A) %in% na.omit(topics_1_5$topic_3)) / max(c(length(na.omit( classes$class_A)),length(na.omit(topics_1_5$topic_3))))
sum( na.omit(classes$class_A) %in% na.omit(topics_1_5$topic_4)) / max(c(length(na.omit( classes$class_A)),length(na.omit(topics_1_5$topic_4))))
sum( na.omit(classes$class_A) %in% na.omit(topics_1_5$topic_5)) / max(c(length(na.omit( classes$class_A)),length(na.omit(topics_1_5$topic_5))))

sum(na.omit(classes$class_C) %in% na.omit(topics_1_5$topic_1) ) / max(c(length(na.omit( classes$class_C)),length(na.omit(topics_1_5$topic_1))))
sum(na.omit(classes$class_C) %in% na.omit(topics_1_5$topic_2) ) / max(c(length(na.omit( classes$class_C)),length(na.omit(topics_1_5$topic_2))))
sum(na.omit(classes$class_C) %in% na.omit(topics_1_5$topic_3) ) / max(c(length(na.omit( classes$class_C)),length(na.omit(topics_1_5$topic_3))))
sum(na.omit(classes$class_C) %in% na.omit(topics_1_5$topic_4) ) / max(c(length(na.omit( classes$class_C)),length(na.omit(topics_1_5$topic_4))))
sum(na.omit(classes$class_C) %in% na.omit(topics_1_5$topic_5) ) / max(c(length(na.omit( classes$class_C)),length(na.omit(topics_1_5$topic_5))))

sum(na.omit(classes$class_E) %in% na.omit(topics_1_5$topic_1) ) / max(c(length(na.omit( classes$class_E)),length(na.omit(topics_1_5$topic_1))))
sum(na.omit(classes$class_E) %in% na.omit(topics_1_5$topic_2) ) / max(c(length(na.omit( classes$class_E)),length(na.omit(topics_1_5$topic_2))))
sum(na.omit(classes$class_E) %in% na.omit(topics_1_5$topic_3) ) / max(c(length(na.omit( classes$class_E)),length(na.omit(topics_1_5$topic_3))))
sum(na.omit(classes$class_E) %in% na.omit(topics_1_5$topic_4) ) / max(c(length(na.omit( classes$class_E)),length(na.omit(topics_1_5$topic_4))))
sum(na.omit(classes$class_E) %in% na.omit(topics_1_5$topic_5) ) / max(c(length(na.omit( classes$class_E)),length(na.omit(topics_1_5$topic_5))))

sum(na.omit(classes$class_V) %in% na.omit(topics_1_5$topic_1) ) / max(c(length(na.omit( classes$class_V)),length(na.omit(topics_1_5$topic_1))))
sum(na.omit(classes$class_V) %in% na.omit(topics_1_5$topic_2) ) / max(c(length(na.omit( classes$class_V)),length(na.omit(topics_1_5$topic_2))))
sum(na.omit(classes$class_V) %in% na.omit(topics_1_5$topic_3) ) / max(c(length(na.omit( classes$class_V)),length(na.omit(topics_1_5$topic_3))))
sum(na.omit(classes$class_V) %in% na.omit(topics_1_5$topic_4) ) / max(c(length(na.omit( classes$class_V)),length(na.omit(topics_1_5$topic_4))))
sum(na.omit(classes$class_V) %in% na.omit(topics_1_5$topic_5) ) / max(c(length(na.omit( classes$class_V)),length(na.omit(topics_1_5$topic_5))))

sum(na.omit(classes$class_X) %in% na.omit(topics_1_5$topic_1) ) / max(c(length(na.omit( classes$class_X)),length(na.omit(topics_1_5$topic_1))))
sum(na.omit(classes$class_X) %in% na.omit(topics_1_5$topic_2) ) / max(c(length(na.omit( classes$class_X)),length(na.omit(topics_1_5$topic_2))))
sum(na.omit(classes$class_X) %in% na.omit(topics_1_5$topic_3) ) / max(c(length(na.omit( classes$class_X)),length(na.omit(topics_1_5$topic_3))))
sum(na.omit(classes$class_X) %in% na.omit(topics_1_5$topic_4) ) / max(c(length(na.omit( classes$class_X)),length(na.omit(topics_1_5$topic_4))))
sum(na.omit(classes$class_X) %in% na.omit(topics_1_5$topic_5) ) / max(c(length(na.omit( classes$class_X)),length(na.omit(topics_1_5$topic_5))))


#maybe use lists instead of dataframes.  The above code is rediculous, and not 
#and not very useful.
#then you dont have to deal with na's

length(classes1[[2]])
str(classes1[[2]])




