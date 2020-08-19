library(readODS)
library(topicmodels)
library(tm)
library(tidyverse)

# run the below in order to have the symbols available. must install 'NotoSansLinearA-LinearB.ttf' on your computer first
#install.packages('extrafont')
#library(extrafont)
#font_import()
#loadfonts()


path <- "modified-LinearAInscriptions.ods"
lin_A_data <- read_ods(path = path, sheet = 1)

l<-length(unlist(strsplit(lin_A_data$`normal copy`[1], NULL)))
mysymb<-as.character(unlist(strsplit(lin_A_data$`normal copy`[1], NULL))[1])
if(l>1){
  myname<-as.character(rep(lin_A_data$name[1], l))
    for (i in 2:l){
      temp<-unlist(strsplit(lin_A_data$`normal copy`[1], NULL))[i]
      mysymb<-as.character(c(mysymb, temp))
      }
    }else{
      myname<-as.character(lin_A_data$name[1])
    }
newdata<-cbind(myname, mysymb)

for (j in 2:length(lin_A_data$`normal copy`)){
    l<-length(unlist(strsplit(lin_A_data$`normal copy`[j], NULL)))
    mysymb<-as.character(unlist(strsplit(lin_A_data$`normal copy`[j], NULL))[1])
    if(l>1){
      myname<-as.character(rep(lin_A_data$name[j], l))
        for (i in 2:l){
        temp<-unlist(strsplit(lin_A_data$`normal copy`[j], NULL))[i]
        mysymb<-as.character(c(mysymb, temp))
        }
      }else{
        myname<-as.character(lin_A_data$name[j])
      }
    temp1<-cbind(myname, mysymb)
    newdata<-as.data.frame(rbind(newdata, temp1))
}


#For inital validation, we should run the tm on just the HT docs to see if we can uncover
#the same stucture tht is in the academic Docs.

#use this on mydocs to filter the data to get just the HT docs.
#filter(str_detect(doc_id, "^HT \\d"))
newdata$myname<-as.character(newdata$myname)

newdata$mysymb<-as.character(newdata$mysymb)
mydocs<-newdata[!is.na(newdata$mysymb),]

#remove the stop symbols.

### Do tm with the unk symbols removed.

stop_symbols <- c(" ", "—", ",", "|", "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
#create a "not in" function
`%not in%`<-Negate(`%in%`) 
mydocs <- mydocs[mydocs$mysymb %not in% stop_symbols,]
mydocs <- filter(mydocs, str_detect(myname, "^HT \\d"))

finaldtm<-as.DocumentTermMatrix(table(mydocs), weighting = weightTf)

linA_groupings<-read.csv("Montecci_classes.csv")
linA_groupings$doc_id<-as.character(linA_groupings$doc_id)
linA_groupings$Classes<-as.character(linA_groupings$Classes)

#I think the regrouped classes are much cleaner and more intuitive.  
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
rownames(linA_regrouped) <- linA_regrouped$doc_id



burnin <- sample(1:1000, 1)
iter <- sample(1:1000, 1)
#thin <- sample(1:1000, 1)
nstart <- sample(1:1000, 1)
seed <- sample(1:1000, nstart)
best <- TRUE
k_topics <- 5
t_terms <- 5

ldaOut <-LDA(finaldtm, k=k_topics, t=t_terms, method="Gibbs", 
             control=list(nstart=nstart, seed = seed, best=best, 
                          burnin = burnin, iter = iter))
ldaOut.terms <- as.matrix(terms(ldaOut,t_terms))
ldaOut.topics <- as.matrix(topics(ldaOut))
topicProbabilities <- as.data.frame(ldaOut@gamma)

ldaOut.topics<-as.data.frame(ldaOut.topics)
ldaOut.topics$doc_id<-finaldtm$dimnames$Docs
compare_dat<-merge(linA_groupings, ldaOut.topics, all = F)

for (i in 1:length(as.data.frame(table(compare_dat$Classes))$Var1)){
    cls<-as.character(as.data.frame(table(compare_dat$Classes))$Var1)
    classes_dat<-compare_dat[which(compare_dat$Classes == cls[i]),]
    academic_doc<-classes_dat$doc_id
      for (j in 1:k_topics){
        tpc<-as.character(as.data.frame(table(compare_dat$V1))$Var1)
        topic_dat<-compare_dat[which(compare_dat$V1 == tpc[j]),]
        topic_doc<-topic_dat$doc_id
        ratio<-length(intersect(academic_doc, topic_doc))/length(union(academic_doc, topic_doc))
        
        print(c(cls[i], tpc[j], ratio))
      }
}

# alternate comparison method.
topics_out <- data.frame(topics = topics(ldaOut))
topics_out <- data.frame(doc_id = row.names(topics_out),
                          topics = topics_out$topics,
                          stringsAsFactors = FALSE) 
barplot(table(topics_out$topics))
ht_topics_only <- topics_out %>% filter(str_detect(doc_id, "^HT \\d"))

ht_topics_out <- data.frame(topic = topics(ldaOut))
ht_topics_out <- data.frame(doc_id = row.names(ht_topics_out),
                         topic = ht_topics_out$topic,
                         stringsAsFactors = FALSE) 

ht_topics_only <- ht_topics_out %>% filter(str_detect(doc_id, "^HT \\d"))
source("compare_tm_to_academic.R")

tm_academic <-compare_tm_to_academic(linA_groupings , ht_topics_only)
write.csv(tm_academic, "tm_academic_full.csv")
View(read_csv("tm_academic_full.csv"))
tm_academic<- compare_tm_to_academic(linA_regrouped , ht_topics_only )
write.csv(tm_academic, "tm_academic_condensed.csv")
x<- compare_tm_to_academic(linA_groupings , k_15_clust)

compare_tm_to_academic(k_clust , ht_topics_only )
legend <- readxl::read_xlsx("montecci_linearA_groupings.xlsx", sheet = 2)

