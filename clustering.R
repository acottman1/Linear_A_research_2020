#clustering on the full corpus.  then filtering the results. 
clust_dtm <- as.matrix(finaldtm)
d <- dist(clust_dtm)

k_5 <-kmeans(d, 5, iter.max = 10000, nstart = 250)
k_clust <- data.frame(cluster = k_5$cluster)
k_clust <- data.frame(doc_id = rownames(k_clust),
                      topic = k_clust$cluster)#same name as the function 
k_3 <-kmeans(d, 3, iter.max = 10000, nstart = 250)

k_clust <- filter(k_clust, (str_detect(doc_id, "^HT \\d")))

k_15 <- kmeans(d, 15, iter.max = 10000, nstart = 250)

k_15_clust <- data.frame(cluster = k_15$cluster)
k_15_clust<- data.frame(doc_id = rownames(k_15_clust),
                      topic = k_15$cluster)
# filter to just the HT docs then conduct clustering.  
mydocs<-newdata[!is.na(newdata$mysymb),]

#remove the stop symbols.
stop_symbols <- c(" ", "—", ",", "|", "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "\U0001076b")
`%not in%`<-Negate(`%in%`) 
mydocs <- mydocs[mydocs$mysymb %not in% stop_symbols,]
mydocs <- filter(mydocs, str_detect(myname, "^HT \\d"))

finaldtm_filtered<-as.DocumentTermMatrix(table(mydocs), weighting = weightTf)

clust_dtm <- as.matrix(finaldtm_filtered)
d <- dist(clust_dtm)

k_means_filt <-kmeans(d, 12, iter.max = 10000, nstart = 2500)
k_clust_filt <- data.frame(cluster = k_means_filt$cluster)
k_clust_filt <- data.frame(doc_id = rownames(k_clust_filt),
                      topic = k_clust_filt$cluster, stringsAsFactors = FALSE)#same name as the function 

k_clust_filt12 <- filter(k_clust_filt, (str_detect(doc_id, "^HT \\d")))




compare_tm_to_kmeans <- function(grouping,topic){
  #extract the uniqe values in the column
  classes <- unique(grouping[2])
  topics <- unique(topic[2])
  # create an empty matrix in the dimentions of the number of classes and topics
  m <- matrix(nrow = nrow(classes), ncol = nrow(topics))
  for(i in 1:nrow(classes)){
    for(j in 1:nrow(topics)){
      #filter the data based on the contets of the unique values
      class_filt <- filter(grouping[1], grouping[2] == classes[i,]) 
      topic_filt <- filter(topic[1], topic[2] == topics[j,])
      #fill the matrix with the ratio of documents in the topic modeling
      # compared to the academic classes.
      m[i,j] <-(nrow(intersect(topic_filt, class_filt))/ nrow(class_filt))
    }
  }
  
  m<-data.frame(m)
  colnames(m) <- paste('cluster', topics$topic)
  rownames(m) <- paste('class', classes$topic)
  m
  
  
}
#write.csv(compare_tm_to_academic(linA_regrouped , k_clust), "academic_to_kmeans_5.csv")

tm_kmeans_5 <-compare_tm_to_kmeans(k_clust , ht_topics_only )
#write.csv(tm_kmeans_5, "tm_kmeans_5.csv")
tm_kmeans_12 <-compare_tm_to_kmeans(k_clust_filt12 , ht_topics_only )
#write.csv(tm_kmeans_12, "tm_kmeans_12.csv")
  str(k_clust_filt)
str(ht_topics_only)
