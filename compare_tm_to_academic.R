#
#this function creates a matrix filled with the ratio of comparing academically/antropologically
#classified tablets with the topic modeled classification tablets.  
# x and y are two data frames where column 1 is the doc_id, 
# and column 2 is the classes or topics.
compare_tm_to_academic <- function(grouping,topic){
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
  rownames(m) <- paste('class', classes$Classes)
  m
  
  
}
compare_tm_to_academic(linA_regrouped , k_clust)







