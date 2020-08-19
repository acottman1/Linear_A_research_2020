### Text mining symbolic written languages
## Linear A

# Questions:
# do the symbols in LinearA follow a Zipf distribution?
# SNA for inscriptions
# check all possible language models for annotations
# compare entropy of Linear A with Linear B
# train the neural net for semantic closeness
# unsupervised clustering; k-nearest neighbors
# 

library(udpipe)
library(textrank)
library(tm)
library(topicmodels)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)

raw_linearA<-read.csv("linear_A_docs_text.csv", header = T)
# data is factored, so we need to get rid of factors
linA<-paste(unlist(raw_linearA[1,2]), collapse =" ")
for (i in 2:length(raw_linearA[,2])){
  linA<-rbind(linA, paste(unlist(raw_linearA[i,2]), collapse =" "))
}
### remove stop symbols. 

stop_symbols <- c(" ", "—", ",", "|", "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
#create a "not in" function
`%not in%`<-Negate(`%in%`) 
linA <- linA[linA[1] %not in% stop_symbols,]
# udmodel_linearA <- udpipe_load_model(file = unlist(raw_linearA)) # for training our own model

ud_model <- udpipe_download_model(language = "ancient_greek")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = as.character(linA), trace = TRUE, tagger = "default", parser = "default" ) ## this is going to take some time
x <- as.data.frame(x)

stats <- subset(x, upos %in% "SYM")
stats <- txt_freq(x = stats$lemma)

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring symbols", xlab = "Freq")


stats <- subset(x, upos %in% "VERB")
stats <- txt_freq(x = stats$lemma)

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring verbs", xlab = "Freq")

ggplot(head(stats, 30), aes(x = freq, y = key))+
  geom_bar(stat = "identity", fill = "cadetblue")+
  xlab("Freq")+
  ylab("Most Occuring Verbs")+
  theme(axis.text.y = element_text(family = "Noto Sans Linear A"))
  

stats <- subset(x, upos %in% "NUM")
stats <- txt_freq(x = stats$lemma)

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring numeric", xlab = "Freq")


stats <- subset(x, upos %in% "PUNCT")
stats <- txt_freq(x = stats$lemma)

stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "cadetblue", main = "Most occurring punct", xlab = "Freq")

## Collocation (words following one another)
stats <- keywords_collocation(x = x, 
                              term = "token", group = c("doc_id", "paragraph_id", "sentence_id"),
                              ngram_max = 4)
stats

### I have no idea how to make the symbols show up in the graphs!!!!!
###fixed it!
#Installed the ttf for linear A, then did it in GGplot for the graphs.  seelines 54-58

## Co-occurrences: How frequent do words occur in the same sentence, in this case only nouns or adjectives
stats <- cooccurrence(x = subset(x, upos %in% c("VERB", "PUNCT")), 
                      term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))


### couldnt get it going here

wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Noto Sans Linear A") +
  theme(legend.position = "none",  text = element_text(family = "Noto Sans Linear A")
        ) +
  labs(title = "Co-occurrences", subtitle = "Verbs & Punctuation")

## Co-occurrences: How frequent do words follow one another
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("VERB", "SYM"))
wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences", subtitle = "Verbs & Symbols")

## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
stats <- cooccurrence(x = x$lemma, 
                      relevant = x$upos %in% c("VERB", "NUM"), skipgram = 2)
wordnetwork <- head(stats, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences", subtitle = "Verbs & Numeric",)




### neural net

library(tensorflow)
library(purrr)
library(keras)
library(tidyverse)

max_features <- 1000
maxlen <- 100
batch_size <- 32
embedding_dims <- 50
filters <- 64
kernel_size <- 3
hidden_dims <- 50
epochs <- 5

tokenizer <- text_tokenizer(num_words = length(linA))
tokenizer %>% fit_text_tokenizer(as.vector(linA))

tokenizer$document_count

tokenizer$word_index %>% 
  head()

text_seqs <- texts_to_sequences(tokenizer, as.vector(linA))

text_seqs %>%
  head()

x_train <- text_seqs %>%
  pad_sequences(maxlen = maxlen)
dim(x_train)

y_train <- as.vector(linA)
length(y_train)

model <- keras_model_sequential() %>% 
  layer_embedding(max_features, embedding_dims, input_length = maxlen) %>%
  layer_dropout(0.2) %>%
  layer_conv_1d(
    filters, kernel_size, 
    padding = "valid", activation = "relu", strides = 1
  ) %>%
  layer_global_max_pooling_1d() %>%
  layer_dense(hidden_dims) %>%
  layer_dropout(0.2) %>%
  layer_activation("relu") %>%
  layer_dense(1) %>%
  layer_activation("sigmoid") %>% compile(
    loss = "binary_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

hist <- model %>%
  fit(
    x_train,
    y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 0.3
  )

plot(hist)

one_hot_results <- texts_to_matrix(tokenizer, as.vector(linA), mode = "binary")
dim(one_hot_results)

hashing_results <- text_hashing_trick(as.vector(linA)[1], n = 100)
hashing_results

## neural net
skipgrams_generator <- function(text, tokenizer, window_size, negative_samples) {
  gen <- texts_to_sequences_generator(tokenizer, sample(text))
  function() {
    skip <- generator_next(gen) %>%
      skipgrams(
        vocabulary_size = tokenizer$num_words, 
        window_size = window_size, 
        negative_samples = 1
      )
    x <- transpose(skip$couples) %>% map(. %>% unlist %>% as.matrix(ncol = 1))
    y <- skip$labels %>% as.matrix(ncol = 1)
    list(x, y)
  }
}


embedding_size <- 128  # Dimension of the embedding vector.
skip_window <- 5       # How many words to consider left and right.
num_sampled <- 1       # Number of negative examples to sample for each word.

input_target <- layer_input(shape = 1)
input_context <- layer_input(shape = 1)

embedding <- layer_embedding(
  input_dim = tokenizer$num_words + 1, 
  output_dim = embedding_size, 
  input_length = 1, 
  name = "embedding"
)

target_vector <- input_target %>% 
  embedding() %>% 
  layer_flatten()

context_vector <- input_context %>%
  embedding() %>%
  layer_flatten()

dot_product <- layer_dot(list(target_vector, context_vector), axes = 1)
output <- layer_dense(dot_product, units = 1, activation = "sigmoid")

model <- keras_model(list(input_target, input_context), output)
model %>% compile(loss = "binary_crossentropy", optimizer = "adam")

summary(model)

model %>%
  fit_generator(
    skipgrams_generator(as.vector(linA), tokenizer, skip_window, negative_samples), 
    steps_per_epoch = 100, epochs = 5
  )




