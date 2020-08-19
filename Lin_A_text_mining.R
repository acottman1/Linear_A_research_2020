#text mining to understand the unkown characters.


library(stringr)
library(rebus)
library(DT)
library(tidyverse)

#import the doc_id column and the text col from onw of the other scripts.

#test vector
#df <- read_csv("/Users/Aron/omniGit/LinearA_1/Dasboard_lin_A/linear_A_docs_text.csv")

#df <- data.frame(doc_id = lin_A_data$name,
  #               text = lin_A_data$`normal copy`,
   #              stringsAsFactors = FALSE)

#df<-na.omit(df)
#df1 <- group_by(df, doc_id) %>%
#  summarize(text = paste(text , collapse = "") )

#write_excel_csv2(df1, '/Users/Aron/omniGit/LinearA_1/Dasboard_lin_A/linear_A_docs_text.csv', delim = ',')

#character counts 


df1 <- read_csv("linear_A_docs_text.csv")

# Individual symbols from entire corpus as a character string 
df1$text <- gsub(" ", "", df1$text)
df2 <- unlist(strsplit(df1$text, ""))

##frequency table of all symbols

df3 <- data.frame(table(df2))
# remove stop symbols
stop_symbols <- c(" ", ",", "|", "⁰", "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "—")
#create a "not in" function
`%not in%`<-Negate(`%in%`) 
df3 <- df3[df3$df2 %not in% stop_symbols,]
#order the symbols by frequency
df3 <- df3[order(-df3$Freq),]
#lock in the symbols as factor order to assist with ordering.
df3$df2 <- factor(df3$df2, levels = df3$df2)
# percent of the corpus that is unreadable.
df3$Freq[1]/sum(df3$Freq)


######
#get the character before the unk character.
#text
t <- df1$text
unk_before <-str_extract_all(t, ANY_CHAR %R% "\U0001076b")
unk_before_df <- data.frame(table(unlist(unk_before)))
unk_before_df <- unk_before_df[order(-unk_before_df$Freq),]

#get the character after the unk character.
unk_after <-str_extract_all(t, "\U0001076b" %R% ANY_CHAR)
unk_after_df <- data.frame(table(unlist(unk_after)))
unk_after_df <- unk_after_df[order(-unk_after_df$Freq),]

#get the character before and after the unk character.
unk_before_after <- str_extract_all(t, ANY_CHAR %R% "\U0001076b" %R% ANY_CHAR)
unk_before_after_df <- data.frame(table(unlist(unk_before_after)))
unk_before_after_df <- unk_before_after_df[order(-unk_before_after_df$Freq),]

#get all pairs of symbols.
all_pairs <- str_extract_all(t, ANY_CHAR %R% ANY_CHAR)
#make dataframe and order it
all_pairs_df <- data.frame(table(unlist(all_pairs)))
all_pairs_df <- all_pairs_df[order(-all_pairs_df$Freq),]
#remove any pairs where ther eis the unk symbol in it.
all_pairs_filt <- filter(all_pairs_df, str_detect(Var1, "\U0001076b")== FALSE)

#get all trios of symbols
all_trios <- str_extract_all(t, ANY_CHAR %R% ANY_CHAR %R% ANY_CHAR)
#make dataframe and order it
all_trios_df<- data.frame(table(unlist(all_trios)))
all_trios_df <- all_trios_df[order(-all_trios_df$Freq),]
#remove any pairs where ther eis the unk symbol in it.
all_trios_filt <-  filter(all_trios_df, str_detect(Var1, "\U0001076b")== FALSE)



##### use the above table to calc entropy and make entropy table

char_counts <- list(df3$Freq, all_pairs_df$Freq, all_pairs_filt$Freq, 
                    all_trios_df$Freq, all_trios_filt$Freq, unk_before_df$Freq, 
                    unk_after_df$Freq, unk_before_after_df$Freq
                    )
#ent_table <- data.frame(lapply(char_counts, Entropy))
#colnames(ent_table)<- c("full_corpus", "all_pairs", "all_pairs_no_unk",
#                       "all_trios", "all_trios__no_unk", "unk_then_symbol", 
#                       "symbol_then_unknown", "unk_before_and_after"
#                       )
#rownames(ent_table) <- "Entropy"
#ent_table <-t(ent_table)
#View(ent_table)
#write.csv(ent_table, "/Users/Aron/omniGit/LinearA_1/Dasboard_lin_A/entropy_table.csv", row.names = TRUE)

###make an untidty table that displays the top 10 of each category to see if any obvious patterns emerge.
row_pairs = 156L
Common_n_grams <- data.frame('pairs' = head(all_pairs_filt$Var1, n =row_pairs),
                             'pairs_freq' = head(all_pairs_filt$Freq, n =row_pairs),
                             'before' = head(unk_before_df$Var1, n =row_pairs),
                             'before_freq' = head(unk_before_df$Freq, n =row_pairs),
                             'after' = head(unk_after_df$Var1, n =row_pairs),
                             'after_freq' = head(unk_after_df$Freq, n =row_pairs),
                             'trios' = head(all_trios_filt$Var1, n =row_pairs),
                             'trios_freq' = head(all_trios_filt$Freq, n =row_pairs),
                             'unk_trios' = head(unk_before_after_df$Var1, n = row_pairs),
                             'unk_trios_freq' = head(unk_before_after_df$Freq, n = row_pairs)
                             )
#write_excel_csv2(df1, '/Users/Aron/omniGit/LinearA_1/Dasboard_lin_A/linear_A_docs_text.csv', delim = ',')

write_excel_csv2(Common_n_grams, "Common_n_grams.csv", delim = ',')

# looks cool, but not particularly helpful.  try somthing different.

# the most frequent character before the unk character 
# most common pair of characters that have an unk char is 2 unkown characters
# second is a "dot" meaning work break. 
# the most common "word" symbol is the number 1.
# 3 - 6 are the intergers 1,2,3,4.  
# 7-9 are symbols, 10 is the interger 5
# now, find the list of symbols that come after the symbol for the top few symbols.

after_pair_1 <- data.frame(table(unlist(str_extract_all(t, "\U00010107" %R% ANY_CHAR))))
after_pair_1 <- after_pair_1[order(-after_pair_1$Freq),]

after_pair_2 <- data.frame(table(unlist(str_extract_all(t, "\U00010108" %R% ANY_CHAR))))
after_pair_2 <- after_pair_2[order(-after_pair_2$Freq),]

after_pair_3 <- data.frame(table(unlist(str_extract_all(t, "\U00010109" %R% ANY_CHAR))))
after_pair_3 <- after_pair_3[order(-after_pair_3$Freq),]

after_pair_4 <- data.frame(table(unlist(str_extract_all(t, "\U0001010a" %R% ANY_CHAR))))
after_pair_4 <- after_pair_4[order(-after_pair_4$Freq),]

after_pair_5 <- data.frame(table(unlist(str_extract_all(t, "\U00010746" %R% ANY_CHAR))))
after_pair_5 <- after_pair_5[order(-after_pair_4$Freq),]

after_pair_6 <- data.frame(table(unlist(str_extract_all(t, "\U00010624" %R% ANY_CHAR))))
after_pair_6 <- after_pair_6[order(-after_pair_4$Freq),]

after_pair_7 <- data.frame(table(unlist(str_extract_all(t, "\U00010631" %R% ANY_CHAR))))
after_pair_7 <- after_pair_7[order(-after_pair_4$Freq),]

row_pairs = 48L
after_pair_df <- data.frame('pair_1' = head(after_pair_1$Var1, n = row_pairs),
                            "pair_1_freq" = head(after_pair_1$Freq, n = row_pairs),
                            'pair_2' = head(after_pair_2$Var1, n = row_pairs),
                            "pair_2_freq" = head(after_pair_2$Freq, n = row_pairs),
                            'pair_3' = head(after_pair_3$Var1, n = row_pairs),
                            "pair_3_freq" = head(after_pair_3$Freq, n = row_pairs),
                            'pair_4' = head(after_pair_4$Var1, n = row_pairs),
                            "pair_4_freq" = head(after_pair_4$Freq, n = row_pairs),
                            'pair_5' = head(after_pair_5$Var1, n = row_pairs),
                            "pair_5_freq" = head(after_pair_5$Freq, n = row_pairs),
                            'pair_6' = head(after_pair_6$Var1, n = row_pairs),
                            "pair_6_freq" = head(after_pair_6$Freq, n = row_pairs),
                            'pair_7' = head(after_pair_7$Var1, n = row_pairs),
                            "pair_7_freq" = head(after_pair_7$Freq, n = row_pairs)
                            )

View(after_pair_df)
write_excel_csv2(after_pair_df, "after_pair_df.csv", delim = ',')
#now for the other direction.

before_pair_1 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U00010631"))))
before_pair_1 <- before_pair_1[order(-before_pair_1$Freq),]

before_pair_2 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "-"))))
before_pair_2 <- before_pair_2[order(-before_pair_2$Freq),]

before_pair_2 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U00010107"))))
before_pair_2 <- before_pair_2[order(-before_pair_2$Freq),]

before_pair_3 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U00010108"))))
before_pair_3 <- before_pair_3[order(-before_pair_3$Freq),]

before_pair_4 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U0001061a"))))
before_pair_4 <- before_pair_4[order(-before_pair_4$Freq),]

before_pair_5 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U00010624"))))
before_pair_5 <- before_pair_5[order(-before_pair_5$Freq),]

before_pair_6 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U00010109"))))
before_pair_6 <- before_pair_6[order(-before_pair_6$Freq),]

before_pair_7 <- data.frame(table(unlist(str_extract_all(t, ANY_CHAR %R% "\U00010624"))))
before_pair_7 <- before_pair_7[order(-before_pair_7$Freq),]

row_pairs <- 37L
#pair 2 only has 19 patterns, so the length is shortened to that.  
before_pair_df <- data.frame('pair_1' = head(before_pair_1$Var1, n = row_pairs),
                            "pair_1_freq" = head(before_pair_1$Freq, n = row_pairs),
                            'pair_2' = head(before_pair_2$Var1, n = row_pairs),
                            "pair_2_freq" = head(before_pair_2$Freq, n = row_pairs),
                            'pair_3' = head(before_pair_3$Var1, n = row_pairs),
                            "pair_3_freq" = head(before_pair_3$Freq, n = row_pairs),
                            'pair_4' = head(before_pair_4$Var1, n = row_pairs),
                            "pair_4_freq" = head(before_pair_4$Freq, n = row_pairs),
                            'pair_5' = head(before_pair_5$Var1, n = row_pairs),
                            "pair_5_freq" = head(before_pair_5$Freq, n = row_pairs),
                            'pair_6' = head(before_pair_6$Var1, n = row_pairs),
                            "pair_6_freq" = head(before_pair_6$Freq, n = row_pairs),
                            'pair_7' = head(before_pair_7$Var1, n = row_pairs),
                            "pair_7_freq" = head(before_pair_7$Freq, n = row_pairs)
)

View(before_pair_df)
write_excel_csv2(before_pair_df, "before_pair_df.csv", delim = ',')


