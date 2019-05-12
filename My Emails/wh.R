library(tidyverse)
library(tidytext)
library(caret)
library(tm)
library(stringr)
library(text2vec)

setwd('C://Users//Owner//Documents//GitHub//tf_idfpresentationonbaseballandcricket//My Emails')

files <- list.files(pattern = '.*.txt')

text.files <- lapply(files, function(x) read.delim2(x, 
                                                   header = FALSE, 
                                                   fill = FALSE, 
                                                   col.names = x, 
                                                   stringsAsFactors = FALSE, 
                                                   quote=""))

names(text.files) <- paste('email', seq(1:length(files)), sep = "")

new.list <- list()
for(i in 1:length(text.files)){
  tmp <- unnest_tokens(text.files[[i]], 
                       output = 'word', 
                       input = !!sym(colnames(text.files[[i]])))
  tmp.df <- as.data.frame(tmp)
  tmp.df['doc_id'] <- names(text.files)[i]
  new.list[[i]] <-  tmp.df
  names(new.list)[i] <- names(text.files)[i]
}

df <- 
  bind_rows(new.list) %>%
  filter(!grepl('\\d', word)) %>%
  anti_join(get_stopwords(), by = 'word')

df.count <- df %>% count(doc_id, word) %>% ungroup()



wordfreq <- 
  df %>%
  group_by(doc_id, word) %>%
  summarize(N = n()) %>%
  mutate(total = sum(N)) %>%
  ungroup()

binded_tfidf <- 
  wordfreq %>%
  bind_tf_idf(term = word, document = doc_id, n = N)


binded_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(doc_id) %>%
  top_n(3, tf_idf) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = doc_id)) + 
  #scale_fill_manual(values = pal[c(1,4)])+
  geom_col(show.legend = FALSE) +
  facet_wrap(~doc_id, scales = 'free') + 
  coord_flip() 

df <- 
  bind_rows(new.list) %>%
  count(doc_id, word) %>%
  cast_dtm(term = word, document = doc_id, value = n, weighting = tm::weightTfIdf)


cos.sim <- function(ix) 
{
  A = df[ix[1],]
  B = df[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(df) 
cmb <- expand.grid(i=1:n, j=1:n) 
C <- matrix(apply(cmb,1,cos.sim),n,n)