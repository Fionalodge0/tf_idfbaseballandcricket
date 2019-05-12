library(tidyverse)
library(tidytext)
library(caret)
library(tm)
library(stringr)
library(text2vec)

setwd('C://Users//Owner//Documents//GitHub//tf_idfpresentationonbaseballandcricket')

filelist = list.files(pattern = ".*.txt")
test <- filelist[c(1,16,22,19,12, 2, 4)]
text.files <- lapply(test, function(x) read.delim2(x, 
                                                   header = FALSE, 
                                                   fill = FALSE, 
                                                   col.names = x, 
                                                   stringsAsFactors = FALSE, 
                                                   quote=""))
names(text.files) <- c('Baseball', 'Softball', 
                       'WiffleBall', 'TeeBall', 'Netball', 'Basketball', 'Cricket')

new.list <- list()
for(i in 1:length(text.files)){
    tmp <- unnest_tokens(text.files[[i]], 
                         output = 'words', 
                         input = !!sym(colnames(text.files[[i]])))
    tmp.df <- as.data.frame(tmp)
    tmp.df['sport'] <- names(text.files)[i]
    new.list[[i]] <-  tmp.df
    names(new.list)[i] <- names(text.files)[i]
}

df1 <- bind_rows(new.list)

df <- 
  bind_rows(new.list) %>%
  count(sport, words) %>%
  cast_dtm(term = words, document = sport, value = n, weighting = tm::weightTfIdf)
df <- DocumentTermMatrix(as.matrix(df1), control = list(weighting =
                                              function(x)
                                                weightTfIdf(x, normalize =
                                                              FALSE),
                                            stopwords = TRUE))

cos.sim <- function(ix) 
{
  A = df[ix[1],]
  B = df[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
n <- nrow(df) 
cmb <- expand.grid(i=1:n, j=1:n) 
C <- matrix(apply(cmb,1,cos.sim),n,n)

library(lsa)

mat <- cosine(t(as.matrix(df)))  


