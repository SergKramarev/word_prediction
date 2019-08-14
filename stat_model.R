# That is file for analysis of texts and calculating word probabilities 
# according to Kneser-Ney algorithm.
# Preliminary text cleaning is done by text_prepare function

# Loading functions and libraies
source("text_prepare.R")

library(dplyr)
library(tokenizers)
library(parallel)

# Declaring variables
min.bigr.freq <- 2
min.trig.freq <- 2
min.fourgr.freq <- 2
min.fivegr.freq <- 2

# Loading data

twitter <- text.prepare("en_US.twitter.txt", n.lines = 5000, min.words.in.sentence = 5)
blogs <- text.prepare("en_US.blogs.txt", n.lines = 1000, min.words.in.sentence = 5)
text <- c(twitter, blogs)
rm(twitter, blogs, profanities)

#maybe add start symbola here???????

# Creating bigrams. Calculating frequency of bigrams
bigram <- tokenizers::tokenize_ngrams(text, n = 2)

bigram <- data_frame(bigram = unlist(bigram)) %>% count(bigram, sort = TRUE)
names(bigram)[2] <- "frequency"
bigram <- bigram[bigram$frequency >= min.bigr.freq, ]

cores <- detectCores() - 1
clust <- makeCluster(cores)
bigram$word1 <- parSapply(clust ,bigram$bigram, function(x) strsplit(x, " ")[[1]][1])
bigram$word2 <- parSapply(clust, bigram$bigram, function(x) strsplit(x, " ")[[1]][2])
stopCluster(clust)

number.of.bigram.types <- nrow(bigram)

# Calculating necessary columns for KN
bigram1 <- bigram %>% group_by(word1) %>% summarise(sum.fw.is.fw = sum(frequency), n.disc.bigr = n())
bigram <- merge(bigram, bigram1, all.x = TRUE)
rm(bigram1)
bigram2 <- bigram %>% group_by(word2) %>% summarise(sum.sw.is.sw = sum(frequency))
bigram <- merge(bigram, bigram2, all.x = TRUE)
rm(bigram2)

bigram <- bigram %>% mutate(
                            Prob.bigram = round((frequency - 0.75)/sum.fw.is.fw 
                            + 0.75*n.disc.bigr/sum.fw.is.fw*(sum.sw.is.sw/number.of.bigram.types),
                            digits = 4))
bigram <- select(bigram, word1, word2, bigram, Prob.bigram)


# Creating trigrams
trigram <- tokenizers::tokenize_ngrams(text, n = 3)
trigram <- data_frame(trigram = unlist(trigram)) %>% count(trigram, sort = TRUE)
names(trigram)[2] <- "frequency"
trigram <- trigram[trigram$frequency >= min.trig.freq, ]

cores <- detectCores() - 1
clust <- makeCluster(cores)
trigram$bigram <- parSapply(clust, trigram$trigram, function(x){ 
                            z <- strsplit(x, " ")
                            paste(z[[1]][1], z[[1]][2], sep = " ")
                            })
trigram$word3 <- parSapply(clust, trigram$trigram, function(x) strsplit(x, " ")[[1]][3])
stopCluster(clust)

trigram1 <- trigram %>% group_by(bigram) %>% summarise(
                                            sum.bigram.is.bigram = sum(frequency),
                                            n.disc.trig = n())
trigram <- merge(trigram, trigram1, all.x = TRUE)
trigram <- merge(trigram, bigram, all.x = TRUE, by.x = "bigram", by.y = "bigram")
#rm(bigram, trigram1)

trigram <- trigram %>% mutate(
                            Prob.tri = round((frequency - 0.75)/sum.bigram.is.bigram 
                            + 0.75/sum.bigram.is.bigram*n.disc.trig*Prob.bigram, digits = 4))
                                        
trigram <- select(trigram, 
                  word1, word2, bigram, Prob.bigram, 
                  word3, trigram, Prob.tri)


# Creating fourgrams
fourgram <- tokenizers::tokenize_ngrams(text, n = 4)
fourgram <- unlist(fourgram)

fourgram <- data_frame(fourgram = fourgram) %>% count(fourgram, sort = TRUE)
names(fourgram)[2] <- "frequency"

fourgram <- fourgram[fourgram$frequency >= min.fourgr.freq, ]

cores <- detectCores() - 1
clust <- makeCluster(cores)
fourgram$trigram <- parSapply(clust, fourgram$fourgram, function(x){
                            z <- strsplit(x, " ")
                            paste(z[[1]][1], z[[1]][2], z[[1]][3], sep = " ")
                            })
fourgram$word4 <- parSapply(clust, fourgram$fourgram, function(x) strsplit(x, " ")[[1]][4])
stopCluster(clust)

four1 <- fourgram %>% group_by(trigram) %>% summarise(sum.trigram.is.trigram = sum(frequency), n.disc.four = n())
fourgram <- merge(fourgram, four1, all.x = TRUE)

fourgram <- merge(fourgram, trigram, all.x = TRUE, by.x = "trigram", by.y = "trigram")
#rm(trigram, four1)

fourgram <- fourgram %>% mutate(Prob.four = (frequency - 0.75)/sum.trigram.is.trigram + 0.75*n.disc.four/sum.trigram.is.trigram*Prob.tri)
fourgram <- select(fourgram, 
                   word1, word2, bigram, Prob.bigram, 
                   word3, trigram, Prob.tri, 
                   word4, fourgram, Prob.four)


# Creating fivegrams

fivegram <- tokenizers::tokenize_ngrams(text, n = 5)
# rm(text)
fivegram <- data_frame(fivegram = unlist(fivegram)) %>% count(fivegram, sort = TRUE)
names(fivegram)[2] <- "frequency"

fivegram <- fivegram[fivegram$frequency >= min.fourgr.freq, ]

cores <- detectCores() - 1
clust <- makeCluster(cores)
fivegram$fourgram <- parSapply(clust, fivegram$fivegram, function(x){
                                z <- strsplit(x, " ")
                                paste(z[[1]][1], z[[1]][2], z[[1]][3], z[[1]][4], sep = " ")
                                })
fivegram$word5 <- parSapply(clust, fivegram$fivegram, function(x) strsplit(x, " ")[[1]][5])
stopCluster(clust)

five1 <- fivegram %>% group_by(fourgram) %>%
        summarise(sum.fourgram.is.fourgram = sum(frequency), n.disc.four = n())

fivegram <- merge(fivegram, five1, all.x = TRUE)
fivegram <- merge(fivegram, fourgram, all.x = TRUE, 
                  by.x = "fourgram", by.y = "fourgram")
# rm(fourgram, five1)

fivegram <- fivegram %>% 
            mutate(Prob.five = round((frequency -0.75)/sum.fourgram.is.fourgram 
                   + 0.75*n.disc.four/sum.fourgram.is.fourgram*Prob.four, digits = 4))

fivegram <- select(fivegram, 
                   word1, word2, bigram, Prob.bigram,
                   word3, trigram, Prob.tri,
                   word4, fourgram, Prob.four,
                   word5, fivegram, Prob.five)


# Creating dictionary
words <- unique(c(fivegram$word1, fivegram$word2, fivegram$word3, fivegram$word4, fivegram$word5))

table <- select(fivegram, word1, word2, word3, word4, word5, Prob.bigram, Prob.tri, Prob.four, Prob.five)
#rm(fivegram)

dict <- 1:length(words)
names(dict) <- words
rev.dict <- names(dict)
names(rev.dict) <- dict
rm(words)
table$word1 <- dict[table[,"word1"]]
table$word2 <- dict[table[,"word2"]] 
table$word3 <- dict[table[,"word3"]]
table$word4 <- dict[table[,"word4"]]
table$word5 <- dict[table[,"word5"]] 

write.csv(table, "prob.table.Kneser-Ney.csv")
write.table(dict, "dictionary.txt")
write.table(rev.dict, "rev.dictionary.txt")
