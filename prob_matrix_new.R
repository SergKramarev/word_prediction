library(dplyr)
library(tm)
library(hunspell)
library(data.table)
library(tokenizers)


# Loading data
con.twitter <- file("C:/R/Capstone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", "r")
con.blogs <- file("C:/R/Capstone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", "r")
con.news <- file("C:/R/Capstone/Coursera-SwiftKey/final/en_US/en_US.news.txt", "r")
twitter <- readLines(con.twitter, n = 100)
blogs <- readLines(con.blogs, n = 100)
news <- readLines(con.news, n = 100)
close(con.twitter)
close(con.blogs)
close(con.news)

# Profanities 
con.prof <- file("C:/R/Capstone/blacklist_words_YT/blacklist_words.txt", "r")
profanities <- readLines(con.prof)[14]
close(con.prof)
profanities <<- unlist(strsplit(profanities, ", ", fixed = TRUE))

# Creating test and train set
set.seed(258)
twitter.numbers <- seq_len(length(twitter))
twitter.train <- sort(sample(twitter.numbers, length(twitter.numbers)*0.5))
blogs.numbers <- seq_len(length(blogs))
blogs.train <- sort(sample(blogs.numbers, length(blogs.numbers)*0.5))
news.numbers <- seq_len(length(news))
news.train <- sort(sample(news.numbers, length(news.numbers)*0.5))



text.prepare <- function(file.Path, n.lines = -1L, seed = 659, max.mistakes = 2, tokens.in.seq.min.max = c(20, 200)) {
    
    #Reading files
    #con <- file(file.Path, "r")
    #print(sprintf("Reading %d lines", n.lines))
    #text <- readLines(con, n = n.lines)
    #close(con)
    text <- file.Path
    # Some basic cleaning
    text <- iconv(text, from =  "utf-8", to = "ascii", sub = " ")
    text <- removeWords(text, profanities)
    text <- gsub("Im ", "I'm ", text, fixed = TRUE)
    text <- gsub("Ive ", "I've ", text, fixed = TRUE)
    text <- tolower(text)
    text <- gsub("&", " and ", fixed = TRUE, text)
    text <- gsub("(?![a-z']).", " ", text, perl = TRUE)
    text <- gsub(" u ", " you ", text)
    words <- tokenize_words(text, strip_punct = FALSE)
    words <- unique(unlist(words))
    len <- nchar(words)
    one.letter <- words[len == 1]
    two.letter <- words[len == 2]
    three.letter <- words[len == 3]
    
    # there are onle two one letter word in English "a" and "i"
    one.letter.wrong <- one.letter[!(one.letter %in% c("i", "a", "m", "s", "d", "t"))]
    one.letter.wrong <- removePunctuation(one.letter.wrong)
    text <- removeWords(text, one.letter.wrong)
    
    # there are only 24 most used two letters words, so removing other words combinations will make text cleaner
    two.letters <- readLines("two_letter_words.txt")
    two.letters <- unlist(strsplit(two.letters, " "))
    two.letters <- c(two.letters, tolower(state.abb),"dc", "cd", "tv", "kg", "re", "ll", "ve", "nt")
    two.letter.wrong <- two.letter[!(two.letter %in% two.letters)]
    text <- removeWords(text, two.letter.wrong)
    
    three.letter.wrong <- three.letter[!(hunspell_check(three.letter))]
    three.letter.wrong <- three.letter.wrong[-(grep("i'm",three.letter.wrong, fixed = TRUE))]
    three.letter.wrong <- three.letter.wrong[-(grep("i'd",three.letter.wrong, fixed = TRUE))]
    
    tlw <- length(three.letter.wrong)
    tlw1 <- floor(tlw/2)
    
    three.letter.wrong1 <- three.letter.wrong[1:tlw1]
    three.letter.wrong2 <- three.letter.wrong[tlw1:tlw]
    text <- removeWords(text, three.letter.wrong1)
    text <- removeWords(text, three.letter.wrong2)
    
    n.chars <- count_characters(text)
    textlen <- count_words(text)
    rate <- n.chars/textlen
    
    stat.text <- data.frame(row.number = 1:length(textlen), n.words = textlen, n.chars = n.chars, rate = rate)
    stat.text <- stat.text[!(stat.text$n.words <= 5 & stat.text$rate > 8), ]
    text <- text[stat.text$row.number]
    
    
    # This line should be in any code!
    # text <- text[n.chars >= tokens.in.seq.min.max[1] & n.chars <= tokens.in.seq.min.max[2]]
    
    
    delete.mistakes <- function(text, max.m = max.mistakes){
        text <- unlist(tokenize_sentences(text))
        sentences <- length(text)
        mistakes <- hunspell(text)
        mistakes <- sapply(mistakes, length)
        correct <- mistakes <= max.m
        total.correct <- length(correct)
        total.mistakes <- sum(mistakes)
        text <- text[correct]
        return(list(text.correct = text, mistakes = total.mistakes, correct.sent = total.correct, num.sent = sentences))
    }
    
    correct.text <- delete.mistakes(text)
    print(sprintf("Text contains %d mistakes", correct.text$mistakes))
    print(sprintf("Text contains %d total sentences", correct.text$num.sent))
    print(sprintf("Text contains %d correct sentences", correct.text$correct.sent))
    
    text <- correct.text$text.correct
    text <- gsub("( )+", " ", text)
    #    text <- paste0(text, collapse = " ")
    #    text <- tokenize_characters(text, strip_non_alphanum = FALSE, simplify = TRUE)
    # text <- tokenize_words(text, strip_non_alphanum = FALSE, simplify = FALSE)   
    return(text)
} 




# Data cleaning. Step 1 - removing profanities, removing non-latin symbols
clean.text <- function(text){
  require(tm)
  # converting text to the same encoding
  text <- iconv(text, from =  "utf-8", to = "ascii", sub = " ")
  text <- removeWords(text, profanities)
  text <- removeNumbers(text)
  text <- tolower(text)
  text <- unlist(tokenizers::tokenize_sentences(text))
  # changing all symbols that is NOT English letters, Numbers or apostrophe to space " ". That will remove all punctuation as well
  text <- gsub("(?![a-z']).", " ", text, perl = TRUE)
  text
}

require(parallel)

twitter <- text.prepare(twitter)
blogs <- text.prepare(blogs)
news <- text.prepare(news)



text <- twitter[twitter.train]
twitter.test <- twitter[-twitter.train]
text1 <- blogs[blogs.train]
blogs.test <- blogs[-blogs.train]
text2 <- news[news.train]
news.test <- news[-news.train]

text <- c(text, text1, text2)
text <- c(twitter, blogs, news)
# test <- c(twitter.test, blogs.test, news.test)
# con <- file("test.txt", "w")
# writeLines(test, con)
# close(con)
# rm(test)
rm(text1)
rm(text2)
rm(twitter)
rm(blogs)
rm(news)
rm(twitter.numbers)
rm(news.numbers)
rm(blogs.numbers)
rm(twitter.test)
rm(blogs.test)
rm(news.test)

# Data cleaning. Step 2. Removing single letter words
text <- gsub(" u ", " you ", text)
text <- gsub("(?!i)(?!a) ([a-z]{1}) ", "", text, perl = TRUE)

rm(profanities)

# text <- text.check(text)
words <- tokenizers::count_words(unlist(text))
text <- text[words >= 4]


# Creating bigrams. Calculating frequency
bigram <- tokenizers::tokenize_ngrams(text, n = 2)

bigram <- data_frame(bigram = unlist(bigram)) %>% count(bigram, sort = TRUE)
names(bigram)[2] <- "frequency"



cores <- detectCores() - 1
clust <- makeCluster(cores)

bigram$word1 <- parSapply(clust ,bigram$bigram, function(x) strsplit(x, " ")[[1]][1])
bigram$word2 <- parSapply(clust, bigram$bigram, function(x) strsplit(x, " ")[[1]][2])
stopCluster(clust)

#bigram <- merge(bigram, w2, by = c("word1", "word2", "bigram"), all.x = TRUE, all.y = TRUE)
#rm(w2)
#bigram$frequency <- rowSums(bigram[, c(4,5)], na.rm = TRUE)
#bigram <- bigram %>% select(word1, word2, bigram, frequency)
number.of.bigram.types <- nrow(bigram)

# Calculating necessary columns for KN
bigram1 <- bigram %>% group_by(word1) %>% summarise(sum.fw.is.fw = sum(frequency), n.disc.bigr = n())
bigram <- merge(bigram, bigram1, all.x = TRUE)
rm(bigram1)
bigram2 <- bigram %>% group_by(word2) %>% summarise(sum.sw.is.sw = sum(frequency))
bigram <- merge(bigram, bigram2, all.x = TRUE)
rm(bigram2)


bigram <- bigram %>% mutate(P2 = (frequency - 0.75)/sum.fw.is.fw + 0.75*n.disc.bigr/sum.fw.is.fw*(sum.sw.is.sw/number.of.bigram.types))


# Delete unnecessary columns!
bigram <- bigram[, c(1, 2, 3,  8)]


# Creating trigrams
trigram <- tokenizers::tokenize_ngrams(text, n = 3)
trigram <- data_frame(trigram = unlist(trigram)) %>% count(trigram, sort = TRUE)
names(trigram)[2] <- "frequency"
# trigram <- trigram[trigram$frequency >= 3, ]

cores <- detectCores() - 1
clust <- makeCluster(cores)
trigram$bigram <- parSapply(clust, trigram$trigram, function(x){ 
  z <- strsplit(x, " ")
  paste(z[[1]][1], z[[1]][2], sep = " ")
})
trigram$word3 <- parSapply(clust, trigram$trigram, function(x) strsplit(x, " ")[[1]][3])
stopCluster(clust)

#trigram <- merge(trigram, w3, by = c("bigram", "word3"), all.x = TRUE, all.y = TRUE)
#rm(w3)
#trigram$frequency <- rowSums(trigram[, 4:5], na.rm = TRUE)
#trigram <- trigram %>% mutate(trigram = paste(bigram, word3, sep = " " ))
#trigram <- trigram[, -c(4,5)]



trigram1 <- trigram %>% group_by(bigram) %>% summarise(sum.bigram.is.bigram = sum(frequency), n.disc.trig = n())
trigram <- merge(trigram, trigram1, all.x = TRUE)
rm(trigram1)

trigram <- merge(trigram, bigram, all.x = TRUE, by.x = "bigram", by.y = "bigram")
rm(bigram)

trigram[is.na(trigram$Pb.kn), "P2"] <- 0

trigram <- trigram %>% mutate(P3 = (frequency - 0.75)/sum.bigram.is.bigram + 0.75/sum.bigram.is.bigram*n.disc.trig*P2)
trigram <- trigram[, c(7, 8, 1, 9, 2, 3, 10)]


# Creating fourgrams
fourgram <- tokenizers::tokenize_ngrams(text, n = 4)
fourgram <- unlist(fourgram)

fourgram <- data_frame(fourgram = fourgram) %>% count(fourgram, sort = TRUE)
names(fourgram)[2] <- "frequency"

fourgram <- fourgram[fourgram$frequency >= 2, ]

cores <- detectCores() - 1
clust <- makeCluster(cores)
fourgram$trigram <- parSapply(clust, fourgram$fourgram, function(x){
  z <- strsplit(x, " ")
  paste(z[[1]][1], z[[1]][2], z[[1]][3], sep = " ")
})
fourgram$word4 <- parSapply(clust, fourgram$fourgram, function(x) strsplit(x, " ")[[1]][4])
stopCluster(clust)

#fourgram <- merge(fourgram, w4, by = c("trigram", "word4"), all.x = TRUE, all.y = TRUE)
#rm(w4)
#fourgram$frequency <- rowSums(fourgram[, 4:5], na.rm = TRUE)
#fourgram <- fourgram %>% mutate(fourgram = paste(trigram, word4, sep = " "))
#fourgram <- fourgram[, -c(4,5)]

# Чего то не работает с 4-граммами. Нужно проверить слияния таблиц

four1 <- fourgram %>% group_by(trigram) %>% summarise(sum.trigram.is.trigram = sum(frequency), n.disc.four = n())
fourgram <- merge(fourgram, four1, all.x = TRUE)
rm(four1)


fourgram <- merge(fourgram, trigram, all.x = TRUE, by.x = "trigram", by.y = "trigram")
rm(trigram)
fourgram[is.na(fourgram$Pt.kn), "P3"] <- 0

fourgram <- fourgram %>% mutate(P4 = (frequency - 0.75)/sum.trigram.is.trigram + 0.75*n.disc.four/sum.trigram.is.trigram*P3)

fourgram <- fourgram[, c(7, 8, 9, 10, 11, 1, 12, 2, 3, 13)]


# Creating fivegrams

fivegram <- tokenizers::tokenize_ngrams(text, n = 5)
rm(text)
fivegram <- data_frame(fivegram = unlist(fivegram)) %>% count(fivegram, sort = TRUE)
names(fivegram)[2] <- "frequency"

fivegram <- fivegram[fivegram$frequency >= 2, ]
cores <- detectCores() - 1
clust <- makeCluster(cores)
fivegram$fourgram <- parSapply(clust, fivegram$fivegram, function(x){
  z <- strsplit(x, " ")
  paste(z[[1]][1], z[[1]][2], z[[1]][3], z[[1]][4], sep = " ")
})
fivegram$word5 <- parSapply(clust, fivegram$fivegram, function(x) strsplit(x, " ")[[1]][5])
stopCluster(clust)

fivegram <- merge(fivegram, w5, by = c("fourgram", "word5"), all.x = TRUE, all.y = TRUE)
rm(w5)
fivegram$frequency <- rowSums(fivegram[, 4:5], na.rm = TRUE)
fivegram <- fivegram %>% mutate(fivegram = paste(fourgram, word5, sep = " "))
fivegram <- fivegram[, -c(4,5)]



five1 <- fivegram %>% group_by(fourgram) %>% summarise(sum.fourgram.is.fourgram = sum(frequency), n.disc.four = n())

fivegram <- merge(fivegram, five1, all.x = TRUE)
rm(five1)

fivegram <- merge(fivegram, fourgram, all.x = TRUE, by.x = "fourgram", by.y = "fourgram")
rm(fourgram)
fivegram[is.na(fivegram$Pkn), "P4"] <- 0


fivegram <- fivegram %>% mutate(P5 = (frequency -0.75)/sum.fourgram.is.fourgram + 0.75*n.disc.four/sum.fourgram.is.fourgram*P4)

fivegram <- fivegram[, c(7,8,9,10,11,12,13,14,1,15,2,16)]


# Creating dictionary
wordsbigram <- unique(unlist(tokenizers::tokenize_words(fivegram[,3], strip_punct = FALSE)))
wordstrigram <- unique(unlist(tokenizers::tokenize_words(fivegram[,6], strip_punct = FALSE)))
wordsfourgram <- unique(unlist(tokenizers::tokenize_words(fivegram[,9], strip_punct = FALSE)))
wordsfivegram <- unique(fivegram[, 11])
words <- unique(c(wordsbigram, wordstrigram, wordsfourgram, wordsfivegram))
rm(wordsbigram)
rm(wordstrigram)
rm(wordsfourgram)
rm(wordsfivegram)

table <- as.data.table(fivegram[, c(1,2,4,5,7,8,10,11,12)])
rm(fivegram)

dict <- 1:length(words)
names(dict) <- words
rev.dict <- names(dict)
names(rev.dict) <- dict
rm(words)
table$word1 <- dict[table[,word1]]
table$word2 <- dict[table[,word2]] 
table$word3 <- dict[table[,word3]]
table$word4 <- dict[table[,word4]]
table$word5 <- dict[table[,word5]] 

write.csv(table, "full.table.csv")


give.next.word <- function(data, sentence) {
  require(tm)
  require(data.table)
  sentence <- tolower(sentence)
  sentence <- removePunctuation(sentence, preserve_intra_word_contractions = TRUE,
                                preserve_intra_word_dashes =TRUE)
  x <- strsplit(sentence, split = " ")
  x <- dict[x[[1]]]
  len <- length(x)
  result <- data[word1 == x[len-3] & word2 == x[len-2] & word3 == x[len-1] & word4 == x[len], .(word5, P5)]
  result <- arrange(result, desc(P5))[1,1]
  if (is.na(result) == TRUE) {
    result <- data[word1 == x[len-2] & word2 == x[len-1] & word3 == x[len], .(word4, P4)]
    result <- arrange(result, desc(P4))[1,1]
  } else if (is.na(result) == TRUE) {
    result <- data[word1 == x[len-1] & word2 == x[len], .(word3, P3)]
    result <- arrange(result, desc(P3))[1,1]
  } else if (is.na(result) == TRUE) {
    result <- data[word1 == x[len], .(word2, P2)]
    result <- arrange(result, desc(P2))[1,1]
  } else {result <- 1}
  return(result)
} 
 