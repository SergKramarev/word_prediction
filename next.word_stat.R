# That is the function for searching through the probabilities table and returning 
# the word with higher probability
# 
# The fucntion starts with the fivegrams and in case of absence of five grams move
# back to fourgram, trigram bigram and the most common word in the end 

# Loading functions and libraries
library(tm)
library(data.table)


give.next.word1 <- function(data, sentence) {
    sentence <- tolower(sentence)
    sentence <- removePunctuation(sentence, preserve_intra_word_contractions = TRUE,
                                  preserve_intra_word_dashes =TRUE)
    x <- strsplit(sentence, split = " ")
    x <- dict[x[[1]]]
    len <- length(x)
    tmp <- data.table(data[data$word1 == x[len] | data$word2 == x[len] | data$word3 == x[len] | data$word4 == x[len], ])
    
    result <- tmp[word1 == x[len-3] & word2 == x[len-2] & word3 == x[len-1] & word4 == x[len], .(word5, Prob.five)]
    result <- arrange(result, desc(Prob.five))[1,1]
    if (!is.na(result)) {return(unname(rev.dict[result]))}
    
    if (is.na(result)) {
        result <- tmp[word1 == x[len-2] & word2 == x[len-1] & word3 == x[len], .(word4, Prob.four)]
        result <- arrange(result, desc(Prob.four))[1,1]
    } else if (is.na(result)) {
        result <- tmp[word1 == x[len-1] & word2 == x[len], .(word3, Prob.tri)]
        result <- arrange(result, desc(Prob.tri))[1,1]
    } else if (is.na(result)) {
        result <- tmp[word1 == x[len], .(word2, Prob.bigram)]
        result <- arrange(result, desc(Prob.bigram))[1,1]
    } else {result <- 1}
    return(unname(rev.dict[result]))
}
