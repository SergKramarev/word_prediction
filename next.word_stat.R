

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