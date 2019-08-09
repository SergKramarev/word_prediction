# That is the fucntion for reading and cleaning textfor coursera capstone project.
# All other files in this capstone use this fucntion as a first step towards further
# analysis.


library(dplyr)
library(tm)
library(hunspell)
library(tokenizers)

text.prepare <- function(file.Path, 
                         n.lines = -1L, 
                         seed = 659, 
                         max.mistakes = 2, 
                         tokens.in.seq.min.max = c(20, 200)) {
    
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