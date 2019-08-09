# That is the fucntion for reading and cleaning text for coursera capstone project.
# All other files in this capstone use this fucnction as a first step towards further
# analysis.
# 
# Function converts all sumbols to ASCII, removes profanities, does some basic
# cleaning like addition of apostrophe, removing non-letter symbols, removing 
# numbers, correct some contradictions,


library(dplyr)
library(tm)
library(hunspell)
library(tokenizers)

# Creating list of profanities. This list is the list of profanities that 
# downloaded from YT and that is their official list.

con.prof <- file("blacklist_words.txt", "r")
profanities <- readLines(con.prof)[14]
close(con.prof)
profanities <<- unlist(strsplit(profanities, ", ", fixed = TRUE))


text.prepare <- function(file.Path, 
                         n.lines = -1L, 
                         seed = 659, 
                         max.mistakes = 2, 
                         min.words.in.sentence = 3,
                         separate.sentences = TRUE) {
    
    #Reading file with text
    con <- file(file.Path, "r")
    print(sprintf("Reading %d lines", n.lines))
    text <- readLines(con, n = n.lines)
    close(con)
    
    #Tokenizing text in sentences if separate.sentences = TRUE
    if (separate.sentences) {
        text <- tokenize_sentences(text, simplify = TRUE)
        text <- unlist(text)
    }
    
    # Some basic cleaning
    text <- text %>% 
        iconv(from =  "utf-8", to = "ascii", sub = " ") %>%
        removeWords(profanities) %>%
        tolower() %>%
        gsub(pattern = "^im ", replacement = "i'm ", fixed = TRUE) %>%
        gsub(pattern = "^ive ", replacement =  "i've ", fixed = TRUE) %>%
        gsub(pattern = " im ", replacement = "i'm ", fixed = TRUE) %>%
        gsub(pattern = " ive ", replacement =  "i've ", fixed = TRUE) %>%
        gsub(pattern = "&", replacement =  " and ", fixed = TRUE) %>%
        gsub(pattern = "(?![a-z']).", replacement = " ", perl = TRUE) %>%
        gsub(pattern = " u ", replacement =  " you ")
    
    
    
    # Creating vocabulary
    words <- tokenize_words(text, strip_punct = FALSE)
    words <- unique(unlist(words))
    len <- nchar(words)
    one.letter <- words[len == 1]
    two.letter <- words[len == 2]
    print(two.letter)
    three.letter <- words[len == 3]
    
    # there are onle two one letter word in English "a" and "i"
    one.letter.wrong <- one.letter[!(one.letter %in% c("i", "a", "m", "s", "d", "t"))]
    one.letter.wrong <- removePunctuation(one.letter.wrong)
    text <- removeWords(text, one.letter.wrong)
    
    # There are 24 most used two letters words, so removing other words 
    # combinations will make text cleaner. These 24 most used two-letter words
    # are in two_letter_words.txt file
    two.letters <- readLines("two_letter_words.txt")
    two.letters <- unlist(strsplit(two.letters, " "))
    two.letters <- c(two.letters, tolower(state.abb),"dc", "cd", "tv", "kg", "re", "ll", "ve", "nt")
    two.letter.wrong <- two.letter[!(two.letter %in% two.letters)]
    print(two.letter.wrong)
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
    
    # Calculating some basic statistic that can show abnormalities in our text.
    # This stat can show too long words (sentences entered without spaces) or too
    # short words
    n.chars <- count_characters(text)
    textlen <- count_words(text)
    rate <- n.chars/textlen
    
    stat.text <- data.frame(row.number = 1:length(textlen), n.words = textlen,
                            n.chars = n.chars, rate = rate)
    stat.text <- stat.text[!(stat.text$n.words <= 5 & stat.text$rate > 8), ]
    text <- text[stat.text$row.number]
    
    # Deleting sentences with number of mistakes more than certain amount specified
    # in function
    
    text <- unlist(tokenize_sentences(text))
    sentences <- length(text)
    mistakes <- hunspell(text)
    mistakes <- sapply(mistakes, length)
    correct <- mistakes <= max.mistakes
    total.correct <- length(correct)
    total.mistakes <- sum(mistakes)
    text <- text[correct]
   
    print(sprintf("Text contains %d mistakes", total.mistakes))
    print(sprintf("Text contains %d total sentences", sentences))
    print(sprintf("Text contains %d correct sentences", total.correct))
    
    text <- gsub("( )+", " ", text)
    #text <- removePunctuation(text)
    n.words <- count_words(text)
    text <- text[n.words >= min.words.in.sentence]
    
     
    return(text)
} 