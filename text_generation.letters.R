library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)
library(hunspell)
library(tm)

# Text preparation --------------------------------------------------------

text.prepare <- function(file.Path, n.lines = -1L, seed = 659, max.mistakes = 2) {
    
    #Reading files
    con <- file(file.Path, "r")
    print(sprintf("Reading %d lines", n.lines))
    text <- readLines(con, n = n.lines)
    close(con)
    
    # Some basic cleaning
    text <- iconv(text, from =  "utf-8", to = "ascii", sub = " ")
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
    
    textlen <- count_words(text)
    # This line should be in any code!
    text <- text[textlen >= 15]
    
    
    
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
    text <- paste0(text, collapse = " ")
    text <- tokenize_characters(text, strip_non_alphanum = FALSE, simplify = TRUE)
   
    return(text)
}

text <- text.prepare("en_US.twitter.txt", n.lines = 150000)


# Parameters --------------------------------------------------------------

maxlen <- 40

# Data Preparation --------------------------------------------------------

print(sprintf("corpus length: %d", length(text)))

chars <- text %>%
    unique() %>%
    sort()

print(sprintf("total chars: %d", length(chars)))  

# Cut the text in semi-redundant sequences of maxlen characters
dataset <- map(
    seq(1, length(text) - maxlen - 1, by = 3), 
    ~list(sentece = text[.x:(.x + maxlen - 1)], next_char = text[.x + maxlen])
)

dataset <- transpose(dataset)

# Vectorization
x <- array(0, dim = c(length(dataset$sentece), maxlen, length(chars)))
y <- array(0, dim = c(length(dataset$sentece), length(chars)))

for(i in 1:length(dataset$sentece)){
    
    x[i,,] <- sapply(chars, function(x){
        as.integer(x == dataset$sentece[[i]])
    })
    
    y[i,] <- as.integer(chars == dataset$next_char[[i]])
    
}



# Creating sequences for test set -----------------------------------------------
train.ratio = 0.7
numbers <- seq_len(length(dataset$sentece))
train <- sample(numbers, length(numbers)*train.ratio)
rm(dataset)

x.test <- x[-train, , ]
x <- x[train, , ]
y.test <- y[-train, ]
y <- y[train, ]


# Model Definition --------------------------------------------------------

model <- keras_model_sequential()

model %>%
    layer_lstm(512, input_shape = c(maxlen, length(chars))) %>%
    layer_dropout(0.5) %>%
    layer_dense(length(chars)) %>%
    layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = 0.001)

model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = optimizer,
    metrics = "accuracy"
)

# model <- load_model_hdf5("LSTM_26.03_hdf5.h5")
load_model_weights_hdf5(model, "LSTM_01.05-5.weights.h5")

callbacks <- callback_early_stopping(monitor = "val_loss", min_delta = 0,005, patience = 5,
                           verbose = 0, mode = "auto")


model %>% fit(
    x, y,
    batch_size = 128,
    epochs = 250,
    validation_data = list(x.test, y.test),
    verbose = 2, 
    callbacks = callbacks)

save_model_weights_hdf5(model, "LSTM_01.05-5.weights.h5")


save_model_hdf5(model, "LSTM_01.05-3.h5")











# Results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
        as.integer() %>%
        which.max()
}

on_epoch_end <- function(epoch, logs) {
    
    cat(sprintf("epoch: %02d ---------------\n\n", epoch))
    
    for(diversity in c(0.2, 0.5, 1, 1.2)){
        
        cat(sprintf("diversity: %f ---------------\n\n", diversity))
        
        start_index <- sample(1:(length(text) - maxlen), size = 1)
        sentence <- text[start_index:(start_index + maxlen - 1)]
        generated <- ""
        
        for(i in 1:400){
            
            x <- sapply(chars, function(x){
                as.integer(x == sentence)
            })
            x <- array_reshape(x, c(1, dim(x)))
            
            preds <- predict(model, x)
            next_index <- sample_mod(preds, diversity)
            next_char <- chars[next_index]
            
            generated <- str_c(generated, next_char, collapse = "")
            sentence <- c(sentence[-1], next_char)
            
        }
        
        cat(generated)
        cat("\n\n")
        
    }
}

print_callback <- callback_lambda(on_epoch_end = on_epoch_end)

