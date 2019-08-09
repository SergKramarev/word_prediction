library(keras)
library(readr)
library(stringr)
library(purrr)
library(tokenizers)
library(hunspell)
library(tm)
library(devtools)
install_github("bmschmidt/wordVectors")
library(wordVectors)


# Text preparation --------------------------------------------------------

text.prepare <- function(file.Path, n.lines = -1L, seed = 659, max.mistakes = 2, tokens.in.seq.min.max = c(3, 8)) {
    
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
    
    n.chars <- count_characters(text)
    textlen <- count_words(text)
    print(textlen)
    rate <- n.chars/textlen
    
    stat.text <- data.frame(row.number = 1:length(textlen), n.words = textlen, n.chars = n.chars, rate = rate)
    stat.text <- stat.text[!(stat.text$n.words <= 5 & stat.text$rate > 8), ]
    text <- text[stat.text$row.number]
    
    textlen <- count_words(text)
    print(textlen)
    
    # This line should be in any code!
    text <- text[textlen >= tokens.in.seq.min.max[1] & textlen <= tokens.in.seq.min.max[2]]
    
    
    
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
    # text <- paste0(text, collapse = " ")
    text <- tokenize_words(text, strip_punct = TRUE, strip_numeric = TRUE, simplify = FALSE)
    
    return(text)
}

text <- text.prepare("en_US.twitter.txt", n.lines = 30000)


# Parameters --------------------------------------------------------------

maxlen <- 5

# Data Preparation --------------------------------------------------------

print(sprintf("corpus length: %d", length(text)))

words <- unlist(text) %>%
    unique()

print(sprintf("total chars: %d", length(words))) 
# This variable should be equal to max value tokens.in.seq.minmax 
max.sex.length <- 8

# Creating word vectors for representing words
# Here i need to add code for addind extra "words" to beginning and to an end of sentences
text.for.vectors <- list()
for (i in 1:length(text)) {
    text.for.vectors[[i]] <- c(rep("<empty_word>", 4), text[[i]], rep("<empty_word>", 4))
    
}

writeLines(unlist(text.for.vectors), "twitter.txt")
word.vect <- train_word2vec('twitter.txt', vectors = 100, min_count = 5, force = TRUE, window = 8, negative_samples = 10, iter = 10)
write.csv(word.vect, "vectors.csv")




# 4. Creating train set and labels for future LSTM ------------------------------
vectors <- read.csv("vectors.csv")
vectors <- tibble::column_to_rownames(vectors, "X")
vectors.dim <- ncol(vectors)

for (i in 1:length(text)) {
    text[[i]] <- c(rep("<empty_word>", max.sex.length - length(text[[i]])), text[[i]])
    
}

# Cut the text in semi-redundant sequences of maxlen characters
dataset <- list()
t <- 1

for (i in 1:length(text)){
    seq.i <- length(seq(1, length(text[[i]]) - maxlen - 1, by = 1))
    t.next <- t + seq.i - 1
    dataset[t:t.next] <- map(
        seq(1, length(text[[i]]) - maxlen - 1, by = 1), 
        ~list(sentece = text[[i]][.x:(.x + maxlen - 1)], next_char = text[[i]][.x + maxlen]))
    t <- t.next + 1
}


dataset <- transpose(dataset)

# Creating vectors
# Creating input and output arrays. Input represented by embedding vectors, output one-hot vectors of length = length(words)
x <- array(0, dim = c(length(dataset$sentece), maxlen, vectors.dim))

# Here I have to put labels represented as one hot encoding using vocabulary 
y <- array(0, dim = c(length(dataset$sentece), length(words)))

remove <- vector(mode = "integer")

for (i in 1:length(dataset$sentece)) {
    for (j in 1:maxlen){
        tmp <- which(rownames(vectors) == dataset$sentece[[i]][j])
        
        #vectors is the name of variable that contains word embeddings
        if (!is_empty(tmp)){ # checking is there word in vectors table that now in our dataset sequence
            x[i, j, ] <- unlist(vectors[tmp, ], use.names = FALSE)
        } else {
            x[i, j, ] <- unlist(vectors[1, ], use.names = FALSE)
        }
    }
    if (sum(as.integer(words == dataset$next_char[[i]])) == 0){
        y[i,] <- c(1, rep(0, length(words)-1))
        remove <- c(remove, i)
    } else {
        y[i,] <- as.integer(words == dataset$next_char[[i]])
    }
}



# Vectorization
# x <- array(0, dim = c(length(dataset$sentece), maxlen, length(words)))
# y <- array(0, dim = c(length(dataset$sentece), length(words)))

# for(i in 1:length(dataset$sentece)){
    
#    x[i,,] <- sapply(words, function(x){
#        as.integer(x == dataset$sentece[[i]])
#    })
    
#    y[i,] <- as.integer(words == dataset$next_char[[i]])
    
#}



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
    layer_lstm(128, input_shape = c(maxlen, vectors.dim), return_sequences = TRUE) %>%
    layer_lstm(128) %>% 
    layer_dense(128) %>%
    layer_dense(length(words)) %>%
    layer_activation("softmax")

optimizer <- optimizer_rmsprop(lr = 0.002)

model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = optimizer,
    metrics = "accuracy"
)

# model <- load_model_hdf5("LSTM_26.03_hdf5.h5")
# load_model_weights_hdf5(model, "LSTM_01.05-5.weights.h5")

name.file <- as.character(Sys.Date())

callbacks <- list(callback_early_stopping(monitor = "val_loss", min_delta = 0,005, patience = 5,
                                          verbose = 0, mode = "auto"),
                  callback_csv_logger(paste("train.log.", name.file, ".csv", sep = "")),
                  callback_model_checkpoint(paste("model.", name.file, ".h5", sep = ""))
)

model %>% fit(
    x, y,
    batch_size = 128,
    epochs = 150,
    validation_data = list(x.test, y.test),
    verbose = 2)

# save_model_weights_hdf5(model, "LSTM_01.05-5.weights.h5")
save_model_hdf5(model, "LSTM_words_09.06-1.h5")











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

