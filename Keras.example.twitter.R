library(tokenizers)
library(tm)
library(keras)
library(purrr)
library(hunspell)
install_github("bmschmidt/wordVectors")
library(wordVectors)

# 1. Data Preparation --------------------------------------------------------

text.prepare <- function(file.Path, n.lines = -1L, seed = 659, max.mistakes = 2) {
    
    #Reading files
    con <- file(file.Path, "r")
    print(sprintf("Reading %d lines", n.lines))
    text <- readLines(con, n = n.lines)
    close(con)
    
    # Some basic cleaning
    text <- iconv(text, from =  "utf-8", to = "ascii", sub = " ")
    text <- gsub("Im ", "I'm ", text, fixed = TRUE)
    text <- gsub("Ive", "I've ", text, fixed = TRUE)
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
    text <- text[textlen >= 8]
    
    
    
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
    
    # text <- removeWords(text, stopwords("en"))
    
    
    
    # Some cleaning
    tokenize.text <- function(text) {
        text <- tokenize_word_stems(text, strip_punct = TRUE)
        
        
        for (i in 1:length(text)) {
           text[[i]] <- c(rep("<empty>", 4), text[[i]])
            
        }
        return(text)
    }
    
    print("Tokenization")
    text <- tokenize.text(text)
   
    
    return(text)
}

doc <- text.prepare("C:/Users/Slonoc/Documents/Capstone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")



# 3. Creating embeddings --------------------------------------------------------
writeLines(unlist(doc), "twitter.txt")
word.vect <- train_word2vec('twitter.txt', vectors = 50, min_count = 850, force = TRUE)
write.csv(word.vect, "vectors.csv")




# 4. Creating train set and labels for future LSTM ------------------------------
vectors <- read.csv("vectors.csv")
vectors <- tibble::column_to_rownames(vectors, "X")
vectors.dim <- ncol(vectors)

# Creating vocabulary ----------------------------------------------------------
words <- rownames(vectors)
dict <- 1:length(words)
names(dict) <- words
rev.dict <- names(dict)
names(rev.dict) <- dict

print(sprintf("dimension of vectors: %d", vectors.dim))  




# Cut the text in semi-redundant sequences of maxlen characters
maxlen <- 7
u <- 0
dataset <- list()

for (i in 1:length(doc)){
    for (j in seq(1, length(doc[[i]]) - maxlen - 1, by = 2)){
        u <- u + 1
        dataset$sentece[[u]] <- doc[[i]][j:(j + maxlen - 1)]
        dataset$next_char[[u]] <- doc[[i]][j+maxlen]
        }
}
class(dataset$next_char) <- "list"


# Creating input and output arrays. Input represented by embedding vectors, output one-hot vectors of length = length(words)
x <- array(0, dim = c(length(dataset$sentece), maxlen, vectors.dim))

# Here I have to put labels represented as one hot encoding using vocabulary 
y <- array(0, dim = c(length(dataset$sentece), length(words)))

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
    } else {
        y[i,] <- as.integer(words == dataset$next_char[[i]])
    }
}


# Creating sequences for test set -----------------------------------------------
train.ratio = 0.7
numbers <- seq_len(length(dataset$sentece))
train <- sample(numbers, length(numbers)*train.ratio)
x <- x[train, , ]
x.test <- x[-train, , ]
y <- y[train, ]
y.test <- y[-train, ]

# Model Definition --------------------------------------------------------

model <- keras_model_sequential()

model %>%
    layer_lstm(units = 256, return_sequences = TRUE, input_shape = c(maxlen, vectors.dim)) %>%
    layer_lstm(units = 128) %>%
    layer_dense(length(words)) %>%
    layer_activation("softmax")

    
    

optimizer <- optimizer_rmsprop(lr = 0.001)

model %>% compile(
    loss = "categorical_crossentropy", 
    optimizer = optimizer,
    metrics = "accuracy"
)

model %>% fit(
    x, y,
    batch_size = 128,
    epochs = 750, 
    validation_data = list(x.test, y.test),
    verbose = 2
)



# Training & Results ----------------------------------------------------

sample_mod <- function(preds, temperature = 0.5){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
        as.integer() %>%
        which.max()
}


save_model_hdf5(model, "LSTM_24.02_hdf5.h5")

next.word <- function(sentence, mod = model){
    
    # Preparation of given sentence
    text <- unlist(tokenize_words(sentence))
    seq.length <- maxlen
    if (length(text) < seq.length) {
        seq.input <- c(rep("<empty>", seq.length - length(text)), text)
    } else {
        seq.input <- tail(text, seq.length)
    }
    input <- array(0, dim = c(1, seq.length, vectors.dim))
    for (i in 1:14){
        if (sum(rownames(vectors) == seq.input[i], na.rm = TRUE) == 0){
            input[1, i, ] <- unlist(vectors[1, ])
        } else {
        input[1,i,] <- unlist(vectors[rownames(vectors) == seq.input[i], ])
        }
    }
    
            preds <- predict(mod, input)
            next_index <- sample_mod(preds, 1.0)
            next_word <- rownames(vectors)[next_index]
            
            return(next_word)
}













# Cut the text in semi-redundant sequences of maxlen characters
maxlen <- 20
dataset.test <- map(
    seq(1, length(text.test) - maxlen - 1, by = 1), 
    ~list(sentece = text.test[.x:(.x + maxlen - 1)], next_char = text.test[.x + maxlen])
)

dataset.test <- transpose(dataset.test)

# Creating input and output arrays. Input represented by embedding vectors, output one-hot vectors of length = length(words)
x.test <- array(0, dim = c(length(dataset.test$sentece), maxlen, vectors.dim))

# Here I have to put labels represented as one hot encoding using vocabulary 
y.test <- array(0, dim = c(length(dataset.test$sentece), length(words)))

for (i in 1:length(dataset.test$sentece)) {
    for (j in 1:maxlen){
        tmp <- which(rownames(vectors) == dataset.test$sentece[[i]][j])
        
        #vectors is the name of variable that contains word embeddings
        if (!is_empty(tmp)){ # checking is there word in vectors table that now in our dataset sequence
            x.test[i, j, ] <- unlist(vectors[tmp, ], use.names = FALSE)
        } else {
            x.test[i, j, ] <- unlist(vectors[1, ], use.names = FALSE)
        }
    }
    if (sum(as.integer(words == dataset.test$next_char[[i]])) == 0){
        y.test[i,] <- c(1, rep(0, length(words)-1))
    } else {
        y.test[i,] <- as.integer(words == dataset.test$next_char[[i]])
    }
}

