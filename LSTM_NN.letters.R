# LSTM_letters function is the algorithm for analysis of the text for coursera 
# capstone project using LSTM neural net.  It is letter based model.
# First stage of the analysis is text cleaning using text_prepare function
# After cleaning prepared text separated into characters, created arrays and 
# learning of NN.

# Loading functions and libraries
source("text_prepare.R")

library(keras)
library(stringr)
library(purrr)
library(tokenizers)
library(hunspell)
library(tm)

# Declaring variables
maxlen <- 40


# Text preparation --------------------------------------------------------

twitter <- text.prepare("en_US.twitter.txt", n.lines = 100, min.words.in.sentence = 5)
blogs <- text.prepare("en_US.blogs.txt", n.lines = 100, min.words.in.sentence = 5)
text <- c(twitter, blogs)

# Text.prepare function uses word-based limatations in sentence length. Because we have 
# letter based model we need to limit minimal amount of letters in sentence in order
# to properly create arrays for learning.

len <- count_characters(text)
text <- text[len >= maxlen + 10]

# Data Preparation --------------------------------------------------------
text <- tokenize_characters(text, strip_non_alphanum = FALSE, simplify = FALSE)

chars <- unlist(text) %>%
    unique() %>%
    sort()

print(sprintf("total chars: %d", length(chars)))  

# Cut the text in semi-redundant sequences of maxlen characters
dataset <- list()
t <- 1

for (i in 1:length(text)){
    seq.i <- length(seq(1, length(text[[i]]) - maxlen - 1, by = 3))
    t.next <- t + seq.i - 1
    dataset[t:t.next] <- map(
        seq(1, length(text[[i]]) - maxlen - 1, by = 3), 
        ~list(sentece = text[[i]][.x:(.x + maxlen - 1)], next_char = text[[i]][.x + maxlen]))
    t <- t.next + 1
}

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
train.ratio = 0.85
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

callbacks <- callback_early_stopping(monitor = "val_loss", min_delta = 0,005, patience = 5,
                                    verbose = 0, mode = "auto")

model %>% fit(
    x, y,
    batch_size = 128,
    epochs = 3,
    validation_data = list(x.test, y.test),
    verbose = 2, 
    callbacks = callbacks)

save_model_weights_hdf5(model, "LSTM_01.05-5.weights.h5")

save_model_hdf5(model, "LSTM_01.05-3.h5")

writeLines(chars, "chars.txt")

