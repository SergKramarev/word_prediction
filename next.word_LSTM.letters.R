# This is the fucntion for next word generation with LSTM neural net.
# Neural net was trained using the file LSTM_NN.letters and have final accuracy
# around 55%.

# Loading libraries
library(keras)
library(stringi)
library(stringr)
library(tokenizers)

# Variables declaration
maxlen <- 40
chars <- readLines("chars.txt")
model <- load_model_hdf5("!!!model.2019-07-15.h5")

# Code

sample_mod <- function(preds, temperature = 1){
    preds <- log(preds)/temperature
    exp_preds <- exp(preds)
    preds <- exp_preds/sum(exp(preds))
    
    rmultinom(1, 1, preds) %>% 
        as.integer() %>%
        which.max()
}

NN_next.word <- function(sentence_entered) {
    
    sentence <- tokenizers::tokenize_characters(sentence_entered, simplify = FALSE, strip_non_alphanum = FALSE)
    sentence1 <- tail(unlist(sentence), maxlen)
    
    for(diversity in c(1)){
        generated <- ""
        next_index <- 1
        
        while (next_index != 2){
            
            x <- sapply(chars, function(x){
                as.integer(x == sentence1)
            })
            x <- array_reshape(x, c(1, dim(x)))
            
            preds <- keras::predict_on_batch(model, x) # just "predict" don't work in app deployed on shiny apps server
            next_index <- sample_mod(preds, diversity)
            next_char <- chars[next_index]
            
            generated <- str_c(generated, next_char, collapse = "")
            sentence1 <- c(sentence1[-1], next_char)
            
        }
        
        prev <- word(sentence_entered, -1)
        print(prev)
        return(paste(prev, generated, sep = ""))
        
    }
}