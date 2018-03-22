#' Get sentiment from text
#'
#' @param textdata character vector; The text you want to get sentiment on
#' @param cleanText logical; If TRUE it will clean text, before getting sentiment
#' @return Sentiment score as integer
#' @text
textfunc.getSentimentScore <- function(textdata, cleanText = TRUE){

  if(cleanText) textdata <- textfunc.textcleaner(textdata)

  library(tokenizers)
  library(data.table)

  textdata <- tokenizers::tokenize_words(textdata)
  names(textdata) <- "word"

  # devtools::use_data(afinn_list, overwrite = TRUE)
  textdata <- as.data.table(textdata)
  textdata <- merge(textdata, afinn_list, by = "word", all.x = TRUE, sort = FALSE)

  textdata[, row_id := .I]

  negatorList <- c("ikke", "aldrig", "ej")

  for(rowN in textdata[!is.na(sentiment) & row_id != 1, row_id]){
    if(textdata[row_id == rowN-1, tolower(word)] %in% negatorList){
      newSentiment <- textdata[row_id == rowN, sentiment] * -1
      set(textdata, i = rowN, j = "sentiment", value = newSentiment)
    }
  }

  return(sum(textdata$sentiment, na.rm = TRUE))

}


#' Get sentiment from sentimen score
#'
#' @param SentimentScore integer; The Sentiment Score you want sentiment text on
#' @return Sentiment as character - positive, neutral og negative
#' @export
textfunc.getSentimentFromScore <- function(SentimentScore){

  sentiment <- if(SentimentScore == 0) "neutral" else if(SentimentScore > 0) "positive" else if(SentimentScore < 0) "negative"

  return(sentiment)

}



