#' Clean SoMe text
#'
#' @param textdata character vector; The text you want cleaned
#' @param removePunctuation logical; If TRUE (default) it wil remove all punctuation and symbols from text
#' @param removeURLs logical; If TRUE (default) it wil remove all URLs from text
#' @param removeRT logical; If TRUE it wil remove 'RT' and 'via' from text
#' @param removeHashtag logical; If TRUE it wil remove all words begining with '#' from text
#' @param removeHandles logical; If TRUE it wil remove all words begining with '@' from text
#' @return Character string
#' @export

textfunc.textcleaner <- function(textdata,
                                 removePunctuation = TRUE,
                                 removeURLs = TRUE,
                                 removeRT = FALSE,
                                 removeHashtag = FALSE,
                                 removeHandles = FALSE){

  if(removeRT) textdata <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", textdata)
  if(removeHashtag) textdata <- gsub("[#]\\S+", "", textdata)
  if(removeHandles) textdata <- gsub("[@]\\S+", "", textdata)
  if(removeURLs) textdata <- gsub("http://t.co/[a-z,A-Z,0-9]*{8}", "", textdata)
  if(removeURLs) textdata <- gsub("https://t.co/[a-z,A-Z,0-9]*{8}", "", textdata)
  if(removePunctuation) textdata <- gsub("[[:punct:]]+", "", textdata)
  textdata <- tolower(textdata)
  textdata <- trimws(textdata)

  return(textdata)

}
