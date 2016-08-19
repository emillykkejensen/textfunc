#' Comma separat a list of text with "and" in the end
#'
#' @param text c("text"); List of character
#' @param lastsep character("og"); The last separator to use
#' @return Character string
#' @export

textfunc.listtext <- function(text, lastsep = "og") {
  if(length(text) == 0) {
    stop("Text is missing")
  } else {
    return(if(length(text) != 1)paste(paste0(text[-length(text)], collapse = ", "), paste(lastsep, text[length(text)])) else text)
  }
}
