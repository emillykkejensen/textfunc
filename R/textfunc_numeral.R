#' Trun a number into a numeral (Danish)
#'
#' @param number numeric(number) or integer(number); Number to convert to a numeral
#' @param ordinal logical(); If the number should be converted into an ordinal
#' @param et logical(); If "et" should be used insted of "en"
#' @return Character
#' @export

textfunc.numeral <- function(number, ordinal = FALSE, et = FALSE) {
  if(!(ordinal) & !(et)) return(c("en", "to", "tre", "fire", "fem", "seks", "syv", "otte", "ni", "ti")[number])
  if(!(ordinal) & et) return(c("et", "to", "tre", "fire", "fem", "seks", "syv", "otte", "ni", "ti")[number])
  if(ordinal) return(c("første", "anden", "tredje", "fjerde", "femte", "sjette", "syvende", "ottende", "niende", "tiende")[number])
}
