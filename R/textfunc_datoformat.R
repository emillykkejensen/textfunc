#' Converts a Date into date format
#'
#' @param date Date("date"), POSIX("date"), etc.; Date to convert
#' @return Character string in date format "%e. %B %Y"
#' @export

textfunc.datoformat <- function(date) {
  return(stringr::str_trim(format(date, "%e. %B %Y")))
}
