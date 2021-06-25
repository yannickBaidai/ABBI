

#' Title
#'
#' @param x   : <integer> current value
#' @param max : <integer> maximum value
#'
#' @details : write in the R console and generate a progress bar for a loop
#' @export
#'
#' @examples
#' 
#' for(i in 1:100)
#' { 
#'   progress_bar(title ="TEST :", x=i, max= 100)
#' }
#' 
progress_bar <- function (title = "", x, max = 100) {
  percent <- x / max * 100
  cat( sprintf('\r%s [%-50s] %d%%',
               title,
              paste(rep('>', percent / 2), collapse = ''),
              floor(percent)))
  if (x == max)
    cat('\n')
}


progress_tilt <- function (x, max = 100) {
  percent <- x / max * 100
  cat(sprintf('\r[%-50s] %d%%',
              paste(rep('>', percent / 2), collapse = ''),
              floor(percent)))
  if (x == max)
    cat('\n')
}

