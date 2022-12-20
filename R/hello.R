#' Hello, world!
#'
#' @param x the name of the person saying hello.
#'
#' @return the output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("John")
#' \dontrun{
#' hello("James")
#' }
hello <- function(x) {
  print(paste("Hello, world! This is", x))
}
