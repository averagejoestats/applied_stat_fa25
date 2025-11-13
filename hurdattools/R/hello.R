#' A Hello Function
#'
#' This function prints a friendly greeting.
#'
#' @param name A character string with the name to greet.
#' @return A character string containing the greeting.
#' @examples
#' hello("Tengis")
#' @export
hello <- function(name = "world") {
    paste0("Hello, ", name, "!")
}
