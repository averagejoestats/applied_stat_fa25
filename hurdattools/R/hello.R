#' A Hello Function
#'
#' This function prints a friendly greeting.
#'
#' @param name A character string with the name to greet.
#' @param greeting A character string with a greeting.
#' @return A character string containing the greeting.
#' @examples
#' hello("Tengis")
#' @export
hello <- function(name = "world", greeting = "Have a great day!") {
    paste0("Hello, ", name, "!", " ", greeting)
}
