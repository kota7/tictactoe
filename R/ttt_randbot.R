#' Random AI Player of Tic-Tac-Toe Game
#' @param name player name
#' @export
#' @return \code{ttt_randbot} object
ttt_randbot <- function(name = "random AI")
{
  ttt_ai(name = name)
}


#' @export
print.ttt_randbot <- function(x, ...)
  cat("Random AI Tic-Tac-Toe Player:", x$name, "\n")
