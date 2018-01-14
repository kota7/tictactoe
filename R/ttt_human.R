#' Human Tic-Tac-Toe Player
#' @description Create an human tic-tac-toe player
#' @param name player name
#' @export
#' @examples
#' p <- ttt_human()
#' \dontrun{
#' p$getmove()
#' }
#' @return \code{ttt_human} object
#'
#' \strong{Fields}
#' \describe{
#'   \item{\code{name}}{Player name}
#' }
#'
#' \strong{Methods}
#' \describe{
#'   \item{\code{getmove(game, prompt = "choose move (e.g. A1) > ", ...)}}{Communicate with users to type in the next move.
#'
#'     \emph{Input:}
#'     \itemize{
#'       \item{\code{game}: \code{\link{ttt_game}} object}
#'       \item{\code{prompt}: prompt message}
#'     }
#'
#'     \emph{Output:} a character of a move
#'   }
#' }
ttt_human <- function(name = "no name")
{
  getmove <- function(game, prompt = "choose move (e.g. A1) > ", ...)
  {
    # returns move to a game from standard input
    while (TRUE)
    {
      ans <- readline(prompt)
      if (grepl("^[abcABC][123]$", ans)) return(ans)

      message(sprintf('`%s` is invalid move', ans))
      #return(ans)
    }
  }

  self <- environment()
  class(self) <- c("ttt_human", "ttt_player")
  return(self)
}


#' @export
print.ttt_human <- function(x, ...)
  cat("Human Tic-Tac-Toe Player:", x$name, "\n")
