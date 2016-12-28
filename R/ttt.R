#' Play Tic-Tac-Toe Game
#' @description Start tic-tac-toe game on the console.
#' @details At default, the game is played between humans.
#' Set \code{player1} or \code{player2} to \code{ttt_ai()} to play against
#' an AI player.
#' The strength of the AI can be adjusted by passing the \code{level}
#' argument (0 (weekest) to 5 (strongest)) to the \code{ttt_ai} function.
#'
#' To input your move, type the position like "a1". Only two-length string
#' consisting of an alphabet and a digit is accepted.  Type "exit" to
#' finish the game.
#'
#' You may set both \code{player1} and \code{player2} as AI players.
#' In this case, the game transition is displayed on the console without
#' human inputs.
#' For conducting a large sized simulations of games between AIs, refer to
#' \code{\link{ttt_simulate}}
#' @param player1,player2 objects that inherit \code{ttt_player} class
#' @param sleep interval to take before an AI player to make decision, in second
#' @export
#' @examples
#' \dontrun{
#' ttt(ttt_human(), ttt_random())
#' }
#' @seealso \code{\link{ttt_ai}}, \code{\link{ttt_human}},
#' \code{\link{ttt_simulate}}

ttt <- function(player1 = ttt_human(), player2 = ttt_human(), sleep = 0.5)
{
  ## make sure players are proper object
  stopifnot(inherits(player1, "ttt_player"))
  stopifnot(inherits(player2, "ttt_player"))
  ## further check if both players have 'getmove' function
  stopifnot(is.function(player1$getmove))
  stopifnot(is.function(player2$getmove))

  game <- ttt_game()
  while(TRUE)
  {
    while (game$result < 0)
    {
      game$show_board()
      p <- if (game$nextmover == 1L) player1 else player2
      cat(sprintf(" Player %d (%s) to play\n", game$nextmover, p$name))

      ## obtain action
      while (TRUE)
      {
        if (inherits(p, "ttt_ai")) Sys.sleep(sleep)
        action <- p$getmove(game, prompt = "choose move (e.g. A1) > ")
        if (action == "exit") return()
        if (game$is_legal(action)) break
      }
      cat(" action =", game$index_to_str(action), "\n")

      game$play(action)
    }
    # game over
    cat("game over\n")
    game$show_board()
    if (game$result == 0L) cat(" draw!")
    else {
      winner <- if (game$result == 1L) player1 else player2
      cat(sprintf(" won by Player %d (%s)!\n",
                  game$result, winner$name))
    }

    ans <- readline("one more game? (y/n) > ")
    if (!(substring(ans, 1, 1) %in% c("y", "Y"))) break
    game$initialize()
  }
  cat("see you!\n")
}
