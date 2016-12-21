#' Play Tic-Tac-Toe Game
#' @param player1,player2 objects that inherit \code{ttt_player} class
#' @param sleep interval to take before an AI player to make decision, in second
#' @export
#' @examples
#' \dontrun{
#' ttt(ttt_human(), ttt_randbot())
#' }
ttt <- function(player1, player2, sleep = 0.5)
{
  stopifnot(inherits(player1, "ttt_player"))
  stopifnot(inherits(player2, "ttt_player"))

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
        action <- p$getmove(game)
        if (action == "quit") return()
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
