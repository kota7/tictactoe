#' Play Tic-Tac-Toe Game
#' @param player1,player2 objects that inherit \code{ttt_player} class
#' @export
#' @examples
#' \dontrun{
#' ttt(ttt_human(), ttt_randbot())
#' }
ttt <- function(player1, player2)
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
        action <- p$getmove(game)
        if (action == "quit") return()
        if (game$is_legal(action)) break
      }
      cat(" action =", action, "\n")

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

    ans <- readline("one more game? (y/n) >")
    if (!(substring(ans, 1, 1) %in% c("y", "Y"))) break
    game$initialize()
  }
  cat("see you!\n")
}
