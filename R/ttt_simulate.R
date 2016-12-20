#' Simulate Tic-Tac-Toe Games between AIs
#' @param player1,player2 AI players to simulate
#' @param N number simulation games
#' @param verbose if true, show progress report
#' @return integer vector of simulation outcomes
#' @examples
#' res <- ttt_simulate(ttt_ai(), ttt_ai())
#' prop.table(table(res))
#' @export
ttt_simulate <- function(player1, player2 = player1, N = 1000L, verbose = TRUE)
{
  results <- rep(NA_integer_, N)
  game <- ttt_game()
  fmt <- sprintf("\rSimulating ...%%%dd/%%%dd", nchar(N), nchar(N))
  for (count in 1:N)
  {
    game$initialize()
    if (verbose) cat(sprintf(fmt, count, N))

    while (game$result < 0)
    {
      p <- if (game$nextmover == 1L) player1 else player2
      action <- p$getmove(game)
      if (!game$is_legal(action)) {
        # should not happen unless there is a bug
        cat("\n")
        game$show_board()
        cat("chosen:", action)
        stop("illegal move has been chosen")
      }
      game$play(action)
    }
    results[count] <- game$result
  }
  if (verbose) cat("\n")

  return(results)
}
