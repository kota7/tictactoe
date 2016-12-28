#' Simulate Tic-Tac-Toe Games between AIs
#' @param player1,player2 AI players to simulate
#' @param N number of simulation games
#' @param verbose if true, show progress report
#' @param showboard if true, game transition is displayed
#' @param pauseif pause the simulation when specified results occur.
#' This can be useful for explorative purposes.
#' @return integer vector of simulation outcomes
#' @examples
#' res <- ttt_simulate(ttt_ai(), ttt_ai())
#' prop.table(table(res))
#' @export
ttt_simulate <- function(player1, player2 = player1, N = 1000L, verbose = TRUE,
                         showboard = FALSE, pauseif = integer(0))
{
  ## make sure both players have 'getmove' function
  stopifnot(is.function(player1$getmove))
  stopifnot(is.function(player2$getmove))

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
      if (showboard) {
        cat("\n")
        game$show_board()
      }
      action <- p$getmove(game)
      if (!game$is_legal(action)) {
        # should not happen unless there is a bug in the AI
        cat("\n")
        game$show_board()
        cat("chosen:", action)
        stop("illegal move has been chosen")
      }
      game$play(action)
    }
    results[count] <- game$result
    if (showboard) {
      cat("\n")
      game$show_board()
    }
    if (game$result %in% pauseif) {
      ans <- readline("Press ENTER to proceed ('quit' to exit)> ")
      if (ans == "quit") return(results)
    }

  }
  if (verbose) cat("\n")

  return(results)
}
