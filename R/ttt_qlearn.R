#' Q-Learning to Train Tic-Tac-Toe AI
#' @param player AI player to train
#' @param N number of episode, i.e. training games
#' @param epsilon fraction of random exploration
#' @param alpha learning rate
#' @param gamma discount factor
#' @param sim_every conduct simulation after this many episode
#' @param N_sim number of simulation games
#' @param verbose if true, progress report is shown
#' @return \code{data.frame} of simulation outcomes
#' @export
#' @examples
#' p <- ttt_ai()
#' o <- ttt_qlearn(p, N = 200)
ttt_qlearn <- function(player, N = 1000L,
                       epsilon = 0.1, alpha = 0.8, gamma = 0.99,
                       sim_every = 250L, N_sim = 1000L,
                       verbose = TRUE)
{
  game <- ttt_game()
  out <- NULL
  fmt <- sprintf("\rTraining ...%%%dd/%%%dd", nchar(N), nchar(N))
  for (count in 0:(N-1))
  {
    if (count %% sim_every == 0) {
      cat("\n")
      res <- ttt_simulate(player1 = player, player2 = player,
                          N = N_sim, verbose = verbose)
      if (verbose) {
        print(prop.table(table(res)))
        cat("\n")
      }
      out <- rbind(out,
                   cbind(data.frame(n_train = count, n_sim = N_sim),
                         data.frame(prop.table(table(res)))))
    }

    if (verbose) cat(sprintf(fmt, count+1, N))
    game$initialize()
    while (game$result < 0)
    {
      ## switch first player so the both player can learn both sides

      ## update value function
      choices <- game$legal_moves()
      possible_states <- lapply(choices, game$next_state)
      possible_values <- unlist(getvalues(player$value_func, possible_states))

      ## update the value for all equivalent states
      cur_val <- player$value_func[game$state]
      states <- game$equivalent_states(game$state)
      ## first player maximizes the value, second minimizes the value
      if (game$nextmover == 1L) {
        setvalues(player$value_func, states,
                  (1-alpha)*cur_val + alpha*gamma*max(possible_values))
        player$policy_func[game$state] <-
          choices[possible_values == max(possible_values)]
      } else {
        setvalues(player$value_func, states,
                  (1-alpha)*cur_val + alpha*gamma*min(possible_values))
        player$policy_func[game$state] <-
                  choices[possible_values == min(possible_values)]
      }

      ## action is chosen by epsilon greedy
      if (runif(1) < epsilon) {
        choices <- game$legal_moves()
        action <- if (length(choices) <= 1) choices else sample(choices, 1)
      } else {
        action <- player$getmove(game)
      }
      if (!game$is_legal(action)) {
        # should not happen unless there is a bug
        cat("\n")
        game$show_board()
        cat("chosen:", action)
        stop("illegal move has been chosen")
      }
      game$play(action)
    }
    ## update the value of final state and equivalent states
    ## no need for policy since there is nothing to play
    states <- game$equivalent_states(game$state)

    if (game$result == 1L) {
      player$value_func[game$state] <- 100
    } else if (game$result == 2L) {
      player$value_func[game$state] <- -100
    } else {
      player$value_func[game$state] <- 0
    }
  }
  if (verbose) cat("\n")
  return(out)
}
