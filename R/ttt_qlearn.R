#' Q-Learning for Training Tic-Tac-Toe AI
#' @description Train a tic-tac-toe AI through Q-learning
#' @param player AI player to train
#' @param N number of episode, i.e. training games
#' @param epsilon fraction of random exploration move
#' @param alpha learning rate
#' @param gamma discount factor
#' @param simulate if true, conduct simulation during training
#' @param sim_every conduct simulation after this many training games
#' @param N_sim number of simulation games
#' @param verbose if true, progress report is shown
#' @return \code{data.frame} of simulation outcomes, if any
#' @export
#' @examples
#' p <- ttt_ai()
#' o <- ttt_qlearn(p, N = 200)
#' @details This function implements Q-learning to train a tic-tac-toe AI player.
#' It is designed to train one AI player, which plays against itself to update its
#' value and policy functions.
#'
#' The employed algorithm is Q-learning with epsilon greedy.
#' For each state \eqn{s}, the player updates its value evaluation by
#' \deqn{V(s) = (1-\alpha) V(s) + \alpha \gamma max_s' V(s')}
#' if it is the first player's turn.  If it is the other player's turn, replace
#' \eqn{max} by \eqn{min}.
#' Note that \eqn{s'} spans all possible states you can reach from \eqn{s}.
#' The policy function is also updated analogously, that is, the set of
#' actions to reach \eqn{s'} that maximizes \eqn{V(s')}.
#' The parameter \eqn{\alpha} controls the learning rate, and \eqn{gamma} is
#' the discount factor (earlier win is better than later).
#'
#' Then the player chooses the next action by \eqn{\epsilon}-greedy method;
#' Follow its policy with probability \eqn{1-\epsilon}, and choose random
#' action with probability \eqn{\epsilon}.  \eqn{\epsilon} controls
#' the ratio of explorative moves.
#'
#' At the end of game, the player set the value of the final state either to
#' 100 (if the first player wins), -100 (if the second player wins), or
#' 0 (if draw).
#'
#' This learning process is repeated for \code{N} training games.
#' When \code{simulate} is set true, simulation is conducted after
#' \code{sim_every} training games.
#' This would be usefule for observing the progress of training.
#' In general, as the AI gets smarter, the game tends to result in draw more.
#'
#' See Sutton and Barto (1998) for more about the Q-learning.
#' @references
#' Sutton, Richard S and Barto, Andrew G. Reinforcement Learning: An Introduction. The MIT Press (1998)
ttt_qlearn <- function(player, N = 1000L,
                       epsilon = 0.1, alpha = 0.8, gamma = 0.99,
                       simulate = TRUE, sim_every = 250L, N_sim = 1000L,
                       verbose = TRUE)
{
  ## make sure player is proper object
  stopifnot(inherits(player, "ttt_ai"))
  ## further check if player has 'getmove' function
  stopifnot(is.function(player$getmove))

  game <- ttt_game()
  out <- NULL
  fmt <- sprintf("\rTraining ...%%%dd/%%%dd", nchar(N), nchar(N))
  for (count in 0:(N-1))
  {
    if (simulate && (count %% sim_every == 0)) {
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
      if (game$nextmover == 1L) {
        optim_actions <- choices[possible_values == max(possible_values)]
      } else {
        optim_actions <- choices[possible_values == min(possible_values)]
      }

      ## update the value for all equivalent states
      cur_val <- player$value_func[game$state]
      equivalents <- equivalent_states_actions(game$state, optim_actions)
      ## first player maximizes the value, second minimizes the value
      if (game$nextmover == 1L) {
        setvalues(player$value_func, equivalents$states,
                  (1-alpha)*cur_val + alpha*gamma*max(possible_values))
        setvalues(player$policy_func, equivalents$states,
                  equivalents$actions)
      } else {
        setvalues(player$value_func, equivalents$states,
                  (1-alpha)*cur_val + alpha*gamma*min(possible_values))
        setvalues(player$policy_func, equivalents$states,
                  equivalents$actions)
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
    states <- equivalent_states(game$state)

    if (game$result == 1L) {
      setvalues(player$value_func, states, 100)
    } else if (game$result == 2L) {
      setvalues(player$value_func, states, -100)
    } else {
      setvalues(player$value_func, states, 0)
    }
  }
  if (verbose) cat("\n")

  ## simulate with the last outcome
  if (simulate) {
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

  return(out)
}
