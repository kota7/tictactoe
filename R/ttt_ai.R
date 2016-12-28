#' Tic-Tac-Toe AI Player
#' @description Create an AI tic-tac-toe game player
#' @param name player name
#' @param level AI strength. must be Integer 0 (weekest) to 5 (strongest)
#' @return \code{ttt_ai} object
#' @details \code{level} argument controls the strength of AI, from
#' 0 (weekest) to 5 (strongest).
#' \code{ttt_random} is an alias of \code{ttt_ai(level = 0)}.
#'
#' A \code{ttt_ai} object has the \code{getmove} function, which takes
#' \code{ttt_game} object and returns a move considered as optimal.
#' \code{getmove} function is designed to take a \code{ttt_game} object
#' and returns a move using the policy function.
#'
#' The object has the value and policy functions.
#' The value function maps a game state
#' to the evaluation from the first player's viewpoint.
#' The policy function maps a game state to a set of
#' optimal moves in light of the value evaluation.
#' The functions have been trained through the Q-learning.
#' @export
ttt_ai <- function(name = "ttt AI", level = 0L)
{
  stopifnot(is.numeric(level))
  stopifnot(length(level) >= 1)
  if (length(level) > 1) {
    warning("multiple levels supplied. Only the first element is used")
    level <- level[1]
  }
  level <- as.integer(level)
  if (level < 0 || level > 5)
    stop("level must be 0 to 5")

  if (level == 0) {
    # random bot
    value_func <- xhash(function(state, ...) paste0(state, collapse = ""),
                        default_value = 0)
    policy_func <- xhash(function(state, ...) paste0(state, collapse = ""),
                         default_value = NULL)
  } else {
    value_func  <- trained_value_funcs[[level]]$clone()
    policy_func <- trained_policy_funcs[[level]]$clone()
  }

  getmove <- function(game, ...)
  {
    opt_acts <- unlist(policy_func[game$state])
    if (is.null(opt_acts)) {
      # no policy is defined, act random
      choices <- game$legal_moves()
      if (length(choices) <= 1) return(choices)
      else return(sample(choices, 1))
    }
    if (length(opt_acts) <= 1) return(opt_acts)
    else return(sample(opt_acts, 1))
  }

  self <- environment()
  class(self) <- c("ttt_ai", "ttt_player")
  return(self)
}

#' @export
print.ttt_ai <- function(x, ...)
  cat("AI Tic-Tac-Toe Player:", x$name, "\n")




#' @export
#' @rdname ttt_ai
ttt_random <- function(name = "random AI")
{
  ttt_ai(name = name, level = 0)
}



