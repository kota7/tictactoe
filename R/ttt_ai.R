#' Tic-Tac-Toe AI Player
#' @param name player name
#' @param value_func \code{xhash} object that maps game state onto value
#' @param policy_func \code{xhash} object that maps game state onto action set
#' @return \code{ttt_ai} object
#' @export
ttt_ai <- function(name = "ttt AI", value_func = NULL, policy_func = NULL)
{
  if (is.null(value_func))
    value_func <- xhash(function(state, ...) paste0(state, collapse = ""),
                        default_value = 0)
  if (is.null(policy_func))
    policy_func <- xhash(function(state, ...) paste0(state, collapse = ""),
                         default_value = NULL)


  getmove <- function(game)
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

