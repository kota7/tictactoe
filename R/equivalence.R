#' Equivalent States
#' @description Returns a set of equivalent states and actions
#' @param state state, 3x3 matrix
#' @export
#' @return list of state matrices
equivalent_states <- function(state)
{
  # returns all equivalent states
  s1 <- state[3:1, 1:3]   # vertical flip
  s2 <- state[1:3, 3:1]   # horizontal flip
  s3 <- state[3:1, 3:1]
  s4 <- t(state)          # transpose

  s5 <- s4[3:1, 1:3]
  s6 <- s4[1:3, 3:1]
  s7 <- s4[3:1, 3:1]
  unique(list(state, s1, s2, s3, s4, s5, s6, s7))
}


#' @param action integer vector of indices (1 to 9)
#' @export
#' @rdname equivalent_states
#' @return list of two lists. \code{states} is the set of equivalent states
#' \code{actions} is the set of equivalent actions
equivalent_states_actions <- function(state, action)
{
  # returns all equivalent states and actions
  s1 <- state[3:1, 1:3]   # vertical flip
  s2 <- state[1:3, 3:1]   # horizontal flip
  s3 <- state[3:1, 3:1]
  s4 <- t(state)          # transpose

  s5 <- s4[3:1, 1:3]
  s6 <- s4[1:3, 3:1]
  s7 <- s4[3:1, 3:1]

  a <- matrix(0L, nrow = 3, ncol = 3)
  a[action] <- 1L
  a1 <- which(a[3:1, 1:3] > 0)   # vertical flip
  a2 <- which(a[1:3, 3:1] > 0)   # horizontal flip
  a3 <- which(a[3:1, 3:1] > 0)
  a4 <- t(a)           # transpose

  a5 <- which(a4[3:1, 1:3] > 0)
  a6 <- which(a4[1:3, 3:1] > 0)
  a7 <- which(a4[3:1, 3:1] > 0)
  a4 <- which(a4 > 0)

  list(states = list(s1, s2, s3, s4, s5, s6, s7),
       actions = list(a1, a2, a3, a4, a5, a6, a7))
}
