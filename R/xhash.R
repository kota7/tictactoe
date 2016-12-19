
#' Create Hash Table for Generic Keys
#' @param convfunc  function that converts a game state to a key.
#' It must take a positional argument \code{state} and keyword arguments
#' represented by \code{...}, and returns a character.
#' @param convfunc_vec function for vectorized conversion from states to keys.
#' This function must receive a positional argument \code{states}
#' and keyword arguments \code{...}
#' and returns character vector.  By default, it tries to vectorize
#' \code{convfunc} using \code{Map}. User may specify more efficient function
#' if any.
#' @param default_value value to be returned when a state is not recorded in
#' the table.
#' @return \code{game_ht} object
#' @export
xhash <- function(
  convfunc = function(state, ...) state,
  convfunc_vec = function(states, ...) unlist(Map(convfunc, states, ...)),
  default_value = NULL)
{
  hashtable <- hash()

  ## single key operations
  haskey <- function(state, ...) {
    has.key(convfunc(state, ...), hashtable)
  }

  deletekey <- function(state, ...) {
    del(convfunc(state, ...), hashtable)
  }

  setvalue <- function(state, value, ...) {
    hashtable[[convfunc(state, ...)]] <<- value
  }

  getvalue <- function(state, ...) {
    if (haskey(state, ...)) hashtable[[convfunc(state, ...)]]
    else default_value
  }


  ## multiple value operations
  haskeys <- function(states, ...) {
    has.key(convfunc_vec(states, ...), hashtable)
  }

  deletekeys <- function(states, ...) {
    del(convfunc(states, ...), hashtable)
  }

  setvalues <- function(states, values, ...) {
    hashtable[convfunc_vec(states, ...)] <<- values
  }

  getvalues <- function(states, ...) {
    keys <- convfunc_vec(states, ...)
    out <- lapply(keys, function(k) default_value)
    flg <- has.key(keys, hashtable)
    out[flg] <- values(hashtable, keys[flg])
    out
  }

  self <- environment()
  class(self) = "xhash"
  self
}




