
#' Create Hash Table for Generic States
#'
#' This function creates an \code{xhash} object, extended version of
#' \code{\link[hash]{hash}}.
#' While \code{\link[hash]{hash}} accepts only strings as indices, \code{xhash}
#' can deal with generic index variables, termed as "state".
#'
#' @param convfunc  function that converts a state to a key.
#' It must take a positional argument \code{state} and keyword arguments
#' represented by \code{...}, and returns a character.
#' @param convfunc_vec function for vectorized conversion from states to keys.
#' This function must receive a positional argument \code{states}
#' and keyword arguments \code{...}
#' and returns character vector.  By default, it vectorizes
#' \code{convfunc} using \code{Map}. User may specify a more efficient function
#' if any.
#' @param default_value value to be returned when a state is not recorded in
#' the table.
#' @export
#' @seealso \link{hash-ops}, \link{vectorized-hash-ops}
#' @examples
#' h <- xhash(convfunc = function(state, ...) paste0(state, collapse='-'))
#'
#' # insert
#' h[c(1, 2, 3)] <- 100
#' h[matrix(1:9, nrow=3, ncol=3)] <- -5
#'
#' # retrieve
#' h[c(1, 2, 3)]
#' h[matrix(1:9, nrow=3, ncol=3)]
#' h[1:9]          # equivalent as above, due to conversion to a same key
#' h[c(3, 2, 1)]   # this is undefined
#'
#' # delete
#' h[c(1, 2, 3)] <- NULL
#'
#' # vectorized operations
#' ## insert
#' setvalues(h, list(1:2, 1:3), c(9, 8))
#' ## retrieve
#' getvalues(h, list(1:9, 1:2, 3:1))
#' ## delete
#' setvalues(h, list(1:9, 1:3), NULL)
#' @return \code{xhash} object
xhash <- function(
  convfunc = function(state, ...) state,
  convfunc_vec = function(states, ...) unlist(Map(convfunc, states, ...)),
  default_value = NULL)
{
  hashtable <- hash::hash()

  ## single key operations
  haskey <- function(state, ...) {
    hash::has.key(convfunc(state, ...), hashtable)
  }

  deletekey <- function(state, ...) {
    hash::del(convfunc(state, ...), hashtable)
    invisible(self)
  }

  setvalue <- function(state, value, ...) {
    hashtable[[convfunc(state, ...)]] <<- value
    invisible(self)
  }

  getvalue <- function(state, ...) {
    if (haskey(state, ...)) hashtable[[convfunc(state, ...)]]
    else default_value
  }


  ## multiple value operations
  haskeys <- function(states, ...) {
    hash::has.key(convfunc_vec(states, ...), hashtable)
  }

  deletekeys <- function(states, ...) {
    hash::del(convfunc(states, ...), hashtable)
    invisible(self)
  }

  setvalues <- function(states, values, ...) {
    hashtable[convfunc_vec(states, ...)] <<- values
    invisible(self)
  }

  getvalues <- function(states, ...) {
    keys <- convfunc_vec(states, ...)
    out <- lapply(keys, function(k) default_value)
    flg <- hash::has.key(keys, hashtable)
    out[flg] <- hash::values(hashtable, keys[flg])
    out
  }

  clone <- function() {
    out <- xhash(convfunc = convfunc,
                 convfunc_vec = convfunc_vec,
                 default_value = default_value)
    out$hashtable <- hashtable
    return(out)
  }


  self <- environment()
  class(self) = "xhash"
  self
}


#' Hash Operations for Single State
#' @name hash-ops
#' @param x object
#' @param state state object
#' @param ... additional arguments to determine the key
#' @param value value to assign
#' @return
#' \itemize{
#'   \item{\code{haskey} returns a logical}
#'   \item{\code{`[`} returns a reference to the object}
#'   \item{\code{`[<-`} returns a value}
#' }


#' @export
#' @rdname hash-ops
haskey <- function(x, ...) { UseMethod("haskey") }

#' @export
#' @rdname hash-ops
`[.xhash` <- function(x, state, ...) x$getvalue(state, ...)

#' @export
#' @rdname hash-ops
`[<-.xhash` <- function(x, state, ..., value) x$setvalue(state, value, ...)

#' @export
#' @rdname hash-ops
haskey.xhash <- function(x, state, ...) x$haskey(state, ...)




#' Vectorized Hash Operations
#' @name vectorized-hash-ops
#' @param x object
#' @param states state object
#' @param ...    additional arugments to determine the keys
#' @param values values to assign
#' @return
#' \itemize{
#'   \item{\code{haskeys} returns a logical vector}
#'   \item{\code{setvalues} returns a reference to the object}
#'   \item{\code{getvalues} returns a list of values}
#' }

#' @export
#' @rdname vectorized-methods
haskeys <- function(x, ...) { UseMethod("haskeys") }

#' @export
#' @rdname vectorized-methods
setvalues <- function(x, ...) { UseMethod("setvalues") }

#' @export
#' @rdname vectorized-methods
getvalues <- function(x, ...) { UseMethod("getvalues") }


#' @export
#' @rdname vectorized-methods
getvalues.xhash <- function(x, states, ...) x$getvalues(states, ...)

#' @export
#' @rdname vectorized-methods
setvalues.xhash <- function(x, states, values, ...)
  x$setvalues(states, values, ...)

#' @export
#' @rdname vectorized-methods
haskeys.xhash <- function(x, states, ...) x$haskeys(states, ...)
