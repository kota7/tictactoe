#' Tic-Tac-Toe Game
#' @export
#' @return \code{ttt_game} object
#' @examples
#' x <- ttt_game()
#' x$play(3)
#' x$play(5)
#' x$show_board()
ttt_game <- function()
{
  # place holder of member variables
  state <- NULL
  result <- NULL
  history <- NULL
  nextmover <- NULL
  prevmover <- NULL

  initialize <- function()
  {
    state <<- matrix(0L, nrow = 3, ncol = 3)
    result <<- -1L
    history <<- matrix(0, nrow = 0, ncol = 2)
    nextmover <<- 1L
    prevmover <<- 2L
  }
  initialize()



  show_board <- function()
  {
    cat("    A B C\n")
    cat("   ------\n")
    board <- state
    board[state == 0L] <- "."
    board[state == 1L] <- "X"
    board[state == 2L] <- "O"
    lapply(1:3, function(i) {
      cat(" ")
      cat(i)
      cat("| ")
      cat(paste0(board[i,], collapse = " "))
      cat("\n")
    })
    cat("\n")
  }


  legal_moves <- function()
  {
    ## returns integer vector of legal moves
    which(state == 0L)
  }

  to_index <- function(position)
  {
    ## convert position into index
    ## position is either:
    ##   single integer   1~9
    ##   single character like A1~C3
    ##   integer vector like (1,1)
    ## invalid positions get zero, which is out of bounds

    if (!is.numeric(position) & !is.character(position)) return(0L)
    if (is.numeric(position)) {
      position <- as.integer(position)
      if (length(position) == 2L) {
        if (any((position < 1) | position > 3)) return(0L)
        return(position[1] + position[2]*3L - 3L)
      } else if (length(position) == 1L) {
        if (position < 1 | position > 9) return(0L)
        return(position)
      }
    } else if (is.character(position)) {
      # expect "A1" form
      if (length(position) != 1L) return(0L)
      if (nchar(position) != 2L) return(0L)
      # convert positio to integer vector of size 2
      position <- utf8ToInt(toupper(position)) - c(64L, 48L)
      # out of bounds
      if (any((position < 1) | position > 3)) return(0L)
      return(position[2] + position[1]*3L - 3L)
    }
    return(0L)
  }

  index_to_str <- function(action)
  {
    # convert action into string form like "A1"
    # if the input is invalid, return "  "
    action <- to_index(action)
    action <- as.integer(action)
    i <- 1L + ((action - 1L) %% 3)
    j <- 1L + ((action - 1L) %/% 3)
    if (is.na(i) || is.na(j)) return("  ")
    if (i < 1 || j < 1 || i > 3 || j > 3) return("  ")
    sprintf("%s%d", intToUtf8(i + 64L), j)
  }


  is_legal <- function(position)
  {
    ## check if a move is legal
    index <- to_index(position)
    if (index < 1L || index > 9L) return(FALSE)
    return(state[index] == 0L)
  }

  next_state <- function(position, ...)
  {
    ## returns hypothetical next state without changing the state
    ## if position is invalid move, then returns the current state
    ## nextmover can be overloaded
    if (!is_legal(position)) return(state)
    if (nextmover != 1L && nextmover != 2L) return(state)

    state[to_index(position)] <- nextmover
    state
  }

  play <- function(position, ...)
  {
    ## play the position
    ## return logical indicating the play was successful
    ##   FALSE if game is already over
    ##   FALSE if position is illegal
    ##   TRUE otherwise

    ## game is already over
    if (result >= 0) return(FALSE)

    ## play a move
    ## nextmover may be overloaded
    ## returns if the play is legal
    if (!is_legal(position)) return(FALSE)

    ## play, update result
    index <- to_index(position)
    state[index] <<- nextmover
    history <<- rbind(history, c(nextmover, index))

    ## update result
    if (check_win(nextmover)) {
      result <<- nextmover
    } else {
      if (all(state > 0)) result <<- 0L
    }
    ## switch mover
    prevmover <<- nextmover
    nextmover <<- 3L - nextmover
    return(TRUE)
  }

  check_result <- function()
  {
    if (check_win(1L)) 1L
    else if (check_win(2L)) 2L
    else if (all(state > 0)) 0L
    else -1L
  }

  check_win <- function(player)
  {
    ## check if player has won
    if (player != 1L && player != 2L) return(FALSE)
    for (i in 1:3)
    {
      if (all(state[i,] == player)) return(TRUE)
      if (all(state[,i] == player)) return(TRUE)
    }
    if (all(state[cbind(1:3,1:3)] == player)) return(TRUE)
    if (all(state[cbind(1:3,3:1)] == player)) return(TRUE)

    return(FALSE)
  }



  self <- environment()
  class(self) <- "ttt_game"
  return(self)
}



#' @export
print.ttt_game <- function(x, ...)
{
  cat("* Tic-Tac-Toe Game *\n\n")
  x$show_board()
  if (x$result < 0) cat("next mover: ", x$nextmover, "\n")
  else if (x$result > 0) cat("won by: ", x$result, "\n")
  else cat("draw\n")
  cat("\n")
}
