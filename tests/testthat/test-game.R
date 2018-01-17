library(tictactoe)
library(testthat)
library(combiter)

context("ttt game")


test_that("plays and undos", {
  game <- ttt_game()
  expect_true(all(game$state == 0))
  expect_equal(game$nextmover, 1)
  expect_equal(game$prevmover, 2)
  expect_equal(nrow(game$history), 0)
  expect_equal(game$result, -1)

  p1 <- 2
  game$play(p1)
  expect_true(all(game$state[-p1] == 0))
  expect_equal(game$state[p1], 1)
  expect_equal(game$nextmover, 2)
  expect_equal(game$prevmover, 1)
  expect_equal(nrow(game$history), 1)
  expect_equal(game$result, -1)

  p2 <- 5
  game$play(p2)
  expect_true(all(game$state[-c(p1, p2)] == 0))
  expect_equal(game$state[p2], 2)
  expect_equal(game$nextmover, 1)
  expect_equal(game$prevmover, 2)
  expect_equal(nrow(game$history), 2)
  expect_equal(game$result, -1)

  game$undo()
  expect_equal(game$state[p2], 0)
  expect_equal(game$nextmover, 2)
  expect_equal(game$prevmover, 1)
  expect_equal(nrow(game$history), 1)
  expect_equal(game$result, -1)

  # play until win
  game$play(5)
  expect_equal(game$result, -1)
  game$play(1)
  expect_equal(game$result, -1)
  game$play(6)
  expect_equal(game$result, -1)
  game$play(3)
  expect_equal(game$result, 1)

  # undo
  game$undo()
  expect_equal(game$result, -1)

  game$play(9)
  expect_equal(game$result, -1)
  game$play(4)
  expect_equal(game$result, 2)

})


test_that("invalid ai", {
  expect_error(ttt(1, 2))
  expect_error(ttt(structure(1, class = "ttt_player"),
                   structure(2, class = "ttt_player")))
  expect_error(ttt(structure(list(1), class = "ttt_player"),
                   structure(list(2), class = "ttt_player")))
})



test_that("index conversion", {
  g <- ttt_game()
  ## index to index (identical)
  expect_equal(g$to_index(1), 1L)
  expect_equal(g$to_index(8), 8L)

  ## 2-length integer to index
  expect_equal(g$to_index(c(2, 2)), 5L)
  expect_equal(g$to_index(c(3, 1)), 3L)

  ## string to index
  expect_equal(g$to_index("b1"), 4L)
  expect_equal(g$to_index("c2"), 8L)


  ## to string
  expect_equal(g$index_to_str(1), "A1")
  expect_equal(g$index_to_str(3), "A3")
  expect_equal(g$index_to_str(6), "B3")
  expect_equal(g$index_to_str(c(1, 3)), "C1")
  expect_equal(g$index_to_str(c(2, 3)), "C2")
  expect_equal(g$index_to_str(c(3, 2)), "B3")

})

