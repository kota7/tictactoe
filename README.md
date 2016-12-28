
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/kota7/tictactoe.svg?branch=master)](https://travis-ci.org/kota7/tictactoe)

tictactoe
=========

Play and learn Tic-Tac-Toe Game on R

installation and import
-----------------------

To install,

``` r
devtools::install_github("kota7/tictactoe")
```

To use,

``` r
library(tictactoe)
```

Play a Game
-----------

You can play tic-tac-toe on R console.

``` r
ttt(ttt_human(), ttt_ai())
```

This would give you a prompt like:

        A B C
       ------
     1| . . .
     2| . . .
     3| . . .

     Player 1 (no name) to play
    choose move (e.g. A1) > 

Type the move, then the oppoenet will respond. To finish the game in the middle, type "exit".

The default AI player is very week (in fact, he plays randomly). To play against a more sophisticated player, set the `level` argument (from 0 (weekest) to 5 (strongest)).

``` r
ttt(ttt_human(), ttt_ai(level = 4))
```

You may play as the second mover by `ttt(ttt_ai(), ttt_human())`. You may watch games between AI players by `ttt(ttt_ai(), ttt_ai())`.

Simulation
----------

To conduct a large scale simulation between AI players, use `ttt_simulate` function. The code below conducts 100 simulation games between random AIs. The result 0, 1, and 2 indicate draw, won by player 1, and won by player 2 respectively.

``` r
res <- ttt_simulate(ttt_ai(), ttt_ai(), N = 100, verbose = FALSE)
prop.table(table(res))
#> res
#>    0    1    2 
#> 0.13 0.57 0.30
```

Q-learning
----------

Q-learning is implemented to train AI players. The code below trains a random AI through Q-learninig of 500 episodes.

``` r
p <- ttt_ai()
o <- ttt_qlearn(p, N = 500, verbose = FALSE)
```

Now this player is much stronger than the random player.

``` r
res <- ttt_simulate(ttt_ai(), p, N = 100, verbose = FALSE)
prop.table(table(res))
#> res
#>    0    1    2 
#> 0.15 0.25 0.60
```

References
----------

-   Sutton, Richard S and Barto, Andrew G. Reinforcement Learning: An Introduction. The MIT Press (1998)
