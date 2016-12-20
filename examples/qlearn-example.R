library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(tictactoe)

set.seed(123)

p <- ttt_ai()
o <- ttt_qlearn(p, N = 5000)

dat <- select(o, -n_sim) %>%
  spread(key = res, value = Freq, fill = 0) %>%
  melt(id.vars = "n_train", variable.name = "result_num", value.name = "frac")

dat$result <- ""
dat$result[dat$result_num == "0"] <- "Draw"
dat$result[dat$result_num == "1"] <- "Player1"
dat$result[dat$result_num == "2"] <- "Player2"
ggplot(dat, aes(n_train, frac, linetype = result, shape = result)) +
  geom_line(size = 0.8, color = "grey25") +
  geom_point(size = 2.2, color = "grey50") +
  geom_point(size = 2.0, color = "grey10") +
  xlab("number of training") + ylab("fraction") +
  theme_bw()
ggsave("examples/ttt-qlearn.pdf", width = 8, height = 4)
ggsave("examples/ttt-qlearn.png", width = 8, height = 4)
