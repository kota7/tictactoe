## make various level of AIs
set.seed(123)

depth <- c(500, 1000, 1500, 2000, 5000)
trained_value_funcs <- list()
trained_policy_funcs <- list()
for (i in seq_along(depth))
{
  p <- ttt_randbot()
  ttt_qlearn(p, N = depth[i], simulate = FALSE)
  res <- ttt_simulate(p)
  print(prop.table(table(res)))

  trained_value_funcs <- c(trained_value_funcs, list(p$value_func))
  trained_policy_funcs <- c(trained_policy_funcs, list(p$policy_func))
}

devtools::use_data(trained_value_funcs, trained_policy_funcs,
                   internal = TRUE, overwrite = TRUE)
