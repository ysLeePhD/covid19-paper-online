for (i in 1:n) {
  pairwise.wilcox.test(
    get(paste0("al_score_", n))[, i] %>%
      deframe(),
    get(paste0("al_score_", n))[, n + 1] %>%
      deframe(),
    alternative = "l"
  ) %>%
    print()
  pairwise.wilcox.test(
    get(paste0("al_score_", n))[, i] %>%
      deframe(),
    get(paste0("al_score_", n))[, n + 1] %>%
      deframe(),
    alternative = "g"
  ) %>%
    print()
  print("--------------------------------------------")
}
