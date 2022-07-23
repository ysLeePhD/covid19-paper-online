
attach_significance <- function(coef, p) {
  for (i in 1:length(p)) {
    if (coef[i] > 0) {
      coef[i] <- paste0("  ", coef[i])
    } else {
      coef[i] <- paste0(" ", coef[i])
    }
    if (p[i] < 0.05 & p[i] >= 0.01) {
      coef[i] <- paste0("*", coef[i])
    }
    if (p[i] < 0.01) {
      coef[i] <- paste0("**", coef[i])
    }
  }
  return(coef)
}

do_lr_attitudinal <- function(target) {
  model_lr <- lm(
    paste0(
      target,
      " ~ ",
      "b01_yearborn +",
      "b05_gender +",
      "b06_age_driver_license +",
      "b07_educational_background +",
      "b08_hh_income +",
      "d03_neighborhood_type +",
      "source"
    ) %>%
      as.formula(),
    data = lr_data
  )
  vif(model_lr) %>% print()
  stepped <- step(
    model_lr, model_lr %>% formula(),
    direction = "backward"
  )
  coef <- attach_significance(
    stepped %>%
      summary() %>%
      coefficients() %>%
      .[, 1] %>%
      round(3) %>%
      format(nsmall = 3),
    stepped %>%
      summary() %>%
      coefficients() %>%
      .[, 4]
  ) %>%
    as.data.frame()
  write.csv(
    coef,
    file.path("..", "dist", "lr", paste0(target, ".csv")),
    quote = F
  )
}

do_lr_attitudinal("MR1")
do_lr_attitudinal("MR2")
do_lr_attitudinal("MR5")
do_lr_attitudinal("MR3")
do_lr_attitudinal("MR4")
do_lr_attitudinal("MR6")
