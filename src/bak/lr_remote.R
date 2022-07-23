do_lr_remote <- function(target) {
  column <- paste0("work_status_", target)
  lr_data_temp <- lr_data %>%
    filter(!!sym(column) != "Non-worker") %>%
    mutate(
      !!sym(column) := factor(
        !!sym(column),
        levels = c(
          "Commuter",
          "Hybrid-worker",
          "Teleworker"
        )
      )
    )

  assign(
    "lr_data_temp",
    lr_data_temp,
    envir = globalenv()
  )

  model_lr <-
    multinom(
      paste0(
        column,
        " ~ ",
        "b01_yearborn +",
        "b05_gender +",
        "b06_age_driver_license +",
        "b07_educational_background +",
        "b08_hh_income +",
        "source"
      ) %>%
        as.formula(),
      lr_data_temp,
      Hess = T,
      trace = 0
    )

  assign(
    "model_lr",
    model_lr,
    envir = globalenv()
  )

  stepped <- step(
    get("model_lr", envir = globalenv()),
    direction = "backward"
  )
  z <- summary(stepped)$coefficients /
    summary(stepped)$standard.errors
  p <- ((1 - pnorm(abs(z), 0, 1)) * 2) %>%
    as.data.frame() %>%
    round(3) %>%
    format(nsmall = 3) %>%
    t()
  coef <- stepped %>%
    coef() %>%
    as.data.frame() %>%
    round(3) %>%
    format(nsmall = 3) %>%
    t()
  for (i in 1:dim(p)[1]) {
    if (coef[i, 1] > 0) {
      coef[i, 1] <- paste0("  ", coef[i, 1])
    } else {
      coef[i, 1] <- paste0(" ", coef[i, 1])
    }
    if (p[i, 1] < 0.05 & p[i, 1] >= 0.01) {
      coef[i, 1] <- paste0("*", coef[i, 1])
    }
    if (p[i, 1] < 0.01) {
      coef[i, 1] <- paste0("**", coef[i, 1])
    }
    if (coef[i, 2] > 0) {
      coef[i, 2] <- paste0("  ", coef[i, 2])
    } else {
      coef[i, 2] <- paste0(" ", coef[i, 2])
    }
    if (p[i, 2] < 0.05 & p[i, 2] >= 0.01) {
      coef[i, 2] <- paste0("*", coef[i, 2])
    }
    if (p[i, 2] < 0.01) {
      coef[i, 2] <- paste0("**", coef[i, 2])
    }
  }
  write.csv(
    coef,
    file.path("..", "dist", "lr", paste0(target, ".csv")),
    quote = F
  )
}

do_lr_remote("precovid")
do_lr_remote("2021")
do_lr_remote("2022")
