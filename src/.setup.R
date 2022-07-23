
# install, update, & load packages ---- 

# to increase speed, install packages from source (i.e., compile) instead of pre-compiled version
# remove.packages("tibble")
# install.packages("magrittr", type="source")
# install.packages("devtools", type="source", dep = TRUE)

pkgs <- c(
  "here", "tidyverse", "data.table", # "fst", 
  "readxl", "writexl", "haven", # "plyr", 
  "mvtnorm", # for missing imputation 
  "GPArotation", "psych", "EFAtools", # this order b/c dependencies 
  "PerformanceAnalytics", "exactRankTests", 
  "coin", "car", "sampling", "nnet", 
  "corrplot", "ggraph", "tidygraph", "stargazer", "tictoc" # "viridis", "ggridges", 
  )

for (i in seq_along(pkgs)){
  devtools::update_packages(pkgs[[i]])#, dep = TRUE) # update one at a time 
  }

pkgs2 <- c(pkgs, "foreign") # "ggpubr", 
inst = lapply(pkgs2, library, character.only = TRUE) # load them
options(stringsAsFactors = FALSE)
options(digits=2)# options(digits=22)


# define file paths here ---- 

path_data_raw <- file.path(str_remove(getwd(), "/covid19-paper-online"), "data_raw")
path_script <- file.path(getwd(), "src")


# run basic scripts  ---- 

# source(file.path(path_script, ".setup.R"))
source(file.path(path_script, "columns.R"))
source(file.path(path_script, "data_import.R"))


# define frequently-used functions here ---- 

get_attitudinal <- function(name) {
  data <- get(
    name,
    envir = globalenv()
  )
  return(
    data %>%
      select(
        attitudinal_tag_list_long[1:27], source
      ) %>%
      filter(if_all(starts_with("a"), ~ !is.na(.))) %>%
      filter(if_all(starts_with("a"), ~ . > 0)) %>%
      mutate_at(vars(
        "a1o_rightofway_cars",
        "a1p_prefer_being_driver",
        "a1q_no_alternative_to_driving",
        "a1r_like_idea_pt_as_transportation",
        "a1s_like_latest_technology"
      ), ~ {
        case_when(
          . == 1 ~ 1,
          . == 2 ~ 2,
          . == 6 ~ 3,
          . == 7 ~ 4,
          . == 8 ~ 5
        )
      })
  )
}

get_factor <- function(name, n) {
  data <- get(
    name,
    envir = globalenv()
  ) %>%
    select(all_of(attitudinal_tag_list_long))
  
  factor <- psych::fa(
    data,
    nfactors = n, scores = "Bartlett", rotate = "promax"
  )
  
  print(
    factor$Vaccounted %>%
      round(3) %>%
      .[1, ]
  )
  
  factor_score <- data.frame(
    psych::factor.scores(
      data, factor
    )$scores
  )
  
  ggplot(
    data.frame(
      col = 1:length(factor$e.values),
      value = factor$e.values,
      sum = cumsum(factor$e.values) * max(factor$e.values) / sum(factor$e.values)
    )
  ) +
    geom_point(aes(x = col, y = value)) +
    geom_line(aes(x = col, y = sum)) +
    scale_y_continuous(
      "Eigenvalue",
      sec.axis = sec_axis(~ . / max(factor$e.values), name = "Cumulative Share")
    ) +
    scale_x_continuous(
      "Factor No."
    ) +
    theme_bw()
  
  ggsave(
    paste0("../dist/scree_", name, "_", n, ".png"),
    width = 10, height = 5, unit = "in"
  )
  
  colnames(factor_score) <- paste0(
    "FA", 1:n, "A"
  )
  write.csv(
    factor$loadings[],
    paste0("../dist/factor_", name, "_", n, ".csv")
  )
  
  assign(
    paste(
      name, "factor", n,
      sep = "_"
    ),
    factor,
    envir = globalenv()
  )
}

