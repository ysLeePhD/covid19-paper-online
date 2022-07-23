
source("src/.setup.R")
ls()

al_attitudinal <- "data_00" %>% get_attitudinal()
al_attitudinal %>% map_int(~is.na(.) %>% sum()) %>% sum() # no missing 
al_attitudinal %>% map(table)
al_attitudinal %>% map_df(as.numeric) %>% map(table)




al_attitudinal[1:27] 
n <- 6


get_factor("al_attitudinal", n)

assign(
  paste0("al_score_", n),
  get(paste0("al_attitudinal_factor_", n))$scores %>%
    as.data.frame() %>%
    tibble() %>%
    mutate(source = al %>%
      filter(if_all(starts_with("a"), ~ !is.na(.))) %>%
      filter(if_all(starts_with("a"), ~ . > 0)) %>%
      .$source),
  envir = globalenv()
)


environment()
.GlobalEnv