
## set up the plate ---- 
# ls()
source("src/.setup.R")

al_attitudinal <- "data_00" %>% get_attitudinal()
# al_attitudinal %>% map_int(~is.na(.) %>% sum()) %>% sum() # no missing 
# al_attitudinal %>% map(table)
# al_attitudinal %>% map_df(as.numeric) %>% map(table)



## factor analysis in R ---- 
## read data, define function, & scree plot ----

input_02 <- 
  al_attitudinal[1:27] %>% 
  select(-starts_with(c("a1ac_", "a1h_", "a1i_", "a1k_", "a1m_", "a1n_", 
                        "a1o_", "a1r_", "a1t_", "a1u_", "a1v_", "a1y_"))) %>% #
  map_df(as.numeric)


fa.preset <- function(input = input_02, x, y = "pa"){
  fa(r = input, nfactors = x, rotate = "oblimin", scores="tenBerge", max.iter = 2000, 
     SMC=TRUE, warnings=TRUE, fm= y) #, oblique.scores=TRUE)
  }
remove(output_fa)
output_fa <- map(1:10, ~fa.preset(x =., y = "ml"))

tibble(
  nfactor = 1:ncol(input_02), 
  evalue = output_fa[[10]]$e.values
  ) %>% 
  ggplot(aes(x = nfactor, y = evalue)) +
  geom_line() +
  geom_point()



## ... factors from 6/15 statements ----

# KMO test suitability for EFA 
# https://search.r-project.org/CRAN/refmans/EFAtools/html/KMO.html 
# input %>% KMO(use = "complete.obs")
input_02 %>% KMO(use = "complete.obs")

# Bartlett test suitability for EFA 
# https://search.r-project.org/CRAN/refmans/EFAtools/html/BARTLETT.html
# input %>% BARTLETT(use = "complete.obs")
input_02 %>% BARTLETT(use = "complete.obs")


value_cutoff <- 0.3
fa.input_02 <- fa.preset(input = input_02, x =6, y = "pa")
print(fa.input_02$loadings, cutoff = value_cutoff, sort = TRUE)

score.input_02 <- 
  fa.input_02$scores %>% 
  as_tibble() %>% 
  rename(pro_env = PA1, 
         pro_car = PA2, 
         pro_tech = PA4, 
         pro_bike = PA6, 
         pro_walk = PA3, 
         car_dep = PA5) %>% 
  as_tibble()

score.input_02 %>% cor()
#           pro_env pro_car pro_tech pro_bike pro_walk car_dep
# pro_env     1.00  -0.250    0.260    0.361   0.1908 -0.1701
# pro_car    -0.25   1.000    0.237    0.030   0.0507  0.3429
# pro_tech    0.26   0.237    1.000    0.356   0.1992  0.0379
# pro_bike    0.36   0.030    0.356    1.000   0.4281 -0.0527
# pro_walk    0.19   0.051    0.199    0.428   1.0000  0.0026
# car_dep    -0.17   0.343    0.038   -0.053   0.0026  1.0000

fa.input_02$STATISTIC 
fa.input_02$dof 
fa.input_02$PVAL


input_03 <- 
  al_attitudinal[1:27] %>% 
  select(starts_with(c("a1ac_", "a1h_", "a1i_", "a1k_", "a1m_", "a1n_", 
                       "a1o_", "a1r_", "a1t_", "a1u_", "a1v_", "a1y_"))) %>% #
  map_df(as.numeric)


input_03 %>% names()



# from Keita ----

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
