
source("src/.setup.R")
ls()

al_attitudinal <- "data_00" %>% get_attitudinal()
al_attitudinal %>% map_int(~is.na(.) %>% sum()) %>% sum() # no missing 
al_attitudinal %>% map(table)
al_attitudinal %>% map_df(as.numeric) %>% map(table)




al_attitudinal[1:27] 


n <- 6

## 03. factor analysis in R ---- 

## read data, define function, & scree plot ----

A1_04 <- read_rds(file.path(path_data_inter, "impute/SectionA_imputed_v02.rds"))
names(A1_04)

input <- A1_04[-1] %>% map_df(as.numeric)

input02 <- input 

fa.preset <- function(input = input02, x, y = "pa"){
  fa(r = input, nfactors = x, rotate = "oblimin", scores="tenBerge", max.iter = 2000, 
     SMC=TRUE, warnings=TRUE, fm= y) #, oblique.scores=TRUE)
}

output_fa <- map(1:10, ~fa.preset(x =.))

tibble(
  nfactor = 1:ncol(input), 
  evalue = output_fa[[10]]$e.values
) %>% 
  ggplot(aes(x = nfactor, y = evalue)) +
  geom_line() +
  geom_point() #+
# ggsave(filename = file.path(path_root, "55_data_viz_intermediate/A1_FA_scree_plot.png"), 
#        width = 9, height = 6, units = c("in"), dpi = 300)



## six factors from 17/31 statements ----

input02 <- input %>% 
  select(A1d_car_mustown, A1h_drive_like, A1q_car_tool, A1cc_car_status, # pro-car 
         A1o_nbhd_transit, A1z_nbhd_largehome, # pro-TOD
         A1y_wait_annoy, A1i_wait_useful, A1r_commute_useful, # waiting-hurts, but remove A1n_teleport_like, 
         A1v_exercise_imp, A1k_walk_like, A1dd_exercise_overrated, A1b_bike_like, # pro-exercise
         A1p_tech_frust, A1a_tech_1st,         # tech-clumsy
         A1bb_too_busy, A1ee_life_satisfied)   # life-tough

# KMO test suitability for EFA 
# https://search.r-project.org/CRAN/refmans/EFAtools/html/KMO.html 
# input %>% KMO(use = "complete.obs")
input02 %>% KMO(use = "complete.obs")

# Bartlett test suitability for EFA 
# https://search.r-project.org/CRAN/refmans/EFAtools/html/BARTLETT.html
# input %>% BARTLETT(use = "complete.obs")
input02 %>% BARTLETT(use = "complete.obs")

value_cutoff <- 0.2
fa.input02 <- fa.preset(input = input02, x = 6, y = "pa")
print(fa.input02$loadings, cutoff = value_cutoff, sort = TRUE)
# Loadings:
#                             PA1    PA2    PA5    PA3    PA4    PA6   
# A1d_car_mustown          0.801                                   
# A1h_drive_like           0.817                                   
# A1o_nbhd_transit                0.860                            
# A1z_nbhd_largehome             -0.622                            
# A1v_exercise_imp                       0.642                     
# A1i_wait_useful                               0.639              
# A1p_tech_frust                                       0.689       
# A1q_car_tool            -0.375                              0.214
# A1cc_car_status          0.388                       0.246       
# A1y_wait_annoy                               -0.440         0.284
# A1r_commute_useful                            0.422              
# A1k_walk_like                          0.473                     
# A1dd_exercise_overrated               -0.294                     
# A1b_bike_like                          0.276                     
# A1a_tech_1st             0.243                      -0.242       
# A1bb_too_busy                                               0.490
# A1ee_life_satisfied                                        -0.371
# 
#                 PA1   PA2   PA5   PA3   PA4   PA6
# SS loadings    1.7 1.206 0.892 0.838 0.706 0.617
# Proportion Var 0.1 0.071 0.052 0.049 0.042 0.036
# Cumulative Var 0.1 0.172 0.225 0.274 0.316 0.352

score.input02 <- 
  fa.input02$scores %>% 
  as_tibble() %>% 
  cbind(A1_04$ResponseId) %>% 
  rename(ResponseId = "A1_04$ResponseId", 
         pro_car = PA1, 
         pro_tod = PA2, 
         pro_exercise = PA5, 
         pro_wait = PA3, 
         tech_frust = PA4, 
         busy_unhappy = PA6) %>% 
  select(ResponseId, everything()) %>% 
  as_tibble()

score.input02[-1] %>% cor()
#              pro_car pro_tod pro_exercise pro_wait tech_frust busy_unhappy
# pro_car       1.0000  -0.041        0.120  -0.0026     -0.175        0.199
# pro_tod      -0.0407   1.000       -0.132  -0.0480      0.067        0.032
# pro_exercise  0.1197  -0.132        1.000   0.1471     -0.065       -0.016
# pro_wait     -0.0026  -0.048        0.147   1.0000      0.011       -0.100
# tech_frust   -0.1747   0.067       -0.065   0.0110      1.000        0.095
# busy_unhappy  0.1988   0.032       -0.016  -0.1002      0.095        1.000

fa.input02$STATISTIC 
fa.input02$dof 
fa.input02$PVAL






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


environment()
.GlobalEnv