
temp_00 <- 
  read_sav(file.path(path_data_raw, "mm.sav")) %>% 
  select(all_of(tag_list)) %>%
  rename_all(~tag_list_long) %>%
  mutate(source = "mm")

temp_01 <- 
  read_sav(file.path(path_data_raw, "altogether_cleaned.sav")) %>%
  select(all_of(tag_list_long), meta_distributionchannel) %>%
  rename(source = meta_distributionchannel)

data_00 <-
  rbind(
    filter(temp_01, source != "mail" & source != "-88888"),
    temp_00
    ) %>%
  mutate(
    source = case_when(
      source == "convenience" ~ "cs",
      source == "longitudinal" ~ "lg",
      source == "mailed_online" ~ "mo",
      source == "opinion_panel" ~ "op",
      T ~ "mm"
      )
    ) %>%
  filter(source != "cs") %>%
  mutate(
    source = source %>%
      factor(levels = c("lg","op","mm","mo")
             )
    )

