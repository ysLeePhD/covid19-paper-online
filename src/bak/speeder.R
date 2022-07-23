get_time_list <- function(name) {
  data <- get(
    name,
    envir = globalenv()
  ) %>%
    select(
      contains("Page_Submit"),
      contains("Click_Count")
    ) %>%
    mutate(id = seq_len(dim(.)[1])) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    rowwise() %>%
    mutate(
      section_a_time = Timer_07_Page_Submit +
        Timer_08_Page_Submit +
        Timer_09_Page_Submit,
      section_b_time = Timer_04_Page_Submit +
        Timer_05_Page_Submit +
        Timer_06_Page_Submit,
      section_c_time = Timer_11_Page_Submit +
        Timer_12_Page_Submit +
        Timer_13_Page_Submit +
        Timer_14_Page_Submit +
        Timer_15_Page_Submit +
        Timer_16_Page_Submit +
        Timer_17_Page_Submit +
        Timer_18_Page_Submit,
      section_d_time = Timer_19_Page_Submit +
        Timer_20_Page_Submit,
      section_e_time = Timer_25_Page_Submit +
        Timer_26_Page_Submit +
        Timer_27_Page_Submit +
        Timer_28_Page_Submit +
        Timer_29_Page_Submit +
        Timer_30_Page_Submit +
        Timer_31_Page_Submit +
        Timer_32_Page_Submit +
        Timer_33_Page_Submit,
      section_f_time = Timer_22_Page_Submit,
      section_g_time = Timer_34_Page_Submit +
        Timer_35_Page_Submit +
        Timer_36_Page_Submit +
        Timer_37_Page_Submit +
        Timer_38_Page_Submit,
      section_a_click = Timer_07_Click_Count +
        Timer_08_Click_Count +
        Timer_09_Click_Count,
      section_b_click = Timer_04_Click_Count +
        Timer_05_Click_Count +
        Timer_06_Click_Count,
      section_c_click = Timer_11_Click_Count +
        Timer_12_Click_Count +
        Timer_13_Click_Count +
        Timer_14_Click_Count +
        Timer_15_Click_Count +
        Timer_16_Click_Count +
        Timer_17_Click_Count +
        Timer_18_Click_Count,
      section_d_click = Timer_19_Click_Count +
        Timer_20_Click_Count,
      section_e_click = Timer_25_Click_Count +
        Timer_26_Click_Count +
        Timer_27_Click_Count +
        Timer_28_Click_Count +
        Timer_29_Click_Count +
        Timer_30_Click_Count +
        Timer_31_Click_Count +
        Timer_32_Click_Count +
        Timer_33_Click_Count,
      section_f_click = Timer_22_Click_Count,
      section_g_click = Timer_34_Click_Count +
        Timer_35_Click_Count +
        Timer_36_Click_Count +
        Timer_37_Click_Count +
        Timer_38_Click_Count,
    )

  return(
    data %>%
      ungroup() %>%
      select(
        id,
        section_a_time,
        section_b_time,
        section_c_time,
        section_d_time,
        section_e_time,
        section_f_time,
        section_g_time,
        section_a_click,
        section_b_click,
        section_c_click,
        section_d_click,
        section_e_click,
        section_f_click,
        section_g_click
      ) %>%
      gather("time_key", "time_value", ends_with("time")) %>%
      gather("click_key", "click_value", ends_with("click")) %>%
      filter(time_key %>% substr(1, 9) == click_key %>% substr(1, 9)) %>%
      mutate(source = name)
  )
}

do_speeder <- function() {
  altogether <- rbind(
    get_time_list("lg"),
    get_time_list("op"),
    get_time_list("cs"),
    get_time_list("mo")
  )

  altogether %>%
    mutate(source = as_factor(source)) %>%
    select(id, time_key, time_value, source) %>%
    rbind(
      altogether %>%
        group_by(id, source) %>%
        summarize(time_value = time_value) %>%
        mutate(time_key = "entire_survey")
    ) %>%
    ggplot() +
    geom_boxplot(
      aes(
        x = time_key,
        y = time_value,
        fill = source
      )
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 7.5, hjust = 1),
      plot.title = element_text(size = 10)
    ) +
    scale_y_continuous(trans = "log10")


  rbind(
    get_time_list("lg"),
    get_time_list("op"),
    get_time_list("cs"),
    get_time_list("mo")
  ) %>%
    mutate(source = as_factor(source)) %>%
    ggplot() +
    geom_freqpoly(
      aes(
        x = log10(time_value),
        y = stat(density),
        color = source
      ),
      binwidth = 0.1
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 7.5, hjust = 1),
      plot.title = element_text(size = 10)
    ) +
    facet_wrap(vars(time_key))

  rbind(
    get_time_list("lg"),
    get_time_list("op"),
    get_time_list("cs"),
    get_time_list("mo")
  ) %>%
    group_by(id, source) %>%
    summarize(sum = time_value) %>%
    mutate(source = as_factor(source)) %>%
    ggplot() +
    geom_boxplot(
      aes(
        y = sum,
        fill = source
      )
    ) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 7.5, hjust = 1),
      plot.title = element_text(size = 10)
    ) +
    scale_y_continuous(trans = "log10")
}

do_trap()

plot_data <- get_time_list("lg")

rbind(
  get_time_list("lg"),
  get_time_list("op"),
  get_time_list("cs"),
  get_time_list("mo")
) %>%
  mutate(source = as_factor(source)) %>%
  ggplot() +
  geom_point(
    aes(
      x = click_value,
      y = time_value,
      color = source,
      fill = source
    ),
    alpha = 0.2
  ) +
  facet_wrap(vars(time_key)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 7.5, hjust = 1),
    plot.title = element_text(size = 10)
  ) +
  xlim(c(0, 300)) +
  ylim(c(0, 7200))

rbind(
  get_time_list("lg"),
  get_time_list("op"),
  get_time_list("cs"),
  get_time_list("mo")
) %>%
  filter(time_key == "section_b_time") %>%
  select(click_value) %>%
  ggplot() +
  geom_histogram(
    aes(
      x = click_value
    ),
    binwidth = 1
  )
