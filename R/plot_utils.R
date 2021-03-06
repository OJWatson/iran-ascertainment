#' @noRd
# save figure
save_figs <- function(name,
                      fig,
                      width = 6,
                      height = 6,
                      root = file.path(here::here(), "analysis/plots")) {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  grDevices::pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  grDevices::dev.off()

}

#' Create a final death per age dataset
#' @param res squire model run
#' @importFrom dplyr summarise
final_deaths <- function(res) {

  date_0 <- max(res$pmcmc_results$inputs$data$date)
  D <- na.omit(nim_sq_format(res, "D", reduce_age = FALSE, date_0 = date_0))

  group_by(D[D$t == max(D$t, na.rm = TRUE), ], age_group) %>%
    summarise(across(y, .fns = list(
      min =~ quantile(y, 0.025, na.rm = TRUE),
      med =~ quantile(y, 0.5, na.rm = TRUE),
      max =~ quantile(y, 0.975, na.rm = TRUE)
    ),.names = "{.fn}")) %>%
    mutate(across(min:max, ~ .x * (100000/res$parameters$population), .names = "cap_{.col}")) %>%
    mutate(province = res$parameters$province)

}


#' Create a final death per age dataset
#' @param res squire model run
#' @param df Excess deaths per age per province
#'
final_death_comp_df <- function(res, df) {

  df %>% filter(province_name == res$parameters$province) %>%
    group_by(age_group) %>%
    summarise(d = sum((excess_deaths_mean)),
              d_pos = sum(pos(excess_deaths_mean)),
              d_max = sum((excess_deaths_low)),
              d_pos_max = sum(pos(excess_deaths_low)),
              d_min = sum((excess_deaths_high)),
              d_pos_min = sum(pos(excess_deaths_high))) %>%
    cbind(final_deaths(res)[,-1]) %>%
    cbind(data.frame("n" = res$parameters$population)) %>%
    mutate(dcap_med = pos((d*(100000/n))),
           dcappos_med = (d_pos*(100000/n)),
           dcap_min = pos((d_min*(100000/n))),
           dcappos_min = (d_pos_min*(100000/n)),
           dcap_max = pos((d_max*(100000/n))),
           dcappos_max = (d_pos_max*(100000/n))) %>%
    tidyr::pivot_longer(cols = c("cap_med", "cap_min", "cap_max",
                                 "dcap_med", "dcap_min", "dcap_max",
                                 "dcappos_med", "dcappos_min", "dcappos_max"),
                        names_pattern = "(.*)_(.*)", names_to = c("source", "stat")) %>%
    mutate(province = res$parameters$province) %>%
    tidyr::pivot_wider(id_cols = c("age_group", "province", "source"), names_from = "stat", values_from = "value")

}

#' Create a plot comparing final deaths per province per age from model to observed
#' @param res squire model run
#' @param df Excess deaths per age per province
#'
#' @importFrom dplyr group_by mutate select left_join ungroup
final_death_comp_province <- function(res, df) {

  final_death_comp_df(res, df) %>%
    filter(source != "dcap") %>%
    ggplot(aes(age_group, med, ymin = min, ymax = max, color = source, fill = source, group = source)) +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    scale_y_log10() +
    scale_color_discrete(
      name = "Source",
      labels = c("dcap"="Observed Excess Deaths",
                 "dcappos"="Observed Positive Excess Deaths",
                 "cap"="Modelled Deaths")) +
    scale_fill_discrete(
      name = "Source",
      labels = c("dcap"="Observed Excess Deaths",
                 "dcappos"="Observed Positive Excess Deaths",
                 "cap"="Modelled Deaths")) +
    ylab("Final Epidemic Deaths / 100000") +
    xlab("Age Group") +
    theme_bw()

}

#' @noRd
infs_over_time <- function(res) {

  S_tot <- sum(res$pmcmc_results$inputs$model_params$population)
  date_0 <- max(res$pmcmc_results$inputs$data$date)
  inf <- nim_sq_format(res, "infections", date_0 = date_0) %>%
    mutate(infections = as.integer(y)) %>%
    select(replicate, t, date, infections) %>%
    group_by(replicate) %>%
    mutate(infections = lag(cumsum(replace_na(infections, 0)), 1, default = 0)) %>%
    mutate(province = res$parameters$province)

  return(inf)

}


#' @noRd
infs_over_time_age <- function(res) {

  S_tot <- sum(res$pmcmc_results$inputs$model_params$population)
  date_0 <- max(res$pmcmc_results$inputs$data$date)
  inf <- nim_sq_format(res, "infections", date_0 = date_0, reduce_age = FALSE) %>%
    mutate(infections = as.integer(y)) %>%
    select(replicate, t, date, age_group, infections) %>%
    group_by(replicate, age_group) %>%
    mutate(infections = lag(cumsum(replace_na(infections, 0)), 1, default = 0)) %>%
    mutate(province = res$parameters$province)

  return(inf)

}
