library(tidyverse)

# Group our model fits by their assumptions
reports_all <- function(task = "prov_fit") {

  wd <- here::here()
  wdold <- getwd()
  setwd(wd)
  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  # get parameters
  pars <- DBI::dbGetQuery(db, 'select * from parameters')
  reports <- DBI::dbGetQuery(db, 'select * from report_version')
  reports <- reports %>% filter(report == task)
  pars <- pars %>% filter(report_version %in% reports$id)
  pars <- pars %>% select(-c("type","id")) %>%
    pivot_wider(names_from = "name", values_from = "value")

  setwd(wdold)
  return(pars)
}

# Create the list of run model fit reports
reports_here <- reports_all()
reports_here$scenario <- "Central"
reports_here$scenario[reports_here$dur_R == "222"] <- "Optimistic"
reports_here$scenario[reports_here$dur_R == "70"] <- "Worst"

# Get the model fits for each scenario
ls <- list.files("archive", full.names = TRUE, recursive = TRUE)

central_rds <- vapply(
  reports_here$report_version[reports_here$scenario == "Central"],
  grep, character(1), grep("res.rds", ls, value = TRUE), value = TRUE
)
worst_rds <- vapply(
  reports_here$report_version[reports_here$scenario == "Worst"],
  grep, character(1), grep("res.rds", ls, value = TRUE), value = TRUE
)
optimistic_rds <- vapply(
  reports_here$report_version[reports_here$scenario == "Optimistic"],
  grep, character(1), grep("res.rds", ls, value = TRUE), value = TRUE
)

provs_central <- parallel::mclapply(central_rds, function(x) {
  x <- readRDS(x)
  generate_draws(
    x,
    pars_list = lapply(generate_parameters(x, draws = 10, ll = FALSE, burnin = 10000),
                       function(x) {x[-1] <- x[-1]*runif(length(x)-1, 0.985, 1.005); x}
    ),
    parallel = FALSE)
}, mc.cores = 16, mc.cleanup = TRUE)

provs_optimistic <- parallel::mclapply(optimistic_rds, function(x) {
  x <- readRDS(x)
  generate_draws(
    x,
    pars_list = lapply(generate_parameters(x, draws = 10, ll = FALSE, burnin = 10000),
                       function(x) {x[-1] <- x[-1]*runif(length(x)-1, 0.985, 1.005); x}
    ),
    parallel = FALSE)
}, mc.cores = 16, mc.cleanup = TRUE)

provs_worst <- parallel::mclapply(worst_rds, function(x) {
  x <- readRDS(x)
  generate_draws(
    x,
    pars_list = lapply(generate_parameters(x, draws = 10, ll = FALSE, burnin = 10000),
                       function(x) {x[-1] <- x[-1]*runif(length(x)-1, 0.985, 1.005); x}
    ),
    parallel = FALSE)
}, mc.cores = 16)

names(provs_central) <- vapply(provs_central, function(x){x$parameters$province}, character(1))
names(provs_optimistic) <- vapply(provs_optimistic, function(x){x$parameters$province}, character(1))
names(provs_worst) <- vapply(provs_worst, function(x){x$parameters$province}, character(1))

# uncomment these if memory being an issue

# saveRDS(provs_central, "analysis/data/derived/model_fits_central.rds")
# saveRDS(provs_worst, "analysis/data/derived/model_fits_worst.rds")
# saveRDS(provs_optimistic, "analysis/data/derived/model_fits_optimistic.rds")


# read in data

# provs_central <- readRDS("analysis/data/derived/model_fits_central.rds")
# provs_worst <- readRDS("analysis/data/derived/model_fits_worst.rds")
# provs_optimistic <- readRDS("analysis/data/derived/model_fits_optimistic.rds")

# ------------------------------------------------------------------------------
# final death plot nationally
# ------------------------------------------------------------------------------
df <- readRDS(cp_path("analysis/data/derived/iran_deaths_age_province.rds"))
pos <- function(x){ x[x<0] <- 0; x}

demog <- readRDS(cp_path("analysis/data/derived/demog.rds"))
pop_n <- demog %>% mutate(age_group = factor(age_group, levels = age_group[1:17])) %>%
  group_by(age_group) %>% summarise(n = sum(n)) %>% pull(n)

death_comps_central <- pbapply::pblapply(provs_central, final_deaths)

final_death_national_gg <- left_join(
  do.call(rbind, death_comps_central) %>% group_by(age_group) %>%
    summarise(across(min:max, sum, .names = "model_{.col}")) %>%
    mutate(age_group = squire::population$age_group[1:17]),
  df %>% filter(province_name == "Iran") %>%
    group_by(age_group) %>%
    summarise(d_med = sum((excess_deaths_mean)),
              d_min = sum(excess_deaths_high),
              d_max = sum(excess_deaths_low),
              dpos_med = sum(pos(excess_deaths_mean)),
              dpos_max = sum(pos(excess_deaths_low)),
              dpos_min = sum(pos(excess_deaths_high)))) %>%
  pivot_longer(cols = model_min:dpos_min, names_pattern = "(.*)_(.*)", names_to = c("source", "stat")) %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  # mutate(min = replace(min, name %in% c("d", "d_pos"), NA)) %>%
  # mutate(max = replace(max, name %in% c("d", "d_pos"), NA)) %>%
  mutate(across(.cols = c("min", "max", "med"),  ~ .x * (100000/as.numeric(mapply(rep, pop_n, 3))))) %>%
  mutate(min = pos(min), max = pos(max), med = pos(med)) %>%
  filter(source %in% c("model", "dpos")) %>%
  ggplot(aes(age_group, med, color = source, fill = source, ymin = min, ymax = max, group = source)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(aes(group = source)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()) +
  scale_y_log10() +
  scale_color_discrete(
    name = "Source:",
    labels = c("d"="Observed Excess Deaths",
               "dpos"="Observed Positive Excess Deaths",
               "model"="Modelled Deaths")) +
  scale_fill_discrete(
    name = "Source:",
    labels = c("d"="Observed Excess Deaths",
               "dpos"="Observed Positive Excess Deaths",
               "model"="Modelled Deaths")) +
  ylab("Final Epidemic Deaths / 100000") +
  xlab("Age Group")
save_figs("final_death_national", final_death_national_gg, width = 10, height = 8)

# ------------------------------------------------------------------------------
# PFR plot subnationally COME BACK TO
# ------------------------------------------------------------------------------
df <- readRDS(cp_path("analysis/data/derived/iran_deaths_age_province.rds"))
demog <- readRDS(cp_path("analysis/data/derived/demog.rds"))
final_death_national_gg <- left_join(
  do.call(rbind, death_comps) %>%
    mutate(age_group = rep(squire::population$age_group[1:17], 31)),
  df %>% filter(province_name != "Iran") %>%
    group_by(age_group, province_name) %>%
    summarise(d = sum((excess_deaths_mean)),
              d_min = sum((excess_deaths_high)),
              d_max = sum((excess_deaths_low)),
              dpos = sum(pos(excess_deaths_mean)),
              dpos_min = sum(pos(excess_deaths_high)),
              dpos_max = sum(pos(excess_deaths_low))) %>%
    mutate(province = province_name)
) %>% pivot_longer(cols = c("med", "d", "d_min", "d_max" ,"dpos", "dpos_min", "dpos_max")) %>%
  mutate(min = replace(min, name %in% c("d", "d_pos"), NA)) %>%
  mutate(max = replace(max, name %in% c("d", "d_pos"), NA)) %>%
  mutate(across(.cols = c("min", "max", "value"),  ~ .x * (100000/as.numeric(mapply(rep, squire::get_population("Iran")$n, 3))))) %>%
  ggplot(aes(age_group, value, color = name, ymin = min, ymax = max, group = name)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(aes(group = name)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.key = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10() +
  scale_color_discrete(
    name = "Source:",
    labels = c("d"="Observed Excess Deaths",
               "d_pos"="Observed Positive Excess Deaths",
               "med"="Modelled Deaths")) +
  ylab("Final Epidemic Deaths / 100000") +
  xlab("Age Group")
save_figs("final_death_national", final_death_national_gg, width = 8, height = 8)

# ------------------------------------------------------------------------------
# attack rate nationally plot
# ------------------------------------------------------------------------------
inf_comp_central <-  pbapply::pblapply(provs_central, infs_over_time)
inf_comp_worst <-  pbapply::pblapply(provs_worst, infs_over_time)
inf_comp_optimistic <-  pbapply::pblapply(provs_optimistic, infs_over_time)
demog <- readRDS(cp_path("analysis/data/derived/demog.rds"))
pop_n <- demog %>% mutate(age_group = factor(age_group, levels = age_group[1:17])) %>%
  group_by(age_group) %>% summarise(n = sum(n)) %>% pull(n)

inf_comp <- left_join(
  do.call(rbind, inf_comp_central) %>%
    group_by(date, replicate) %>%
    summarise(infs = sum(infections, na.rm = TRUE)) %>%
    group_by(date) %>%
    summarise(med = median(infs)),
  do.call(rbind, inf_comp_worst) %>%
    group_by(date, replicate) %>%
    summarise(infs = sum(infections, na.rm = TRUE)) %>%
    group_by(date) %>%
    summarise(min = median(infs))) %>%
  left_join(do.call(rbind, inf_comp_optimistic) %>%
              group_by(date, replicate) %>%
              summarise(infs = sum(infections, na.rm = TRUE)) %>%
              group_by(date) %>%
              summarise(max = median(infs)))  %>%
  mutate(across(med:max, ~.x/sum(pop_n)))

national_ar_gg <- inf_comp %>%
  ggplot(aes(date, med, ymin = min, ymax = max)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ylab("Cumulative Attack Rate") +
  xlab("") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_date(date_labels = "%b %Y",
               breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))
save_figs("national_ar", national_ar_gg, width = 6, height = 4)


# ------------------------------------------------------------------------------
# hospital comparison plot
# ------------------------------------------------------------------------------
hosps <- readRDS("analysis/data/derived/hospitalisations.rds")
prov <- "Kohgiluyeh and Boyer-Ahmad"
hosps <- hosps %>% mutate(province = replace(province, which(province == "Kohgiluyeh and Boyer Ahmad"), "Kohgiluyeh and Boyer-Ahmad"))
hosps <- hosps %>% mutate(province = replace(province, which(province == "Yaz"), "Yazd"))

hosp_all_comp_plot <- function(provs_central, provs_worst, provs_optimistic) {

  date_0 <- max(provs_central[[1]]$pmcmc_results$inputs$data$date)

  # H for each range
  H <- lapply(provs_central, nim_sq_format, c("hospitalisations"), date_0 = date_0)
  for(i in seq_along(H)) {
    H[[i]]$province <- names(provs_central)[i]
    H[[i]]$source <- "med"
  }
  H_low <- lapply(provs_optimistic, nim_sq_format, c("hospitalisations"), date_0 = date_0)
  for(i in seq_along(H_low)) {
    H_low[[i]]$province <- names(provs_optimistic)[i]
    H_low[[i]]$source <- "min"
  }
  H_high <- lapply(provs_worst, nim_sq_format, c("hospitalisations"), date_0 = date_0)
  for(i in seq_along(H_high)) {
    H_high[[i]]$province <- names(provs_worst)[i]
    H_high[[i]]$source <- "max"
  }

  H_dat <- do.call(rbind, H) %>%
    group_by(replicate, t, date, province, compartment, source) %>%
    summarise(y = sum(y, na.rm=TRUE)) %>%
    group_by(province, compartment, date, source) %>%
    summarise(across(y, .fns = list(
      med =~ quantile(y, 0.5, na.rm = TRUE)
    ),.names = "{.fn}"))

  H_dat_low <- do.call(rbind, H_low) %>%
    group_by(replicate, t, date, province, compartment, source) %>%
    summarise(y = sum(y, na.rm=TRUE)) %>%
    group_by(province, compartment, date, source) %>%
    summarise(across(y, .fns = list(
      max =~ quantile(y, 0.5, na.rm = TRUE)
    ),.names = "{.fn}"))

  H_dat_high <- do.call(rbind, H_high) %>%
    group_by(replicate, t, date, province, compartment, source) %>%
    summarise(y = sum(y, na.rm=TRUE)) %>%
    group_by(province, compartment, date, source) %>%
    summarise(across(y, .fns = list(
      min =~ quantile(y, 0.5, na.rm = TRUE)
    ),.names = "{.fn}"))

  H_dat2 <- left_join(ungroup(H_dat) %>% select(-source), ungroup(H_dat_high)%>% select(-source),
                      by = c("province", "compartment", "date")) %>%
    left_join(ungroup(H_dat_low)%>% select(-source), by = c("province", "compartment", "date")) %>%
    rowwise() %>%
    mutate(max2 = max(max, min), min2 = min(max, min)) %>% select(-min, -max) %>% rename(max = max2, min = min2)

  ggplot(hosps %>%
           filter(province != "Iran") %>%
           mutate(type = replace(type, which(type == "suspected"), "c_suspected")),
         aes(date, hosp, ymin = hosp, ymax = hosp, color = type, fill = type)) +
    geom_line() +
    geom_ribbon(alpha = 0.2) +
    geom_line(aes(date, med, color = compartment),
              H_dat2[H_dat2$province != "Iran",], inherit.aes = FALSE) +
    geom_ribbon(aes(date, med, ymin = min, ymax = max, color = compartment, fill = compartment),
              H_dat2[H_dat2$province != "Iran",], alpha = 0.2, inherit.aes = FALSE) +
    theme(axis.title = element_blank()) +
    facet_wrap(~province, scales = "free_y", ncol = 4) +
    scale_color_discrete(
      name = "Source:",
      labels = c("confirmed"="Observed Confirmed",
                 "c_suspected"="Observed Suspected",
                 "hospitalisations"="Modelled Daily Hospitalisations")) +
    scale_fill_discrete(
      name = "Source:",
      labels = c("confirmed"="Observed Confirmed",
                 "c_suspected"="Observed Suspected",
                 "hospitalisations"="Modelled Daily Hospitalisations")) +
    ylab("Hospital Data") +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank()) +
    scale_x_date(date_labels = "%b %Y",
                 breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))


}
hosp_all_gg <- hosp_all_comp_plot(provs_central, provs_worst, provs_optimistic)
save_figs("hosp_all", hosp_all_gg, width = 12, height = 16)


# ------------------------------------------------------------------------------
# attack rate per age
# ------------------------------------------------------------------------------

inf_comp_age_central <-  pbapply::pblapply(provs_central, infs_over_time_age)
inf_comp_age_worst <-  pbapply::pblapply(provs_worst, infs_over_time_age)
inf_comp_age_optimistic <-  pbapply::pblapply(provs_optimistic, infs_over_time_age)

demog <- readRDS(cp_path("analysis/data/derived/demog.rds"))
pop_n <- demog %>% mutate(age_group = factor(age_group, levels = age_group[1:17])) %>%
  group_by(age_group) %>% summarise(n = sum(n)) %>% pull(n)

S_tot <- data.frame("pop" = pop_n,
                    "age_group" = levels(inf_comp_age_central[[1]]$age_group))

inf_comp_age <- left_join(
  do.call(rbind, inf_comp_age_central) %>%
    group_by(date, replicate, age_group) %>%
    summarise(infs = sum(infections, na.rm = TRUE)) %>%
    group_by(date, age_group) %>%
    summarise(med = median(infs)),
  do.call(rbind, inf_comp_age_worst) %>%
    group_by(date, replicate, age_group) %>%
    summarise(infs = sum(infections, na.rm = TRUE)) %>%
    group_by(date, age_group) %>%
    summarise(min = median(infs))) %>%
  left_join(do.call(rbind, inf_comp_age_optimistic) %>%
              group_by(date, replicate, age_group) %>%
              summarise(infs = sum(infections, na.rm = TRUE)) %>%
              group_by(date, age_group) %>%
              summarise(max = median(infs)))

national_ar_age_gg <- inf_comp_age  %>%
  mutate(age_group = factor(age_group, levels = S_tot$age_group)) %>%
  left_join(S_tot) %>%
  mutate(across(med:max, ~.x/pop))  %>%
  filter(date == max(inf_comp_age$date)) %>%
  ggplot(aes(age_group, med, ymin = min, ymax = max)) +
  geom_point(size = 2) +
  geom_errorbar(size = 1, width = 0.4) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()) +
  ylab("National Attack Rate") +
  xlab("Age Group") +
  scale_y_continuous(labels = scales::percent)


# ------------------------------------------------------------------------------
# attack rate per wave table
# ------------------------------------------------------------------------------

inf_comp_province <- left_join(
  do.call(rbind, inf_comp_central) %>%
    group_by(date, replicate, province) %>%
    summarise(infs = sum(infections, na.rm = TRUE)) %>%
    group_by(date, province) %>%
    summarise(med = median(infs)),
  do.call(rbind, inf_comp_worst) %>%
    group_by(date, replicate, province) %>%
    summarise(infs = sum(infections, na.rm = TRUE)) %>%
    group_by(date, province) %>%
    summarise(min = median(infs))) %>%
  left_join(do.call(rbind, inf_comp_optimistic) %>%
              group_by(date, replicate, province) %>%
              summarise(infs = sum(infections, na.rm = TRUE)) %>%
              group_by(date, province) %>%
              summarise(max = median(infs)))
demog <- readRDS(cp_path("analysis/data/derived/demog.rds"))


tbl_ars <- left_join(inf_comp_province,
                     group_by(demog, province) %>%
                       summarise(pop = sum(n))) %>%
  mutate(across(med:max, ~.x/pop)) %>%
    filter(date %in% as.Date(c("2020-06-01", "2020-08-01", "2021-02-01","2021-07-01", "2021-10-22"))) %>%
    mutate(ar = paste0(scales::percent(med, accuracy = 0.01), " [", scales::percent(min, accuracy = 0.01), ", ", scales::percent(max, accuracy = 0.01),"]")) %>%
    select(ar, province) %>%
  tidyr::pivot_wider(names_from = date, values_from = ar) %>%
    set_names(c("Province", paste("Wave", 1:5)))
write.table(tbl_ars, cp_path("analysis/tables/province_ars.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# Estimated IFRs
# ------------------------------------------------------------------------------

ifr_tbl_central <- do.call(rbind, lapply(provs_central, function(x){

  left_join(
    nim_sq_format(x, "D") %>%
      group_by(replicate) %>%
      summarise(D = max(y, na.rm =TRUE)),
    nim_sq_format(x, "infections") %>%
      group_by(replicate) %>%
      summarise(infections = sum(y, na.rm =TRUE))
  ) %>%
    mutate(ifr = D/infections) %>%
    summarise(across(ifr, .fns = list(
      min =~ quantile(.x, 0.025, na.rm = TRUE),
      med =~ quantile(.x, 0.5, na.rm = TRUE),
      max =~ quantile(.x, 0.975, na.rm = TRUE)
    ),.names = "{.fn}"))  %>%
    mutate(province = x$parameters$province)

}))
ifr_tbl_optimistic <- do.call(rbind, lapply(provs_optimistic, function(x){

  left_join(
    nim_sq_format(x, "D") %>%
      group_by(replicate) %>%
      summarise(D = max(y, na.rm =TRUE)),
    nim_sq_format(x, "infections") %>%
      group_by(replicate) %>%
      summarise(infections = sum(y, na.rm =TRUE))
  ) %>%
    mutate(ifr = D/infections) %>%
    summarise(across(ifr, .fns = list(
      min =~ quantile(.x, 0.025, na.rm = TRUE),
      med =~ quantile(.x, 0.5, na.rm = TRUE),
      max =~ quantile(.x, 0.975, na.rm = TRUE)
    ),.names = "{.fn}"))  %>%
    mutate(province = x$parameters$province)

}))
ifr_tbl_worst <- do.call(rbind, lapply(provs_worst, function(x){

  left_join(
    nim_sq_format(x, "D") %>%
      group_by(replicate) %>%
      summarise(D = max(y, na.rm =TRUE)),
    nim_sq_format(x, "infections") %>%
      group_by(replicate) %>%
      summarise(infections = sum(y, na.rm =TRUE))
  ) %>%
    mutate(ifr = D/infections) %>%
    summarise(across(ifr, .fns = list(
      min =~ quantile(.x, 0.025, na.rm = TRUE),
      med =~ quantile(.x, 0.5, na.rm = TRUE),
      max =~ quantile(.x, 0.975, na.rm = TRUE)
    ),.names = "{.fn}"))  %>%
    mutate(province = x$parameters$province)

}))

ifr_tbl <- left_join(ifr_tbl_central[,c(4,2)],
                 rename(ifr_tbl_optimistic[,c(4,2)], min = med)) %>%
                   left_join(rename(ifr_tbl_worst[,c(4,2)], max = med))
ifr_tbl <- ifr_tbl %>%
  mutate(ifr = paste0(
    scales::percent(med, accuracy = 0.01), " [",
    scales::percent(min, accuracy = 0.01), ", ",
    scales::percent(max, accuracy = 0.01),"]"
  )) %>%
  select(province, ifr)
write.table(ifr_tbl, cp_path("analysis/tables/province_ifrs.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# Estimated IFRs By Waves
# ------------------------------------------------------------------------------

ifr_by_waves_func <- function(provs) {

  do.call(rbind, lapply(provs, function(x){

    cumsum_na <- function(x) {x[is.na(x)] <- 0; cumsum(x)}

    dat <- left_join(
      nim_sq_format(x, "D", date_0 = max(x$pmcmc_results$inputs$data$date)) %>%
        group_by(replicate, date) %>%
        summarise(D = max(y, na.rm =TRUE)) %>%
        ungroup,
      nim_sq_format(x, "infections", date_0 = max(x$pmcmc_results$inputs$data$date)) %>%
        group_by(replicate) %>%
        mutate(infections = lag(cumsum_na(y),20)) %>%
        ungroup,
      by = c("date", "replicate")
    )

    wave_breaks <- as.Date(c("2020-06-01", "2020-08-01", "2021-02-01","2021-07-01"))
    dat <- dat %>% mutate(
      wave = case_when(date >= as.Date("2018-01-01") & date < wave_breaks[1] ~ "Wave 1",
                       date >= wave_breaks[1] & date < wave_breaks[2] ~ "Wave 2",
                       date >= wave_breaks[2] & date < wave_breaks[3] ~ "Wave 3",
                       date >= wave_breaks[3] & date < wave_breaks[4] ~ "Wave 4",
                       date >= wave_breaks[4] ~ "Wave 5"))

    dat_waves <- split.data.frame(dat, dat$wave)

    ifr_waves <- lapply(dat_waves, function(y) {

      group_by(y, replicate, wave) %>%
        summarise(D = max(D)-min(D),
                  infections = max(infections,na.rm=TRUE) - min(infections, na.rm = TRUE)) %>%
        mutate(ifr = D/infections) %>%
        group_by(wave) %>%
        summarise(across(ifr, .fns = list(
          min =~ quantile(.x, 0.025, na.rm = TRUE),
          med =~ quantile(.x, 0.5, na.rm = TRUE),
          max =~ quantile(.x, 0.975, na.rm = TRUE)
        ),.names = "{.fn}")) %>%
        select(med, wave)

    })

    out <- do.call(rbind, ifr_waves) %>%
      mutate("Province" =  x$parameters$province)
    out

  }))

}

ifr_tbl_central_waves <- ifr_by_waves_func(provs_central)
ifr_tbl_worst_waves <- ifr_by_waves_func(provs_worst)
ifr_tbl_optimistic_waves <- ifr_by_waves_func(provs_optimistic)

ifr_tbl_waves <- left_join(ifr_tbl_central_waves,
                           rename(ifr_tbl_worst_waves, max = med)) %>%
  left_join(rename(ifr_tbl_optimistic_waves, min = med))
ifr_tbl_waves <- ifr_tbl_waves %>%
  group_by(Province, wave) %>%
  mutate(ifr = paste0(
    scales::percent(med, accuracy = 0.01), " [",
    scales::percent(min, accuracy = 0.01), ", ",
    scales::percent(max, accuracy = 0.01),"]"
  )) %>%
  select(Province, wave, ifr) %>%
  pivot_wider(names_from = wave, values_from = ifr)
write.table(ifr_tbl_waves, cp_path("analysis/tables/province_ifrs_waves.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------
# Reinfections over time
# ------------------------------------------------------------------------------

reinfections_by_province <- function(prov) {

  do.call(rbind, lapply(prov, function(x){

out <- nimue_format(x, c("S","infections"), date_0 = max(x$pmcmc_results$inputs$data$date))

reinf_dat <- out %>% pivot_wider(names_from = compartment, values_from = y) %>%
  mutate(infections = replace_na(infections, 0)) %>%
  mutate(S_drop = abs(S - max(S))) %>%
  group_by(replicate) %>%
  mutate(cumu_infs = cumsum(infections)) %>%
  mutate(diff = (cumu_infs - S_drop)/max(S)) %>%
  mutate(reinfs = diff*infections) %>%
  mutate(cumu_reinfs = cumsum(reinfs)) %>%
  mutate(prop_reinfs = cumu_reinfs/cumu_infs) %>%
  group_by(date) %>%
  summarise(med_reinfs = median(cumu_reinfs),
            med_infs = median(cumu_infs),
            med_prop = median(prop_reinfs)) %>%
  mutate(province = x$parameters$province)

return(reinf_dat)
}))
}

reinfections_low <- reinfections_by_province(provs_optimistic)
reinfections_central <- reinfections_by_province(provs_central)
reinfections_worst <- reinfections_by_province(provs_worst)

reinfections_dat <- left_join(
  reinfections_central,
  rename(reinfections_low, min_reinfs = med_reinfs, min_prop = med_prop, min_infs = med_infs)) %>%
  left_join(rename(reinfections_worst, max_reinfs = med_reinfs, max_prop = med_prop, max_infs = med_infs))

reinfections_province_gg <- reinfections_dat %>%
  ggplot(aes(date, med_prop, ymin = min_prop, ymax = max_prop)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme(axis.title.x = element_blank()) +
  facet_wrap(~province, scales = "free_y", ncol = 4) +
  ylab("Proportion of Cumulative Infections \ndue to Reinfections") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_x_date(date_labels = "%b %Y",
               breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))
save_figs("reinfections_province", reinfections_province_gg, width = 12, height = 16)

reinfections_national_gg <- reinfections_dat %>%
  group_by(date) %>%
  summarise(med = sum(med_reinfs)/sum(med_infs),
            min = sum(min_reinfs)/sum(min_infs),
            max = sum(max_reinfs)/sum(max_infs)) %>%
  ggplot(aes(date, med, ymin = min, ymax = max)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme(axis.title.x = element_blank()) +
  ylab("Proportion of Cumulative Infections \ndue to Reinfections") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_x_date(date_labels = "%b %Y",
               breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))
save_figs("reinfections_national", reinfections_national_gg, width = 6, height = 4)

# ------------------------------------------------------------------------------
# Investigate odriscol ifr impact on attack rate by age
# ------------------------------------------------------------------------------

# get the relationship between the IFRS
ifr <- (probs$prob_hosp * probs$prob_severe * probs$prob_severe_death_treatment) +
  (probs$prob_hosp * (1-probs$prob_severe) * probs$prob_non_severe_death_treatment)

odriscoll <- c(0.003, 0.001, 0.001, 0.003, 0.006, 0.013,
               0.024, 0.04, 0.075, 0.121, 0.207, 0.323, 0.456,
               1.075, 1.674, 3.203, 8.292)/100

infs_final <- inf_comp_age %>% filter(date == max(inf_comp_age$date))
infs_final$n <- pop_n

# what is the extra death required assuming same attack rate
curent_deaths_total <- sum(infs_final$max * ifr)
new_deaths_total <- sum(infs_final$max * odriscoll)
multiplier_of_attack_rate <- curent_deaths_total/new_deaths_total

# and get the scale from old IFRs:
ifr_tbl <- read.csv(cp_path("analysis/tables/province_ifrs.csv"))
ifr_tbls <- do.call(rbind,lapply(strsplit(gsub("\\[|\\]|\\%|,", "", ifr_tbl$province.ifr), " "), function(x){tail(x, 3)}))

ifr_tbls <- as.data.frame(ifr_tbls) %>% mutate(across(.fns = as.numeric))
med_scale <- mean(ifr_tbls$V1 / ifr_tbls$V3)
min_scale <- mean(ifr_tbls$V2 / ifr_tbls$V3)

# create new attack rate curves
infs_final$new_max <- infs_final$max * multiplier_of_attack_rate
infs_final$new_med <- infs_final$med * ((multiplier_of_attack_rate-1)*(med_scale)+1)
infs_final$new_min <- infs_final$min * ((multiplier_of_attack_rate-1)*(min_scale)+1)

final_attack_by_age_comp_gg <- infs_final  %>%
  pivot_longer(c("med", "max", "min", "new_med", "new_max", "new_min")) %>%
  mutate(Source = "Brazeau et al.") %>%
  mutate(Source = replace(Source, grepl("new", name), "O'Driscoll et al.")) %>%
  mutate(name = gsub("new_", "", name)) %>%
  mutate(Source = factor(Source, levels = c("O'Driscoll et al.", "Brazeau et al."))) %>%
  pivot_wider(values_from = value, names_from = name) %>%
ggplot(aes(age_group, med/n, ymin = min/n, ymax = max/n, color = Source)) +
  geom_point( size = 2) +
  geom_errorbar(size = 1, width = 0.4) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()) +
  ylab("National Attack Rate") +
  xlab("Age Group") +
  scale_color_manual(values = c("black", "red"), labels = c("Brazeau et al.", "O'Drsicoll et al.")) +
  scale_y_continuous(labels = scales::percent)
save_figs("final_attack_by_age_ifr_comp", final_attack_by_age_comp_gg, width = 6, height = 4)

# and now by national over time
inf_comp$new_med <- ((multiplier_of_attack_rate-1)*(med_scale)+1) * inf_comp$med
inf_comp$new_max <- multiplier_of_attack_rate * inf_comp$max
inf_comp$new_min <- ((multiplier_of_attack_rate-1)*(min_scale)+1) * inf_comp$min

national_attack_rate_comp_gg <-
  inf_comp %>% pivot_longer(med:new_min) %>% mutate(Source = "") %>%
  mutate(Source = "Brazeau et al.") %>%
  mutate(Source = replace(Source, grepl("new", name), "O'Driscoll et al.")) %>%
  mutate(name = gsub("new_", "", name)) %>%
  mutate(Source = factor(Source, levels = c("O'Driscoll et al.", "Brazeau et al."))) %>%
  pivot_wider(values_from = value, names_from = name) %>%
  ggplot(aes(date, med, ymin = min, ymax = max, color = Source, fill = Source)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ylab("Cumulative Attack Rate") +
  xlab("") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1.4)) +
  scale_x_date(date_labels = "%b %Y",
               breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01"))) +
  scale_fill_manual(name = "IFR Source:",
                    values = c("Brazeau et al." = "black", "O'Driscoll et al." = "red")) +
  scale_color_manual(name = "IFR Source:",
                     values = c("Brazeau et al." = "black", "O'Driscoll et al." = "red"))
save_figs("national_attack_rate_comp", national_attack_rate_comp_gg, width = 6, height = 4)

# explore sero likelihood of two methods
inf_comp_province$new_med <- ((multiplier_of_attack_rate-1)*(med_scale)+1) * inf_comp_province$med
inf_comp_province$new_max <- multiplier_of_attack_rate * inf_comp_province$max
inf_comp_province$new_min <- ((multiplier_of_attack_rate-1)*(min_scale)+1) * inf_comp_province$min

# create our model seroprevalence data
inf_comp_province_sero <- group_by(inf_comp_province, province) %>%
  mutate(across(med:new_min, .fns = ~.x-lag(.x, default = 0))) %>%
  mutate(across(med:new_min, roll_func, provs_central[[1]]$pmcmc_results$inputs$pars_obs$sero_det))

inf_comp_province_sero <- left_join(
  inf_comp_province_sero,
  group_by(demog, province) %>% summarise(n = sum(n))
  ) %>%
  mutate(across(med:new_min, .fns = ~.x / n))

# load in teh observed serodata
sero_dat <- readRDS(cp_path("src/prov_fit/sero.rds"))
sero_dat <- sero_dat %>% filter(source %in% "Khalagi et al") %>%
  mutate(province = gsub("Baluchestan", "Baluchistan", province))
sero_dat2 <- read.csv("analysis/data/raw/khalagi_n.csv")
sero_dat <- left_join(sero_dat, sero_dat2[,c("province", "samples")])
sero_dat <- sero_dat %>% mutate(
  sero_pos = round(sero*samples/100),
  date_mid = date_start + ((date_end-date_start)/2)
)

# create our seroprev table for calculation against
sero_comp_table <- left_join(sero_dat %>% select(province, sero_pos, samples),
          inf_comp_province_sero %>%
            filter(date %in% c(sero_dat$date_mid, sero_dat$date_start, sero_dat$date_end)) %>%
            select(date, province, med, new_med)
)

sero_comp_table$ll <- dbinom(sero_comp_table$sero_pos, sero_comp_table$samples, sero_comp_table$med, log = TRUE)
sero_comp_table$ll_new <- dbinom(sero_comp_table$sero_pos, sero_comp_table$samples, sero_comp_table$new_med, log = TRUE)

sero_comp_ll_gg <- sero_comp_table %>% pivot_longer(cols = ll:ll_new) %>%
    mutate(name = case_when(name == "ll" ~ "Brazeau et al.",
                            name == "ll_new" ~ "O'Driscoll et al.")) %>%
    select(province, date, name, value) %>%
    pivot_wider(names_from = date, values_from = value) %>%
    set_names(c("province", "name", "min", "med", "max")) %>%
    select(-c("min","max")) %>%
    ggplot(aes(x=-med, color = name, fill = name)) +
    geom_density(alpha = 0.2) +
    scale_x_log10() +
    xlab("Negative Log Likelihood of Model Fit against Khalaghi et al. Seroprevalence") +
    ggpubr::theme_pubclean() +
    theme(axis.line = element_line()) +
    scale_fill_manual(name = "IFR Source:",
                      values = c("Brazeau et al." = "black", "O'Driscoll et al." = "red")) +
    scale_color_manual(name = "IFR Source:",
                       values = c("Brazeau et al." = "black", "O'Driscoll et al." = "red"))
save_figs("sero_comp_ll", sero_comp_ll_gg, width = 6, height = 4)

# ------------------------------------------------------------------------------
# Investigate per province attack rate by age
# ------------------------------------------------------------------------------

prov <- names(provs_central)[15]
infs <- nim_sq_format(provs_central[[prov]], "infections", FALSE, date_0 = max(provs_central[[prov]]$pmcmc_results$inputs$data$date))
infs_over_time_age(provs_central[[prov]]) %>%
  group_by(date, age_group, replicate) %>%
  summarise(infections = sum(infections, na.rm = TRUE)) %>%
  group_by(replicate, age_group) %>%
  summarise(infections = max(infections, na.rm = TRUE)) %>%
  left_join(data.frame("pop" = provs_central[[prov]]$parameters$population,
                       "age_group" = levels(.$age_group))
  ) %>%
  mutate(ar = infections/pop) %>%
  group_by(age_group) %>%
  summarise(across(ar, .fns = list(
    min =~ quantile(.x, 0.025, na.rm = TRUE),
    med =~ quantile(.x, 0.5, na.rm = TRUE),
    max =~ quantile(.x, 0.975, na.rm = TRUE)
  ),.names = "{.fn}")) %>%
  mutate(age_group = factor(age_group, levels = levels(infs$age_group))) %>%
  ggplot(aes(age_group, med, ymin = min, ymax = max)) +
  #geom_point(size = 0.5) +
  geom_errorbar(size = 1, width = 0.4) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.key = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab(paste0(prov, " Attack Rate")) +
  xlab("Age Group") +
  scale_y_continuous(labels = scales::percent)
