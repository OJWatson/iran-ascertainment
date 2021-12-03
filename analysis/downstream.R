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
saveRDS(provs_central, "analysis/data/derived/model_fits_central.rds")
saveRDS(provs_worst, "analysis/data/derived/model_fits_worst.rds")
saveRDS(provs_optimistic, "analysis/data/derived/model_fits_optimistic.rds")


# read in data

provs_central <- readRDS("analysis/data/derived/model_fits_central.rds")
provs_worst <- readRDS("analysis/data/derived/model_fits_worst.rds")
provs_optimistic <- readRDS("analysis/data/derived/model_fits_optimistic.rds")

# ------------------------------------------------------------------------------
# final death plot nationally
# ------------------------------------------------------------------------------
df <- readRDS(cp_path("analysis/data/derived/iran_deaths_age_province.rds"))
pos <- function(x){ x[x<0] <- 0; x}

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
  mutate(across(.cols = c("min", "max", "med"),  ~ .x * (100000/as.numeric(mapply(rep, squire::get_population("Iran")$n, 3))))) %>%
  mutate(min = pos(min), max = pos(max), med = pos(med)) %>%
  filter(source %in% c("model", "dpos")) %>%
  ggplot(aes(age_group, med, color = source, fill = source, ymin = min, ymax = max, group = source)) +
  geom_ribbon(alpha = 0.2) +
  geom_line(aes(group = source)) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.key = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
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
save_figs("final_death_national", final_death_national_gg, width = 8, height = 8)

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
  mutate(across(med:max, ~.x/sum(squire::get_population("Iran")$n)))

national_ar_gg <- inf_comp %>%
  ggplot(aes(date, med, ymin = min, ymax = max)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ylab("Cumulative Attack Rate") +
  xlab("") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line()) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))
save_figs("national_ar", national_ar_gg, width = 6, height = 4)


# ------------------------------------------------------------------------------
# hospital comparison plot
# ------------------------------------------------------------------------------
hosps <- readRDS("analysis/data/derived/hospitalisations.rds")
prov <- "Kohgiluyeh and Boyer-Ahmad"
hosps <- hosps %>% mutate(province = replace(province, which(province == "Kohgiluyeh and Boyer Ahmad"), "Kohgiluyeh and Boyer-Ahmad"))
hosps <- hosps %>% mutate(province = replace(province, which(province == "Yaz"), "Yazd"))

hosp_all_comp_plot <- function(provs) {

  date_0 <- max(provs[[1]]$pmcmc_results$inputs$data$date)
  H <- lapply(provs, nim_sq_format, c("hospitalisations"), date_0 = date_0)
  for(i in seq_along(H)) {
    H[[i]]$province <- names(provs)[i]
  }
  H <- do.call(rbind, H) %>%
    group_by(replicate, t, date, province, compartment) %>%
    summarise(y = sum(y, na.rm=TRUE))
  H_dat <- group_by(H, province, compartment, date) %>%
    summarise(across(y, .fns = list(
      min =~ quantile(y, 0.025, na.rm = TRUE),
      med =~ quantile(y, 0.5, na.rm = TRUE),
      max =~ quantile(y, 0.975, na.rm = TRUE)
    ),.names = "{.fn}"))

  ggplot(hosps %>%
           filter(province != "Iran") %>%
           mutate(type = replace(type, which(type == "suspected"), "c_suspected")),
         aes(date, hosp, color = type)) +
    geom_line() +
    geom_line(aes(date, med, color = compartment),
              H_dat[H_dat$province != "Iran",], inherit.aes = FALSE) +
    theme(axis.title = element_blank()) +
    facet_wrap(~province, scales = "free_y", ncol = 4) +
    scale_color_discrete(
      name = "Source:",
      labels = c("confirmed"="Observed Confirmed",
                 "c_suspected"="Observed Suspected",
                 "hospitalisations"="Modelled Daily Hospitalisations")) +
    ylab("Hospital Data") +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()) +
    scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))


}
hosp_all_gg <- hosp_all_comp_plot(provs_central)
save_figs("hosp_all", hosp_all_gg, width = 12, height = 16)


# ------------------------------------------------------------------------------
# attack rate per age
# ------------------------------------------------------------------------------

inf_comp_age_central <-  pbapply::pblapply(provs_central, infs_over_time_age)
inf_comp_age_worst <-  pbapply::pblapply(provs_worst, infs_over_time_age)
inf_comp_age_optimistic <-  pbapply::pblapply(provs_optimistic, infs_over_time_age)

S_tot <- data.frame("pop" = squire::get_population("Iran")$n,
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
  #geom_point(size = 0.5) +
  geom_errorbar(size = 1, width = 0.4) +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line(), legend.key = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("National Attack Rate") +
  xlab("Age Group") +
  scale_y_continuous(labels = scales::percent)
save_figs("national_ar_age", national_ar_age_gg, width = 6, height = 4)

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


## Per province plot for attack rate

prov <- "Ardabil"
infs <- nim_sq_format(provs[[prov]], "infections", FALSE, date_0 = max(provs[[prov]]$pmcmc_results$inputs$data$date))
infs_over_time_age(provs[[prov]]) %>%
  group_by(date, age_group, replicate) %>%
  summarise(infections = sum(infections, na.rm = TRUE)) %>%
  group_by(replicate, age_group) %>%
  summarise(infections = max(infections, na.rm = TRUE)) %>%
  left_join(data.frame("pop" = provs[[prov]]$parameters$population,
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
