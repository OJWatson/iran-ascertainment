
# create our model fits
ls <- list.files("archive", full.names = TRUE, recursive = TRUE)
provs <- lapply(grep("20211119", grep("res.rds", ls, value = TRUE), value = TRUE), readRDS)
provs <- pbapply::pblapply(provs, function(x) {
  generate_draws(
    x,
    pars_list = lapply(generate_parameters(x, draws = 10, ll = FALSE, burnin = 10000),
                       function(x) {x[-1] <- x[-1]*runif(length(x)-1, 0.975, 1.025); x}
                       ),
    parallel = TRUE)
})
names(provs) <- vapply(provs, function(x){x$parameters$province}, character(1))
saveRDS(provs, "analysis/data/derived/model_fits.rds")


df <- readRDS(cp_path("analysis/data/derived/iran_deaths_age_province.rds"))

# ------------------------------------------------------------------------------
# final death plot nationally
# ------------------------------------------------------------------------------
death_comps <- pbapply::pblapply(provs, final_deaths)
pos <- function(x){ x[x<0] <- 0; x}
final_death_national_gg <- left_join(
  do.call(rbind, death_comps) %>% group_by(age_group) %>% summarise(across(min:max, sum)) %>%
    mutate(age_group = squire::population$age_group[1:17]),
  df %>% filter(province_name == "Iran") %>%
    group_by(age_group) %>%
    summarise(d = sum((excess_deaths_mean)),
              d_pos = sum(pos(excess_deaths_mean)))
) %>% pivot_longer(cols = c("med", "d", "d_pos")) %>%
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
inf_comp <-  pbapply::pblapply(provs, infs_over_time)

national_ar_gg <- do.call(rbind, inf_comp) %>% group_by(date, replicate) %>%
  summarise(infections = sum(infections, na.rm = TRUE)) %>%
  mutate(ar = infections/sum(squire::get_population("Iran")$n)) %>%
  group_by(date) %>%
  summarise(med = median(ar), min = quantile(ar, 0.025), max = quantile(ar, 0.975)) %>%
  ggplot(aes(date, med, ymin = min, ymax = max)) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  ylab("Cumulative Attack Rate") +
  xlab("") +
  ggpubr::theme_pubclean(base_size = 14) +
  theme(axis.line = element_line()) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))
save_figs("national_ar", national_ar_gg, width = 6, height = 4)


# ------------------------------------------------------------------------------
# hospital comparison plot
# ------------------------------------------------------------------------------
hosps <- readRDS("analysis/data/derived/hospitalisations.rds")
prov <- "Kohgiluyeh and Boyer-Ahmad"
hosps <- hosps %>% mutate(province = replace(province, which(province == "Kohgiluyeh and Boyer Ahmad"), "Kohgiluyeh and Boyer-Ahmad"))
hosps <- hosps %>% mutate(province = replace(province, which(province == "Yaz"), "Yazd"))

hosp_all_comp_plot <- function() {

  date_0 <- max(provs[[1]]$pmcmc_results$inputs$data$date)
  H <- lapply(provs, nim_sq_format, c("hospitalisations","hospital_demand","ICU_demand"), date_0 = date_0)
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
              H_dat[H_dat$province != "Iran",] %>% filter(compartment != "hospital_demand"), inherit.aes = FALSE) +
    theme(axis.title = element_blank()) +
    facet_wrap(~province, scales = "free_y", ncol = 4) +
    scale_color_discrete(
      name = "Source:",
      labels = c("confirmed"="Observed Confirmed",
                 "c_suspected"="Observed Suspected",
                 "hospitalisations"="Modelled Daily Hospitalisations",
                 "ICU_demand"="Modelled ICU Demand")) +
    ylab("Hospital Data") +
    ggpubr::theme_pubclean(base_size = 14) +
    theme(axis.line = element_line(), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank()) +
    scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2020-04-01", "2020-10-01", "2021-04-01", "2021-10-01")))


}
hosp_all_gg <- hosp_all_comp_plot()
save_figs("hosp_all", hosp_all_gg, width = 12, height = 16)


# ------------------------------------------------------------------------------
# attack rate per age
# ------------------------------------------------------------------------------

inf_comp_age <-  pbapply::pblapply(provs, infs_over_time_age)
S_tot <- data.frame("pop" = squire::get_population("Iran")$n,
                    "age_group" = levels(inf$age_group))

national_ar_age_gg <- do.call(rbind, inf_comp_age) %>%
  group_by(date, age_group, replicate) %>%
  summarise(infections = sum(infections, na.rm = TRUE)) %>%
  group_by(replicate, age_group) %>%
  summarise(infections = max(infections, na.rm = TRUE)) %>%
  left_join(S_tot) %>%
  mutate(ar = infections/pop) %>%
  group_by(age_group) %>%
  summarise(across(ar, .fns = list(
    min =~ quantile(.x, 0.025, na.rm = TRUE),
    med =~ quantile(.x, 0.5, na.rm = TRUE),
    max =~ quantile(.x, 0.975, na.rm = TRUE)
  ),.names = "{.fn}")) %>%
  mutate(age_group = factor(age_group, levels = S_tot$age_group)) %>%
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

tbl_ars <- do.call(rbind, lapply(inf_comp, function(x){
  x %>% mutate(pop = sum(provs[[x$province[1]]]$parameters$population)) %>%
    group_by(date, province, replicate) %>%
    summarise(infections = sum(infections, na.rm = TRUE), pop = pop) %>%
    filter(date %in% as.Date(c("2020-06-01", "2020-08-01", "2021-02-01","2021-07-01", "2021-10-22"))) %>%
    mutate(ar = infections/pop) %>%
    group_by(date) %>%
    summarise(across(ar, .fns = list(
      min =~ quantile(.x, 0.025, na.rm = TRUE),
      med =~ quantile(.x, 0.5, na.rm = TRUE),
      max =~ quantile(.x, 0.975, na.rm = TRUE)
    ),.names = "{.fn}")) %>%
    mutate(ar = paste0(scales::percent(med), " [", scales::percent(min), ", ", scales::percent(max),"]")) %>%
    select(ar) %>% t() %>%
    as.data.frame() %>%
    set_names(paste("Wave", 1:5)) %>%
    cbind(data.frame("Province" = x$province[1])) %>%
    select(6, 1:5)
}))

rownames(tbl_ars) <- NULL
write.table(tbl_ars, cp_path("analysis/tables/province_ars.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# Estimated IFRs
# ------------------------------------------------------------------------------

ifr_tbl <- do.call(rbind, lapply(provs, function(x){

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

rownames(ifr_tbl) <- NULL
ifr_tbl <- ifr_tbl[c(4,1:3)]
ifr_tbl <- ifr_tbl %>%
  mutate(ifr = paste0(
    scales::percent(med, accuracy = 0.01), " [",
    scales::percent(min, accuracy = 0.01), ", ",
    scales::percent(max, accuracy = 0.01),"]"
    )) %>%
  select(province, ifr)
write.table(ifr_tbl, cp_path("analysis/tables/province_ifrs.csv"), row.names = FALSE)
