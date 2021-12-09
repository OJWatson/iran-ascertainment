## DEPRECATED CODE USED FOR VERSION 1 OF THE MANUSCRIPT SUBMIITED TO MEDRXIV

mortality <- readRDS(cp_path("data/derived/mortality.rds"))
mortality <- mortality %>% group_by(province) %>% mutate(cum_excess = if(cum_excess[1]<0){cum_excess-cum_excess[1]} else {cum_excess})

demog <- readRDS(cp_path("data/derived/demog.rds"))
demographies <- split(demog, demog$province)
names(demographies) <- vapply(demographies, function(x) {x$province[1]}, character(1))


ll_func <- function(pars, prov) {

  prov <- as.character(prov)
  start_date <- as.Date(as.numeric(as.numeric(as.Date("2020-03-01")) + pars["start_date"]), lubridate::origin)
  Rt_1 <- pars["Rt_1"]
  Rt_2 <- pars["Rt_2"]
  R0 <- pars["R0"]

# Knowns
date_0 <- as.Date("2020-09-20")
swtchdate <- as.Date("2020-03-05")
swtchdate_2 <- as.Date("2020-05-05")

if(start_date < swtchdate) {
R0s <- as.numeric(c(rep(R0, swtchdate-start_date), rep(Rt_1, swtchdate_2 - swtchdate+1), rep(Rt_2, date_0 - swtchdate_2+1)))
} else {
  R0s <- as.numeric(c(rep(Rt_1, swtchdate_2 - swtchdate+1), rep(Rt_2, date_0 - swtchdate_2+1)))
}
tt_R0 <- seq(0, length(R0s)-1, 1)

r <- squire::run_deterministic_SEIR_model(
  population = demographies[[prov]]$n,
  contact_matrix_set = squire::get_mixing_matrix("Iran"),
  R0 = R0s,
  day_return = TRUE,
  hosp_bed_capacity = 1e10,
  ICU_bed_capacity = 1e10,
  tt_R0 = tt_R0,
  time_period = length(tt_R0)
)

comparison_dates <- c("2020-03-20", "2020-06-20", "2020-09-20")
deaths <- squire::format_output(r, "D", date_0 = start_date) %>%
  filter(date %in% as.Date(comparison_dates)) %>%
  select(y) %>% unlist

expected <- tail(mortality$cum_excess[mortality$province == prov], length(deaths))

return((sum(abs(deaths - expected))))

}

generate_simulation <- function(pars, extend = 0) {

  start_date <- as.Date(as.numeric(as.numeric(as.Date("2020-03-01")) + pars["start_date"]), lubridate::origin)
  Rt_1 <- pars["Rt_1"]
  Rt_2 <- pars["Rt_2"]
  R0 <- pars["R0"]
  prov <- as.character(pars$prov)

  # Knowns
  date_0 <- as.Date("2020-09-20")
  swtchdate <- as.Date("2020-03-05")
  swtchdate_2 <- as.Date("2020-05-05")

  if(start_date < swtchdate) {
    R0s <- as.numeric(c(rep(R0, swtchdate-start_date), rep(Rt_1, swtchdate_2 - swtchdate+1), rep(Rt_2, date_0 - swtchdate_2+1)))
  } else {
    R0s <- as.numeric(c(rep(Rt_1, swtchdate_2 - swtchdate+1), rep(Rt_2, date_0 - swtchdate_2+1)))
  }
  tt_R0 <- seq(0, length(R0s)-1, 1)

  r <- squire::run_deterministic_SEIR_model(
    population = demographies[[prov]]$n,
    contact_matrix_set = squire::get_mixing_matrix("Iran"),
    R0 = R0s,
    hosp_bed_capacity = 1e10,
    ICU_bed_capacity = 1e10,
    day_return = TRUE,
    tt_R0 = tt_R0,
    time_period = length(tt_R0) + extend
  )

  return(r)

}

####

grid <- expand.grid(R0 = seq(2.5,4,0.25), Rt_1 = seq(0.6,1.4,0.1), Rt_2 = seq(0.8,2,0.1), start_date = seq(-60,0,10), prov = unique(mortality$province))
grid$ll <- 0
for(i in seq_len(nrow(grid))) {
  if(i %% 1000 == 0) message(i)
  pars <- grid[i,]
  grid$ll[i] <- ll_func(pars, as.character(pars$prov))
}
saveRDS(grid, "analysis/data/derived/grid.rds")

create_focussed_grid <- function(pars) {

  g <- expand.grid("R0" = seq(pars[["R0"]]*0.9, pars[["R0"]]*1.1, length.out = 3),
              "Rt_1" = seq(pars[["Rt_1"]]*0.9, pars[["Rt_1"]]*1.1, length.out = 3),
              "Rt_2" = seq(pars[["Rt_2"]]*0.9, pars[["Rt_2"]]*1.1, length.out = 3),
              "start_date" = seq(round(pars[["start_date"]]*0.9), round(pars[["start_date"]]*1.1), length.out = 3))
  g$prov <- pars[["prov"]]
  g$ll <- 0
  return(g)

}

# make new focussed grid
gs <- list()
for(p in unique(grid$prov)) {
  sub_grid <- grid[grid$prov == p, ]
  pars <- sub_grid[which.min(sub_grid$ll),]
  gs[[p]] <- create_focussed_grid(pars)
}

new_grid <- do.call(rbind, gs)
for(i in seq_len(nrow(new_grid))) {
  message(i)
  pars <- new_grid[i,]
  new_grid$ll[i] <- ll <- ll_func(pars, pars$prov)
}

# Repeat ad infinitum
gs <- list()
for(p in unique(new_grid$prov)) {
  sub_grid <- new_grid[new_grid$prov == p, ]
  pars <- sub_grid[which.min(sub_grid$ll),]
  gs[[p]] <- create_focussed_grid(pars)
}

new_grid <- do.call(rbind, gs)
for(i in seq_len(nrow(new_grid))) {
  message(i)
  pars <- new_grid[i,]
  new_grid$ll[i] <- ll <- ll_func(pars, pars$prov)
}
saveRDS(new_grid, "analysis/data/derived/new_grid.rds")



# overall plotting
rs <- list()
for(p in unique(grid$prov)) {
  sub_grid <- new_grid[new_grid$prov == p, ]
  pars <- sub_grid[which.min(sub_grid$ll),]
  r <- generate_simulation(pars, as.numeric(Sys.Date() - as.Date("2020-09-20")))
  ds <- squire:::format_output(r, c("deaths"), date_0 = Sys.Date()-nrow(r$output))
  ds$prov <- p
  rs[[p]] <- ds
}

D_comp <- function(r) {
  plot(r, "D", date_0 = Sys.Date()-nrow(r$output), x_var = "date") + geom_vline(xintercept = as.Date(comparison_dates)) +
    geom_point(aes(date, cum_excess,group = province),
               data = mortality %>% filter(province == p) %>% mutate(date = as.Date(comparison_dates)[match(season, c("Winter", "Spring", "Summer"))]), inherit.aes = FALSE)
}

deaths_all <- do.call(rbind, rs)
deaths_iran <- deaths_all %>% filter(compartment == "deaths") %>% group_by(date) %>% summarise(deaths = sum(y))
fig3 <- ggplot(deaths_iran, aes(date, deaths)) + geom_line() + ggpubr::theme_pubclean() +
  theme(axis.line = element_line()) + ylab("Reconstructed Iran Epidemic") + xlab("")

ecdc <- roxer::ecdc("2020-12-13")
fig3 + geom_bar(aes(as.Date(dateRep), deaths), ecdc %>% filter(countryterritoryCode == "IRN"), stat = "identity", inherit.aes = FALSE)
save_figs("reconstruct", fig3)

isodate <- function (x = Sys.Date()) {
  xday <- ISOdate(lubridate::year(x), lubridate::month(x), lubridate::day(x), tz = lubridate::tz(x))
  dn <- 1 + (lubridate::wday(x) + 5)%%7
  nth <- xday + lubridate::ddays(4 - dn)
  jan1 <- ISOdate(lubridate::year(nth), 1, 1, tz = lubridate::tz(x))
  return(paste0(sprintf("%s/%02d", format(nth, "%Y"), 1 + (nth - jan1)%/%lubridate::ddays(7)),"/1"))
}

deaths_iran %>% mutate(yw = as.Date(isodate(date), "%Y/%W/%u")) %>%
  group_by(yw) %>% summarise(deaths = sum(deaths)) %>%
ggplot(aes(yw, deaths)) + geom_line() + ggpubr::theme_pubclean() +
  theme(axis.line = element_line()) + ylab("Reconstructed Iran Epidemic") + xlab("")

deaths_all %>% mutate(yw = as.Date(isodate(date), "%Y/%W/%u")) %>%
  group_by(yw, prov) %>% summarise(deaths = sum(y)) %>%
ggplot(aes(yw, deaths)) + geom_line() + ggpubr::theme_pubclean() +
  theme(axis.line = element_line()) + ylab("Reconstructed Epidemic") + xlab("") +
  facet_wrap(~prov)

comparison_dates <- c("2020-03-20", "2020-06-20", "2020-09-20")
fig4 <- deaths_all %>% filter(compartment == "deaths") %>% group_by(prov) %>%
  mutate(deaths_total = cumsum(y)) %>%
  filter(date %in% as.Date(comparison_dates)) %>%
  mutate(season = c("Winter", "Spring", "Summer")[match(date, as.Date(comparison_dates))]) %>%
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer"))) %>%
  mutate(province = prov) %>%
  ggplot(aes(season, y = deaths_total, group = province)) +
  geom_point() + geom_line() +
  geom_point(aes(season, cum_excess, color = province, group = province), data = mortality, inherit.aes = FALSE) +
  geom_line(aes(season, cum_excess, color = province, group = province), data = mortality, inherit.aes = FALSE) +
  facet_wrap(~province, scales = "free_y") + ylab("Excess Deaths") + theme_bw() +
  scale_color_discrete(name = "Province") +
  ggtitle("Black = Model Predicted Deaths, Color = Excess Deaths")
fig4
save_figs("excess_fits", fig4, 14,10)

comparison_dates <- c("2020-03-20", "2020-06-20", "2020-09-20")
fig5 <- deaths_all %>% filter(compartment == "deaths") %>%
  group_by(prov) %>% mutate(deaths_total = cumsum(y)) %>%
  mutate(season = c("Winter", "Spring", "Summer")[match(date, as.Date(comparison_dates))]) %>%
  mutate(season = factor(season, levels = c("Winter", "Spring", "Summer"))) %>%
  mutate(province = prov) %>%
  ggplot(aes(as.Date(date), y = deaths_total, group = province, color = province)) +
  geom_line() +
  geom_point(aes(date, cum_excess, color = province, group = province),
             data = mortality %>% mutate(date = as.Date(comparison_dates)[match(season, c("Winter", "Spring", "Summer"))]), inherit.aes = FALSE) +
  facet_wrap(~province, scales = "free_y") + ylab("Total Deaths") + theme_bw() +
  scale_color_discrete(name = "Province") + xlab("") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b") +
  ggtitle("Line = Model Predicted Deaths over Time, Point = Excess Deaths. Vertical black Lines change in R time points") +
  geom_vline(xintercept = as.Date(c("2020-03-05", "2020-05-05")))
fig5
save_figs("inferred_province", fig5, 14,10)


# overall plotting
s_s <- list()
for(p in unique(grid$prov)) {
  sub_grid <- new_grid[new_grid$prov == p, ]
  pars <- sub_grid[which.min(sub_grid$ll),]
  r <- generate_simulation(pars, as.numeric(Sys.Date() - as.Date("2020-09-20")))
  ages <- c("0-19", "20-29", "30-39", "40-49", "50-59", "60+", "60-65")
  brackets <- list(1:4, 5:6, 7:8, 9:10, 11:12, 13:17, 13)
  ds <- squire:::format_output(r, c("S"), date_0 = Sys.Date()-nrow(r$output), reduce_age = FALSE)
  ds <- ds %>% group_by(date) %>%
    summarise(S_1 = sum(y[brackets[[1]]]),
              S_2 = sum(y[brackets[[2]]]),
              S_3 = sum(y[brackets[[3]]]),
              S_4 = sum(y[brackets[[4]]]),
              S_5 = sum(y[brackets[[5]]]),
              S_6 = sum(y[brackets[[6]]]),
              S_7 = sum(y[brackets[[7]]])) %>%
    pivot_longer(starts_with("S_")) %>%
    rename(age = name) %>%
    rename(S = value) %>%
    mutate(age = replace(age, seq_len(n()), ages[match(age, paste0("S_",1:7))]))
  ds$prov <- p
  s_s[[p]] <- ds
}

pop_total <- sum(unlist(lapply(s_s, function(x) { sum(x$S[1:7])})))
pop_ages <- colSums(do.call(rbind,lapply(s_s, function(x) { t(x$S[1:7])})))

S_all <- do.call(rbind, s_s)

# overall ar by age
S_all %>%
  group_by(prov, age) %>%
  complete(date = seq.Date(min(deaths_all$date), max(deaths_all$date),1)) %>%
  fill(-c("prov", "date"), .direction = "up") %>%
  group_by(date, age) %>% summarise(S = sum(S)) %>%
  mutate(ar = (pop_ages[c(1:5,7,6)] - S)/pop_ages[c(1:5,7,6)]) %>%
  filter(date == "2020-05-24") %>%
  ungroup %>%
  select(age, ar)

# overall ar by province
prov_pops <- (unlist(lapply(s_s, function(x) { sum(x$S[1:6])})))
prov_pops <- prov_pops[order(names(prov_pops))]
date_low <- as.Date("2020-04-21")
date_med <- as.Date("2020-05-07")
date_high <- as.Date("2020-05-21")
prov_sa <- S_all %>%
  group_by(prov, age) %>%
  complete(date = seq.Date(min(deaths_all$date), max(deaths_all$date),1)) %>%
  fill(-c("prov", "date"), .direction = "up") %>%
  group_by(date, prov) %>% summarise(S = sum(S)) %>%
  ungroup %>%
  left_join(data.frame("prov_pops" = prov_pops, "prov" = names(prov_pops))) %>%
  group_by(prov) %>%
  summarise(ar_low = (prov_pops[date == date_low] - S[date == date_low])/prov_pops[date == date_low],
            ar_med = (prov_pops[date == date_med] - S[date == date_med])/prov_pops[date == date_med],
            ar_high = (prov_pops[date == date_high] - S[date == date_high])/prov_pops[date == date_high],
            prov_pops = prov_pops[1]) %>%
  rename(province = prov)


df <- readRDS(cp_path("data/derived/sero.rds")) %>% filter(source == "Poustchi et al")
df$weighted_sero_mean[which(df$city == "Rasht")] <- 22.2
df$weighted_sero_low[which(df$city == "Rasht")] <- 16.4
df$weighted_sero_high[which(df$city == "Rasht")] <- 28.5

left_join(df, prov_sa) %>%
  ggplot(aes(y = ar_med*100,
             ymin = ar_low*100,
             ymax = ar_high*100,
             x = weighted_sero_mean,
             xmin = weighted_sero_low,
             xmax = weighted_sero_high)) +
  geom_point() +
  geom_errorbar() +
  geom_errorbarh() +
  ggrepel::geom_text_repel(aes(y = ar_med*100, x = weighted_sero_mean, label = province), inherit.aes = FALSE, min.segment.length = 0.1) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Attack Rate (%)") +
  xlab("Seropositive (%)") +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line())


# overall ifr
ifr_s <- list()
for(p in unique(grid$prov)) {
  sub_grid <- new_grid[new_grid$prov == p, ]
  pars <- sub_grid[which.min(sub_grid$ll),]
  r <- generate_simulation(pars, as.numeric(Sys.Date() - as.Date("2020-09-20")))
  S <- squire::format_output(r, "S")
  D <- squire::format_output(r, "D")
  ifr_s[[p]] <- data.frame("province" = p, ifr_sim = (tail(D$y, 1) / (S$y[1] - tail(S$y,1)))*100)
}

ifr_s <- do.call(rbind, ifr_s)
overall <- left_join(left_join(df, prov_sa), ifr_s)
