library(tidyverse)

# Data curation

# Demography
demogs <- readxl::read_xlsx(cp_path("analysis/data/raw/iran_demog.xlsx"))
demogs <- demogs %>% filter(provinces != "Iran" & `age group` != "total") %>%
  rename(age_group = `age group`,
         n = `m+f`,
         province = provinces) %>%
  select(province, age_group, n) %>%
  mutate(province=replace(province, province=="East Azarbayjan", "East Azerbaijan"),
         province=replace(province, province=="Khuzistan", "Khuzestan"))

# Prov Specific Where available
provs <- read.csv(cp_path("data/raw/Iran_prov_demog.csv"))
provs <- provs %>% group_by(province, age) %>%
  summarise(population = sum(population)) %>%
  mutate(age = replace(age, age == "100+", rep(99, sum(age == "100+")))) %>%
  mutate(age_group = cut(as.numeric(age), breaks = c(seq(0,80,5), 100), include.lowest = TRUE, right = FALSE)) %>%
  mutate(age_group = unique(demogs$age_group)[match(age_group, levels(age_group))]) %>%
  group_by(province, age_group) %>%
  summarise(n = sum(population)) %>%
  arrange(province) %>%
  mutate(province=replace(province, province=="Khuzistan", "Khuzestan"))

provs$age_group <- factor(provs$age_group, levels = demogs$age_group[1:17])
provs <- provs[order(provs$age_group),]
provs <- provs[order(provs$province),]
provs <- rbind(provs, demogs[demogs$province == "South Khorasan",])
saveRDS(provs, cp_path("analysis/data/derived/demog.rds"))
saveRDS(provs, cp_path("src/prov_fit/demog.rds"))

# Fitting Excess Mortality
mortality <- readxl::read_xlsx(cp_path("analysis/data/raw/seasonal_excess_mortality.xlsx"))
mortality <- mortality  %>%
  rename(excess = `Excess deaths`,
         province = Province) %>%
  select(province, year, season, excess) %>%
  group_by(province) %>%
  mutate(cum_excess = cumsum(excess)) %>%
  mutate(province=replace(province, province=="East Azarbayjan", "East Azerbaijan"),
         province=replace(province, province=="West Azarbayjan", "West Azerbaijan"),
         province=replace(province, province=="Gilan (Guilan)", "Gilan"),
         province=replace(province, province=="Kurdestan", "Kurdistan"),)
mortality <- left_join(
  mortality,
  group_by(demogs, province) %>% summarise(n = sum (n))) %>%
  mutate(cum_excess_per_1000 = cum_excess/(n/1000))


saveRDS(mortality, cp_path("data/derived/mortality.rds"))
mortality <- readRDS(cp_path("data/derived/mortality.rds"))
# Data plotting

# Excess mortality over time
g1 <- ggplot(mortality %>% mutate(season = factor(season, levels = c("Winter", "Spring", "Summer"))),
       aes(season, cum_excess/(n/1000), color = province, group = province)) +
  geom_point() + geom_line() + ggpubr::theme_pubclean(base_size = 14) +
  ylab("Cumulative Excess Mortality / 1000") +
  xlab("") +
  theme(axis.line = element_line(), legend.position = "right",
        legend.key = element_blank(), legend.title = element_blank())

ggplot(mortality %>% mutate(season = factor(season, levels = c("Winter", "Spring", "Summer"))),
       aes(season, cum_excess/(n/1000), color = province, group = province)) +
  geom_point() + geom_line() + ggpubr::theme_pubclean(base_size = 14) +
  ylab("Cumulative Excess Mortality / 1000") +
  xlab("") + facet_wrap(~province) +
  theme(axis.line = element_line(), legend.position = "right",
        legend.key = element_blank(), legend.title = element_blank())


# Pop weighted excess agianst seroprev
df <- readRDS(cp_path("data/derived/sero.rds")) %>% filter(source == "Poustchi et al")
df <- left_join(df, (mortality %>% filter(season == "Spring")), "province")

g2 <- ggplot(df, aes(x = cum_excess_per_1000,
               y = weighted_sero_mean,
               ymin = weighted_sero_low,
               ymax = weighted_sero_high)) +
  geom_point() +
  geom_errorbar(alpha = 0.5, width = 0) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = "lm") +
  xlab("Attack Rate (%)") +
  ylab("Seropositive (%)") +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line())

# Distribution of fastest growth rate
g3 <- group_by(mortality, province) %>% summarise(gr = max(diff(cum_excess_per_1000))) %>%
  ggplot(aes(gr)) + geom_histogram(aes(y=(..count..)/31), binwidth = 0.1, color = "black", ) +
  ggpubr::theme_pubclean() +
  xlab("Maximum Excess Mortality Increase per 3 months (Deaths / 1000)") +
  ylab("Frequency") +
  theme(axis.line = element_line())



