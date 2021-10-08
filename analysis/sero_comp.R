# sh <- googlesheets4::gs4_get("11E66OpXidMIaOPd_p0D8J-ALzxrr8W0lbenvNjUUWsg")
# dat <- googlesheets4::read_sheet(sh)
# write.csv(dat, cp_path("analysis/data/raw/sero_data.csv"))

# 1. Data Formatting -----------------------------

dat <- read.csv(cp_path("analysis/data/raw/sero_data.csv"))

replace_K <- function(x) {

  nums <- gsub("K", "", x)
  nums <- na.omit(as.numeric(strsplit(nums, " |\\(|\\)|-")[[1]])*1000)
  paste0(nums[1], " (", nums[2], "-", nums[3], ")", collapse = "")

}

replace_M <- function(x) {

  nums <- gsub("M", "", x)
  nums <- na.omit(as.numeric(strsplit(nums, " |\\(|\\)|-")[[1]])*1000000)
  paste0(nums[1], " (", nums[2], "-", nums[3], ")", collapse = "")

}

dat$excess_deaths <- as.character(vapply(dat$excess_deaths, replace_K, character(1)))
dat$exposed <- as.character(vapply(dat$exposed, replace_M, character(1)))



extract_mean_and_ci <- function(x) {

  d <- dat[[x]]

  d_mean <- gsub("^(.*) (.*)","\\1", d)
  d_low <- gsub("^(.*) \\((.*)-(.*)","\\2", d)
  d_high <- gsub("^(.*) \\((.*)-(.*)\\)","\\3", d)

  names <- paste(x, c("mean", "low", "high"), sep = "_")
  df <- data.frame(d_mean, d_low, d_high)
  names(df) <- names
  return(df)

}

to_extract <- c("excess_deaths","ifr","exposed","perc_exposed","perc_exposed_spring",
                "weighted_sero","crude_sero","test_1_adjust","test_2_adjust")


extracted <- lapply(to_extract, extract_mean_and_ci)
df <- cbind(dat$City, dat$Provinces, do.call(cbind, extracted))
df <- cbind(df[,1:2], df %>% select(-(1:2)) %>% mutate_if(is.character, as.numeric))
names(df)[1:2] <- c("city", "province")

df$date_start <- as.Date("2020-04-17")
df$date_end <- as.Date("2020-06-02")
df$sero <- df$weighted_sero_mean
df$sero_max <- df$weighted_sero_high
df$sero_min <- df$weighted_sero_low

saveRDS(df, cp_path("analysis/data/derived/sero.rds"))
saveRDS(df, cp_path("src/prov_fit/sero.rds"))

# 2. Data Visualisation -----------------------------

library(tidyverse)

perc <- 1

ggplot(df, aes(x = perc_exposed_spring_mean*perc,
               xmin = perc_exposed_spring_low*perc,
               xmax = perc_exposed_spring_high*perc,
               y = weighted_sero_mean,
               ymin = weighted_sero_low,
               ymax = weighted_sero_high)) +
  geom_point() +
  geom_errorbarh(alpha = 0.5) +
  geom_errorbar(alpha = 0.5, width = 0) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = "lm") +
  xlab("Attack Rate (%)") +
  ylab("Seropositive (%)") +
  ggpubr::theme_pubclean() +
  theme(axis.line = element_line())


