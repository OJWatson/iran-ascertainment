library(tidyverse)

## DEPRECATED CODE USED FOR VERSION 1 OF THE MANUSCRIPT SUBMIITED TO MEDRXIV

# ------------------------------------------------------------
# 1. Get the raw data
# ------------------------------------------------------------
add_zero <- function(x){
  if(nchar(x) == 1) {
    x <- paste0("0", x)
  } else {
    x <- as.character(min(x, 52))
  }
  return(x)
}

# excess deaths
df <- readRDS(cp_path("analysis/data/derived/iran_deaths_province.rds"))
pos <- function(x){ x[x<0] <- 0; x}
df2 <- group_by(df, province_name, date) %>%
  summarise(deaths = sum(excess_deaths_mean),
            pos_deaths = sum(pos(excess_deaths_mean)))

# add in national covid deaths
jhu <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
# format into the same style as ecdc so easy to swap back and forth
jhu$countryterritoryCode <- suppressWarnings(
  countrycode::countrycode(jhu$Country.Region, "country.name.en", "iso3c",
                           custom_match = c(Kosovo = "KSV"))
)
jhu <- jhu %>% tidyr::pivot_longer(matches("X\\d"))
names(jhu) <- c("", "Region","lat","lon","countryterritoryCode","date","deaths")

jhu <- jhu[,c("date","deaths","countryterritoryCode","Region")]
jhu$date <- as.Date(jhu$date, format = "X%m.%d.%y")

# and into daily deaths nationally
jhu <- group_by(jhu, date, countryterritoryCode, Region) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE))
jhu <- group_by(jhu, countryterritoryCode, Region) %>%
  mutate(deaths = c(0, diff(deaths)))
irn_df <- jhu %>% filter(countryterritoryCode == "IRN")
irn_df$date <- as.Date(paste0(lubridate::year(irn_df$date), vapply(lubridate::week(irn_df$date), add_zero, character(1)), 5), "%Y%W%w")
df3 <- left_join(
  df2,
  irn_df %>% ungroup %>% select(date, deaths) %>%
  rename(covid = deaths) %>%
  mutate(province_name = "Iran") %>%
    group_by(date, province_name) %>%
    summarise(covid = sum(covid))
)

# checking
ggplot(df3, aes(date, deaths)) + geom_line() + facet_wrap(~province_name, scales = "free_y")

###

hosp <- readRDS(cp_path("analysis/data/derived/hospitalisations.rds"))
hosp$date <- as.Date(paste0(lubridate::year(hosp$date), vapply(lubridate::week(hosp$date), add_zero, character(1)), 5), "%Y%W%w")
res <- left_join(
  df3,
  hosp %>% ungroup() %>%
  group_by(province, date, type) %>%
  summarise(hosp = sum(hosp)) %>%
  pivot_wider(c("province","date"), names_from = "type", values_from = "hosp", names_prefix = "hosp_") %>%
    rename(province_name = province)
)

cowplot::plot_grid(
ggplot(res %>% filter(province_name == "Iran"), aes(x = covid/hosp_confirmed)) + geom_density() + scale_x_continuous(limits = c(0,1))+
  ggpubr::theme_pubclean() + xlab("Reported COVID-19 Deaths Nationally / Confirmed Hospital Admissions") +
  theme(axis.line = element_line()),
ggplot(res, aes(x = pos_deaths/hosp_suspected, color = province_name)) + geom_density(alpha = 0.2) +
  scale_y_continuous(labels = round) + scale_x_continuous(limits = c(0,1)) +
  ggpubr::theme_pubclean() + xlab("Reported Excess Deaths Per Province / Suspected Hospital Admissions") +
  theme(legend.position = "none", axis.line = element_line()) ,
ncol = 1
)
