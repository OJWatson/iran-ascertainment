# Table 5-18 on page 689 of the country's statistical yearbook in 2017
# hosp_beds <- read.csv("https://media.iranopendata.org/hsp/scihsp195-number-of-active-beds-in-hospital-wards-by-province-in-year-1396-en.csv")
hosp_beds <- read.csv(cp_path("analysis/data/raw/hopsital_beds_province.csv"))

# match provinces first
hosp_beds$provinces <- str_to_sentence(hosp_beds$provinces)
demog <- readRDS(cp_path("analysis/data/derived/demog.rds"))
pos_to_rep <- which(is.na(match(hosp_beds$provinces, demog$province)))
hosp_beds <- hosp_beds %>%
  mutate(
  provinces = replace(
    provinces,
    pos_to_rep,
    c("Iran", "East Azerbaijan", "West Azerbaijan", "Ardabil", "Isfahan",
      "Ilam", "Chahar Mahaal and Bakhtiari", "South Khorasan", "Razavi Khorasan",
      "North Khorasan", "Sistan and Baluchistan", "Kurdistan", "Kohgiluyeh and Boyer-Ahmad"))
)

# hosp bed grabbing
hosp_beds$hosp_beds <- hosp_beds$total
hosp_beds$icu_beds <- hosp_beds$intencive.care.unit
hosp_beds <- hosp_beds %>%
  select(provinces, hosp_beds, icu_beds) %>%
  rename(province = provinces)

# and make numeric
hosp_beds <- hosp_beds %>% mutate(
  hosp_beds = as.numeric(gsub(",", "", hosp_beds)),
  icu_beds = as.numeric(gsub(",", "", icu_beds))
)

# save these
saveRDS(hosp_beds, cp_path("analysis/data/derived/hosp_beds.rds"))
saveRDS(hosp_beds, cp_path("src/prov_fit/hosp_beds.rds"))
