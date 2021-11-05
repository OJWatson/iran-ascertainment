library(tidyverse)

# Data curation of vaccines

# Vaccine breakdown per province
vacc <- read.csv(cp_path("analysis/data/raw/vaccinations.csv"))
vacc$prop_first <- vacc$First_26_10_2021/sum(vacc$First_26_10_2021)
vacc$prop_second <- vacc$Second_26_10_2021/sum(vacc$Second_26_10_2021)

# vacc global data
owid2 <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
owid <- owid2 %>% filter(iso_code == "IRN") %>%
  select(date, total_vaccinations, people_vaccinated, people_fully_vaccinated) %>%
  filter(date <= max(date[which(!is.na(total_vaccinations))]))

tail_approx <- function(x) {

  new <- as.integer(zoo::na.approx(x))
  x[tail(seq_along(x), length(new))] <- new
  return(x)

}

owid$total_vaccinations <- tail_approx(owid$total_vaccinations)
owid$people_vaccinated <- tail_approx(owid$people_vaccinated)
owid$people_fully_vaccinated <- tail_approx(owid$people_fully_vaccinated)

owid$max_vaccine <- c(NA, diff(owid$people_vaccinated))
owid$dose_ratio <- owid$people_fully_vaccinated/owid$people_vaccinated
owid$dose_ratio[which(is.na(owid$dose_ratio))] <- 0

# ref https://wellcomeopenresearch.org/articles/6-185
ve_i_low_wt <- 0.5
ve_i_high_wt <- 0.67
ve_d_low_wt <- 0.5
ve_d_high_wt <- 0.80

# ref https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3895639
# this gives the ve_d 1st dose and 2nd dose efficacies as 0.1 and 0.7
# ref https://www.tandfonline.com/doi/full/10.1080/22221751.2021.1969291
# this is coronovac (same platform) and gives the ve_i 1st dose and 2nd dose efficacies
# as 0.14 and 0.6 overall against all disease. Against mild VEs are < 0, so have
# opted for setting ve_d min to 0.2 as that was against pneuomonia rather than hospitalisation
# and have set the ve_i low to 10%
ve_i_low_delta <- 0.10
ve_i_high_delta <- 0.6
ve_d_low_delta <- 0.2
ve_d_high_delta <- 0.70


# now let's bring these into and scale between them between May and July for Delta
owid$ve_i_low <- NA
owid$ve_i_high <- NA
owid$ve_d_low <- NA
owid$ve_d_high <- NA

owid$ve_i_low[owid$date < "2021-05-01"] <- ve_i_low_wt
owid$ve_i_high[owid$date < "2021-05-01"] <- ve_i_high_wt
owid$ve_d_low[owid$date < "2021-05-01"] <- ve_d_low_wt
owid$ve_d_high[owid$date < "2021-05-01"] <- ve_d_high_wt

owid$ve_i_low[owid$date > "2021-07-01"] <- ve_i_low_delta
owid$ve_i_high[owid$date > "2021-07-01"] <- ve_i_high_delta
owid$ve_d_low[owid$date > "2021-07-01"] <- ve_d_low_delta
owid$ve_d_high[owid$date > "2021-07-01"] <- ve_d_high_delta

owid$ve_i_low <- zoo::na.approx(owid$ve_i_low)
owid$ve_i_high <- zoo::na.approx(owid$ve_i_high)
owid$ve_d_low <- zoo::na.approx(owid$ve_d_low)
owid$ve_d_high <- zoo::na.approx(owid$ve_d_high)
owid <- owid %>% filter(!is.na(max_vaccine))

# format for odin
vaccine_efficacy_infection <- (1-owid$dose_ratio)*owid$ve_i_low + owid$dose_ratio*owid$ve_i_high
vaccine_efficacy_disease <- (1-owid$dose_ratio)*owid$ve_d_low + owid$dose_ratio*owid$ve_d_high
vaccine_efficacy_infection <- c(rep(ve_i_low_wt,28),  rep(vaccine_efficacy_infection, length.out = max(length(owid$max_vaccine) - 28, 1)))
vaccine_efficacy_disease <- c(rep(ve_i_high_wt,28),  rep(vaccine_efficacy_disease, length.out = max(length(owid$max_vaccine) - 28, 1)))
vaccine_efficacy_infection <- lapply(vaccine_efficacy_infection, rep, 17)
vaccine_efficacy_disease <- lapply(vaccine_efficacy_disease, rep, 17)

# this is then what we will use in our fits.
ret_res <- list(
  date_vaccine_change = owid$date,
  max_vaccine = owid$max_vaccine,
  vaccine_efficacy_infection = vaccine_efficacy_infection,
  vaccine_efficacy_disease = vaccine_efficacy_disease
)

# Lastly split by province share of vaccine:

prov_vacc <- read.csv(cp_path("analysis/data/raw/vaccinations.csv"))
prov_vacc$share <- prov_vacc$First_26_10_2021 / max(owid$people_vaccinated, na.rm=TRUE)
prov_vacc$share <- prov_vacc$share/sum(prov_vacc$share)
prov_vacc$share2 <- prov_vacc$Second_26_10_2021 / max(owid$people_fully_vaccinated, na.rm=TRUE)
prov_vacc$share2 <- prov_vacc$share2/sum(prov_vacc$share2)

# shares are fairly close between dose 1 and dose 2 so go with 1st dose share
vacc_inputs <- vector("list", length = length(prov_vacc$Province))
for(i in seq_along(prov_vacc$Province)) {

  vacc_inputs[[i]] <- ret_res
  vacc_inputs[[i]]$max_vaccine <- as.integer(vacc_inputs[[i]]$max_vaccine*prov_vacc$share[i])
  vacc_inputs[[i]]$rel_infectiousness_vaccinated <- rep(0.3, 17)

}

names(vacc_inputs) <- prov_vacc$Province
saveRDS(vacc_inputs, cp_path("analysis/data/derived/vaccines.rds"))
saveRDS(vacc_inputs, cp_path("src/prov_fit/vaccines.rds"))


