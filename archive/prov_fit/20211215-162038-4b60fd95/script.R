RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

system(paste0("echo Iran Subnational for  ", province))

version_min <- "0.6.7"
if(packageVersion("squire") < version_min) {
  stop("squire needs to be updated to at least v", version_min)
}

version_min <- "0.1.22"
if(packageVersion("nimue") < version_min) {
  stop("nimue needs to be updated to at least v", version_min)
}

## -----------------------------------------------------------------------------
## 0. Checks and Function Definitions
## -----------------------------------------------------------------------------

# check on the province name
if (!(province %in% c(
  "East Azerbaijan","West Azerbaijan","Ardabil","Isfahan","Alborz",
  "Ilam","Bushehr","Tehran","Chahar Mahaal and Bakhtiari","South Khorasan",
  "Razavi Khorasan","North Khorasan","Khuzestan","Zanjan","Semnan",
  "Sistan and Baluchistan","Fars","Qazvin","Qom","Kurdistan","Kerman",
  "Kermanshah","Kohgiluyeh and Boyer-Ahmad","Golestan","Gilan","Lorestan",
  "Mazandaran","Markazi","Hormozgan","Hamedan","Yazd"
  ))) {
  stop("Province is not correct")
}

## -----------------------------------------------------------------------------
## 1. GET INPUT DATA
## -----------------------------------------------------------------------------

## a. Get from local files
## -----------------------------------------------------------------------------

# get pop data from file
demog <- readRDS("demog.rds")
pop <- demog$n[demog$province == province]

# get hosp beds from file
hosp_beds <- readRDS("hosp_beds.rds")
icu_beds <- hosp_beds$icu_beds[hosp_beds$province == province]
icu_beds <- icu_beds*as.numeric(hosp_capacity_multiplier)
hosp_beds <- hosp_beds$hosp_beds[hosp_beds$province == province]
hosp_beds <- hosp_beds*as.numeric(hosp_capacity_multiplier)

# get seroprevalence data points
sero_df <- readRDS("sero.rds")
sero_df <- sero_df[sero_df$province == province,]

# seroconversion data from brazeau report 34 addjusted in light of more longer term studies
prob_conversion <-  cumsum(dgamma(0:600,shape = 5, rate = 1/2))/max(cumsum(dgamma(0:300,shape = 5, rate = 1/2)))
sero_det <- cumsum(dweibull(0:600, 3.669807, scale = 210.7046))
sero_det <- prob_conversion-sero_det
sero_det[sero_det < 0] <- 0
sero_det <- sero_det/max(sero_det)

# get deaths from file
deaths <- readRDS("deaths.rds")
deaths <- deaths[deaths$province == province, ]
deaths <- deaths[, c("date","deaths")]
rownames(deaths) <- NULL
deaths <- deaths[deaths$date < date, ] # pre delta & vaccination

# get vaccine info
vacc_inputs <- readRDS("vaccines.rds")
vacc_inputs <- vacc_inputs[[province]]
pos <- which(vacc_inputs$date_vaccine_change < max(deaths$date))
vacc_inputs$date_vaccine_change <- vacc_inputs$date_vaccine_change[pos]
vacc_inputs$max_vaccine <- vacc_inputs$max_vaccine[pos]
vacc_inputs$vaccine_efficacy_infection <- vacc_inputs$vaccine_efficacy_infection[pos]
vacc_inputs$vaccine_efficacy_disease <- vacc_inputs$vaccine_efficacy_disease[pos]

## -----------------------------------------------------------------------------
## 2. Fit Model
## -----------------------------------------------------------------------------

# fit model
res <- fit_spline_rt(data = deaths,
                     country = as.character("Iran"),
                     pop = pop,
                     n_mcmc = as.numeric(n_mcmc),
                     replicates = as.numeric(replicates),
                     hosp_beds = as.numeric(hosp_beds),
                     icu_beds = as.numeric(icu_beds),
                     model = model,
                     pars_obs_dur_R = as.numeric(dur_R),
                     pars_obs_prob_hosp_multiplier = as.numeric(prob_hosp_multiplier),
                     vacc_inputs = vacc_inputs,
                     mix_mat = squire::contact_matrices[[as.integer(mix_mat)]],
                     odriscoll = as.logical(odriscoll)
                     )


# add state for ease and remove the output for memory
res$parameters$province <- province
res$pmcmc_results$inputs$pars_obs$sero_det <- sero_det
output <- res$output
res$output <- NULL

# save output without output for memory
saveRDS(res, "res.rds")
res$output <- output

# make a quick plot so we can check fits easily
if (model == "SQUIRE") {
rtp <- rt_plot_immunity(res)
} else {
  rtp <- rt_plot_immunity_vaccine(res)
}
dp <- dp_plot(res)
cdp <- cdp_plot(res)
sero <- sero_plot(res, sero_df)
ar <- ar_plot(res)

ggsave("fitting.pdf",width=12, height=15,
       cowplot::plot_grid(rtp$plot + ggtitle(province) + scale_x_date(date_labels = "%b %Y", date_breaks = "3 months"),
                          dp, cdp, sero, ar, ncol = 1, align = "v"))


