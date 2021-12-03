
## -----------------------------------------------------------------------------
## A. BUNDLE CREATION
## -----------------------------------------------------------------------------

# how long is the mcmc for
n_mcmc <- 20000

# should tasks be run in parallel and use multiple chains.
# leave this as FALSE and see FAQs for more info on this
model <- "NIMUE"
replicates <- 10
rf <- 1
date <- "2021-10-29"
mix_mat <- rep(10, 31)
mix_mat[c(3,11,12,13,23,24,31)] <- 12

# get the states
provinces <- c(
  "East Azerbaijan","West Azerbaijan","Ardabil","Isfahan","Alborz",
  "Ilam","Bushehr","Tehran","Chahar Mahaal and Bakhtiari","South Khorasan",
  "Razavi Khorasan","North Khorasan","Khuzestan","Zanjan","Semnan",
  "Sistan and Baluchistan","Fars","Qazvin","Qom","Kurdistan","Kerman",
  "Kermanshah","Kohgiluyeh and Boyer-Ahmad","Golestan","Gilan","Lorestan",
  "Mazandaran","Markazi","Hormozgan","Hamedan","Yazd"
)


# varying between high and low seroreversion
# https://www.science.org/doi/10.1126/science.abj9932
# 50%, 27%, 10%
# 1/((60*(1/365) - log(0.73))/60)
dur_R <- c(70, 125, 222)

# increased probability of hospitalisation due to Delta
# 1·45 [1·08–1·95] https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(21)00475-8/fulltext
prob_hosp_multiplier <- c(1.95, 1.45, 1.08)

# Hospital capacity impact on IFR.
# 0.5 = 50% beds are available
# 1 = 100% beds available
# 10 will mean no bed limits reached essentially.
# no limits, multiplier = 10, default = 1, worst = 0.5
hosp_capacity_multiplier <- c(0.5, 1, 10)

# make the orderly bundles to be run on the cluster
path_bundles <- c(
  cp_path("analysis/orderly_bundles/raw_vaccine_complete_worst"),
  cp_path("analysis/orderly_bundles/raw_vaccine_complete_central"),
  cp_path("analysis/orderly_bundles/raw_vaccine_complete_best")
)

for(i in 1:3) {

  dir.create(path_bundles[i], showWarnings = FALSE)

  # bundle these up - this will take like 2 mins to create all the zips.
  bundles <- lapply(
    seq_along(provinces), function(x) {
      orderly::orderly_bundle_pack(
        path = path_bundles[i],
        name = "prov_fit",
        parameters = list(
          province = provinces[x],
          replicates=replicates,
          dur_R=dur_R[i],
          model=model,
          prob_hosp_multiplier=prob_hosp_multiplier[i],
          hosp_capacity_multiplier=hosp_capacity_multiplier[i],
          n_mcmc=n_mcmc,
          date=date,
          mix_mat=mix_mat[x]
        )
      )
    }
  )

  # now label these with the states and save the file paths
  names(bundles) <- provinces
  saveRDS(bundles, file.path(path_bundles[i], "bundles.rds"))

}

## -----------------------------------------------------------------------------
## B. CLUSTER SUBMITTING
## -----------------------------------------------------------------------------

# Setting Up Cluster From New

# Log in to didehpc
credentials = "C:/Users/ow813/.smbcredentials"
options(didehpc.cluster = "fi--didemrchnb",
        didehpc.username = "ow813")

## ------------------------------------
## 1. Package Installations
## ------------------------------------
# drat:::add("mrc-ide")
# install.packages("pkgdepends")
# install.packages("didehpc")
# install.packages("orderly")

## ------------------------------------
## 2. Setting up a cluster configuration
## ------------------------------------

options(didehpc.cluster = "fi--didemrchnb")

# not if T is not mapped then map network drive
didehpc::didehpc_config_global(temp=didehpc::path_mapping("tmp",
                                                          "T:",
                                                          "//fi--didef3.dide.ic.ac.uk/tmp",
                                                          "T:"),
                               home=didehpc::path_mapping("OJ",
                                                          "L:",
                                                          "//fi--didenas5/malaria",
                                                          "L:"),
                               credentials=credentials,
                               cluster = "fi--didemrchnb")

# Creating a Context
context_name <- cp_path("context")

ctx <- context::context_save(
  path = context_name,
  package_sources = conan::conan_sources(
    packages = c(
      "vimc/orderly", "mrc-ide/squire@v0.6.10", "mrc-ide/nimue",
      c('tinytex','knitr', 'tidyr', 'ggplot2', 'ggrepel', 'magrittr', 'dplyr', 'here', "lubridate", "rmarkdown",
        'stringdist','plotly', 'rvest', 'xml2', 'ggforce', 'countrycode', 'cowplot', 'RhpcBLASctl', 'nimue')
    ),
    repos = "https://ncov-ic.github.io/drat/")
)

# set up a specific config for here as we need to specify the large RAM nodes
config <- didehpc::didehpc_config(template = "24Core")
config$resource$parallel <- "FALSE"
config$resource$type <- "Cores"

# Configure the Queue
obj <- didehpc::queue_didehpc(ctx, config = config)

## ------------------------------------
## 3. Submit the jobs
## ------------------------------------

submit_jobs <- function(path_bundles) {

workdir <- gsub("raw", "derived", path_bundles)
dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
workdir <- normalizePath(workdir)

# Grabbing tasks to be run
tasks <- readRDS(gsub("derived", "raw", file.path(workdir, "bundles.rds")))
tasks <- as.character(vapply(tasks, "[[", character(1), "path"))

# submit our tasks to the cluster
split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
bundle_name <- paste0(tail(rev(split_path(workdir)), 2), collapse = "_")
grp <- obj$lapply(tasks, orderly::orderly_bundle_run, workdir = workdir,
                  name = bundle_name, overwrite = TRUE)

return(grp)

}

grp_worst <- submit_jobs(cp_path("analysis/orderly_bundles/raw_vaccine_complete_worst"))
grp_central <- submit_jobs(cp_path("analysis/orderly_bundles/raw_vaccine_complete_central"))
grp_best <- submit_jobs(cp_path("analysis/orderly_bundles/raw_vaccine_complete_best"))

## ------------------------------------
## 4. Check on our jobs
## ------------------------------------

# check on their status
status <- grp$status()
table(status)

# see what has errorred
errs <- lapply(seq_along(which(status == "ERROR")), function(x){
  grp$tasks[[which(status == "ERROR")[x]]]$log()$body
})

# sometimes tasks say running or completed when in fact they have errored:
didehpc:::reconcile(obj, grp$ids)
status <- grp$status()
errs <- lapply(seq_along(which(status == "ERROR")), function(x){
  grp$tasks[[which(status == "ERROR")[x]]]$log()$body[[19]]
})

# do we just need to rerun some of the bundles
to_rerun <- which(grp$status() == "ERROR")
unlink(gsub("\\.zip", "", gsub("raw", "derived", grp$X[to_rerun])), recursive = TRUE)
obj$submit(grp$ids[to_rerun])

## ------------------------------
## 5. Functions to extract objects from zips for checking
## ------------------------------

# use the bundles paths to work out the path to the runs in derived

make_fitting_pdf <- function(grp_grab) {

paths <- gsub("raw", "derived", grp_grab$X[grp_grab$status() == "COMPLETE"])

# now extract the fitting.pdf files
td <- file.path(tempdir(), grp_grab$name, uuid::UUIDgenerate())
dir.create(td, showWarnings = FALSE, recursive = TRUE)

fits <- lapply(paths, function(x) {
  if(file.exists(x)){
    zip::unzip(
      zipfile = x,
      files = file.path(gsub("\\.zip", "", basename(x)), "pack/fitting.pdf"),
      exdir = td
    )
  }
})

# get the filepaths for these
pdfs <- grep("fitting", list.files(td, full.names = TRUE, recursive = TRUE), value = TRUE)

# combine the files that are larger than 0b. Ob files are for countries that have
# no COVID-19 deaths to date and as such don't have a fitting.pdf but this file is
# created because it needs to be for orderly to finish the task
qpdf::pdf_combine(
  input = pdfs[file.size(pdfs) > 0],
  output = cp_path(paste0("analysis/plots/fitting_vacc_", grp_grab$name,".pdf"))
)

}

make_fitting_pdf(grp_best)
make_fitting_pdf(grp_central)
make_fitting_pdf(grp_worst)

## ------------------------------
## 6. Update pars_init to rerun
## ------------------------------





## ------------------------------
## 7. Check individual fits/diagositcs etc
## ------------------------------

fetch_res <- function(grp, i) {

res <- readRDS(vapply(gsub("raw", "derived", grp$X[i]), function(x) {
  td <- tempdir()
  zip::unzip(
    zipfile = x,
    files = file.path(gsub("\\.zip", "", basename(x)), "pack/res.rds"),
    exdir = td
  )
  grep(paste0(gsub(".zip", "", basename(x),fixed = TRUE),".*res.rds$"),
       list.files(td, full.names = TRUE, recursive = TRUE),
       value = TRUE)
}, character(1)))

return(res)
}

plot(res$pmcmc_results$results$log_posterior[seq(1, 50000, 100)])

res$pmcmc_results$chains$chain1$results[1,]

pars <- res$pmcmc_results$chains$chain1$results[1,]
pars$start_date <- squire:::offset_to_start_date(res$pmcmc_results$inputs$data$date[1],start_date = pars$start_date)
source("src/prov_fit/R/spline_fit.R")
source("src/prov_fit/R/vaccine.R")
source("src/prov_fit/R/plotting.R")
ll <- iran_log_likelihood(pars = pars,
                    data = res$pmcmc_results$inputs$data,
                    squire_model = res$pmcmc_results$inputs$squire_model,
                    model_params = res$pmcmc_results$inputs$model_params,
                    pars_obs = res$pmcmc_results$inputs$pars_obs,
                    n_particles = 2, forecast_days = 0, return = "ll",
                    Rt_args = res$pmcmc_results$inputs$Rt_args,
                    interventions = res$pmcmc_results$inputs$interventions)
ll$log_likelihood
ll$sample_state

## generate draws
out <- generate_draws(res, draws = 5, burnin = 1000, parallel = FALSE)
plot(out, "ICU_occupancy", date_0 = "2021-09-03", x_var = "date")

deaths <- squire::format_output(out, "deaths", date_0 = "2021-05-01")
deaths$y <- deaths$y*7
deaths_gg <- deaths %>% group_by(date) %>%
  summarise(ymin = quantile(y, 0.025, na.rm=TRUE),
            ymax = quantile(y, 0.975, na.rm=TRUE),
            y = median(y, na.rm=TRUE)) %>%
ggplot(aes(date, y*(100000/sum(out$parameters$population)),
           ymin = ymin*(100000/sum(out$parameters$population)),
           ymax = ymax*(100000/sum(out$parameters$population)))) +
  geom_ribbon(alpha = 0.5) +
  geom_line() +
  geom_point(aes(date, deaths*(100000/sum(out$parameters$population))),
             data = out$pmcmc_results$inputs$data, inherit.aes = FALSE) +
  theme_bw() +
  ylab("Weekly Deaths / 100,000") +
  xlab("Date") + ggtitle("Hormozgan")


deaths_age <- squire::format_output(out, "D", reduce_age = FALSE, date_0 = "2021-05-01")
deaths_age %>% filter(date == "2021-05-01") %>%
  group_by(age_group) %>%
  summarise(deaths = median(y)) %>%
  mutate(deaths_per_cap = deaths * (100000/out$parameters$population)) %>%
  mutate(ages = (squire::population$age_group[1:17])) %>%
  ggplot(aes(ages, deaths_per_cap)) +
  geom_point() +
  theme_bw() +
  xlab("Age Group") +
  ylab("Deaths / 100,000") +
  scale_y_log10()

S <- squire::format_output(out, "S", reduce_age = FALSE, date_0 = "2021-05-01")
infs <- squire::format_output(out, "infections", reduce_age = FALSE, date_0 = "2021-05-01")
left_join(
  S %>% group_by(age_group, replicate) %>%
  summarise(s_max = max(y,na.rm= TRUE)),
  infs %>% group_by(age_group, replicate) %>%
    summarise(infs = sum(y,na.rm= TRUE))
) %>%
  mutate(ar = infs/s_max) %>%
  group_by(age_group) %>%
  summarise(ar_min = quantile(ar, 0.025),
            ar_med = median(ar),
            ar_max = quantile(ar, 0.975)) %>%
  mutate(ages = (squire::population$age_group[1:17])) %>%
  ggplot(aes(ages, ar_med, ymin = ar_min, ymax = ar_max )) +
  geom_pointrange() +
  theme_bw() +
  ylab("Attack Rate") +
  xlab("Ages")


S <- squire::format_output(out, "S", date_0 = "2021-05-01")
infs <- squire::format_output(out, "infections", date_0 = "2021-05-01")
left_join(
  S %>% group_by(replicate) %>%
    summarise(s_max = max(y,na.rm= TRUE)),
  infs %>% group_by(replicate) %>%
    summarise(infs = sum(y,na.rm= TRUE))
) %>%
  mutate(ar = infs/s_max) %>%
  summarise(ar_min = quantile(ar, 0.025),
            ar_med = median(ar),
            ar_max = quantile(ar, 0.975)) %>%
  mutate(ages = "All") %>%
  ggplot(aes(ages, ar_med, ymin = ar_min, ymax = ar_max )) +
  geom_pointrange() +
  theme_bw() +
  ylab("Attack Rate")





(max(nimue::format(out, "D")$value, na.rm=TRUE) / diff(range(nimue::format(out, "S")$value, na.rm=TRUE)))*100

plot(out, "ICU_occupancy", x_var = "date", date_0 = "2021-05-01")+ ylab("ICU Occupancy") + xlab("Date") +
  theme(legend.position = "none")
