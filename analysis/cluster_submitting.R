
## -----------------------------------------------------------------------------
## A. BUNDLE CREATION
## -----------------------------------------------------------------------------

# how long is the mcmc for
n_mcmc <- 50000

# should tasks be run in parallel and use multiple chains.
# leave this as FALSE and see FAQs for more info on this
model <- "NIMUE"
replicates <- 10
rf <- 1
date <- "2021-10-29"
mix_mat <- rep(10, 31)
mix_mat[c(3,11,12,13,23,24,31)] <- 12
odriscoll <- TRUE

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
  cp_path("analysis/orderly_bundles/raw_vaccine_new_odriscoll_worst"),
  cp_path("analysis/orderly_bundles/raw_vaccine_new_odriscoll_central"),
  cp_path("analysis/orderly_bundles/raw_vaccine_new_odriscoll_best")
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
          mix_mat=mix_mat[x],
          odriscoll=odriscoll
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

grp_worst <- submit_jobs(path_bundles[1])
grp_central <- submit_jobs(path_bundles[2])
grp_best <- submit_jobs(path_bundles[3])

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
## 6. Make pdfs based on archived runs
## ------------------------------

make_fitting_pdf <- function(reps) {

  # get the filepaths for these
  pdfs <- file.path("archive/prov_fit", reps$report_version, "fitting.pdf")

  if(reps$odriscoll[1] == "false") {
  nm <- paste0("fitting_vacc_orderly_bundles_derived_vaccine_complete_",  tolower(reps$scenario[1]))
  } else {
    nm <- paste0("fitting_vacc_orderly_bundles_derived_vaccine_new_odriscoll_",  tolower(reps$scenario[1]))
  }
  # combine the files that are larger than 0b. Ob files are for countries that have
  # no COVID-19 deaths to date and as such don't have a fitting.pdf but this file is
  # created because it needs to be for orderly to finish the task
  qpdf::pdf_combine(
    input = pdfs[file.size(pdfs) > 0],
    output = cp_path(paste0("analysis/plots/",nm,".pdf"))
  )

}

# Group our model fits by their assumptions
library(tidyverse)
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
reports_here$odriscoll[is.na(reports_here$odriscoll)] <- "false"
reports_l <- split(reports_here, list(reports_here$scenario,reports_here$odriscoll))

pdf_outs <- lapply(reports_l, make_fitting_pdf)
