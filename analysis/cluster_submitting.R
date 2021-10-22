
## -----------------------------------------------------------------------------
## A. BUNDLE CREATION
## -----------------------------------------------------------------------------

# how long is the mcmc for
n_mcmc <- 50000

# should tasks be run in parallel and use multiple chains.
# leave this as FALSE and see FAQs for more info on this
model <- "SQUIRE"
dur_R <- 365
prob_hosp_multiplier <- 1
replicates <- 10
rf <- 1
date <- "2021-05-01"

# get the states
provinces <- c(
  "East Azerbaijan","West Azerbaijan","Ardabil","Isfahan","Alborz",
  "Ilam","Bushehr","Tehran","Chahar Mahaal and Bakhtiari","South Khorasan",
  "Razavi Khorasan","North Khorasan","Khuzestan","Zanjan","Semnan",
  "Sistan and Baluchistan","Fars","Qazvin","Qom","Kurdistan","Kerman",
  "Kermanshah","Kohgiluyeh and Boyer-Ahmad","Golestan","Gilan","Lorestan",
  "Mazandaran","Markazi","Hormozgan","Hamedan","Yazd"
)


# make the orderly bundles to be run on the cluster
path_bundles <- cp_path("analysis/orderly_bundles/raw")
dir.create(path_bundles, showWarnings = FALSE)

# bundle these up - this will take like 2 mins to create all the zips.
bundles <- lapply(
  provinces, function(x) {
    orderly::orderly_bundle_pack(
      path = path_bundles,
      name = "prov_fit",
      parameters = list(
        province = x,
        replicates=replicates,
        dur_R=dur_R,
        model=model,
        prob_hosp_multiplier=prob_hosp_multiplier,
        n_mcmc=n_mcmc,
        date=date
      )
    )
  }
)

# now label these with the states and save the file paths
names(bundles) <- provinces
saveRDS(bundles, file.path(path_bundles, "bundles.rds"))

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
obj <- didehpc::queue_didehpc(ctx, config = config, provision = "lazy")

## ------------------------------------
## 3. Submit the jobs
## ------------------------------------

workdir <- cp_path("analysis/orderly_bundles/derived")
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
## 7. Functions to extract objects from zips for checking
## ------------------------------

# use the bundles paths to work out the path to the runs in derived
paths <- gsub("raw", "derived", tasks)

# now extract the fitting.pdf files
td <- file.path(tempdir(), "pdfs3")
dir.create(td, showWarnings = FALSE)
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
  output = cp_path("analysis/plots/fitting.pdf")
)
