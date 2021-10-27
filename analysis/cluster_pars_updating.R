get_best <- function(out) {

  if("pmcmc_results" %in% names(out)) {
    if("chains" %in% names(out$pmcmc_results)) {
      mc <- do.call(rbind, lapply(out$pmcmc_results$chains, "[[", "results"))
    } else {
      mc <- out$pmcmc_results$results
    }
    best <- mc[which.max(mc$log_posterior),]
    best <- best[,seq_len(ncol(best)-3)]
    rownames(best) <- NULL
    best$start_date <- as.character(squire:::offset_to_start_date(out$pmcmc_results$inputs$data$date[1], round(best$start_date)))
    best$date_Meff_change <- out$pmcmc_results$inputs$Rt_args$date_Meff_change
    best$Rt_shift_duration <- out$pmcmc_results$inputs$Rt_args$Rt_shift_duration
    best$Rt_rw_duration <- out$pmcmc_results$inputs$Rt_args$Rt_rw_duration

    if("chains" %in% names(out$pmcmc_results)) {
      best$covariance_matrix <- out$pmcmc_results$chains$chain1$covariance_matrix[1]
      best$scaling_factor <- mean(
        c(tail(na.omit(out$pmcmc_results$chains$chain1$scaling_factor),1),
          tail(na.omit(out$pmcmc_results$chains$chain2$scaling_factor),1),
          tail(na.omit(out$pmcmc_results$chains$chain3$scaling_factor),1))
      )
    } else {
      best$covariance_matrix <- out$pmcmc_results$covariance_matrix[1]
      best$scaling_factor <- tail(na.omit(out$pmcmc_results$scaling_factor),1)
    }

    best$province <- out$parameters$province

    return(best)
  }
}

# central pars
pars_list <- lapply(grp$X[grp$status()=="COMPLETE"], function(x) {
  out <- readRDS(
    gzcon(
      unz(
        gsub("raw", "derived", x),
        file.path(gsub("\\.zip", "", basename(x)), "pack/res.rds")
      )
    )
  )
  get_best(out)
})

pars_list2 <- lapply(grp_short$X[grp$status() == "ERROR" & grp_short$status() == "COMPLETE"], function(x) {
  out <- readRDS(
    gzcon(
      unz(
        gsub("raw", "derived", x),
        file.path(gsub("\\.zip", "", basename(x)), "pack/res.rds")
      )
    )
  )
  get_best(out)
})

# get the old conditions
pars_init <- readRDS(file.path(here::here(), "src/prov_fit/pars_init.rds"))

names(pars_list) <- unlist(lapply(pars_list, "[[", "province"))
names(pars_list2) <- unlist(lapply(pars_list2, "[[", "province"))
pars_init$central[match(names(pars_list), names(pars_init$central))] <- pars_list
pars_init$central[match(names(pars_list2), names(pars_init$central))] <- pars_list2

pars_init$optimistic <- pars_init$central
pars_init$worst <- pars_init$central

# save to location
saveRDS(pars_init, file.path(here::here(), "src/prov_fit/pars_init.rds"))
