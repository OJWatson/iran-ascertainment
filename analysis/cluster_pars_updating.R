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

    best$covariance_matrix[[1]][,] <- 0.03
    best$scaling_factor <- 1
    best$province <- out$parameters$province

    return(best)
  }
}

# best pars
pars_list_best <- lapply(grp_best$X[grp_best$status()=="COMPLETE"], function(x) {
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

# central pars
pars_list_central <- lapply(grp_central$X[grp_central$status()=="COMPLETE"], function(x) {
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

# worst pars
pars_list_worst <- lapply(grp_worst$X[grp_worst$status()=="COMPLETE"], function(x) {
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

names(pars_list_best) <- unlist(lapply(pars_list_best, "[[", "province"))
names(pars_list_central) <- unlist(lapply(pars_list_central, "[[", "province"))
names(pars_list_worst) <- unlist(lapply(pars_list_worst, "[[", "province"))

pars_init$optimistic[match(names(pars_list_best), names(pars_init$optimistic))] <- pars_list_best
pars_init$central[match(names(pars_list_central), names(pars_init$central))] <- pars_list_central
pars_init$worst[match(names(pars_list_worst), names(pars_init$worst))] <- pars_list_worst

pars_init$optimistic_odriscoll[match(names(pars_list_best), names(pars_init$optimistic_odriscoll))] <- pars_list_best
pars_init$central_odriscoll[match(names(pars_list_central), names(pars_init$central_odriscoll))] <- pars_list_central
pars_init$worst_odriscoll[match(names(pars_list_worst), names(pars_init$worst_odriscoll))] <- pars_list_worst


# save to location
saveRDS(pars_init, file.path(here::here(), "src/prov_fit/pars_init.rds"))


# Manual tweaking

source("src/prov_fit/R/spline_fit.R")
source("src/prov_fit/R/vaccine.R")
source("src/prov_fit/R/plotting.R")

plot_pars_deaths <- function(res, pars) {

  ll <- iran_log_likelihood(pars = pars,
                            data = res$pmcmc_results$inputs$data,
                            squire_model = res$pmcmc_results$inputs$squire_model,
                            model_params = res$pmcmc_results$inputs$model_params,
                            pars_obs = res$pmcmc_results$inputs$pars_obs,
                            n_particles = 2, forecast_days = 0, return = "full",
                            Rt_args = res$pmcmc_results$inputs$Rt_args,
                            interventions = res$pmcmc_results$inputs$interventions)

  index <- nimue:::odin_index(res$model)
  deaths <- diff(rowSums(ll[,index$D]))
  df <- data.frame("deaths" = deaths, date = as.Date(names(deaths)))
  ggplot(df, aes(date, deaths)) + geom_line() +
    geom_point(aes(date, deaths/7), res$pmcmc_results$inputs$data) +
    geom_smooth(aes(date, deaths/7), res$pmcmc_results$inputs$data, span = 0.1)

}

get_ll <- function(res, pars) {

  ll <- iran_log_likelihood(pars = pars,
                            data = res$pmcmc_results$inputs$data,
                            squire_model = res$pmcmc_results$inputs$squire_model,
                            model_params = res$pmcmc_results$inputs$model_params,
                            pars_obs = res$pmcmc_results$inputs$pars_obs,
                            n_particles = 2, forecast_days = 0, return = "ll",
                            Rt_args = res$pmcmc_results$inputs$Rt_args,
                            interventions = res$pmcmc_results$inputs$interventions)$log_likelihood
  return(ll)

}

fix_ll <- function(res, pars, min = 0.5, max = 2, n_span = 8) {

  # get the full data we are fitting to
  data_full <- res$pmcmc_results$inputs$data
  days <- 0

  # this was eventually going to be used to check which weeks are affected by which Rt changes
  # but mine are all pegged to a start date the same number of days before the first week of deaths
  # so this isn't needed
  days_impacted_by_rt <- lapply(seq_len(length(grep("Rt_rw", names(pars)))), function(x) {
    (1 + 2 + ((x - 1) * 14)) : (1 + 2 + ((x) * 14)) + 20
  })

  # loop over 2 week periods and scan over changing the relevant Rt_rw and updating in pars
  for(i in seq(2, nrow(data_full),2)) {

    pars_i <- pars
    data_i <- data_full[seq_len(i),]
    days <- seq(max(days),  max(days) + as.integer(max(data_i$date) - max(pars$start_date)), 1)

    ll <- iran_log_likelihood(pars = pars,
                              data = data_i,
                              squire_model = res$pmcmc_results$inputs$squire_model,
                              model_params = res$pmcmc_results$inputs$model_params,
                              pars_obs = res$pmcmc_results$inputs$pars_obs,
                              n_particles = 2, forecast_days = 0, return = "ll",
                              Rt_args = res$pmcmc_results$inputs$Rt_args,
                              interventions = res$pmcmc_results$inputs$interventions)$log_likelihood

    poss <- seq(pars[[paste0("Rt_rw_", i/2)]] - min, pars[[paste0("Rt_rw_", i/2)]] + max, length.out = n_span)
    poss <- c(poss, pars[[paste0("Rt_rw_", i/2)]])
    ll_i <- c(rep(0, n_span), ll)

    for(j in seq_len(n_span)){
      pars_i[[paste0("Rt_rw_", i/2)]] <- poss[j]
      ll_i[j] <- iran_log_likelihood(pars = pars_i,
                                     data = data_i,
                                     squire_model = res$pmcmc_results$inputs$squire_model,
                                     model_params = res$pmcmc_results$inputs$model_params,
                                     pars_obs = res$pmcmc_results$inputs$pars_obs,
                                     n_particles = 2, forecast_days = 0, return = "ll",
                                     Rt_args = res$pmcmc_results$inputs$Rt_args,
                                     interventions = res$pmcmc_results$inputs$interventions)$log_likelihood
    }
    pars[[paste0("Rt_rw_", i/2)]] <- poss[which.max(ll_i)]
  }

  return(pars)

}

# --------------------------
## EA best
# --------------------------


res <- fetch_res(grp_best, 1)
pars <- res$pmcmc_results$results[which.max(res$pmcmc_results$results$log_posterior),]
pars$start_date <- squire:::offset_to_start_date(res$pmcmc_results$inputs$data$date[1],start_date = pars$start_date)

plot_pars_deaths(res, pars)
pars_new <- fix_ll(res, pars, min = 0.5, 0.5, n_span = 10)
pars_new2 <- fix_ll(res, pars_new, min = 0.1, 0.1, n_span = 10)
pars_new3 <- fix_ll(res, pars_new2, min = 0.05, 0.05, n_span = 10)

pars_init$worst$`Razavi Khorasan`[seq_len(length(pars)-3)] <- head(as.numeric(pars_new3[1,]), -3)
pars_init$worst$`Razavi Khorasan`$start_date <- lubridate::as_date(pars_init$worst$`Razavi Khorasan`$start_date)
