devtools::install_github("mrc-ide/squire")

# keep 2.5 as lower age multiplier
ifr_mult <- c(2.5,2.5,2.5,2.7,2.5,2.3,2.3,2.1,1.9,1.9,1.8,1.7,1.6,1.5,1.4,1.3,1.3)

r1 <- squire::run_deterministic_SEIR_model("Iran", hosp_bed_capacity = 1e10, ICU_bed_capacity = 1e10, day_return = TRUE)

# base ifr from brazeau 0.33
(max(squire::format_output(r1, "D")$y) / diff(range(squire::format_output(r1, "S")$y)))*100

# with hosp constraints
r2 <- squire::run_deterministic_SEIR_model("Iran", day_return = TRUE)

# base ifr from brazeau plus hosp constriants 0.71
(max(squire::format_output(r2, "D")$y) / diff(range(squire::format_output(r2, "S")$y)))*100

# constraints meant that for ~50 days not enough ICU beds and about 10 days not enough general beds/oxygen
plot(r2, "ICU_occupancy")
plot(r2, "hospital_occupancy")

# see what new levin paper would be
r3 <- squire::run_deterministic_SEIR_model(
  "Iran", hosp_bed_capacity = 1e10, ICU_bed_capacity = 1e10, day_return = TRUE,
  prob_non_severe_death_treatment = squire::default_probs()$prob_non_severe_death_treatment * ifr_mult,
  prob_severe_death_treatment = mapply(min, squire::default_probs()$prob_severe_death_treatment * ifr_mult,0.95)
)

# ifr from levin 0.53
(max(squire::format_output(r3, "D")$y) / diff(range(squire::format_output(r3, "S")$y)))*100
