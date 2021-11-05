library(tidyverse)

# get prov data
df <- read.csv(cp_path("analysis/data/raw/iran_deaths_province.csv"))
df$year_nm <- df$year - 1398 + 2019
add_zero <- function(x){
  if(nchar(x) == 1) {
    x <- paste0("0", x)
  } else {
    x <- as.character(x)
  }
  return(x)
}
pos <- function(x){ x[x<0] <- 0; x}

df$week_num <- vapply(df$week_num, add_zero, character(1))
df$date <- as.Date(paste0(df$year_nm, df$week_num, 1), "%Y%W%w")
df$date <- df$date + (as.Date("2020-03-20") - df$date[df$year == 1399 & df$week_num == "01"][1])
df <- mutate(df, province_name = replace(province_name, which(province_name == "Kohgiluyeh and Boyer Ahmad"),"Kohgiluyeh and Boyer-Ahmad"))
df <- mutate(df, province_name = replace(province_name, which(province_name == "Chahaar Mahal and Bakhtiari"),"Chahar Mahaal and Bakhtiari"))
saveRDS(df, cp_path("analysis/data/derived/iran_deaths_province.rds"))

# format for fitting to
prov_df <- df %>%
  arrange(date) %>%
  group_by(province_name) %>%
  mutate(deaths_sm = zoo::rollapply(pos(excess_deaths_mean), 4, mean, partial = TRUE)) %>%
  select(date, deaths_sm, province_name, excess_deaths_mean) %>%
  rename(deaths = deaths_sm) %>%
  mutate(deaths = as.integer(deaths)) %>%
  rename(province = province_name) %>%
  mutate(deaths = replace_na(deaths, 0))

# check looks right
prov_df %>%
  ggplot(aes(date, deaths)) + geom_line() +
  #geom_point(aes(y = excess_deaths_mean)) +
  facet_wrap(~province, scales = "free_y")

# then save for fitting
saveRDS(prov_df, cp_path("src/prov_fit/deaths.rds"))


# get age data
df <- read.csv(cp_path("analysis/data/raw/iran_deaths_age_province.csv"))
df$year_nm <- df$year - 1398 + 2019
df$week_num <- vapply(df$week_num, add_zero, character(1))
df$date <- as.Date(paste0(df$year_nm, df$week_num, 1), "%Y%W%w")
df$date <- df$date + (as.Date("2020-03-20") - df$date[df$year == 1399 & df$week_num == "01"][1])
df <- mutate(df, province_name = replace(province_name, which(province_name == "Kohgiluyeh and Boyer Ahmad"),"Kohgiluyeh and Boyer-Ahmad"))
df <- mutate(df, province_name = replace(province_name, which(province_name == "Chahaar Mahal and Bakhtiari"),"Chahar Mahaal and Bakhtiari"))
df$age_group <- squire::population$age_group[as.integer(factor(df$age_group, levels = levels(as.factor(df$age_group))[c(1,10,2:9,11:17)]))]
saveRDS(df, cp_path("analysis/data/derived/iran_deaths_age_province.rds"))

# check looks right
prov_df %>%
  ggplot(aes(date, deaths)) + geom_line() +
  #geom_point(aes(y = excess_deaths_mean)) +
  facet_wrap(~province, scales = "free_y")




## TO DELETE
## --------------------------------v
pos <- function(x) {x[x<0] <- 0; x}

deaths %>%
  arrange(date) %>%
  group_by(province_name) %>%
  mutate(deaths_sm = zoo::rollmean(pos(excess_deaths_mean), 4, na.pad = TRUE)) %>%
ggplot(aes(date, deaths_sm)) +
  geom_line() +
  facet_wrap(~province_name, scales = "free_y")


interp_cumu_deaths <- function(dated_df, k = 80) {

  df <- data.frame(x = seq(nrow(dated_df)), y = cumsum(dated_df$excess_deaths_mean))

  ## This fits the unconstrained model but gets us smoothness parameters that
  ## that we will need later
  unc <- mgcv::gam(y ~ s(x, k = k, bs = "cr"), data = df)

  ## This creates the cubic spline basis functions of `x`
  ## It returns an object containing the penalty matrix for the spline
  ## among other things; see ?smooth.construct for description of each
  ## element in the returned object
  sm <- mgcv::smoothCon(mgcv::s(x, k = k, bs = "cr"), df, knots = NULL)[[1]]

  ## This gets the constraint matrix and constraint vector that imposes
  ## linear constraints to enforce montonicity on a cubic regression spline
  ## the key thing you need to change is `up`.
  ## `up = TRUE` == increasing function
  ## `up = FALSE` == decreasing function (as per your example)
  ## `xp` is a vector of knot locations that we get back from smoothCon
  CF <- mgcv::mono.con(sm$xp, up = TRUE)   # get constraints: up = FALSE == Decreasing con

  ## Fill in G, the object pcsl needs to fit; this is just what `pcls` says it needs:
  ## X is the model matrix (of the basis functions)
  ## C is the identifiability constraints - no constraints needed here
  ##   for the single smooth
  ## sp are the smoothness parameters from the unconstrained GAM
  ## p/xp are the knot locations again, but negated for a decreasing function
  ## y is the response data
  ## w are weights and this is fancy code for a vector of 1s of length(y)
  G <- list(X = sm$X, C = matrix(0,0,0), sp = unc$sp,
            p = sm$xp, # note the - here! This is for decreasing fits!
            y = df$y,
            w = df$y*0+1)
  G$Ain <- CF$A    # the monotonicity constraint matrix
  G$bin <- CF$b    # the monotonicity constraint vector, both from mono.con
  G$S <- sm$S     # the penalty matrix for the cubic spline
  G$off <- 0      # location of offsets in the penalty matrix

  ## Do the constrained fit
  p <- mgcv::pcls(G)  # fit spline (using s.p. from unconstrained fit)

  ## predict at 100 locations over range of x - get a smooth line on the plot
  newx <- with(df, data.frame(x = df$x))
  fv <- mgcv::Predict.matrix(sm, newx) %*% p
  newx <- transform(newx, yhat = fv[,1])

  out_df <- data.frame(date = dated_df$date,
                       deaths = c(0, diff(newx$yhat)))

  return(out_df)

}

deaths <- df
prov_dfs <- list()
k <- rep(13, length(unique(deaths$province_name)))
k <- set_names(k, sort(unique(deaths$province_name)))
k[c("Gilan", "Kerman", "Markazi")] <- 15
k["Qom"] <- 20
set.seed(123)
for(i in sort(unique(deaths$province_name))) {

  death_df <- deaths[deaths$province_name == i, ]
  death_df <- death_df[, c("date","excess_deaths_mean")]
  death_df <- death_df[death_df$date >= as.Date("2019-11-08"), ]
  # poor baseline impacts spline and we know there were infections prior to August from sero
  if(i == "Sistan and Baluchistan") {
    death_df$excess_deaths_mean[death_df$date < as.Date("2020-03-17")] <- 0
  } else {
    death_df$excess_deaths_mean[death_df$date < as.Date("2019-12-28")] <- 0
  }
  death_df_new <- interp_cum_deaths(death_df, k = k[i])
  death_df$province <- i
  death_df$deaths_interp <- death_df_new$deaths
  prov_dfs[[i]] <- death_df

}
prov_df <- do.call(rbind, prov_dfs)



df2 <- left_join(df, demog)

pos <- function(x){ x[x<0] <- 0; x}
# plot of deaths
df2 %>% group_by(date, province_name) %>%
  summarise(deaths = sum(excess_deaths_mean)) %>%
  ungroup %>%
  group_by(province_name) %>%
  mutate(deaths_pos = cumsum(pos(deaths)),
         deaths = cumsum(deaths)) %>%
  ggplot(aes(as.Date(date), deaths)) +
  geom_line() +
  geom_line(aes(as.Date(date), deaths_pos), color = "red") +
  facet_wrap(~province_name, scales = "free_y")

# plot of deaths
df2 %>% group_by(date, province_name) %>%
  summarise(deaths = sum(excess_deaths_mean)) %>%
  ungroup %>%
  group_by(province_name) %>%
  mutate(d = pos(deaths),
         deaths_pos = cumsum(pos(deaths)),
         deaths = cumsum(deaths)) %>%
  ggplot(aes(as.Date(date), d/7)) +
  geom_line() +
  facet_wrap(~province_name, scales = "free_y")


ifr_mod <- readRDS(file.path("/home/oj/GoogleDrive/AcademicWork/covid/githubs/global-lmic-reports-orderly/analysis/spare_capacity/ita_log_model.RDS"))
ifrs <- exp(predict(ifr_mod$lognormmod, data.frame(age = seq(2.5, 82.5, 5))))
df3 <- left_join(df2, data.frame("age_group" = demog$age_group[1:17], ifr = ifrs))

#
df3 %>% group_by(age_group, province_name) %>%
  mutate(deaths = cumsum(pos(excess_deaths_mean))) %>%
  mutate(infs = deaths / ifr) %>%
  group_by(province_name, date) %>%
  summarise(n = sum(n),
            deaths = sum(deaths),
            infs = sum(infs),
            ar = infs/n) %>%
  ggplot(aes(date, infs)) +
  geom_line() +
  facet_wrap(~province_name, scales = "free_y")

df3 %>% group_by(date, province_name) %>%
  summarise(n = sum(n),
            excess_deaths_mean = sum(pos(excess_deaths_mean)),
            excess_deaths_high = sum(pos(excess_deaths_high)),
            excess_deaths_low = sum(pos(excess_deaths_low))) %>%
  ggplot(aes(date, (excess_deaths_mean)/(n/100000), ymin = (excess_deaths_high)/(n/100000), ymax = (excess_deaths_low)/(n/100000))) +
  geom_smooth(span = 0.075) + geom_ribbon(alpha = 0.2) +
  facet_wrap(~province_name, scales = "free_y")


#

df3 %>% group_by(age_group, province_name) %>%
  mutate(deaths = cumsum((excess_deaths_mean))) %>%
  mutate(infs = deaths / ifr) %>%
  group_by(province_name, age_group, date) %>%
  summarise(n = sum(n),
            deaths = sum(deaths),
            infs = sum(infs),
            ar = infs/n) %>%
  filter(province_name == "Alborz") %>%
  ggplot(aes(date, deaths)) +
  geom_line() +
  facet_wrap(~age_group, scales = "free_y")
