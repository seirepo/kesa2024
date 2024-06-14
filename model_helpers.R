calc_reaction_constant <- function(dat) {
  # Calculate the reaction rate constant k (as in Eq. 3 in paper A statistical proxy for sulphuric acid concentration by Mikkonen et al.)
  M <- 0.101 * (1.381 * 1e-23 * dat$temp_K)^-1
  k1 <- 4e-31
  k2 <- 3.3
  k3 <- 2e-12
  k5 <- -0.8
  A <- k1 * M * (300 / dat$temp_K)^k2
  k <- A * k3 / (A + k3) * exp(k5 * (1 + log10(A / k3)^2)^-1)
  return(k)
}

create_datasets <- function(dat) {
  dat <- dat %>% mutate(temp_K = temperature + 273.15) %>% select(-wind_direction, -temperature) # unfiltered data
  dat_filtered <- dat %>% filter(global_radiation > 10 & SO2 > 0.1) # filtered data
  k <- calc_reaction_constant(dat_filtered)
  all_features <- dat_filtered %>% mutate(k = k) %>%
    mutate(x1 = k * global_radiation * SO2 / CS_rate) %>%
    mutate(x2 = k * global_radiation * SO2) %>%
    mutate(x3 = k * global_radiation * SO2^0.5) %>%
    mutate(x4 = k * global_radiation * SO2 / relative_humidity) %>%
    mutate(x5 = k * global_radiation * SO2 / (CS_rate * relative_humidity))
  
  all_proxies <- all_features %>% select(Time, SA_cm3, x1, x2, x3, x4, x5)
  
  l1 <- select(all_proxies, Time, SA_cm3, x1) %>% drop_na
  l2 <- select(all_proxies, Time, SA_cm3, x2) %>% drop_na
  l3 <- select(all_proxies, Time, SA_cm3, x3) %>% drop_na
  l4 <- select(all_proxies, Time, SA_cm3, x4) %>% drop_na
  l5 <- select(all_proxies, Time, SA_cm3, x5) %>% drop_na
  
  dat_filtered <- dat_filtered %>% drop_na
  dat <- dat %>% drop_na
  
  # Return datasets for all of the proxies separately and then combined, for filtered data with and without proxies and the unfiltered data
  l <- list(l1, l2, l3, l4, l5, all_proxies, dat_filtered, all_features, dat)
  names(l) <- list("l1", "l2", "l3", "l4", "l5", "all_proxies", "dat_no_proxies", "all_features", "dat_unfiltered")
  return(l)
}