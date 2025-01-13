vaccine_profile <- function(name,
                            il_t0,
                            boost,
                            ab_50,
                            ab_50_severe,
                            ab_50_death,
                            k,
                            dr_s,
                            dr_l,
                            period_s,
                            future_vfr,
                            immune_escape,
                            vaccine,
                            matched_vacc,
                            matched_vacc_level,
                            matched_vacc_multiplier,
                            t_boost,
                            t_future_vfr,
                            max_t) {
  
  t <- 0:max_t
  
  numerator <- log10(exp(dr_s * t + dr_l * period_s) + exp(dr_l * t + dr_s *
                                                             period_s))
  denominator <- log10(exp(dr_l * period_s) + exp(dr_s * period_s))
  cum_dr_vec <-  numerator - denominator
  
  # infection/vaccine vector
  if (vaccine == 1) {
    if (matched_vacc == 1){boost = boost + matched_vacc_multiplier}
    titre <- c(rep(il_t0, t_boost), rep(boost, max_t - t_boost+1))
  } else {
    titre <- c(rep(il_t0, max_t + 1))
  }
  
  # decay vector
  if (vaccine == 1) {
    decay_vec <- c(cum_dr_vec[1:t_boost], cum_dr_vec[1:(max_t-t_boost+1)])
  } else {
    decay_vec <- c(cum_dr_vec[1:(max_t+1)])
  }
  
  # vfr vector
  vfr_vec <- c(rep(0, t_future_vfr), rep(future_vfr, (max_t - t_future_vfr + 1)))
  
  # combine
  immune_level <- titre + decay_vec - vfr_vec
  
  ef_infection <- 1 / (1 + exp(-k * (immune_level - ab_50)))
  ef_severe <- 1 / (1 + exp(-k * (immune_level - ab_50_severe)))
  ef_death <- 1 / (1 + exp(-k * (immune_level - ab_50_death)))
  
  # return immune_level on linear scale
  immune_level <- (10^(immune_level))
  
  sub <-
    data.frame(name = name,
               t = t,
               future_vfr,
               vaccine,
               immune_escape,
               matched_vacc,
               matched_vacc_level,
               period_s,
               immune_level = immune_level,
               ef_infection = ef_infection,
               ef_severe = ef_severe,
               ef_death = ef_death)
  
  return(sub)
}
