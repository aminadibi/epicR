#' Reports COPD related stats.
#' @param n_sim number of simulated agents.
#' @return COPD-related stats
#' @export
report_COPD_by_ctime <- function(n_sim = 10^6) {
  settings <- default_settings
  settings$record_mode <- record_mode["record_mode_none"]
  # settings$events_to_record<-events[c('event_COPD')]
  settings$agent_stack_size <- 0
  settings$n_base_agents <- n_sim
  settings$event_stack_size <- settings$n_base_agents * 0

  init_session(settings = settings)

  run()

  op <- Cget_output()
  opx <- Cget_output_ex()

  g1 <- 41:55
  g2 <- 56:70
  g3 <- 71:84
  g4 <- 85:111

  # Overall incidence
  y <- rowSums(opx$n_inc_COPD_by_ctime_age)/rowSums(opx$n_alive_by_ctime_age - opx$n_COPD_by_ctime_age)
  plot(2015:2034, y, type = "l", ylim = c(0, max(y)), xlab = "Year", ylab = "Annual incidence")
  title(cex.main = 1, "Incidence over time (simulated)")

  # Incidence by age group
  a <- opx$n_inc_COPD_by_ctime_age
  b <- opx$n_alive_by_ctime_age - opx$n_COPD_by_ctime_age
  y1 <- rowSums(a[, g1])/rowSums(b[, g1])
  y2 <- rowSums(a[, g2])/rowSums(b[, g2])
  y3 <- rowSums(a[, g3])/rowSums(b[, g3])
  y4 <- rowSums(a[, g4])/rowSums(b[, g4])
  y1[is.nan(y1)] = 0
  y2[is.nan(y2)] = 0
  y3[is.nan(y3)] = 0
  y4[is.nan(y4)] = 0
  max_y <- max(c(y1, y2, y3, y4))
  plot(2015:2034, y1, type = "l", ylim = c(0, max_y * 1.5), xlab = "Year", ylab = "Annual incidence", col = "green")
  lines(2015:2034, y2, type = "l", col = "blue")
  lines(2015:2034, y3, type = "l", col = "black")
  lines(2015:2034, y4, type = "l", col = "red")
  legend("topright", c("40-54", "55-64", "65-74", "75+"), lty = c(1, 1, 1, 1), col = c("green", "blue", "black", "red"))
  title(cex.main = 1, "Incidence by age group (simulated)")


  # overall prevalence
  y <- rowSums(opx$n_COPD_by_ctime_age)/rowSums(opx$n_alive_by_ctime_age)
  plot(2015:2034, y, type = "l", ylim = c(0, max(y)), xlab = "Year", ylab = "Annual prevalence")
  title(cex.main = 1, "Prevalence over time (simulated)")

  # Prevalence by age groups;
  a <- opx$n_COPD_by_ctime_age
  b <- opx$n_alive_by_ctime_age
  y1 <- rowSums(a[, g1])/rowSums(b[, g1])
  y2 <- rowSums(a[, g2])/rowSums(b[, g2])
  y3 <- rowSums(a[, g3])/rowSums(b[, g3])
  y4 <- rowSums(a[, g4])/rowSums(b[, g4])
  max_y <- max(c(y1, y2, y3, y4))
  plot(2015:2034, y1, type = "l", ylim = c(0, max_y * 1.5), xlab = "Year", ylab = "Annual incidence", col = "green")
  lines(2015:2034, y2, type = "l", col = "blue")
  lines(2015:2034, y3, type = "l", col = "black")
  lines(2015:2034, y4, type = "l", col = "red")
  legend("topright", c("40-54", "55-64", "65-74", "75+"), lty = c(1, 1, 1, 1), col = c("green", "blue", "black", "red"))
  title(cex.main = 1, "Prevalence by age group (simulated)")


  # Prevalence of GOLD stages over time;
  a <- opx$n_COPD_by_ctime_severity
  b <- opx$n_alive_by_ctime_sex
  y0 <- a[, 1]/rowSums(b)
  y1 <- a[, 2]/rowSums(b)
  y2 <- a[, 3]/rowSums(b)
  y3 <- a[, 4]/rowSums(b)
  y4 <- a[, 5]/rowSums(b)
  max_y <- max(c(y1, y2, y3, y4))
  plot(2015:2034, y1, type = "l", ylim = c(0, max_y * 1.5), xlab = "Year", ylab = "Annual prevalence by GOLD stage", col = "black")
  lines(2015:2034, y2, type = "l", col = "blue")
  lines(2015:2034, y3, type = "l", col = "orange")
  lines(2015:2034, y4, type = "l", col = "red")
  legend("topright", c("GOLD I", "GOLD II", "GOLD III", "GOLD IV"), lty = c(1, 1, 1, 1), col = c("black", "blue", "orange", "red"))
  title(cex.main = 1, "Prevalence by GOLD stage (simulated)")
}




#' Reports exacerbation-related stats.
#' @param n_sim number of simulated agents.
#' @return exacerbation-related stats
#' @export
report_exacerbation_by_time <- function(n_sim = 10^5) {
  settings <- default_settings
  settings$record_mode <- record_mode["record_mode_none"]
  settings$agent_stack_size <- 0
  settings$n_base_agents <- n_sim
  settings$event_stack_size <- settings$n_base_agents * 0

  init_session(settings = settings)

  run()

  op <- Cget_output()
  opx <- Cget_output_ex()

  cat("Annual rate of exacerbation:", sum(opx$n_exac_by_ctime_severity)/(op$cumul_time - opx$cumul_non_COPD_time), "\n")
  cat("Annual rate of exacerbation by severity:", colSums(opx$n_exac_by_ctime_severity)/(op$cumul_time - opx$cumul_non_COPD_time),
      "\n")

  g1 <- 41:55
  g2 <- 56:70
  g3 <- 71:84
  g4 <- 85:111

  # Overall rate
  y <- rowSums(opx$n_exac_by_ctime_severity)/rowSums(opx$n_alive_by_ctime_sex)
  plot(2015:2034, y, type = "l", ylim = c(0, max(y)), xlab = "Year", ylab = "Annual incidence")
  title(cex.main = 0.5, "Exacerbation rate over time (across population)")

  # Rate by age group
  a <- opx$n_exac_by_ctime_age
  b <- opx$n_alive_by_ctime_age
  y1 <- rowSums(a[, g1])/rowSums(b[, g1])
  y2 <- rowSums(a[, g2])/rowSums(b[, g2])
  y3 <- rowSums(a[, g3])/rowSums(b[, g3])
  y4 <- rowSums(a[, g4])/rowSums(b[, g4])
  max_y <- max(c(y1, y2, y3, y4))
  plot(2015:2034, y1, type = "l", ylim = c(0, max_y * 1.5), xlab = "Year", ylab = "Annual rate", col = "green")
  lines(2015:2034, y2, type = "l", col = "blue")
  lines(2015:2034, y3, type = "l", col = "black")
  lines(2015:2034, y4, type = "l", col = "red")
  legend("topright", c("40-54", "55-64", "65-74", "75+"), lty = c(1, 1, 1, 1), col = c("green", "blue", "black", "red"))
  title(cex.main = 1, "Exacerbation rate by age group (across all population)")


  # Overall rate (within COPD)
  y <- rowSums(opx$n_exac_by_ctime_severity)/rowSums(opx$n_COPD_by_ctime_sex)
  plot(2015:2034, y, type = "l", ylim = c(0, max(y)), xlab = "Year", ylab = "Annual rate")
  title(cex.main = 0.5, "Exacerbation rate over time (within COPD)")

  # Rate by age group (within COPD)
  a <- opx$n_exac_by_ctime_age
  b <- opx$n_COPD_by_ctime_age
  y1 <- rowSums(a[, g1])/rowSums(b[, g1])
  y2 <- rowSums(a[, g2])/rowSums(b[, g2])
  y3 <- rowSums(a[, g3])/rowSums(b[, g3])
  y4 <- rowSums(a[, g4])/rowSums(b[, g4])
  max_y <- max(c(y1, y2, y3, y4))
  plot(2015:2034, y1, type = "l", ylim = c(0, max_y * 1.5), xlab = "Year", ylab = "Annual rate", col = "green")
  lines(2015:2034, y2, type = "l", col = "blue")
  lines(2015:2034, y3, type = "l", col = "black")
  lines(2015:2034, y4, type = "l", col = "red")
  legend("topright", c("40-54", "55-64", "65-74", "75+"), lty = c(1, 1, 1, 1), col = c("green", "blue", "black", "red"))
  title(cex.main = 1, "Exacerbation rate by age group (within COPD)")

  # Rate by exacerbation severity (within COPD)
  a <- opx$n_exac_by_ctime_severity
  b <- opx$n_COPD_by_ctime_sex
  y1 <- (a[, 1])/rowSums(b)
  y2 <- (a[, 2])/rowSums(b)
  y3 <- (a[, 3] + a[, 4])/rowSums(b)
  max_y <- max(c(y1, y2, y3))
  plot(2015:2034, y1, type = "l", ylim = c(0, max_y * 1.5), xlab = "Year", ylab = "Annual rate", col = "green")
  lines(2015:2034, y2, type = "l", col = "blue")
  lines(2015:2034, y3, type = "l", col = "black")

  legend("topright", c("mild", "moderate", "severe and very severe"), lty = c(1, 1, 1, 1), col = c("green", "blue", "black"))
  title(cex.main = 1, "Exacerbation rate by exacerbation severity (within COPD)")

  pie(colSums(opx$n_exac_by_ctime_severity), labels = c("Mild", "Moderate", "severe", "very severe"))

  cat("Proportion by exacerbation severity:", format(colSums(opx$n_exac_by_ctime_severity/sum(opx$n_exac_by_ctime_severity)),
                                                     digits = 2), "\n")
  terminate_session()

  # Calculating Exacerbation Rate by GOLD Stage
  init_session()
  run(n_sim)
  all_events <- as.data.frame(Cget_all_events_matrix())
  exac_events <- subset(all_events, event == 5)
  exit_events <- subset(all_events, event == 14)

  Follow_up_Gold <- c(0, 0, 0, 0)
  last_GOLD_transition_time <- 0
  for (i in 2:dim(all_events)[1]) {
    if (all_events[i, "id"] != all_events[i - 1, "id"])
      last_GOLD_transition_time <- 0
    if ((all_events[i, "id"] == all_events[i - 1, "id"]) & (all_events[i, "gold"] != all_events[i - 1, "gold"])) {
      Follow_up_Gold[all_events[i - 1, "gold"]] = Follow_up_Gold[all_events[i - 1, "gold"]] + all_events[i - 1, "followup_after_COPD"] -
        last_GOLD_transition_time
      last_GOLD_transition_time <- all_events[i - 1, "followup_after_COPD"]
    }
    if (all_events[i, "event"] == 14)
      Follow_up_Gold[all_events[i, "gold"]] = Follow_up_Gold[all_events[i, "gold"]] + all_events[i, "followup_after_COPD"] -
        last_GOLD_transition_time
  }

  terminate_session()

  cat("Rates of exacerbation per GOLD stage:\n")
  cat("GOLD I: ", as.data.frame(table(exac_events[, "gold"]))[1, 2]/Follow_up_Gold[1], "\n")
  cat("GOLD II: ", as.data.frame(table(exac_events[, "gold"]))[2, 2]/Follow_up_Gold[2], "\n")
  cat("GOLD III: ", as.data.frame(table(exac_events[, "gold"]))[3, 2]/Follow_up_Gold[3], "\n")
  cat("GOLD IV: ", as.data.frame(table(exac_events[, "gold"]))[4, 2]/Follow_up_Gold[4], "\n")

  terminate_session()
}
