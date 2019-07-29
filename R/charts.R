##################################### Severe_Exacerbations_sex_year #####################################################

plotSevereExacerbations = function(nPatients = 1e4){

  settings <- default_settings
  settings$record_mode <- record_mode["record_mode_event"]
  settings$agent_stack_size <- 0
  settings$n_base_agents <- nPatients
  settings$event_stack_size <- nPatients * 1.7 * 30 * 2
  init_session(settings = settings)
  input <- model_input$values

  run(input=input)
  data <- as.data.frame(Cget_all_events_matrix())
  op <- Cget_output()
  op_ex <- Cget_output_ex()
  terminate_session()


sev_exac_by_sex_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
colnames(sev_exac_by_sex_by_year) <- c("Year", "male", "female")
sev_exac_by_sex_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

sev_exac_by_sex_by_year[, 3] <- rowSums(op_ex$n_exac_by_ctime_severity_female [, 3:4]) / (op_ex$n_COPD_by_ctime_sex)[, 2]
sev_exac_by_sex_by_year[, 2] <- rowSums(op_ex$n_exac_by_ctime_severity [, 3:4] -
                                          op_ex$n_exac_by_ctime_severity_female [, 3:4]) / (op_ex$n_COPD_by_ctime_sex)[, 1]

sev_exac_by_sex_by_year <- as.data.frame(sev_exac_by_sex_by_year)
dfm <- reshape2::melt(sev_exac_by_sex_by_year[,c("Year", "male", "female")],id.vars = 1)
names(dfm) = c("Year", "Gender", "value")

p <- ggplot(dfm, aes(x = Year, y = value, color = Gender)) +
  theme_tufte(base_size=14, ticks=F) +
  geom_point () +
  geom_line() +
  labs(title = "Severe and Very Severe Exacerbations by Gender") +
  ylab ("Exacerbation Rate")  +
  scale_colour_manual(values = c("#CC6666", "#56B4E9")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

p
}
