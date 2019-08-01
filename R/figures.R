#' @title Create Excel Figures
#' @description Runs the model and exports an excel file with all output data
#' @param nPatients number of agents
#' @param directory full file path of folder where you would like the figures to be saved. The default
#' is the current working directory.
#' @return an excel file with all output
#' @export
export_figures <- function(nPatients = 1e4, directory = NULL) {

  settings <- default_settings
  settings$record_mode <- record_mode["record_mode_event"]
  settings$agent_stack_size <- 0
  settings$n_base_agents <- nPatients
  settings$event_stack_size <- nPatients * 1.7 * 30 * 2
  init_session(settings = settings)
  input <- model_input$values
 # input$manual$explicit_mortality_by_age_sex <-  input$manual$explicit_mortality_by_age_sex * 0 #DEBUG shutting down background mortality adjustment
 # input$smoking$mortality_factor_current <- t(as.matrix(c(age40to49 = 1, age50to59 = 1, age60to69 = 1  , age70to79 = 1, age80p = 1 ))) #DEBUG shutting down background mortality adjustment
#  input$smoking$mortality_factor_former<- t(as.matrix(c(age40to49 = 1, age50to59 = 1, age60to69 = 1  , age70to79 = 1, age80p = 1 ))) #DEBUG shutting down background mortality adjustment


  run(input=input)
  data <- as.data.frame(Cget_all_events_matrix())
  op <- Cget_output()
  op_ex <- Cget_output_ex()
  terminate_session()

  ## Create Workbook object and add worksheets
  wb <- openxlsx::createWorkbook()

  ## Add worksheets
  openxlsx::addWorksheet(wb, "List of Figures")
  openxlsx::addWorksheet(wb, "COPD_incidence_by_age_sex")
  openxlsx::addWorksheet(wb, "COPD_incidence_by_year_sex")
  openxlsx::addWorksheet(wb, "COPD_incidence_by_age_group_sex")
  openxlsx::addWorksheet(wb, "COPD_prevalence_by_year_sex")
  openxlsx::addWorksheet(wb, "Prev_Age_Group_CanCOLD-BOLD")
  openxlsx::addWorksheet(wb, "COPD_prev_by_age_group_GOLD")
  openxlsx::addWorksheet(wb, "COPD_related_mortality_by_age")
  openxlsx::addWorksheet(wb, "COPD_related_mortality_by_year")
  openxlsx::addWorksheet(wb, "Age_Specific_Mortality_per1000")
  openxlsx::addWorksheet(wb, "Cost_by_GOLD")
  openxlsx::addWorksheet(wb, "QALY_by_GOLD")
  openxlsx::addWorksheet(wb, "Clinical_trials")
  openxlsx::addWorksheet(wb, "GOLD_stage_by_year")
  openxlsx::addWorksheet(wb, "GOLD_by_sex_CanCOLD")
  openxlsx::addWorksheet(wb, "FEV1_by_sex_year")
  openxlsx::addWorksheet(wb, "Exacerbation_severity_per1000")
  openxlsx::addWorksheet(wb, "Exacerbation_GOLD_per1000")
  openxlsx::addWorksheet(wb, "Exacerbation_rate_GOLD")
  openxlsx::addWorksheet(wb, "Exacerbation_sex_year")
  openxlsx::addWorksheet(wb, "Severe_exacerbation_sex_year")
  openxlsx::addWorksheet(wb, "Exac_by_age_year")
  openxlsx::addWorksheet(wb, "Population_by_year")
  openxlsx::addWorksheet(wb, "Smokers_by_year")
  openxlsx::addWorksheet(wb, "avg_pack_years_ctime")
  openxlsx::addWorksheet(wb, "mortality_by_smoking_per_year")
  openxlsx::addWorksheet(wb, "all_cause_mortality_COPD")
  openxlsx::addWorksheet(wb, "exac_mortality_by_sev_year")



  ## Populate workbooks

  ##################################################### List of Figures #####################################################
  list_of_figures <- matrix (NA, nrow = 27, ncol = 3)
  colnames(list_of_figures) <- c("Name", "Description", "epicR version")
  list_of_figures[1:27, 1] <- c(  "COPD_incidence_by_age_sex",
                                  "COPD_incidence_by_year_sex",
                                  "COPD_incidence_by_age_group_sex",
                                  "COPD_prevalence_by_year_sex",
                                  "Prev_Age_Group_CanCOLD-BOLD",
                                  "COPD_prev_by_age_group_GOLD",
                                  "COPD_related_mortality_by_age",
                                  "COPD_related_mortality_by_year",
                                  "Age_Specific_Mortality_per1000",
                                  "Cost_by_GOLD",
                                  "QALY_by_GOLD",
                                  "Clinical_trials",
                                  "GOLD_stage_by_year",
                                  "GOLD_by_sex_CanCOLD",
                                  "FEV1_by_sex_year",
                                  "Exacerbation_severity_per1000",
                                  "Exacerbation_GOLD_per1000",
                                  "Exacerbation_rate_GOLD",
                                  "Exacerbation_sex_year",
                                  "Severe_exacerbation_sex_year",
                                  "Population_by_year",
                                  "Exac_by_age_year",
                                  "Smokers_by_year",
                                  "avg_pack_years_ctime",
                                  "mortality_by_smoking_per_year",
                                  "all_cause_mortality_COPD",
                                  "exac_mortality_by_sev_year")

  list_of_figures[1:27, 3] <- paste (packageVersion("epicR"))
  openxlsx::setColWidths(wb, 1, cols = c(1, 2, 3), widths = c(35, 50, 15))
  openxlsx::writeData(wb, "List of Figures", list_of_figures, startCol = 1, startRow = 1, colNames = TRUE)
  openxlsx::writeFormula(wb, "List of Figures", startRow = 2
               , x = openxlsx::makeHyperlinkString(sheet = list_of_figures[1:27,1], row = 1, col = 2,  text = list_of_figures[1:27,1]))


  #######################################################  COPD_incidence_by_age  #####################################################
  COPD_inc_by_age_sex <- matrix (NA, nrow = 90-40+1, ncol = 1) #limiting it to people under 90 years old to avoid noise error
  #  colnames(COPD_inc_by_age_sex) <- c("Age", "Incidence")
  #  COPD_inc_by_age_sex[1:(110-40+1), 1] <- c(40:110)
  COPD_inc_by_age_sex <-  colSums(as.data.frame (op_ex$n_inc_COPD_by_ctime_age)[40:90]) / colSums(as.data.frame (op_ex$n_alive_by_ctime_age)[40:90])
  n_COPD_inc_by_age <- as.data.frame(colSums(as.data.frame (op_ex$n_inc_COPD_by_ctime_age)[40:90]))
  COPD_inc_by_age_sex <- as.data.frame(COPD_inc_by_age_sex) * 100 #converting values to percentage.
  alive_by_age <- as.data.frame(colSums(as.data.frame (op_ex$n_alive_by_ctime_age))[40:90])

  SE_COPD_inc_by_age_sex <- sqrt (COPD_inc_by_age_sex * (100 - COPD_inc_by_age_sex) / alive_by_age)

  df <- data.frame(Age = c(40:90), Incidence = COPD_inc_by_age_sex)
  colnames(df) <- c("Age", "Incidence")
  openxlsx::writeData(wb, "COPD_incidence_by_age_sex", df  , startCol = 2, startRow = 3, colNames = TRUE)

  errorbar_min <- df$Incidence - 1.96*SE_COPD_inc_by_age_sex
  errorbar_min <- errorbar_min$COPD_inc_by_age_sex
  errorbar_max <- df$Incidence + 1.96*SE_COPD_inc_by_age_sex
  errorbar_max <- errorbar_max$COPD_inc_by_age_sex

  plot_COPD_inc_by_age_sex  <- ggplot(df, aes(x = Age, y = Incidence)) + theme_tufte(base_size=14, ticks=F) +
    geom_bar(stat = "identity", position = "dodge",  fill = "#FF6666") +
    geom_errorbar(aes(ymin = errorbar_min, ymax = errorbar_max), width=.2, position = position_dodge(.9)) +
     labs(title = "Incidence of COPD by Age") + ylab ("COPD Incidence (%)") + labs(caption = "(based on population at age 40 and above)")

  plot(plot_COPD_inc_by_age_sex) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_incidence_by_age_sex",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")



  ####################################################### COPD_incidence_by_age_group_sex #######################################################
  COPD_inc_by_age <- as.data.frame (op_ex$n_inc_COPD_by_ctime_age)
  alive_by_age <- as.data.frame (op_ex$n_alive_by_ctime_age)
  COPD_inc_by_agegroup <- matrix (0, nrow = 6, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
  num_COPD_inc_by_agegroup <- matrix (0, nrow = 6, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
  denom_COPD_inc_by_agegroup <- matrix (0, nrow = 6, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+

  for (i in 1:5) {
    for (j in 0:9){
      num_COPD_inc_by_agegroup [i] <- num_COPD_inc_by_agegroup [i] + colSums(COPD_inc_by_age[10*(3+i)+j])
      denom_COPD_inc_by_agegroup [i] <- denom_COPD_inc_by_agegroup[i] + colSums(alive_by_age[10*(3+i)+j])
    }
  }

  #handling the special case of 90-110 years, ie: i=6
  for (j in 0:20){
    num_COPD_inc_by_agegroup [6] <- num_COPD_inc_by_agegroup [6] + colSums(COPD_inc_by_age[10*(3+6)+j])
    denom_COPD_inc_by_agegroup [6] <-denom_COPD_inc_by_agegroup [6] + colSums(alive_by_age[10*(3+6)+j])
  }

  COPD_inc_by_agegroup  <- num_COPD_inc_by_agegroup / denom_COPD_inc_by_agegroup

  COPD_inc_by_agegroup <- as.data.frame(COPD_inc_by_agegroup ) * 100 #converting into percentage
  SE_COPD_inc_by_agegroup <- sqrt (COPD_inc_by_agegroup  * (100 - COPD_inc_by_agegroup ) / denom_COPD_inc_by_agegroup)

  df <- data.frame(Age_Group = c("40-50", "50-60", "60-70", "70-80", "80-90", "90+"), Incidence = COPD_inc_by_agegroup) #converting values to percentage.
  colnames(df) <- c("Age_Group", "Incidence")
  openxlsx::writeData(wb, "COPD_incidence_by_age_group_sex", df  , startCol = 2, startRow = 3, colNames = TRUE)

  errorbar_min <- df$Incidence - 1.96*SE_COPD_inc_by_agegroup
  errorbar_min <- errorbar_min$V1
  errorbar_max <- df$Incidence + 1.96*SE_COPD_inc_by_agegroup
  errorbar_max <- errorbar_max$V1

  plot_COPD_inc_by_agegroup  <- ggplot(df, aes(x = Age_Group, y = Incidence)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(stat = "identity", position = "dodge", width = 0.2,  fill = "#FF6666") +
      geom_errorbar(aes(ymin = errorbar_min, ymax = errorbar_max), width=.2, position = position_dodge(.9)) +
      ylim(low = 0, high = 5) + labs(title = "Incidence of COPD by Age Group") + ylab ("COPD Incidence (%)") + labs(caption = "(based on population at age 40 and above)")

  plot(plot_COPD_inc_by_agegroup ) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_incidence_by_age_group_sex",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")



  ####################################################### COPD_Incidence_by_year_sex  #####################################################
  COPD_inc_by_year_sex <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
  SE_COPD_inc_by_year_sex <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 2)

  colnames(COPD_inc_by_year_sex) <- c("Year", "Male", "Female")
  colnames(SE_COPD_inc_by_year_sex) <- c("Male", "Female")

  COPD_inc_by_year_sex[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  data_COPD_inc <- subset(data, event == 4)

  for (i in 1:input$global_parameters$time_horizon){
    COPD_inc_by_year_sex [i, 2] <- dim(subset(data_COPD_inc, ((female == 0) & (ceiling(local_time + time_at_creation) == i)) ))[1]
    COPD_inc_by_year_sex [i, 3] <- dim(subset(data_COPD_inc, ((female == 1) & (ceiling(local_time + time_at_creation) == i)) ))[1]
  }
  COPD_inc_by_year_sex [1:input$global_parameters$time_horizon, 2:3] <- COPD_inc_by_year_sex [1:input$global_parameters$time_horizon, 2:3] / op_ex$n_alive_by_ctime_sex * 100 #converting to percentage
  SE_COPD_inc_by_year_sex [1:input$global_parameters$time_horizon, 1:2] <- sqrt (COPD_inc_by_year_sex [1:input$global_parameters$time_horizon, 2:3] * (100 - COPD_inc_by_year_sex [1:input$global_parameters$time_horizon, 2:3]) / op_ex$n_alive_by_ctime_sex) #TODO Make sure it's correct and add it to ggplot.

  openxlsx::writeData(wb, "COPD_incidence_by_year_sex", COPD_inc_by_year_sex , startCol = 2, startRow = 3, colNames = TRUE)

  df <- as.data.frame(COPD_inc_by_year_sex)
  dfm <- reshape2::melt(df[,c('Year','Male','Female')], id.vars = 1)
  plot_COPD_inc_by_year_sex <- ggplot2::ggplot(dfm, aes(x = Year, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    ylim(low=0, high=5) + labs(title = "Incidence of COPD by Year") + ylab ("COPD Incidence (%)") + labs(caption = "(based on population at age 40 and above)")

  plot(plot_COPD_inc_by_year_sex ) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_incidence_by_year_sex",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")
  rm(data_COPD_inc)


  ####################################################### COPD Prevalence per year and sex  #####################################################
  COPD_prev_by_sex <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
  colnames(COPD_prev_by_sex) <- c("Year", "Male", "Female")
  COPD_prev_by_sex[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))
  COPD_prev_by_sex[1:input$global_parameters$time_horizon,2:3] <- op_ex$n_COPD_by_ctime_sex / op_ex$n_alive_by_ctime_sex * 100 #converting to percentage
  openxlsx::writeData(wb, "COPD_prevalence_by_year_sex", COPD_prev_by_sex, startCol = 2, startRow = 3, colNames = TRUE)

  df <- as.data.frame(COPD_prev_by_sex)
  dfm <- reshape2::melt(df[,c('Year','Male','Female')],id.vars = 1)
  plot_COPD_prev_by_sex <- ggplot2::ggplot(dfm, aes(x = Year, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") + ylim(low=0, high=50) + labs(title = "Prevalence of COPD by Year") + ylab ("Prevalence (%)") + labs(caption = "(based on population at age 40 and above)")

  plot(plot_COPD_prev_by_sex) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_prevalence_by_year_sex",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ## now cumul prevalence
  cumul_COPD_prev_by_sex <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
  colnames(cumul_COPD_prev_by_sex) <- c("Year", "Male", "Female")
  cumul_COPD_prev_by_sex[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))
  cumul_COPD_prev_by_sex[1:input$global_parameters$time_horizon,2:3] <- op_ex$n_COPD_by_ctime_sex  / default_settings$n_base_agents * 18.6e6  #for entire Canada

  openxlsx::writeData(wb, "COPD_prevalence_by_year_sex", cumul_COPD_prev_by_sex, startCol = 2, startRow = 30, colNames = TRUE)

  df <- as.data.frame(cumul_COPD_prev_by_sex)
  dfm <- reshape2::melt(df[,c('Year','Male','Female')],id.vars = 1)

  plot_cumul_COPD_prev_by_sex <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Number of COPD cases by Year") + ylab ("Number of Cases")  +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + labs(caption = "")


  plot(plot_cumul_COPD_prev_by_sex) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_prevalence_by_year_sex",  xy = c("G", 35), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### COPD Prevalence by Age Group and Sex #####################################################

  COPD_by_ctime_age <- as.data.frame(op_ex$n_COPD_by_ctime_age)
  alive_by_ctime_age <- as.data.frame(op_ex$n_alive_by_ctime_age)
  COPD_prev_by_agegroup <- matrix (0, nrow = 6, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
  num_COPD_prev_by_agegroup <- matrix (0, nrow = 6, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
  denom_COPD_prev_by_agegroup <- matrix (0, nrow = 6, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+


  for (i in 1:5) {
    for (j in 0:9){
      num_COPD_prev_by_agegroup [i] <- num_COPD_prev_by_agegroup [i] + colSums(COPD_by_ctime_age[10*(3+i)+j])
      denom_COPD_prev_by_agegroup [i] <- denom_COPD_prev_by_agegroup [i] + colSums(alive_by_ctime_age[10*(3+i)+j])
    }
  }

  #handling the special case of 90-110 years, ie: i=4
  for (j in 0:20){
    num_COPD_prev_by_agegroup [6] <- num_COPD_prev_by_agegroup [6] + colSums(COPD_by_ctime_age[10*(3+6)+j])
    denom_COPD_prev_by_agegroup [6] <- denom_COPD_prev_by_agegroup [6] + colSums(alive_by_ctime_age[10*(3+6)+j])
  }

  COPD_prev_by_agegroup <- num_COPD_prev_by_agegroup / denom_COPD_prev_by_agegroup * 100 #converting into percentage
  SE_COPD_prev_by_agegroup <- sqrt (COPD_prev_by_agegroup*(100-COPD_prev_by_agegroup)/denom_COPD_prev_by_agegroup)

  df <- data.frame(Age_Group = c("40-50", "50-60", "60-70", "70-80", "80-90", "90+"), Prevalence = COPD_prev_by_agegroup)
  openxlsx::writeData(wb, "Prev_Age_Group_CanCOLD-BOLD", df, startCol = 2, startRow = 3, colNames = TRUE)

  errorbar_min <- df$Prevalence - 1.96*SE_COPD_prev_by_agegroup
  errorbar_max <- df$Prevalence + 1.96*SE_COPD_prev_by_agegroup

  plot_COPD_prev_by_agegroup  <- ggplot2::ggplot(df, aes(x = Age_Group, y = Prevalence)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(stat = "identity", position = "dodge", width = 0.2, fill = "#FF6666") +
    geom_errorbar(aes(ymin = errorbar_min, ymax =errorbar_max),
                                                                                                       width=.2, position=position_dodge(.9)) + ylim(low = 0, high = 50) + labs (title = "Prevalence of COPD by Age Group") + ylab ("Prevalence (%)") + labs(caption = "(error bars represent 95% CI)")

  plot(plot_COPD_prev_by_agegroup) #plot needs to be showing
  openxlsx::insertPlot(wb, "Prev_Age_Group_CanCOLD-BOLD",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### COPD Prevalence by Age Group and GOLD #####################################################

  # COPD_prev_by_agegroup_GOLD <- matrix (0, nrow = 4, ncol = 4) #40-55, 55-70, 70-85, 85+
  # num_COPD_prev_by_agegroup_GOLD <- matrix (0, nrow = 4, ncol = 4) #40-55, 55-70, 70-85, 85+
  # denom_COPD_prev_by_agegroup_GOLD <- matrix (0, nrow = 4, ncol = 4) #40-55, 55-70, 70-85, 85+
  #
  # for (i in (1:3)) {
  #   for (j in (0:15)) {
  #    num_COPD_prev_by_agegroup_GOLD [i, ] <- num_COPD_prev_by_agegroup_GOLD [i, ] + op_ex$COPD


   # }
  #}

  #df <- data.frame(Age_Group = c("40-50", "50-60", "60-70", "70-80", "80-90", "90+"), Prevalence = COPD_prev_by_agegroup)
  #openxlsx::writeData(wb, "COPD_prev_by_age_group_GOLD", df, startCol = 2, startRow = 3, colNames = TRUE)

  #plot_COPD_prev_by_agegroup  <- ggplot2::ggplot(df, aes(x = Age_Group, y = Prevalence)) +
   # geom_bar(stat = "identity", position = "dodge", width = 0.2, fill = "#FF6666") + geom_errorbar(aes(ymin = Prevalence - 1.96*SE_COPD_prev_by_agegroup, ymax = Prevalence + 1.96*SE_COPD_prev_by_agegroup ),
#
    #                                                                                               width=.2, position=position_dodge(.9)) + ylim(low = 0, high = 50) + labs (title = "Prevalence of COPD by Age Group") + ylab ("Prevalence (%)") + labs(caption = "(error bars represent 95% CI)")

  #plot(plot_COPD_prev_by_agegroup) #plot needs to be showing
  #openxlsx::insertPlot(wb, "COPD_prev_by_age_group_GOLD",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")



  ##################################################### FEV1 by sex and COPD year #####################################################

  fev1_by_sex <- matrix (NA, nrow = input$global_parameters$time_horizon+1, ncol = 4)
  colnames(fev1_by_sex) <- c("Year_with_COPD", "Male", "Female", "All")
  fev1_by_sex[1:(input$global_parameters$time_horizon+1), 1] <- 1:(input$global_parameters$time_horizon+1)
  data_COPD <- subset (data, gold > 0)

  for (i in 0:input$global_parameters$time_horizon) {
    temp_data_COPD_years <- subset(data, ((followup_after_COPD == i) & (female == 0)))
    fev1_by_sex[i+1, 2] <- mean (temp_data_COPD_years[,"FEV1"])

    temp_data_COPD_years <- subset(data, ((followup_after_COPD == i) & (female == 1)))
    fev1_by_sex[i+1, 3] <- mean (temp_data_COPD_years[,"FEV1"])

    temp_data_COPD_years <- subset(data, followup_after_COPD == i)
    fev1_by_sex[i+1, 4] <- mean (temp_data_COPD_years[,"FEV1"])
  }

  openxlsx::writeData(wb, "FEV1_by_sex_year", fev1_by_sex, startCol = 2, startRow = 3, colNames = TRUE)

  df <- as.data.frame(fev1_by_sex)
  dfm <- reshape2::melt(df[,c('Year_with_COPD','Male','Female', "All")],id.vars = 1)

  plot_fev1_by_sex <- ggplot2::ggplot(dfm, aes(Year_with_COPD, value, colour = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point() + geom_line() + labs(title = "Mean FEV1 by Number of Years with COPD") + ylab ("FEV1 (L)")
  plot(plot_fev1_by_sex) #plot needs to be showing
  openxlsx::insertPlot(wb, "FEV1_by_sex_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")



  ##################################################### GOLD Stages by Year #####################################################

  GOLD_perc_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(GOLD_perc_by_year) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  GOLD_perc_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))
  GOLD_perc_by_year[1:input$global_parameters$time_horizon, 2] <- op_ex$n_COPD_by_ctime_severity[, 2]
  GOLD_perc_by_year[1:input$global_parameters$time_horizon, 3] <- op_ex$n_COPD_by_ctime_severity[, 3]
  GOLD_perc_by_year[1:input$global_parameters$time_horizon, 4] <- op_ex$n_COPD_by_ctime_severity[, 4]
  GOLD_perc_by_year[1:input$global_parameters$time_horizon, 5] <- op_ex$n_COPD_by_ctime_severity[, 5]

  for (i in (1:input$global_parameters$time_horizon)){
      GOLD_perc_by_year [i, 2:5] <- GOLD_perc_by_year [i, 2:5] / sum(GOLD_perc_by_year [i, 2:5]) * 100
    }

  GOLD_perc_by_year <- as.data.frame(GOLD_perc_by_year)
  openxlsx::writeData(wb, "GOLD_stage_by_year", GOLD_perc_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(GOLD_perc_by_year[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_gold_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "GOLD Stages per year") + ylab ("%")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_gold_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "GOLD_stage_by_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### Exacerbation Severity by year per 1000 #####################################################

  exac_severity_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(exac_severity_by_year) <- c("Year", "Mild", "Moderate", "Severe", "Very Severe")
  exac_severity_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  exac_severity_by_year[, 2:5] <- op_ex$n_exac_by_ctime_severity [, 1:4] / rowSums(op_ex$n_COPD_by_ctime_sex)[] * 1000

  exac_severity_by_year <- as.data.frame(exac_severity_by_year)
  openxlsx::writeData(wb, "Exacerbation_severity_per1000", exac_severity_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(exac_severity_by_year[,c("Year", "Mild", "Moderate", "Severe", "Very Severe")],id.vars = 1)

  plot_exac_severity_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Exacerbation severity per year") + ylab ("Number of cases per 1000")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_exac_severity_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Exacerbation_severity_per1000",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### Exacerbation by GOLD by year per 1000 #####################################################


  exac_GOLD_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(exac_GOLD_by_year) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  exac_GOLD_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  exac_GOLD_by_year[, 2:5] <- op_ex$n_exac_by_ctime_GOLD [, 1:4] / rowSums(op_ex$n_COPD_by_ctime_sex)[] * 1000

  exac_GOLD_by_year <- as.data.frame(exac_GOLD_by_year)
  openxlsx::writeData(wb, "Exacerbation_GOLD_per1000", exac_GOLD_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(exac_GOLD_by_year[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_exac_GOLD_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Number of Exacerbations per GOLD per year") + ylab ("Exacerbations per 1000 patients")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_exac_GOLD_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Exacerbation_GOLD_per1000",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### Exacerbation Rate by GOLD by year  #####################################################


  exac_rate_by_GOLD_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(exac_rate_by_GOLD_by_year) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  exac_rate_by_GOLD_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  exac_rate_by_GOLD_by_year[, 2:5] <- op_ex$n_exac_by_ctime_GOLD [, 1:4] / (op_ex$cumul_time_by_ctime_GOLD)[,2:5]

  exac_rate_by_GOLD_by_year <- as.data.frame(exac_rate_by_GOLD_by_year)
  openxlsx::writeData(wb, "Exacerbation_rate_GOLD", exac_rate_by_GOLD_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(exac_rate_by_GOLD_by_year[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_exac_rate_by_GOLD_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Exacerbations Rate per GOLD per year") + ylab ("Exacerbations Rate")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_exac_rate_by_GOLD_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Exacerbation_rate_GOLD",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ##################################################### Exacerbation_sex_year #####################################################

  exac_rate_by_sex_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
  colnames(exac_rate_by_sex_by_year) <- c("Year", "male", "female")
  exac_rate_by_sex_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  exac_rate_by_sex_by_year[, 3] <- rowSums(op_ex$n_exac_by_ctime_severity_female [, 1:4]) / (op_ex$n_COPD_by_ctime_sex)[, 2]
  exac_rate_by_sex_by_year[, 2] <- rowSums(op_ex$n_exac_by_ctime_severity [, 1:4] - op_ex$n_exac_by_ctime_severity_female [, 1:4]) / (op_ex$n_COPD_by_ctime_sex)[, 1]

  exac_rate_by_sex_by_year <- as.data.frame(exac_rate_by_sex_by_year)
  openxlsx::writeData(wb, "Exacerbation_sex_year", exac_rate_by_sex_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(exac_rate_by_sex_by_year[,c("Year", "male", "female")],id.vars = 1)

  plot_exac_rate_by_sex_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Exacerbation rate by gender") + ylab ("Exacerbation Rate")  +
    scale_colour_manual(values = c("#CC6666", "#56B4E9")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_exac_rate_by_sex_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Exacerbation_sex_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ## now cumul prevalence
  cumul_exac_rate_by_sex_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
  colnames(cumul_exac_rate_by_sex_by_year) <- c("Year", "male", "female")
  cumul_exac_rate_by_sex_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  cumul_exac_rate_by_sex_by_year[, 3] <- rowSums(op_ex$n_exac_by_ctime_severity_female [, 1:4]) / default_settings$n_base_agents * 18.6e6 #for entire Canada
  cumul_exac_rate_by_sex_by_year[, 2] <- rowSums(op_ex$n_exac_by_ctime_severity [, 1:4] - op_ex$n_exac_by_ctime_severity_female [, 1:4]) / default_settings$n_base_agents * 18.6e6

  cumul_exac_rate_by_sex_by_year <- as.data.frame(cumul_exac_rate_by_sex_by_year)

  openxlsx::writeData(wb, "Exacerbation_sex_year", cumul_exac_rate_by_sex_by_year, startCol = 2, startRow = 30, colNames = TRUE)
  dfm <- reshape2::melt(cumul_exac_rate_by_sex_by_year[,c("Year", "male", "female")],id.vars = 1)


  plot_cumul_exac_rate_by_sex_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Number of Exacerbations") + ylab ("Exacerbations")  +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_cumul_exac_rate_by_sex_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Exacerbation_sex_year",  xy = c("G", 35), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ##################################################### Severe_Exacerbations_sex_year #####################################################

  sev_exac_by_sex_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 3)
  colnames(sev_exac_by_sex_by_year) <- c("Year", "male", "female")
  sev_exac_by_sex_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  sev_exac_by_sex_by_year[, 3] <- rowSums(op_ex$n_exac_by_ctime_severity_female [, 3:4]) / (op_ex$n_COPD_by_ctime_sex)[, 2]
  sev_exac_by_sex_by_year[, 2] <- rowSums(op_ex$n_exac_by_ctime_severity [, 3:4] - op_ex$n_exac_by_ctime_severity_female [, 3:4]) / (op_ex$n_COPD_by_ctime_sex)[, 1]

  sev_exac_by_sex_by_year <- as.data.frame(sev_exac_by_sex_by_year)
  openxlsx::writeData(wb, "Severe_exacerbation_sex_year", sev_exac_by_sex_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(sev_exac_by_sex_by_year[,c("Year", "male", "female")],id.vars = 1)

  plot_sev_exac_by_sex_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Severe and very severe exacerbations by gender") + ylab ("Exacerbation Rate")  +
    scale_colour_manual(values = c("#CC6666", "#56B4E9")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_sev_exac_by_sex_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Severe_exacerbation_sex_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ##################################################### exac_by_age_year #####################################################

  exac_by_age_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 6)
  colnames(exac_by_age_year) <- c("Year", "40-55", "55-70", "70-85", "85+", "All")
  exac_by_age_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  for (i in (1:3)){
  exac_by_age_year[, i+1] <- rowSums(op_ex$n_exac_by_ctime_age [, (25+(15*i)):(35+15*(i+1-1))])
  }

  exac_by_age_year[, 5] <- rowSums(op_ex$n_exac_by_ctime_age [, 85:111]) # special case of 80+
  exac_by_age_year[, 6] <- rowSums (exac_by_age_year[, 2:5]) # all

  exac_by_age_year[, 2:6]<- exac_by_age_year[, 2:6] / nPatients * 18e6 #18e6 is roughly the 40+ population of Canada as of 2015
  exac_by_age_year <- as.data.frame(exac_by_age_year)
  openxlsx::writeData(wb, "Exac_by_age_year", exac_by_age_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(exac_by_age_year[,c("Year", "40-55", "55-70", "70-85", "85+", "All")],id.vars = 1)

  plot_exac_by_age_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Number of Exacerbations per age group") + ylab ("Number of Exacerbations")  +
     scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) +
    labs(caption = "(All severity levels, assuming 40+ population of Canada as 18.6 million as of the start of the simulation)")

  plot(plot_exac_by_age_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Exac_by_age_year",  xy = c("I", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ##################################################### Cost by GOLD #####################################################

  cost_by_GOLD <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(cost_by_GOLD) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  cost_by_GOLD[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))
  cost_by_GOLD[, 2:5] <- (op_ex$cumul_cost_gold_ctime [, 2:5])

  for (i in (input$global_parameters$time_horizon:2)){
    cost_by_GOLD[i, 2:5] <- cost_by_GOLD[i, 2:5] - (op_ex$cumul_cost_gold_ctime [i-1, 2:5])
  }
  cost_by_GOLD[, 2:5] <- cost_by_GOLD[, 2:5] / op_ex$n_COPD_by_ctime_severity[, 2:5] #per capita
  cost_by_GOLD <- as.data.frame(cost_by_GOLD)
  openxlsx::writeData(wb, "Cost_by_GOLD", cost_by_GOLD, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(cost_by_GOLD[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_cost_by_GOLD <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Cost per GOLD stage") + ylab ("Canadian dollars")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + labs(caption = "per capita")

  plot(plot_cost_by_GOLD) #plot needs to be showing
  openxlsx::insertPlot(wb, "Cost_by_GOLD",  xy = c("I", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ## now cumul QALY
  Cumul_cost_by_GOLD <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(Cumul_cost_by_GOLD) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  Cumul_cost_by_GOLD[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))
  Cumul_cost_by_GOLD[, 2:5] <- (op_ex$cumul_cost_gold_ctime [, 2:5])/1e6 / default_settings$n_base_agents * 18.6e6  #per million for entire Canada

  Cumul_cost_by_GOLD <- as.data.frame(Cumul_cost_by_GOLD)
  openxlsx::writeData(wb, "Cost_by_GOLD", Cumul_cost_by_GOLD, startCol = 2, startRow = 35, colNames = TRUE)
  Cumul_dfm <- reshape2::melt(Cumul_cost_by_GOLD[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_Cumul_cost_by_GOLD <- ggplot2::ggplot(Cumul_dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Cost per GOLD stage") + ylab ("Canadian dollars")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12), label=scales::dollar_format(suffix = "M")) + labs(caption = "Cumulative cost for Canada")

  plot(plot_Cumul_cost_by_GOLD) #plot needs to be showing
  openxlsx::insertPlot(wb, "Cost_by_GOLD",  xy = c("I", 35), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### QALY by GOLD #####################################################

  QALY_by_GOLD <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(QALY_by_GOLD) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  QALY_by_GOLD[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  QALY_by_GOLD[, 2:5] <- (op_ex$cumul_qaly_gold_ctime [, 2:5])

  for (i in (input$global_parameters$time_horizon:2)){
    QALY_by_GOLD[i, 2:5] <- QALY_by_GOLD[i, 2:5] - (op_ex$cumul_qaly_gold_ctime [i-1, 2:5])
  }

  QALY_by_GOLD[, 2:5] <- QALY_by_GOLD[, 2:5] / op_ex$n_COPD_by_ctime_severity[, 2:5] #per capita
  QALY_by_GOLD <- as.data.frame(QALY_by_GOLD)
  openxlsx::writeData(wb, "QALY_by_GOLD", QALY_by_GOLD, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(QALY_by_GOLD[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_QALY_by_GOLD <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "QALY per GOLD stage") + ylab ("QALYs")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + labs(caption = "per capita")

  plot(plot_QALY_by_GOLD) #plot needs to be showing
  openxlsx::insertPlot(wb, "QALY_by_GOLD",  xy = c("I", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ## now cumul QALY
  Cumul_QALY_by_GOLD <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(Cumul_QALY_by_GOLD) <- c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  Cumul_QALY_by_GOLD[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  Cumul_QALY_by_GOLD[, 2:5] <- (op_ex$cumul_qaly_gold_ctime [, 2:5]) / default_settings$n_base_agents * 18.6e6
  Cumul_QALY_by_GOLD <- as.data.frame(Cumul_QALY_by_GOLD)

  openxlsx::writeData(wb, "QALY_by_GOLD", Cumul_QALY_by_GOLD, startCol = 2, startRow = 35, colNames = TRUE)
  Cumul_dfm <- reshape2::melt(Cumul_QALY_by_GOLD[,c("Year", "GOLD I", "GOLD II", "GOLD III", "GOLD IV")],id.vars = 1)

  plot_Cumul_QALY_by_GOLD <- ggplot2::ggplot(Cumul_dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "QALY per GOLD stage") + ylab ("QALYs")  +
    scale_colour_manual(values = c("#56B4E9", "#66CC99", "gold2" , "#CC6666")) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + labs(caption = "Cumulative QALY for Canada")

  plot(plot_Cumul_QALY_by_GOLD) #plot needs to be showing
  openxlsx::insertPlot(wb, "QALY_by_GOLD",  xy = c("I", 35), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ######################################################## COPD_related_mortality_per_age_group #########################################################

  COPD_related_mortality_by_age <- matrix (0, nrow = 12, ncol = 3) #40-45, 45-50, 50-55, ..., 90-95, 95-max
  num_COPD_related_mortality_by_age <- matrix (0, nrow = 12, ncol = 3) #40-45, 45-50, 50-55, ..., 90-95, 95-max
  denom_COPD_related_mortality_by_age <- matrix (0, nrow = 12, ncol = 3) #40-45, 45-50, 50-55, ..., 90-95, 95-max

  #COPD_related_mortality_by_age[, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  for (i in 1:11) {
    for (j in 0:4) {
      num_COPD_related_mortality_by_age[i, 2:3] <- num_COPD_related_mortality_by_age[i, 2:3] + (op_ex$n_exac_death_by_age_sex)[5*(7+i)+j, ]
      denom_COPD_related_mortality_by_age[i, 2:3] <- denom_COPD_related_mortality_by_age[i, 2:3] + (op_ex$n_death_by_age_sex)[5*(7+i)+j, ]

    }
  }

  #handling the special case of 95+ years, ie: i=12
  for (j in 0:16){
    num_COPD_related_mortality_by_age[12, 2:3] <- num_COPD_related_mortality_by_age[12, 2:3] + (op_ex$n_exac_death_by_age_sex)[5*(7+12)+j, ]
    denom_COPD_related_mortality_by_age[12, 2:3] <- denom_COPD_related_mortality_by_age[12, 2:3] + (op_ex$n_death_by_age_sex)[5*(7+12)+j, ]
  }

  COPD_related_mortality_by_age [, 2:3]<- num_COPD_related_mortality_by_age[, 2:3] / denom_COPD_related_mortality_by_age[, 2:3] * 100 #converting to percentage
  COPD_related_mortality_by_age [, 1] <- c("40-45", "45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90", "90-95", "95p")


  COPD_related_mortality_by_age <- as.data.frame(COPD_related_mortality_by_age)
  colnames(COPD_related_mortality_by_age) <- c("Age", "Male", "Female")

  openxlsx::writeData(wb, "COPD_related_mortality_by_age", COPD_related_mortality_by_age, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(COPD_related_mortality_by_age[,c("Age", "Male", "Female")],id.vars = 1)
  dfm[['value']] <- as.numeric(dfm[['value']])
  plot_COPD_related_mortality_by_age <- ggplot2::ggplot(dfm, aes(x = Age, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    ylim (low=0, high = 20) +
    labs(title = "Percentage of COPD-related mortality among causes of death") + ylab ("Mortality (%)")
  plot(plot_COPD_related_mortality_by_age) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_related_mortality_by_age",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ##################################################### COPD-related Mortality by Calendar Year #####################################################
  COPD_death_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 4)
  colnames(COPD_death_by_year) <- c("Year", "Male", "Female", "All")
  data_COPD_death <- subset (data, event == 7)
  COPD_death_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  for (i in 1:input$global_parameters$time_horizon) {
    COPD_death_by_year[i, 2] <- dim(subset(data_COPD_death, ((female == 0) & (floor(local_time + time_at_creation)==i))))[1] / op_ex$n_COPD_by_ctime_sex[i, 1] * 100
    COPD_death_by_year[i, 3] <- dim(subset(data_COPD_death, ((female == 1) & (floor(local_time + time_at_creation)==i))))[1] / op_ex$n_COPD_by_ctime_sex[i, 2] * 100
    COPD_death_by_year[i, 4] <- COPD_death_by_year[i, 2] + COPD_death_by_year[i, 3] / (op_ex$n_COPD_by_ctime_sex[i, 1] + op_ex$n_COPD_by_ctime_sex[i, 2]) * 100
  }

  openxlsx::writeData(wb, "COPD_related_mortality_by_year", COPD_death_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  COPD_death_by_year <- as.data.frame(COPD_death_by_year)
  dfm <- reshape2::melt(COPD_death_by_year[,c("Year", "Male", "Female", "All")], id.vars = 1)

  plot_COPD_death_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    labs(title = "COPD-related mortality by year") + ylab ("Mortality among COPD patients (%)")

  # plot_COPD_death_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +
  #  geom_point () + geom_line() + labs(title = "Percentage of COPD-related mortality among causes of death") + ylab ("Number of Deaths")  +
  #  scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  plot(plot_COPD_death_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "COPD_related_mortality_by_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")



  ##################################################### Age_Specific_Mortality_per1000 #####################################################
  mortality_by_age <- matrix (NA, nrow = 110-40+1, ncol = 3)
  mortality_by_age[1:(110-40+1), 1] <- c(40:110)

  mortality_by_age[, 2:3] <- op_ex$n_death_by_age_sex[40:110, ] / op_ex$sum_time_by_age_sex[40:110,] * 1000

  mortality_by_age <- as.data.frame(mortality_by_age)
  colnames(mortality_by_age) <- c("Age", "Male", "Female")

  openxlsx::writeData(wb, "Age_Specific_Mortality_per1000", mortality_by_age, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(mortality_by_age[,c("Age", "Male", "Female")],id.vars = 1)

  plot_mortality_by_age <- ggplot2::ggplot(dfm, aes(x = Age, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Age_Specific_Mortality_per1000") + ylab ("Mortality Rate")  +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + ylim(low = 0, high = 400)

  plot(plot_mortality_by_age) #plot needs to be showing
  openxlsx::insertPlot(wb, "Age_Specific_Mortality_per1000",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")


  ##################################################### Population by Sex and Year #####################################################

  population_by_sex_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 4)
  colnames(population_by_sex_year) <- c("Year", "male", "female", "all")
  population_by_sex_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  population_by_sex_year[, 2] <- (op_ex$n_alive_by_ctime_sex [, 1]) #male
  population_by_sex_year[, 3] <- (op_ex$n_alive_by_ctime_sex [, 2]) #female
  population_by_sex_year[, 4] <- rowSums(op_ex$n_alive_by_ctime_sex [, 1:2])


  population_by_sex_year[, 2:4] <- population_by_sex_year[, 2:4] / (nPatients) * 18.6e6 #18.6e.6 is roughly the 40+ population of Canada as of 2017 http://www.statcan.gc.ca/tables-tableaux/sum-som/l01/cst01/demo10a-eng.htm
  population_by_sex_year <- as.data.frame(population_by_sex_year)
  openxlsx::writeData(wb, "Population_by_year", population_by_sex_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(population_by_sex_year[,c("Year", "male", "female", "all")],id.vars = 1)

  plot_population_by_sex_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Population of Canada per year") + ylab ("Number")  +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 12)) + labs(caption = "(assuming 40+ population of Canada as 18.6 million as of 2017)")

  plot(plot_population_by_sex_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Population_by_year",  xy = c("I", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")



  ##################################################### Smokers_by_Year #####################################################
  smokers_by_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(smokers_by_year) <- c("Year", "Never_Smoked", "Former_Smoker", "Smoker", "All")
  data_annual <- subset (data, ((event == 0) | (event == 1)))

  for (i in (1:input$global_parameters$time_horizon)) {
    smokers_by_year[i, 2] <- dim(subset(data_annual, ((smoking_status == 0) & (pack_years == 0) & (floor(local_time + time_at_creation) == (i - 1)))))[1]
    smokers_by_year[i, 3] <- dim(subset(data_annual, ((smoking_status == 0) & (pack_years != 0) & (floor(local_time + time_at_creation) == (i - 1)))))[1]
    smokers_by_year[i, 4] <- dim(subset(data_annual, ((smoking_status > 0) & (floor(local_time + time_at_creation) == (i - 1)))))[1]
    smokers_by_year[i, 5] <- dim(subset(data_annual, (floor(local_time + time_at_creation) == (i-1) )))[1]
  }

  smokers_by_year <- 100 * smokers_by_year / smokers_by_year [, 5]
  smokers_by_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))
  smokers_by_year <- as.data.frame(smokers_by_year)
  openxlsx::writeData(wb, "Smokers_by_year", smokers_by_year, startCol = 2, startRow = 3, colNames = TRUE)
  dfm <- reshape2::melt(smokers_by_year[,c("Year", "Never_Smoked", "Former_Smoker", "Smoker")],id.vars = 1)

  plot_smokers_by_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Smoking Status per year") + ylab ("%")  +
    scale_colour_manual(values = c("#66CC99", "#CC6666", "#56B4E9")) + scale_y_continuous(breaks = scales::pretty_breaks(n = 12))
#  plot_smokers_by_year <- qplot(Year, Smoker, data = smokers_by_year)

  plot(plot_smokers_by_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "Smokers_by_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  #####################################################   mortality_by_smoking_per_year  #####################################################
  mortality_by_smoking_per_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 5)
  colnames(mortality_by_smoking_per_year) <- c("Year", "Never_Smoked", "Former_Smoker", "Smoker", "All")
  mortality_by_smoking_per_year[1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  data_annual <- subset(data, event == 1)
  data_deaths <- subset(data, ((event == 7) | (event == 13)))

  for (i in 1:input$global_parameters$time_horizon) {
    mortality_by_smoking_per_year[i, 2] <- dim (subset(data_deaths, ((pack_years == 0)  & (smoking_status == 0) & (floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual, ((pack_years == 0)  & (smoking_status == 0) & (floor(time_at_creation + local_time) == i ))))[1] * 100
    mortality_by_smoking_per_year[i, 3] <- dim (subset(data_deaths, ((pack_years > 0) & (smoking_status == 0) & (floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual, ((pack_years > 0) & (smoking_status == 0) & (floor(time_at_creation + local_time) == i ))))[1] * 100
    mortality_by_smoking_per_year[i, 4] <- dim (subset(data_deaths, ((smoking_status == 1) & (floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual, ((smoking_status == 1)  & (floor(time_at_creation + local_time) == i ))))[1] * 100
    mortality_by_smoking_per_year[i, 5] <- dim (subset(data_deaths, ((floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual, ((floor(time_at_creation + local_time) == i ))))[1] * 100
  }

  openxlsx::writeData(wb, "mortality_by_smoking_per_year", mortality_by_smoking_per_year, startCol = 2, startRow = 3, colNames = TRUE)
  mortality_by_smoking_per_year <- as.data.frame(mortality_by_smoking_per_year)
  dfm <- reshape2::melt(mortality_by_smoking_per_year[,c("Year", "Never_Smoked", "Former_Smoker", "Smoker", "All")], id.vars = 1)

  plot_mortality_by_smoking_per_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    labs(title = "All-cause Mortality by smoking status") + ylab ("%")

  plot(plot_mortality_by_smoking_per_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "mortality_by_smoking_per_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  #####################################################   All_cause mortality for COPD patients #####################################################
  all_cause_mortality_COPD <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 4)
  colnames(all_cause_mortality_COPD) <- c("Year", "Male", "Female", "All")
  all_cause_mortality_COPD [1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  data_annual_COPD <- subset(data, ((event == 1) & (gold > 0)))
  data_deaths_COPD <- subset(data, ((event == 7) | (event == 13)))
  data_deaths_COPD <- subset(data_deaths_COPD, gold > 0)


  for (i in 1:input$global_parameters$time_horizon) {
    all_cause_mortality_COPD [i, 2] <- dim (subset(data_deaths_COPD, ((female == 0) & (floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual_COPD, ((female == 0) & (floor(time_at_creation + local_time) == i ))))[1] * 100
    all_cause_mortality_COPD [i, 3] <- dim (subset(data_deaths_COPD, ((female == 1)  & (floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual_COPD, ((female == 1) & (floor(time_at_creation + local_time) == i ))))[1] * 100
    all_cause_mortality_COPD [i, 4] <- dim (subset(data_deaths_COPD, ((floor(time_at_creation + local_time) == i ))))[1] / dim(subset(data_annual_COPD, ((floor(time_at_creation + local_time) == i ))))[1] * 100
  }

  openxlsx::writeData(wb, "all_cause_mortality_COPD", all_cause_mortality_COPD , startCol = 2, startRow = 3, colNames = TRUE)
  all_cause_mortality_COPD  <- as.data.frame(all_cause_mortality_COPD )
  dfm <- reshape2::melt(all_cause_mortality_COPD [,c("Year", "Male", "Female", "All")], id.vars = 1)

  plot_all_cause_mortality_COPD  <- ggplot2::ggplot(dfm, aes(x = Year, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    labs(title = "All-cause mortality for COPD patients") + ylab ("%")

  plot(plot_all_cause_mortality_COPD ) #plot needs to be showing
  openxlsx::insertPlot(wb, "all_cause_mortality_COPD",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  #####################################################  Exacerbation mortality by severity and year #####################################################
  exac_mortality_by_sev_year <- matrix (NA, nrow = input$global_parameters$time_horizon, ncol = 4)
  colnames(exac_mortality_by_sev_year) <- c("Year", "Mild", "Moderate", "Severe and Very Severe")
  exac_mortality_by_sev_year  [1:input$global_parameters$time_horizon, 1] <- c(2015:(2015+input$global_parameters$time_horizon-1))

  exac_mortality_by_sev_year [, 2:3] <- op_ex$n_exac_death_by_ctime_severity [,1:2] / op_ex$n_exac_by_ctime_severity[, 1:2] * 100
  exac_mortality_by_sev_year [, 4] <- (op_ex$n_exac_death_by_ctime_severity [, 3] + op_ex$n_exac_death_by_ctime_severity [, 4])  / (op_ex$n_exac_by_ctime_severity[, 3] + op_ex$n_exac_by_ctime_severity[, 4]) * 100

  openxlsx::writeData(wb, "exac_mortality_by_sev_year", exac_mortality_by_sev_year , startCol = 2, startRow = 3, colNames = TRUE)
  exac_mortality_by_sev_year  <- as.data.frame(exac_mortality_by_sev_year)
  dfm <- reshape2::melt(exac_mortality_by_sev_year  [,c("Year", "Mild", "Moderate", "Severe and Very Severe")], id.vars = 1)

  plot_exac_mortality_by_sev_year <- ggplot2::ggplot(dfm, aes(x = Year, y = value)) +  theme_tufte(base_size=14, ticks=F) +
    geom_bar(aes(fill = variable), stat = "identity", position = "dodge") +
    labs(title = "Exacerbation Mortality by Severity and Year") + ylab ("%")

  plot(plot_exac_mortality_by_sev_year) #plot needs to be showing
  openxlsx::insertPlot(wb, "exac_mortality_by_sev_year",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")


  #####################################################  Average pack-years per year  #####################################################

  dataS <- subset (data, (event == 0 | event == 1 ))
  data_all <- dataS
  dataS <- subset (dataS, pack_years != 0)
  avg_pack_years_ctime <- matrix (NA, nrow = input$global_parameters$time_horizon + 1, ncol = 4)
  colnames(avg_pack_years_ctime) <- c("Year", "Smokers PYs", "Former Smokers PYs", "all")

  avg_pack_years_ctime[1:(input$global_parameters$time_horizon + 1), 1] <- c(2015:(2015 + input$global_parameters$time_horizon))

  for (i in 0:input$global_parameters$time_horizon) {
    smokers <- subset (dataS, (floor(local_time + time_at_creation) == (i)) & smoking_status != 0)
    prev_smokers <- subset (dataS, (floor(local_time + time_at_creation) == (i)) & smoking_status == 0)
    all <- subset (data_all, floor(local_time + time_at_creation) == i)
    avg_pack_years_ctime[i+1, "Smokers PYs"] <- colSums(smokers)[["pack_years"]] / dim (smokers)[1]
    avg_pack_years_ctime[i+1, "Former Smokers PYs"] <- colSums(prev_smokers)[["pack_years"]] / dim (prev_smokers) [1]
    avg_pack_years_ctime[i+1, "all"] <- colSums(all)[["pack_years"]] / dim (all) [1]

  }
  openxlsx::writeData(wb, "avg_pack_years_ctime", avg_pack_years_ctime , startCol = 2, startRow = 3, colNames = TRUE)
  avg_pack_years_ctime <- as.data.frame(avg_pack_years_ctime)
  dfm <- reshape2::melt(avg_pack_years_ctime[,c( "Year", "Smokers PYs", "Former Smokers PYs", "all")], id.vars = 1)
  plot_avg_pack_years_ctime <- ggplot2::ggplot(dfm, aes(x = Year, y = value, color = variable)) +  theme_tufte(base_size=14, ticks=F) +
    geom_point () + geom_line() + labs(title = "Average pack-years per year ") + ylab ("Pack-years")

  plot(plot_avg_pack_years_ctime) #plot needs to be showing
  openxlsx::insertPlot(wb, "avg_pack_years_ctime",  xy = c("G", 3), width = 20, height = 13.2 , fileType = "png", units = "cm")

  ####################################################### Save workbook #####################################################
  ## Open in excel without saving file: openXL(wb)
  if(is.null(directory)) {
    wbfilename <- paste0(Sys.Date(), " Figures EpicR ver", packageVersion("epicR"), ".xlsx")
  } else {
    wbfilename <- paste0(directory, "/", Sys.Date(), " Figures EpicR ver", packageVersion("epicR"), ".xlsx")
  }
  openxlsx::saveWorkbook(wb, wbfilename, overwrite = TRUE)

  }
