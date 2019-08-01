# ---- Exacerbations --------

#' @title Plot Exacerbations
#' @description Creates plots of exacerbations
#' @param nPatients number of patients/agents/individuals to simulate
#' @param argX string: one of "year"
#' @param argY string: one of "exacerbation_rate", "number_of_exacerbations"
#' @param groupBy string: one of "sex", "age", "gold"
#' @param exacType string: "all" or "severe"
#' @param perCapita numeric: if null, parameter is ignored, otherwise, population size to look at
#' @return Returns a plot

#' @export
plotExacerbations = function(nPatients = 1e4,
                             argX = "year",
                             argY = "exacerbation_rate",
                             groupBy = "sex",
                             exacType = c("all", "severe"), perCapita = NULL){

  settings <- default_settings
  settings$record_mode <- record_mode["record_mode_event"]
  settings$agent_stack_size <- 0
  settings$n_base_agents <- nPatients
  settings$event_stack_size <- nPatients * 1.7 * 30 * 2
  init_session(settings = settings)
  input <- model_input$values
  run(input=input)
  eventsMatrix <- as.data.frame(Cget_all_events_matrix())
  op <- Cget_output()
  extendedResults <- getExResults()
  terminate_session()

  argX <- match.arg(argX)
  genderOptions = c("male", "female")
  exacOptions = c("Mild", "Moderate", "Severe", "Very Severe")
  ageOptions = c("40-55", "55-70", "70-85", "85+", "All")
  goldOptions = c("GOLD I", "GOLD II", "GOLD III", "GOLD IV")

  if(argX == "year") {
    columnNames = c("Year")
  }
  if(groupBy == "sex") {
    columnNames = c(columnNames, genderOptions)
  } else if(groupBy == "age") {
    columnNames = c(columnNames, ageOptions)
  }

  numColumns = length(columnNames)
  numRows = input$global_parameters$time_horizon
  data = matrix (NA, nrow = numRows, ncol = numColumns)
  colnames(data) = columnNames
  data[1:input$global_parameters$time_horizon, 1] = c(2015:(2015+input$global_parameters$time_horizon-1))

  if(argY=="number_of_exacerbations") {
    idString = paste0('n_exac_by_ctime_', groupBy)
    prelimData = extendedResults[[idString]]
    groupedData = groupAgeColumns(prelimData, nPatients)
    data[,2:6] = groupedData
    graphTitle = "Number of Exacerbations by Age Group"
    yTitle = "Number of Exacerbations"
    colorScale = c("#330033", "#8cf2f2", "#c51672", "#007bff", "#56B4E9")
  }
  if(argY=="exacerbation_rate") {
    if(groupBy=="sex") {
      rates = exacerbationRateGender(extendedResults, numRows, exacType)
      data[,2] = rates$male
      data[,3] = rates$female
      if(exacType=="severe") {
        graphTitle = "Severe and Very Severe Exacerbation Rate by Gender"
      } else {
        graphTitle = "Exacerbation Rate by Gender"
      }
      yTitle = "Exacerbation Rate"
      colorScale = c("#CC6666", "#56B4E9")
    } else if(groupBy == "gold") {
      rates = exacerbationRateGold(extendedResults, numRows)
      data[,2:5] = rates
      graphTitle = "Exacerbation Rate by GOLD Status"
      yTitle = "Exacerbation Rate"
      colorScale = c("#330033", "#8cf2f2", "#c51672", "#007bff")
    }
  }

  if(!is.null(perCapita)) {
    data[,2:numColumns] = data[,2:numColumns] / perCapita
  }

  data <- as.data.frame(data)
  dfm <- reshape2::melt(data[,columnNames],id.vars = 1)
  names(dfm) = c("Year", groupBy, "value")

  p <- ggplot(dfm, aes_string(x = "Year", y = "value", color = groupBy)) +
  theme_tufte(base_size=14, ticks=F) +
  geom_point () +
  geom_line() +
  labs(title = graphTitle) +
  ylab (yTitle)  +
  scale_colour_manual(values = colorScale) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 12))

  p
}


exacerbationRateGender = function(extendedResults, numRows, exacType = c("all", "severe")) {

  exacType = match.arg(exacType)
  if(exacType == "all") {
    indices = c(1,2,3,4)
  } else {
    indices = c(3,4)
  }
  rates = data.frame(male = rep(0, numRows))
  rates$female <- rowSums(extendedResults$n_exac_by_ctime_severity_female [,indices]) / (extendedResults$n_COPD_by_ctime_sex)[,2]
  rates$male <- rowSums(extendedResults$n_exac_by_ctime_severity [,indices] -
                                            extendedResults$n_exac_by_ctime_severity_female [,indices]) /
    (extendedResults$n_COPD_by_ctime_sex)[,1]
  rates$all = rowSums(extendedResults$n_exac_by_ctime_severity[,indices]) /
    rowSums(extendedResults$n_COPD_by_ctime_sex[,c(1,2)])
  return(rates)
}

exacerbationRateGold = function(extendedResults, numRows, numColumns) {
  rates = matrix (0, nrow = numRows, ncol = 4)
  rates[2:20, 1:4] = extendedResults$n_exac_by_ctime_GOLD[2:20, 1:4] /
    (extendedResults$cumul_time_by_ctime_GOLD)[2:20,2:5]
  return(rates)
}


groupAgeColumns = function(data, nPatients) {
  numRows = nrow(data)
  dataGrouped <- matrix (NA, nrow = numRows, ncol = 5)
  for (i in (1:3)){
    dataGrouped[,i] <- rowSums(data[, (25+(15*i)):(35+15*(i+1-1))])
  }

  dataGrouped[,4] <- rowSums(data[, 85:111]) # special case of 80+
  dataGrouped[,5] <- rowSums(dataGrouped[, 1:4]) # all

  dataGrouped[, 1:5] = dataGrouped[,1:5] / nPatients * 18e6 #18e6 is roughly the 40+ population of Canada as of 2015
  return(dataGrouped)
}




