
#----------- Exacerbations -------------------------------------------------------------
#' @title Plot COPD Exacerbations
#' @description Creates a plot of exacerbations
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
                             exacType = c("severe", "all"),
                             perCapita = NULL){

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

#--------------------- COPD Incidence/Occurrence -------------------------------------------

#' @title Plot COPD Occurrence/Incidence
#' @description Creates a plot of exacerbations
#' @param nPatients number of patients/agents/individuals to simulate
#' @param argX string: one of "year", "age"
#' @param argY string: one of "incidence", "prevalence"
#' @param groupBy string: one of "sex", "age", "gold"
#' @param aggregateBy string: one of "age", "none"
#' @param perCapita numeric: if null, parameter is ignored, otherwise, population size to look at
#' @return plot
#' @export
plotCOPD = function(nPatients = 1e4,
                    argX = "age",
                    argY = "incidence",
                    aggregateBy = "none",
                    groupBy = NULL,
                    perCapita = NULL){

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
  extendedResults <- Cget_output_ex()
  terminate_session()
  themeColors = c("#330033", "#8cf2f2", "#c51672", "#007bff")

  if(argX == "age") {
    if(aggregateBy == "age") {
      xOptions = c("40-50", "50-60", "60-70", "70-80", "80-90")
    } else {
      xOptions = c(40:90)
    }
    xLabel = "Age"
  } else if(argX == "year") {
    xOptions = c(2015:(2015+input$global_parameters$time_horizon-1))
    xLabel = "Year"
  }
  if(groupBy == "sex") {
    groupByNames = c("Male", "Female")
    legendTitle = "Gender"
    colorPalette = c("#CC6666", "#56B4E9")
  } else {
    groupByNames = c("Incidence")
    legendTitle = ""
    colorPalette = themeColors[4]
  }
  columnNames = c(xLabel, groupByNames)
  numRows = length(xOptions)
  if(argY == "incidence") {
    yMatrix = getIncidence(extendedResults, eventsMatrix, aggregateBy, argX, numRows)
    yTitle = "COPD Incidence (%)"
    plotTitle = paste0("COPD Incidence by ", xLabel)
  } else if(argY == "prevalence"){
    yMatrix = getPrevalence(extendedResults, eventsMatrix, aggregateBy, argX, numRows)
    yTitle = "COPD Prevalence (%)"
    plotTitle = paste0("COPD Prevalence by ", xLabel)
  }
  yMatrix = as.data.frame(yMatrix) * 100 #converting values to percentage.
  alive = getAlive(extendedResults, groupBy, aggregateBy = aggregateBy)

  standardError <- sqrt (yMatrix * (100 - yMatrix) / alive)

  df <- as.data.frame(cbind(xOptions, yMatrix))
  colnames(df) <- columnNames
  dfm <- reshape2::melt(df[,columnNames], id.vars = 1)

  errorbar_min <- yMatrix - 1.96*standardError
  errorbar_max <- yMatrix + 1.96*standardError

  colnames(errorbar_max) = groupByNames
  errorbar_max = reshape2::melt(errorbar_max[,groupByNames])
  errorbar_max = errorbar_max$value
  colnames(errorbar_min) = groupByNames
  errorbar_min = reshape2::melt(errorbar_min[,groupByNames])
  errorbar_min = errorbar_min$value

  plotCaption = "(based on population at age 40 and above)"
  if(length(columnNames)==2) {
    legendPosition = "none"
  } else {
    legendPosition = "right"
  }

  p <- ggplot(dfm, aes_string(x = xLabel, y = "value", fill="variable")) +
    theme_tufte(base_size=14, ticks=F) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = errorbar_min, ymax = errorbar_max), width=.2, position = position_dodge(.9)) +
    labs(title = plotTitle) +
    ylab (yTitle) +
    labs(caption = plotCaption)+
    scale_fill_manual(legendTitle, values = colorPalette)+
    theme(legend.position=legendPosition)

  p
}

getIncidence = function(extendedResults, eventsMatrix, aggregateBy, argX, numRows) {

  if(argX == "age") {
  incidence = matrix (NA, nrow = 51, ncol = 1) #limiting it to people under 90 years old to avoid noise error

  incidence[,1] = colSums(extendedResults$n_inc_COPD_by_ctime_age[,40:90])


  if(aggregateBy == "age") {
    groupedIncidence = matrix (0, nrow = 5, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
    groupedAlive = getAlive(extendedResults, "age", aggregateBy)
    for(i in 0:4) {
      indices = c((10*i+1):(10*i + 10))
      groupedIncidence[i+1,] = sum(incidence[indices,]) / groupedAlive[i+1,]
    }
    return(groupedIncidence)
  } else {
    alive = getAlive(extendedResults, "age", "none")
    incidence = incidence / alive
    return(incidence)
  }
  } else if(argX == "year") {
    incEvent = subset(eventsMatrix, event == 4)
    incidence = matrix(NA, nrow = numRows, ncol = 2)
    for (i in 1:numRows){
      incidence[i, 1] = dim(subset(incEvent, ((female == 0) & (ceiling(local_time + time_at_creation) == i)) ))[1]
      incidence[i, 2] = dim(subset(incEvent, ((female == 1) & (ceiling(local_time + time_at_creation) == i)) ))[1]
    }
    alive = getAlive(extendedResults, "sex", "none")
    incidence = incidence / alive
    return(incidence)
  }
}

getPrevalence = function(extendedResults, eventsMatrix, aggregateBy, argX, numRows) {

  if(argX == "age") {
    prevalence = matrix (NA, nrow = 51, ncol = 1) #limiting it to people under 90 years old to avoid noise error

    prevalence[,1] = colSums(extendedResults$n_COPD_by_ctime_age[,40:90])


    if(aggregateBy == "age") {
      groupedPrevalence = matrix (0, nrow = 5, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
      groupedAlive = getAlive(extendedResults, "age", aggregateBy)
      for(i in 0:4) {
        indices = c((10*i+1):(10*i + 10))
        groupedPrevalence[i+1,] = sum(prevalence[indices,]) / groupedAlive[i+1,]
      }
      return(groupedPrevalence)
    } else {
      alive = getAlive(extendedResults, "age", "none")
      prevalence = prevalence / alive
      return(prevalence)
    }
  } else if(argX == "year") {
    prevalence = matrix (NA, nrow = numRows, ncol = 2) #limiting it to people under 90 years old to avoid noise error

    prevalence[,1:2] = extendedResults$n_COPD_by_ctime_sex
    alive = getAlive(extendedResults, "sex", "none")
    prevalence = prevalence / alive
    return(prevalence)
  }
}

getAlive = function(extendedResults, groupBy, aggregateBy) {
  idString = paste0("n_alive_by_ctime_", groupBy)
  if(groupBy == "age" || groupBy == "none"){
    idString = paste0("n_alive_by_ctime_", "age")
  alive = as.data.frame(colSums(as.data.frame (extendedResults[[idString]]))[40:90])
  if(aggregateBy == "age") {
    groupedAlive = matrix (0, nrow = 5, ncol = 1) #40-50, 50-60, 60-70, 70-80,  80-90, 90+
    for(i in 0:4) {
      indices = c((10*i+1):(10*i + 10))
      groupedAlive[i+1,] = sum(alive[indices,])
    }
    return(groupedAlive)
  } else {
    return(alive)
  }
  } else if(groupBy == "sex") {
    alive = as.data.frame(extendedResults[[idString]])
    return(alive)
  }
}
