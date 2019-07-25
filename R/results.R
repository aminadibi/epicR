
getExResults = function() {

  extendedResults = Cget_output_ex()
  outputNames = names(extendedResults)
  timeHorizon = nrow(extendedResults$n_alive_by_ctime_age)
  maxAge = ncol(extendedResults$n_alive_by_ctime_age)
  sexOptions = c("Male", "Female")
  timeOptions = paste("Year", seq(1, timeHorizon, by=1))
  ageOptions = paste(seq(0, maxAge-1, by=1), "years old")
  smokingOptions = c("Sustained Quitter", "Intermittent Quitter", "Continuous Smoker")
  goldOptions = c("GOLD I", "GOLD II", "GOLD III", "GOLD IV")
  exacerbationOptions = c("Mild", "Moderate", "Severe", "Very Severe")

  for(name in outputNames) {
    res = extendedResults[[name]]
    if(grepl("by", name)) {
      by = strsplit(name, "by_")[[1]][2]
    } else {
      next
    }
    if(class(res)!="matrix") {
      next
    }
    if(grepl("ctime", by)) {
      rownames(res) = timeOptions
    }
    if(grepl("sex", by)) {
      colnames(res) = sexOptions
    }
    if(grepl("age", by)) {
      if(grepl("sex", by)) {
        rownames(res) = ageOptions
      } else {
        colnames(res) = ageOptions
      }
    }
    if(grepl("smoking_status", by)) {
      colnames(res) = smokingOptions
    }
    if(grepl("gold", by)) {
      if(grepl("exac", name)) {
        if(grepl("severity", by)) {
          rownames(res) = goldOptions
        } else {
          colnames(res) = goldOptions
        }
      }
    }
    if(grepl("severity", by)) {
      if(grepl("exac", name)) {
        colnames(res) = exacerbationOptions
      }
    }

    extendedResults[[name]] = res
  }

  return(extendedResults)
}

getMainResults = function() {
  mainResults = Cget_output()
}
