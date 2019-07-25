Rcpp::sourceCpp("./src/model.WIP.cpp")

session_env<-new.env()
session_env$global_error_code_chain<-NULL
session_env$global_error_message_chain<-NULL



# Cleaning up when package unloads
.onUnload <- function(libpath) {
  library.dynam.unload("epicR", libpath)
}


default_settings <- list(record_mode = record_mode["record_mode_event"],
                         events_to_record = c(0),
                         agent_creation_mode = agent_creation_mode["agent_creation_mode_one"],
                         update_continuous_outcomes_mode = 0,
                         n_base_agents = 6e4,
                         runif_buffer_size = 5e4,
                         rnorm_buffer_size = 5e4,
                         rexp_buffer_size = 5e4,
                         agent_stack_size = 0,
                         event_stack_size = 5e4 * 1.7 * 30)

#' Exports default settings
#' @return default settings
#' @export
get_default_settings<-function()
{
  return(default_settings)
}



# Population of Canada over 40 years by StatsCan 18,415.60

#' Initializes a model. Allocates memory to the C engine.
#' @param settings customized settings.
#' @return 0 if successful.
#' @export
init_session <- function(settings = get_default_settings()) {
  message("Initializing the session")
  if (exists("Cdeallocate_resources"))
    Cdeallocate_resources()
  if (!is.null(settings))
    apply_settings(settings)
  init_input()
  Cinit_session()
  return(Callocate_resources())
}

#' Terminates a session and releases allocated memory.
#' @return 0 if successful.
#' @export
terminate_session <- function() {
  message("Terminating the session")
  return(Cdeallocate_resources())
}


apply_settings <- function(settings = settings) {
  res <- 0
  ls <- Cget_settings()
  for (i in 1:length(ls)) {
    nm <- names(ls)[i]
    # message(nm)
    if (!is.null(settings[nm])) {
      res <- Cset_settings_var(nm, settings[[nm]])
      if (res != 0)
        return(res)
    }
  }
  return(res)
}



update_run_env_setting <- function(setting_var, value) {
  res <- Cset_settings_var(setting_var, value)
  if (res < 0)
    return(res)
  settings[setting_var] <<- value
  return(0)
}




#' Get list elements
#' @param ls ls
#' @param running_name running_name
#' @export
get_list_elements <- function(ls, running_name = "") {
  out <- NULL
  if (length(ls) > 0) {
    for (i in 1:length(ls)) {
      if (typeof(ls[[i]]) == "list") {
        out <- c(out, paste(names(ls)[i], "$", get_list_elements(ls[[i]]), sep = ""))
      } else {
        out <- c(out, names(ls)[i])
      }
    }
  }
  return(out)
}


set_Cmodel_inputs <- function(ls) {
  if(length(ls)==0) return(0)
  nms <- get_list_elements(ls)
  for (i in 1:length(nms)) {
    last_var <- nms[i]
    # message(nms[i])
    val <- eval(parse(text = paste("ls$", nms[i])))
    # important: CPP is column major order but R is row major; all matrices should be tranposed before vectorization;
    if (is.matrix(val))
      val <- as.vector(t(val))
    res <- Cset_input_var(nms[i], val)
    if (res != 0) {
      message(last_var)
      set_error(res,paste("Invalid input:",last_var))
      return(res)
    }
  }

  return(0)
}

#' Express matrix.
#' @param mtx a matrix
#' @export
express_matrix <- function(mtx) {
  nr <- dim(mtx)[1]
  nc <- dim(mtx)[2]
  rnames <- rownames(mtx)
  cnames <- colnames(mtx)

  for (i in 1:nc) {
    cat(cnames[i], "=c(")
    for (j in 1:nr) {
      if (!is.null(rnames))
        cat(rnames[j], "=", mtx[j, i]) else cat(mtx[j, i])
      if (j < nr)
        cat(",")
    }
    cat(")\n")
    if (i < nc)
      cat(",")
  }
}  #Takes a named matrix and write the R code to populate it; good for generating input expressions from calibration results



#' Returns events specific to an agent.
#' @param id Agent number
#' @return dataframe consisting all events specific to agent \code{id}
#' @export
get_agent_events <- function(id) {
  x <- Cget_agent_events(id)
  data <- data.frame(matrix(unlist(x), nrow = length(x), byrow = T))
  names(data) <- names(x[[1]])
  return(data)
}

#' Returns certain events by type
#' @param event_type event_type number
#' @return dataframe consisting all events of the type \code{event_type}
#' @export
get_events_by_type <- function(event_type) {
  x <- Cget_events_by_type(event_type)
  data <- data.frame(matrix(unlist(x), nrow = length(x), byrow = T))
  names(data) <- names(x[[1]])
  return(data)
}

#' Returns all events.
#' @return dataframe consisting all events.
#' @export
get_all_events <- function() {
  x <- Cget_all_events()
  data <- data.frame(matrix(unlist(x), nrow = length(x), byrow = T))
  names(data) <- names(x[[1]])
  return(data)
}



#' Runs the model, after a session has been initialized.
#' @param max_n_agents maximum number of agents
#' @param input customized input criteria
#' If no input is provided, will use the default input
#' See createInput function
#' @return 0 if successful.
#' @export
run <- function(max_n_agents = NULL, input = NULL) {

  #Cinit_session()
  #In the updated version (2019.02.21) user can submit partial input. So better first set the input with default values so that partial inputs are incremental.

  reset_errors()

  default_input<-init_input()$values
  res<-set_Cmodel_inputs(process_input(default_input))
  print(res)
  if (!is.null(input) || length(input)==0)
  {
    res<-set_Cmodel_inputs(process_input(input))
    if(res<0)
    {
      set_error(res,"Bad Input")
    }
  }

  if (res == 0) {
    if (is.null(max_n_agents)) {
      max_n_agents = .Machine$integer.max
    }
    res <- Cmodel(max_n_agents)
    print(res)
  }
  if (res < 0) {
    message("ERROR:", names(which(errors == res)))
  }

  return(res)

}

#' Resumes running of model.
#' @param max_n_agents maximum number of agents
#' @return 0 if successful.
#' @export
resume <- function(max_n_agents = NULL) {
  if (is.null(max_n_agents))
    max_n_agents = settings$n_base_agents
  res <- Cmodel(max_n_agents)
  if (res < 0) {
    message("ERROR:", names(which(errors == res)))
  }
  return(res)
}




# processes input and returns the processed one
process_input <- function(ls, decision = 1)
{
  if(!is.null(ls$manual))
  {
    ls$agent$p_bgd_by_sex <- ls$agent$p_bgd_by_sex - ls$manual$explicit_mortality_by_age_sex
    ls$agent$p_bgd_by_sex <- ls$agent$p_bgd_by_sex


    ls$smoking$ln_h_inc_betas[1] <- ls$smoking$ln_h_inc_betas[1] + log(ls$manual$smoking$intercept_k)

    ls$manual <- NULL
  }
  return(ls)
}




reset_errors<-function()
{
  session_env$global_error_code_chain<-NULL
  session_env$global_error_message_chain<-NULL
}



set_error <- function(error_code, error_message="")
{
  session_env$global_error_code_chain<-c(session_env$global_error_code_chain,error_code)
  session_env$global_error_message_chain<-c(session_env$global_error_message_chain,error_message)
}


#' Returns errors
#' @return a text with description of error messages
#' @export
get_errors <- function()
{
  return(cbind(session_env$global_error_code_chain,session_env$global_error_message_chain))
}

