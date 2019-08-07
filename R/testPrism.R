
  p = function(...) {
      arguments = list(...)
      func <- arguments$func
      session_id <- arguments$session_id
      api_key <- arguments$api_key
      if (is.null(session_id)) {
          session_id ="\\"
          }
      if (is.null(api_key)){
          api_key = "\\"
          }
      if (func != "test\\"){
        check_access(api_key, session_id, func)
      }
      set_redis_var(paste("RT:LastModelCall:\\", api_key, sep = "\\"),
          get_my_name())
      set_redis_var(paste("RT:LastCallTime:\\", api_key, sep = "\\"),
          Sys.time())
      rredis::redisIncr(paste("RT:CallCount:\\", api_key, sep = "\\"))
      rredis::redisRPush(paste("RT:CallTimes:\\", api_key, sep = "\\"),
          Sys.time())
      session_id <<- session_id
      arguments$func <- NULL
      arguments$api_key <- NULL
      arguments$session_id <- NULL
      if (!is.null(session_id)){
          restore_session(session_id)
      }
      if (length(arguments) == 0) {
          out <- eval(parse(text = paste(func, "()\\")))
      }
      else {
          out <- eval(parse(text = paste(func, substring(deparse(arguments),
              5))))
      }
      if (!is.null(session_id)){
          save_session(session_id)
      }
      return(jsonlite::toJSON(out))
  }

  modelRun = function(model_input = NULL)
  {
    input<-unflatten_list(model_input)

    debug<-input$debug
    input$debug<-NULL

    settings<-epicR::get_default_settings()
    new_settings<-input$setting

    l<-length(new_settings)
    if(l>0)
      for(i in 1:l)
      {
        settings[names(new_settings[i])]<-new_settings[[i]]
      }

    input$setting<-NULL

    init_session(settings=settings)

    res<-epicR::run(input=input)

    output_ex<<-Cget_output_ex()

    plot(output_ex$n_alive_by_ctime_sex[,1],type='l',col='red')
    lines(output_ex$n_alive_by_ctime_sex[,1],type='l',col='green')

    data(cars)
    plot(cars)

    terminate_session()

    if(!is.null(debug$input_back) && debug$input_back==1)
    {
      input_back<-list()
      input_back$setting<-Cget_settings()
      input_back$input<-Cget_inputs()

      if(res==0)
        return(c(status=0,flatten_list(Cget_output()),Cget_output_ex(),flatten_list(input_back)))
      else
        return(c(list(status=paste(as.vector(epicR::get_errors()),sep="+",collapse = ".")),flatten_list(input_back)))

    }
    else
    {
      if(res==0)
        return(c(status=0,flatten_list(Cget_output()),Cget_output_ex()))
      else
        return(list(status=paste(as.vector(epicR::get_errors()),sep="+",collapse = ".")))
    }
  }
