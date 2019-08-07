
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
    # out <- eval(parse(text = paste(func, substring(deparse(arguments),
    #                                                5))))
    out <- do.call(func, args = arguments)
  }
  if (!is.null(session_id)){
    save_session(session_id)
  }
  return(jsonlite::toJSON(out))
}
