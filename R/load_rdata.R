load_rdata <- function(path) {
  env <- new.env()
  load(path, envir = env)
  return(as.list(env))
}