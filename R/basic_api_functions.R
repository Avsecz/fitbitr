## https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

## required functions
## library(httr)
## library(jsonlite)
## library(RCurl)


##' Make a GET request on \url{https://api.fitbit.com}.
##'
##' This is the main building block of this package. It is a wrapper for the \code{\link[httr]{GET}} function.
##' 
##' @param path Character scalar specifying the requested address from \url{https://api.fitbit.com/}.
##' @param token Fitbit API token generated with \code{\link{get_fitbit_token}}.
##' @param ... Additional arguments passed to \code{\link{get_fitbit_token}}.
##' @return Response object of class "response", as returned by the \code{\link[httr]{GET}} function. Use \code{\link{fitbit_parse}} to get the body of the output.
##' 
##' @details This function has one side effect: if updates the "rate_limit" attribute in the token according to the returned request header.
##' @examples
##' \dontrun{
##' resp <- fitbit_GET("1/user/-/sleep/date/2015-12-12.json", token = fitbit_token )
##' ret <- fitbit_parse(resp)
##' }
##'
##' @export
##' @import httr
##' @import data.table
fitbit_GET <- function(path, token, ...){
  req <- GET("https://api.fitbit.com", path = path, config = config(token = token))
  ## add_headers("Accept-Locale" = "Germany"))
  fitbit_check(req)

  ## append token information
  attr(token, "rate_limit") <- extract_rate_limit(req)
  req
}

##" Check if the request was successful
##" HIDE
fitbit_check <- function(req) {
  if (req$status_code < 400) return(invisible())

  message <- fitbit_parse(req)$message
  stop("HTTP failure: ", req$status_code, "\n", message, call. = FALSE)
}

##' Parse the output of \code{\link{fitbit_GET}} using \code{\link[jsonlite]{fromJSON}}.
##'
##' @param req Output object of \code{\link{fitbit_GET}}.
##' @return List representing the JSON content of the GET response.
##'
##' @export
fitbit_parse <- function(req) {
  text <- content(req, as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

##' Get the fitbit authentication token.
##'
##' This function generates the fitbit token and performs the required API authorization.
##' 
##' @param file_path Character scalar; Path to the credentials file described in details. Leave the arguments key, secret and scope set to NULL if you want to use the credentials file for the authentication.
##' @param appname Character scalar; Fitbit application name.
##' @param key Character scalar; Fitbit OAuth 2.0 Client ID.
##' @param secret Character scalar; Fitbit Client (Consumer) Secret. 
##' @param scope Character vector; API permission scope. Leave the defaults in order to access
##'   all the features listed in \code{\link{activity_features}}.
##' @return Fitbit token providing the access to your Fitbit account data.
##'
##' @details Request rate limit information is stored in the fitbit token as the "rate_limit" attribute. It gets updated every time fitbit_GET is called.
##'
##' @details Example credentials file (3 rows: appname, key, secret):
##' \preformatted{
##' GetMyData
##' 14C889
##' apaojsdpoj213412p4j115j1p5oj21p5o
##' }
##' 
##' @examples
##' \dontrun{
##' ## If the credentials file is located in "~/.fitbitr" just run:
##' fitbit_token <- get_fitbit_token()
##'
##' ## Else, specify the new credentials file location:
##' 
##' fitbit_token <- get_fitbit_token(file_path = "~/.custom_fitbitr")
##'
##' ## or enter the credentials as arguments:
##' fitbit_token <- get_fitbit_token(appname = "GetMyData",
##'                                  key = "14C889",
##'                                  secret = "apaojsdpoj213412p4j115j1p5oj21p5o"
##'                                  )
##' }
##' @export
get_fitbit_token <- function(file_path = "~/.fitbitr",
                             appname = NULL,
                             key = NULL,
                             secret = NULL,
                             scope = c("activity", "heartrate", "sleep", "weight")) {

  ## read the file from the file
  if (all(is.null(appname), is.null(key), is.null(secret))) {
    myapp <- read_fitbit_auth(file_path = file_path)
  } else {
    ## all app args have to be specified
    if (any(is.null(appname), is.null(key), is.null(secret))) {
      warning("Please provide all the authorization arguments (appname, key, secret) \nor none at all. file_path will be used to get the credentials.")
      myapp <- read_fitbit_auth(file_path = file_path)
    } else {
      ## generate the app
      myapp <- oauth_app(
        appname = appname,
        key = key,
        secret = secret
      )
    }
  }
  
  fitbit_endpoint <- oauth_endpoint(
    request = "https://api.fitbit.com/oauth2/token",
    authorize = "https://www.fitbit.com/oauth2/authorize",
    access = "https://api.fitbit.com/oauth2/token")

  fitbit_token <- oauth2.0_token(endpoint = fitbit_endpoint,
                                 app = myapp,
                                 scope = scope,
                                 use_basic_auth = TRUE)

  attr(fitbit_token, "rate_limit") <- list(total = 150,
                                      remains = 150,
                                      next_reset = NA,
                                      current_time = Sys.time())
  return(fitbit_token)
}

##" Read the authorization information from file
##"
##" This function reads lines from \code{file_path} and then uses \code{\link[httr]{oauth_app}}
##" to generate the app object.
##"
##" @details Example authorization file (3 rows: appname, key, secret):
##" \preformatted{
##" GetMyData
##" 14C889
##" apaojsdpoj213412p4j115j1p5oj21p5o
##" }
##" 
##" #@param file_path Character scalar; File path to the authorization file.
##" #@return oauth_app used by \code{\link{get_fitbit_token}} as input.
read_fitbit_auth <- function(file_path = "~/.fitbitr"){
  if(!file.exists(file_path)) stop("Authorization file (", file_path, ") doesn't exits")

  fitbit_info <- readLines(file_path)
  stopifnot(length(fitbit_info)==3)

  myapp <- oauth_app(
    appname = fitbit_info[1],
    key = fitbit_info[2],
    secret = fitbit_info[3]
  )
  return(myapp)
}

##' Refresh the expired fitbit token.
##'
##' This function refreshes the fitbit token as described in \url{https://dev.fitbit.com/docs/oauth2/#refreshing-tokens}.
##' The internal refresh function (\code{refresh_oauth2.0})
##' of the \code{\link{httr}} package somehow fails to refresh it.
##'
##' @param token Fitbit API token generated with \code{\link{get_fitbit_token}}.
##' @examples
##' \dontrun{
##' token <- refresh_fitbit_token(token)
##' }
##' @export
refresh_fitbit_token <- function(token){
  ## See the implementation of oauth-refresh.
  ## https://github.com/hadley/httr/blob/master/R/oauth-refresh.R
  endpoint <- token$endpoint
  app <- token$app
  credentials <- token$credentials

  if (is.null(credentials$refresh_token)) {
    stop("Refresh token not available", call. = FALSE)
  }

  response <- POST("https://api.fitbit.com/oauth2/token",
                   encode = "form",
                   body = list(grant_type = "refresh_token",
                               refresh_token = credentials$refresh_token
                               ),
                   add_headers(Authorization =
                                 paste("Basic",
                                       RCurl::base64(paste0(app$key,":", app$secret)))
                               ),
                   content_type("application/x-www-form-urlencoded")
                   )


  refresh_data <- content(response)

  ## not
  if(!("access_token" %in% names(refresh_data))) {
    warning("Unable to refresh token")
    warning(refresh_data$errors[[1]]$message)
    return(NULL)
  }

  new_credentials <- utils::modifyList(credentials, refresh_data)

  token$credentials <- new_credentials
  return(token)
}

##" Parse response to get the request limit info.
##" Number of requests left.
##" TODO - display error only if we are close to the end of requests
##" @param req Request response of \code{\link{fitbit_GET}}.
extract_rate_limit <- function(req){
  req_headers <- headers(req)

  total <- req_headers[["fitbit-rate-limit-limit"]]
  remains <- req_headers[["fitbit-rate-limit-remaining"]]
  next_reset <- req_headers[["fitbit-rate-limit-reset"]]
  current_time <- Sys.time()

  ## current_time <- req_headers[["date"]]
  ## current_time <- strsplit(current_time, split = " ")[[1]][5:6]
  ## current_time <- strptime(current_time[1], format = "%H:%M:%OS", tz = current_time[2])
  ## base::format(as.POSIXct(current_time), tz = base::format(Sys.time(), format="%Z"), usetz=TRUE)
  ## Sys.Date()

  return(list(total = as.integer(total),
              remains = as.integer(remains),
              next_reset = as.integer(next_reset),
              current_time = current_time
              )
         )
  ## cat(remains, " / ", total, " (Reset in ", round(as.integer(next_reset)/60), " min)\n", sep = "");
}


## BUG - Reset can be negative.
print_fitbit_rate_limit <- function(rate_limit) {

  next_reset <- rate_limit$current_time + rate_limit$next_reset
  due_reset <- round(as.numeric(as.character(next_reset - Sys.time())))

  message("Requests left:\n",rate_limit$remains, " / ",
      rate_limit$total,
      " (reset in ", due_reset,
      " min, at ", strftime(next_reset, "%H:%M:%S"), ")", sep = "")

}

get_rate_limit <- function(token) {
  return(attr(token, "rate_limit"))
}

##' Get the number of requests left
##'
##' This function displays the information about how many API requests are left and
##' when the rate limit will be reset.
##' 
##' @param token Fitbit API token generated with \code{\link{get_fitbit_token}}.
##' @param show_message Boolean scalar; Should the function print the message?
##' @return List containing the information about the number of requests left returned invisibly.
##'
##' @examples
##' \dontrun{
##' resp <- fitbit_GET("1/user/-/sleep/date/2015-12-12.json", token = fitbit_token )
##' rate_limit(fitbit_token)
##' ## Requests left:
##' ## 120 / 150 (reset in 22 min, at 18:00:01)
##' }
##' @export
rate_limit <- function(token, show_message = TRUE) {
  rl <- get_rate_limit(token)
  if(show_message == TRUE) print_fitbit_rate_limit(rl)
  return(invisible(rl))
}


