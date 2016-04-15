## Parse functions

##' List all possible activity features
##'
##' @return Character vector of all possible features.
##' @examples
##' activity_features()
##' 
##' ## [1] "calories"  "steps"     "distance"  "floors"    "elevation" "heart"
##' @export
activity_features <- function(){
  return(c("calories",
           "steps",
           "distance",
           "floors",
           "elevation",
           "heart"                      #heart rate
           ))
}

##' Get the information about the activity features.
##' 
##' Lists the minimal `detail_level` (argument of \code{\link{get_activity}}) for each feature in `features`
##'
##' @param features Character vector of features.
##' @return Data.table with 2 columns: features and minimal_detail_level.
##' @examples
##' activity_features_info()
##' 
##' ##     features minimal_detail_level
##' ## 1:  calories                 1min
##' ## 2:     steps                 1min
##' ## 3:  distance                 1min
##' ## 4:    floors                 1min
##' ## 5: elevation                 1min
##' ## 6:     heart                 1sec
##' @export
activity_features_info <- function(features = activity_features()){
  stopifnot(all(features %in% activity_features()))
  data.table(features, minimal_detail_level = ifelse(features == "heart", "1sec", "1min"))
}

##" features + detail level checking
##" HIDE FROM USER
features_input_check <- function(features, detail_level){
  stopifnot(detail_level %in% c("1min", "1sec", "15min"))
  if(detail_level == "1sec" & (length(features)>1 | all(features != "heart"))) {
    stop("detail_level of 1sec can only be used with feature = heart!")
  }
}

##" convert daily heartRateZones to a data.table with many columns
##" HIDE FROM USER
heartRateZones_to_dt <- function(heartRateZones) {
  cbindlist(lapply(heartRateZones, function(zone){
    dt <- as.data.table(zone[names(zone)!="name"])
    setnames(dt,names(dt), paste0(clean_name(zone$name), "_", names(dt)))
    return(dt)
  }))
}


##" Get activity for 1 day for 1 feature
##" HIDDEN FOR USER
get_activity_1f_1d <- function(feature = "calories", date, detail_level = "1min", token){

  ## check the detail level
  features_input_check(feature, detail_level)
  
  req <- fitbit_GET(paste0("1/user/-/activities/",feature, "/date/",date,"/1d/",detail_level, ".json"),
                    token = token)

  ## get daily values

  daily <- fitbit_parse(req)[[1]][[1]]

  if (feature == "heart") {
    daily_value = daily$value$restingHeartRate
  } else {
    daily_value = as.numeric(daily$value)
  }
    
  dt_daily <- data.table(date = daily$dateTime, daily_value = daily_value)
  ## value -> feature
  setnames(dt_daily, "daily_value", paste0("daily_",feature))
  
  ## get intraday values
    if (is.null(fitbit_parse(req)[[2]])) {
      stop("fitbit_GET didn't return the intraday data. Do you have the proper authorization?")
    }
  intraday <- fitbit_parse(req)[[2]][[1]]
  dt_intraday <- rbindlist(lapply(intraday, as.data.table))

  ## append feature name to columns other than time or value
  exclude <- c("time", "value")
  append_names <- names(dt_intraday)[!names(dt_intraday) %in% exclude]
  if(length(append_names)>0) setnames(dt_intraday, append_names, paste0(feature,"_",append_names ))
  ## value -> feature
  setnames(dt_intraday, "value", feature)

  ## merge daily and intraday
  return(data.table(dt_intraday, dt_daily))
}

##" Get activity for one day
##" HIDDEN FOR USER
get_activity_1d <- function(features = activity_features(), date = "2016-01-01",
                            detail_level = "1min",
                            token) {
  ## input check 
  features_input_check(features, detail_level)
  
  mergelist(lapply(features, get_activity_1f_1d,
                   date = date, token = token, detail_level = detail_level),
            merge_by = c("time","date"), all = TRUE)
}


##' Get Fitbit activity data in a tidy format.
##'
##' This function queries the Fitbit API and
##' returns the activity data for different features in a tidy format.
##'
##' @param from_date From which date to query the data.
##'   Character scalar in the format "YYYY-MM-DD".
##' @param to_date Up to which date to query the data. Data for that day are included in the return value. Character scalar in the format "YYYY-MM-DD".
##' @param features A character vector listing which features should be queried
##'   and included in the final data.table. Use \code{\link{activity_features}} to see all the available features.
##' @param detail_level A character scalar specifying how frequently should the returned data be sampled.
##'   Possible options are: '15min', '1min' and '1sec'.
##'   Use \code{\link{activity_features_info}} to get minimal sampling time for each feature.
##' @param token Fitbit API token generated with \code{\link{get_fitbit_token}}.
##'
##' @return \code{data.table} with columns corresponding to different features as described in \url{https://dev.fitbit.com/docs/activity/} and \url{https://dev.fitbit.com/docs/heart-rate/}. Each feature also contains the daily summary denoted with a \code{daily_} prefix.
##' 
##' @details This is the main function of the fitbitr package. I queries the Fitbit API multiple times
##' using the \code{fitbit_GET} function. It then parses the API outputs and combines them
##' into a single data.table.
##'
##' @section Feature description:
##' \describe{
##'   \item{\code{calories_level}}{Level field that reflects calculated activity level for that time period ( 0 - sedentary; 1 - lightly active; 2 - fairly active; 3 - very active.)}
##'   \item{\code{calories_mets}}{Probably the MET score \url{https://help.fitbit.com/articles/en_US/Help_article/What-are-very-active-minutes}.}
##'   \item{\code{calories}}{Calories burned in this time interval.}
##'   \item{\code{daily_calories}}{Total amount of calories burned at that day.}
##'   \item{\code{steps}}{Number of steps in this time interval.}
##'   \item{\code{daily_steps}}{Total number of steps at that day.}
##'   \item{\code{distance}}{Distance made in this time interval.}
##'   \item{\code{daily_distance}}{Total distance made at that day.}
##'   \item{\code{floors}}{Number of floors made in this time interval.}
##'   \item{\code{daily_floors}}{Number of floors made in this time interval.}
##'   \item{\code{elevation}}{Elevation units made in this time interval.}
##'   \item{\code{daily_elevation}}{Total number of elevation units made at that day.}
##'   \item{\code{heart}}{Mean heart rate in this time interval [beats per second].}
##'   \item{\code{daily_elevation}}{Daily mean heart rate [beats per second].}
##' }
##' Currently used units are 'Metric' (described in \url{https://dev.fitbit.com/docs/basics/#unit-systems}).
##' 
##' Time and date are stored as character vectors.
##' @examples
##' \dontrun{
##' fitbit_token <- get_fitbit_token()
##' dt <- get_activity(from_date = "2015-12-15", to_date = "2015-12-18", token = fitbit_token)
##'
##' ## Heart rate at 1 second precision for the day 2015-12-15
##' dt <- get_activity(from_date = "2015-12-15", features = "heart",
##'                    detail_level = "1sec",
##'                    token = fitbit_token)
##' }
##' @export
get_activity <- function(from_date, to_date = from_date, features = activity_features(), 
                                detail_level = "1min", token) {
  date_range <- date_seq(from_date, to_date)

  features_input_check(features, detail_level)

  number_of_requests <- length(features) * length(date_range)

  remains <- rate_limit(token, show_message = FALSE)$remains

  ## print error message if we don't have enough requests left
  if(number_of_requests > remains) {
    stop("Number of requests (",
         number_of_requests,
         ") is greater than the number of available requests (",
         remains,").")
  }
  
  dt_final <- rbindlist(lapply(date_range, get_activity_1d,
                               features = features, token = token,detail_level = detail_level))

  rate_limit(token)
  return(dt_final)
                   
}

