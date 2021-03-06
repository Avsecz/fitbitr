% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basic_api_functions.R
\name{get_fitbit_token}
\alias{get_fitbit_token}
\title{Get the fitbit authentication token.}
\usage{
get_fitbit_token(file_path = "~/.fitbitr", appname = NULL, key = NULL,
  secret = NULL, scope = c("activity", "heartrate", "sleep", "weight"))
}
\arguments{
\item{file_path}{Character scalar; Path to the credentials file described in details. Leave the arguments key, secret and scope set to NULL if you want to use the credentials file for the authentication.}

\item{appname}{Character scalar; Fitbit application name.}

\item{key}{Character scalar; Fitbit OAuth 2.0 Client ID.}

\item{secret}{Character scalar; Fitbit Client (Consumer) Secret.}

\item{scope}{Character vector; API permission scope. Leave the defaults in order to access
all the features listed in \code{\link{activity_features}}.}
}
\value{
Fitbit token providing the access to your Fitbit account data.
}
\description{
This function generates the fitbit token and performs the required API authorization.
}
\details{
Request rate limit information is stored in the fitbit token as the "rate_limit" attribute. It gets updated every time fitbit_GET is called.

Example credentials file (3 rows: appname, key, secret):
\preformatted{
GetMyData
14C889
apaojsdpoj213412p4j115j1p5oj21p5o
}
}
\examples{
\dontrun{
## If the credentials file is located in "~/.fitbitr" just run:
fitbit_token <- get_fitbit_token()

## Else, specify the new credentials file location:

fitbit_token <- get_fitbit_token(file_path = "~/.custom_fitbitr")

## or enter the credentials as arguments:
fitbit_token <- get_fitbit_token(appname = "GetMyData",
                                 key = "14C889",
                                 secret = "apaojsdpoj213412p4j115j1p5oj21p5o"
                                 )
}
}

