# fitbitr: An R Interface To The Fitbit API

## Description 

This package provides functions to get your Fibit intraday activity data in R in a tidy form. 
It uses the [Fitbit API](https://dev.fitbit.com/docs/basics/) to access the data. The maximal time resolution for the heart rate data is 1 second and 1 minute for other features.

Available intraday features:

- calories
- steps
- distance
- floors
- elevation
- hear rate


## Installation

```r
devtools::install_github("avsecz/fitbitr")
```

## Provided functions

- `get_fitbit_token`
- `get_activity`
- `refresh_fitbit_token`
- `rate_limit`
- `fitbit_GET`
- `fitbit_parse`

## Usage

### 1. Authorization

**1. Create a developer account at the fitbit developer page <https://dev.fitbit.com/>.**

**2. Login to <https://dev.fitbit.com/> and register your application (click 'REGISTER AN APP'):**
- **Application Name:** 'GetMyData' (*free choice*)
- **Description:** 'This is an app used by the fitbitr package.' (*free choice*)
- **Application website:** 'https://github.com/'  (*free choice*)
- **Organization:** 'GetMyData'  (*free choice*)
- **Organization Website:** 'https://github.com/' (*free choice*)
- **OAuth 2.0 Application Type:** 'Personal'
- **Callback URL:** 'http://localhost:1410'
- **Default Access Type:** 'Read-only'

It is important to use the last three points as listed above.

**3. Copy the credentials (Application Name, Client ID and Client Secret) into the file `~/.fitbitr`,
each to its own row:**

```
<Application Name>
<OAuth 2.0 Client ID>
<Client Secret>
```

Example `~/.fitbitr` file:

```
GetMyData2
395A3C
4124asdasdfjapf9uq0w9r7q09573597
```

**4. Make `~/.fitbitr` readable only by you:**

```bash
chmod 600 ~/.fitbitr
```

**5. Install the `fitbitr` package and create the access token in R:**

```r
## install_github("avsecz/fitbitr")
library(fitbitr)
token <- get_fitbit_token()
```

You can answer with '2' to the question: *Use a local file to cache OAuth access credentials between R sessions?*

Allow the application (GetMyData) to access your fitbit data in the browser. Make sure all the checkboxes are ticked to have full access. Login if neccessary.

You should see *'Authentication complete. Please close this page and return to R.'*. Congrats!

### 2. Querying your data

You can now query your own data. The main function is `get_activity`. Example:

```r
dt <- get_activity(from_date = "2016-04-13", token = token)
```
```
Requests left:
132 / 150 (reset in 11 min, at 00:00:01)
```

Check the documentation of `get_activity` for more information.

### Getting the information about number of requests left

Use function `rate_limit`:

```r
rate_limit(token)
```
```
Requests left:
132 / 150 (reset in 11 min, at 00:00:01)
```

### Expired token

Token expires at some point. To refresh it, use `fitbit_refresh_token`:

```r
token <- fitbit_refresh_token(token)
```


###  Writing your own GET requests

Get familiar with the documentation at <https://dev.fitbit.com/docs/>. Use `fitbit_GET` and `fitbit_parse` to make API requests. Example (accessing sleep data):

```r
req <- fitbit_GET("1/user/-/sleep/date/2015-12-12.json", token = fitbit_token ) 
output <- fitbit_parse(req)
```

Please make a pull request if you wrote a cool new query function.

## Troubleshooting:


**1. In case you get the following errors:**

```
Error in curl::curl_fetch_memory(url, handle = handle) (from api_best_practice.R!12568FoJ#2) : 
  Timeout was reached
```

or 

```
Error in refresh_oauth2.0(self$endpoint, self$app, self$credentials, self$params$user_params) (from basic_api_functions.R#10) : 
  Unauthorized (HTTP 401).
```

Refresh your token using `fitbit_refresh_token`:

`fitbit_token <- fitbit_refresh_token(fitbit_token)`

**2. Token refresh doesn't solve the issue**

Reset your credentials on the fitbit developer page:
<https://dev.fitbit.com/apps/details/>. Click 'Reset consumer Key/Secret' and overwrite the new Client (consumer) secret in your credentials file: `~/.fitbitr`.
