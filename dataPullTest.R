source("setup.R")
source("periods.R")
source("grants.R")
source("config/credentials.R")

pull_program_data <- function(start_date = NULL, end_date = NULL, grant = NULL, orgunit = NULL, 
                              base_url = NULL, username = NULL, password = NULL) {
  
  # Check if essential parameters are provided
  if (is.null(start_date) || is.null(end_date) || is.null(orgunit) || is.null(base_url) || 
      is.null(username) || is.null(password)) {
    stop("Missing required parameters: start_date, end_date, orgunit, base_url, username, or password.")
  }
  
  # Construct the API URL
  api_url <- paste0(
    base_url, "api/dataValueSets?dataSet=", grant,
    "&&startDate=", start_date, 
    "&endDate=", end_date, 
    "&orgUnit=", orgunit, 
    "&children=true"
  )
  
  cat("API URL: ", api_url, "\n")
  
  # Send the API request
  response <- tryCatch({
    GET(
      url = api_url,
      authenticate(username, password),
      accept_json()
    )
  }, error = function(e) {
    return(e)
  })
  
  # Check if the response encountered an error
  if (inherits(response, "error")) {
    message <- paste("Error in API request:", response$message)
    cat(message, "\n")
    return(NULL)
  }
  
  # Process the response if successful
  if (status_code(response) == 200 && http_type(response) == "application/json") {
    data <- fromJSON(content(response, as = "text"), flatten = TRUE)
    if (!is.null(data$dataValues)) {
      return(data$dataValues)
    } else {
      cat("No 'dataValues' found in the response.\n")
      return(NULL)
    }
  } else {
    message <- paste("API request failed. Status code:", status_code(response), 
                     "Content Type:", http_type(response))
    cat(message, "\n")
    return(NULL)
  }
}



#---------------------------------------------------------------
des <- fread("metadata/dataElements.txt") 



test_data <- pull_program_data(start_date = "2024-01-01", end_date = "2024-12-31",
                               grant = "Vg7RJh2mM35",
                               orgunit = orgunit, 
                               base_url = base_url,
                               username = username, password = password) %>%
  as.data.frame() |>
  left_join(des, by = c("dataElement" = "id")) %>%
  left_join(all_periods, by = c("period" = "quarter"))



# Global fund API
library(httr)
library(jsonlite)
api_url <- "https://data.api.theglobalfund.org/allocations/cycles"
response <- GET(api_url)
if (status_code(response) == 200) {
  # Proceed to parse the content
} else {
  stop("Failed to retrieve data: ", status_code(response))
}
content <- content(response, as = "text", encoding = "UTF-8")
data <- fromJSON(content)
























