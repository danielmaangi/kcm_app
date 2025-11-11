get_financial_summary <- function(geography_filter = NULL) {
  
  base_url <- "https://data-service.theglobalfund.org/v4.2/odata/Grants"
  
  # Build query parameters
  params <- list()
  
  # Add filter if specified
  if (!is.null(geography_filter)) {
    params$`$filter` <- paste0("geography/name eq '", geography_filter, "'")
  }
  
  # Expand implementation periods
  params$`$expand` <- "implementationPeriods"
  
  # Select specific fields
  params$`$select` <- paste(c(
    "code",
    "name",
    "totalSignedAmount_ReferenceRate",
    "totalCommitmentAmount_ReferenceRate", 
    "totalDisbursedAmount_ReferenceRate"
  ), collapse = ",")
  
  # Make the API call
  response <- httr::GET(
    url = base_url,
    query = params
  )
  
  # Check for errors
  httr::stop_for_status(response)
  
  # Parse the response
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, flatten = TRUE)
  
  return(data$value)
}

# Test it
kenya_grants <- get_financial_summary(geography_filter = "Kenya")



kenya_grants_detailed <- kenya_grants %>%
  select(grant_code = code, implementationPeriods) %>%
  unnest(implementationPeriods) %>%
  mutate(periodFrom = as.numeric(periodFrom)) %>%
  filter(periodFrom >= 2023)

# Now you have one row per implementation period!
glimpse(kenya_grants_detailed)























