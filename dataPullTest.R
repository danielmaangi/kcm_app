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



rating <- test_data %>%
  filter(class %in% c("Rating")) %>%
  select(period, Indicator , value) %>%
  filter(Indicator != "Comments") %>%
  select(-period) %>%
  transmute(
    Category = case_when(str_detect(Indicator, "Financial") ~ "Financial",
                         str_detect(Indicator, "Programmatic") ~ "Programmatic",
                         TRUE ~ NA_character_
    ),
    type = case_when(str_detect(Indicator, "End Date") ~ "End Date",
                     str_detect(Indicator, "Start Date") ~ "Start Date",
                     str_detect(Indicator, "Start Date") ~ "Start Date",
                     str_detect(Indicator, "Rating Comments") ~ "Comments",
                     str_detect(Indicator, "Financial Rating") ~ "Rating",
                     str_detect(Indicator, "Programmatic Rating") ~ "Rating",
                     TRUE ~ NA_character_
    ),
    value = value
    
  ) %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(period = paste0(format(as.Date(`Start Date`),"%b-%y"), 
                         " to ", 
                         format(as.Date(`End Date`), "%b-%y")),
         .before = Comments) %>%
  select(-c(`Start Date`, `End Date` ))


prog_rate <- rating %>% filter(Category == "Programmatic") %>% pull(Rating) %>%
  substr(1,1)
fin_rate <- rating %>% filter(Category == "Financial") %>% pull(Rating) %>%
  substr(1,1)

overall_rating <- tibble(
  Category = "Overall",
  Rating = paste0(prog_rate, "-", fin_rate)
)

rating_all <- bind_rows(rating, overall_rating)























