# Load the lubridate package
library(lubridate)

# Create the data
quarter <- c("2024Q3", "2024Q4", "2025Q1", "2025Q2", "2025Q3", "2025Q4", 
             "2026Q1", "2026Q2", "2026Q3", "2026Q4", "2027Q1", "2027Q2")

gf_period <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", 
            "P10", "P11", "P12")

# Generate end_date using lubridate's yq function
end_date <- yq(quarter) + months(3) - days(1)


# Combine into a data frame
all_periods <- data.frame(quarter, gf_period, end_date) %>%
  mutate(
    select = case_when(end_date > Sys.Date() ~ "no",
                       TRUE ~ "yes")
  )
  

gf_period <- all_periods %>% 
  filter(select == "yes") %>%
  pull(gf_period)
