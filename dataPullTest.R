#---------------------------------------------------------------
dataElements <- datimutils::getMetadata("dataElements", 
                                        fields = "name, id, code, shortName, description, formName, fieldMask, domainType, valueType, aggregationOperator, categoryCombo") |>
  dplyr::arrange(name) %>%
  mutate(
    data = case_when(str_detect(name, "TARGET") ~ "Target",
                     str_detect(name, "RESULT") ~ "Result",
                     str_detect(name, "COMMENT") ~ "Comment",
                     TRUE ~ NA_character_
    ),
    type = case_when(str_detect(name, "COVERAGE") ~ "Coverage",
                     str_detect(name, "OUTCOME") ~ "Outcome",
                     str_detect(name, "IMPACT") ~ "Impact",
                     TRUE ~ NA_character_
    ))

startdate <- "2024-01-01"
enddate <- Sys.Date()
tbOne_url <- paste0(base_url,
                    "api/dataValueSets?dataSet=QcuqhnHSt5j&&startDate=", 
                    startdate, "&endDate=", enddate,"&orgUnit=",
                    orgunit, "&children=true")

tbOne_01 <- content(GET(tbOne_url), as = "parsed")

tbOne <- rbindlist(tbOne_01$dataValues, fill = TRUE) |>
  left_join(dataElements, by = c("dataElement" = "id"))


tbOne_wide <- tbOne %>%
  select(period, description , fieldMask, type, data, value) %>%
  pivot_wider(names_from = "data",
              values_from = "value",
              values_fill = NA) %>%
  select(period, description , fieldMask, type, Target, Result, Comment) %>%
  mutate(
    Percent = round(as.numeric(Result)*100/ as.numeric(Target),0),
    .before = Comment
  )
