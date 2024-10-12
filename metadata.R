source("setup.R")
source("config/credentials.R")

# Metadata
# ==============================================================
# organisations <- datimutils::getMetadata("organisationUnits", 
#                                         fields = "name, id, code, shortName, parent, path") %>%
#   dplyr::arrange(name)



dataSets <- datimutils::getMetadata("dataSets", 
                                    fields = "name, id") |>
  dplyr::arrange(name) 

glimpse(dataSets)

fwrite(dataSets, "metadata/datasets.csv")

# KRCS
hiv_krcs_gc7 <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:WFU6M2XN4W4", 
                                    fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    cycle = "GC7",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

fwrite(hiv_krcs_gc7, "metadata/hiv_krcs_gc7.txt")

DetailedDataElements <- datimutils::getMetadata("dataElements", 
                                                fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") |>
  dplyr::arrange(name)


# data elements

finance_pr <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:hhqiQm61ezm", 
                                      fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id) |>
  mutate(
    class = "Finance PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


fin_c19rm <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:x5UiUT11987", 
                                     fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id) |>
  mutate(
    class = "Finance PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


fin_cpt <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:wFfbzCuLhO1", 
                                   fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id) |>
  mutate(
    class = "CPT PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )



finance_sr <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:O7LYGHTAVmT", 
                                      fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Finance SR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )


hiv_krcs <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:bQpMWqkXcDB", 
                                    fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )




hiv_tnt <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:htRBkqwskRf", 
                                   fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(hiv_tnt)


malaria_amref <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:tHy55yTBFAu", 
                                         fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(malaria_amref)



malaria_tnt <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:gqYxVkqLzBp", 
                                       fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(malaria_tnt)




tb_amref <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:YVGVA2Gg0ew", 
                                    fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(tb_amref)



tb_tnt <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:TD2zFcKJetG", 
                                  fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(tb_tnt)


tb_igad <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:ks2UFCtFz8s", 
                                   fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(tb_igad)


tb_ecsa <- datimutils::getMetadata("dataElements?filter=dataSetElements.dataSet.id:eq:EY7JqGbgMLH", 
                                   fields = "name, id, code, shortName, description, formName,domainType, valueType, aggregationOperator, categoryCombo") %>%
  dplyr::arrange(name) |>
  mutate(aggregationoperator = NA_character_) |>
  select(name, id, code, shortName, description, formName,domainType, valueType, aggregationoperator, categoryCombo.id)|>
  mutate(
    class = "Programmes PR",
    Inverse = case_when(str_detect(code, "INVERSE") ~ "Yes",
                        TRUE ~ "No")
  )

glimpse(tb_ecsa)



DetailedDataElements <- bind_rows(finance_pr, finance_sr, fin_c19rm, fin_cpt, 
                                  hiv_krcs, hiv_tnt,
                                  malaria_amref, malaria_tnt,
                                  tb_amref, tb_tnt, tb_igad, tb_ecsa) |>
  mutate(
    category = case_when(class == "Finance PR" | class == "Finance SR" ~ class,
                         str_detect(name, "TARGET]") ~ "TARGET",
                         str_detect(name, "RESULT]") ~ "RESULT",
                         str_detect(name, "COMMODITY]") ~ "COMMODITY",
                         str_detect(name, "COMMENT]") ~ "COMMENT",
                         TRUE ~ NA_character_))



write_csv(DetailedDataElements, 'data/dataElements.csv')


indicators <- datimutils::getMetadata("indicators", 
                                      fields = "name, id, code, shortName, numerator, denominator") %>%
  dplyr::arrange(name) %>%
  mutate(aggregationoperator = NA_character_) %>%
  select(name, id, code, shortName, numerator, denominator) |>
  mutate(
    numerator = gsub("#\\{|\\}", "", numerator),
    denominator = gsub("#\\{|\\}", "", denominator),
  )

glimpse(indicators)


vizualizationIndicators <- indicators |>
  filter(grepl("PERCENT\\]", code)) |>
  filter(!is.na(code)) |>
  mutate(
    name = str_replace(name,"PERCENT\\]", ""),
    code = str_replace(code,"PERCENT\\]", ""),
    shortName = str_replace(shortName,"PERCENT\\]", "")
  ) |>
  arrange(name)


categoryOptions <- datimutils::getMetadata("categoryOptions", 
                                           fields = "name, id") %>%
  dplyr::arrange(name) 

glimpse(categoryOptions)


categoryCombos <- datimutils::getMetadata("categoryCombos", 
                                          fields = "name, id") %>%
  dplyr::arrange(name) 

glimpse(categoryCombos)

categoryOptionCombos <- datimutils::getMetadata("categoryOptionCombos", 
                                                fields = "name, id") %>%
  dplyr::arrange(name) 

glimpse(categoryOptionCombos)

write_csv(categoryOptionCombos, 'data/categoryOptionCombos.csv')


fpOptions <- categoryOptionCombos |>
  mutate(PR_Grant = case_when(str_detect(name, "KEN-") | 
                                str_detect(name, "QPA-") ~ "Grant",
                              TRUE ~ NA_character_)) |>
  filter(!is.na(PR_Grant)) |>
  mutate(Grant = case_when(str_detect(name, ",") ~ "SR",
                           TRUE ~ "PR")) |>
  filter(!grepl("HIV|MALARIA|TB", name)) |>
  mutate(GrantName = case_when(str_detect(name, "KEN-H-KRCS") ~ "KRCS, HIV",
                               str_detect(name, "KEN-M-AMREF") ~ "AMREF, Malaria",
                               str_detect(name, "KEN-T-AMREF") ~ "AMREF, TB",
                               str_detect(name, "KEN-M-TNT") ~ "TNT, Malaria",
                               str_detect(name, "KEN-T-TNT") ~ "TNT, TB",
                               str_detect(name, "KEN-H-TNT") ~ "TNT, HIV",
                               str_detect(name, "QPA-T-ECSA") ~ "ECSA, Multicountry",
                               str_detect(name, "QPA-T-IGAD") ~ "IGAD, Multicountry",
                               TRUE ~ NA_character_
  ))

write_csv(fpOptions,
          "data/PRs.csv")

grantsOnly <- fpOptions |>
  filter(Grant == "PR") |>
  rename(
    grantid_dhis = id
  ) |>
  select(-c(PR_Grant, Grant))


grantsAndSRs <- fpOptions |>
  filter(Grant == "SR") |>
  select(-c(PR_Grant, Grant)) |>
  separate(name, c("GrantID", "SR"), ", ") |>
  left_join(grantsOnly, by = c("GrantID" = "name")) |>
  mutate(GrantName = case_when(GrantID == "KEN-H-KRCS" ~ "KRCS, HIV",
                               GrantID == "KEN-M-AMREF" ~ "AMREF, Malaria",
                               GrantID == "KEN-T-AMREF" ~ "AMREF, TB",
                               GrantID == "KEN-M-TNT" ~ "TNT, Malaria",
                               GrantID == "KEN-T-TNT" ~ "TNT, TB",
                               GrantID == "KEN-H-TNT" ~ "TNT, HIV",
                               GrantID == "QPA-T-ECSA" ~ "ECSA, Multicountry",
                               GrantID == "QPA-T-IGAD" ~ "IGAD, Multicountry",
                               TRUE ~ NA_character_
  )
  
  )


write_csv(grantsAndSRs,
          "data/SRs.csv")



