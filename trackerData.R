# Grants: lvrhp60kkp1
#---------------------------------------------------------------
grants_url <- paste0(
  base_url,
  "api/40/tracker/events.csv?program=lvrhp60kkp1&programStage=GDaw1VVUL1i&orgUnit=HfVjCurKxh2",
  "&fields=dataValues,occurredAt,event,status",
  "&ouMode=SELECTED&order=occurredAt:desc&skipPaging=true"
)
grants_01 <- content(GET(grants_url), "text")
# Parse the CSV text into a data frame
grants <- read_csv(grants_01) |>
  select(event, status, dataElement, value) |>
  pivot_wider(names_from = dataElement,
              values_from = value) |>
  transmute(
    event = event,
    status = status,
    grant_number = JNNg3GI0aRQ,
    component = GzWft4oGtTX,
    principal_recipient = SRsN6vYnQgv,
    start_date = TToLBIWglqb,
    end_date = PTjOzPGQ4SH,
    signed_amount = B10phkwSM5n,
    commited_amount = fyKtOEGtPRS,
    disbursed_amount = JrgZEBmvQug,
    goal = gBvbTi6CZGF,
    objectives = rI5rhJSPdvx,
  ) %>%
  arrange(grant_number) |>
  # Add grant name
  mutate(
    Component = case_when(str_detect(JNNg3GI0aRQ, "-H-") ~"HIV",
                          str_detect(JNNg3GI0aRQ, "C19") ~"COVID-19",
                          str_detect(JNNg3GI0aRQ, "-T-") ~"TB",
                          str_detect(JNNg3GI0aRQ, "-M-") ~"Malaria",
                          str_detect(JNNg3GI0aRQ, "-H-") ~"HIV",
                          TRUE ~ NA_character_
                             ),
    
    `GrantName` = paste(SRsN6vYnQgv,
      case_when(str_detect(JNNg3GI0aRQ, "QPA") ~"Multicountry",
                             TRUE ~ Component ),
      sep = ", "
    ),
    pr = case_match(SRsN6vYnQgv,
                    "TNT" ~"National Treasury of the Republic of Kenya (TNT)",
                    "AMREF" ~"Amref Health Africa (Amref Kenya)",
                    "KRCS" ~"Kenya Red Cross Society (KRCS)",
                    "ECSA" ~"East, Central and Southern Africa Health Community (ECSA)",
                    "IGAD" ~"Intergovernmental Authority on Development (IGAD)",
                    ),
    id = case_match(JNNg3GI0aRQ,
                    "KEN-H-KRCS" ~"ugvkE8KjVMf",
                    "KEN-H-TNT" ~"wHB8K8gRXUo",
                    "KEN-M-AMREF" ~"gutyP1WPLOj",
                    "KEN-M-TNT" ~"bGp3L1KQ4JE",
                    "KEN-T-AMREF" ~"kQEPWJF0usa",
                    "KEN-T-TNT" ~"ACnzwhSiP22",
                    "QPA-T-ECSA" ~"MjTaIfBIICA",
                    "QPA-T-IGAD" ~"clY9HQGUEmQ",
                    "KEN-T-AMREF [C19RM]" ~ "h6VE02j0BVX",
                    "KEN-T-TNT [C19RM]" ~ "PbzVVHYzSNM"
    ),
    Regular = case_when(str_detect(JNNg3GI0aRQ, "C19RM") ~"COVID19",
                        TRUE ~ "Regular")
  ) |>
  rename(
    name = JNNg3GI0aRQ,
    start = TToLBIWglqb,
    end = PTjOzPGQ4SH,
    amount = B10phkwSM5n,
    intro = gBvbTi6CZGF,
  )

write_csv(grants, 
          paste0("data\\Grants",".csv"), na = "")


