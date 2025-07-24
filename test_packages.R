# Test if basic packages load
cat("Testing package loading...\n")

# Test basic packages first
tryCatch({
  library(shiny)
  cat("✓ shiny loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading shiny:", e$message, "\n")
})

tryCatch({
  library(httr)
  cat("✓ httr loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading httr:", e$message, "\n")
})

tryCatch({
  library(jsonlite)
  cat("✓ jsonlite loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading jsonlite:", e$message, "\n")
})

tryCatch({
  library(dplyr)
  cat("✓ dplyr loaded successfully\n")
}, error = function(e) {
  cat("✗ Error loading dplyr:", e$message, "\n")
})

cat("Package testing complete.\n")
