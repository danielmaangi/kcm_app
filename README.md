# KCM Integrated Dashboard

<!-- badges: start -->
<!-- badges: end -->

## Overview

The **Kenya Coordinating Mechanism (KCM)** Dashboard is a platform designed to harness the full potential of partners and resources to fight HIV/AIDS, Tuberculosis, and Malaria in Kenya. It facilitates oversight and decision-making for the implementation of Global Fund grants by visualizing both programmatic and financial data in a user-friendly manner.

This dashboard allows decision-makers, stakeholders, and partners to track and monitor the performance of Principal Recipients (PRs) and Sub-Recipients (SRs) entrusted with the management of resources. It also supports efficient data storage, analysis, and sharing of reports to ensure transparency and timely corrective actions.

## Features

- **Programmatic and Financial Performance Monitoring**: Track quarterly (or custom) performance of PRs, SRs, and government co-financing.
- **Resource Flow Tracking**: Monitor resources at county and sub-county levels.
- **Recommendation Tracking**: Track the status of recommendations from the KCM Oversight Committee.
- **Field Visit Reporting**: Generate reports and visualizations on oversight field visits.
  
## Key Capabilities

1. **Integrated Data Management**: Supports data analysis across multiple funding cycles, reporting periods, and programs (HIV, Tuberculosis, Malaria, COVID-19).
2. **Visualization**: Create charts, maps, and other data visualizations.
3. **Web-Based Access**: The dashboard is fully accessible online through common web browsers.
4. **User-Friendly**: Designed to be intuitive and easy to use for non-technical users.
5. **Cost-Free Utilization**: No expensive licenses or additional software installation needed.

## Technology Stack

- **Backend**: [DHIS2](https://www.dhis2.org/) for data storage and management.
- **Frontend**: [R Shiny](https://shiny.rstudio.com/) for interactive data visualization.
- **Hosting**: Web-based, no special licenses required.

## Installation

To set up the dashboard locally:

1. **Clone the repository**:

```r
git clone https://github.com/danielmaangi/kcm_app.git
cd kcm_app

# Install dependencies: Ensure that R is installed on your system. Install required packages using:

install.packages(c("shiny", "jsonlite", "httr", "ggplot2", "dplyr"))

# Configure DHIS2 credentials
username <- "your_dhis2_username"
password <- "your_dhis2_password"
base_url <- "https://your_dhis2_url/"
dataSet <- "your_dhis2_dataSet"
orgunit <- "your_dhis2_orgUnit"

# Run the app
shiny::runApp()

```






