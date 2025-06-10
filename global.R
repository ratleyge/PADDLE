# Packages ----

library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)
library(DT)
library(plotly)
library(enrichR)
library(shinycssloaders)


# Read in Data ----
load("Data/appData.RData")


# List of all the datasets that contain disease and chemical associations
chemDiseaseDatasetList_Air_adults <- c(
  "non_spatial_Adult_ns_Air",
  "non_spatial_Geriatric_Air",
  "non_spatial_Retirement_Air",
  "spatial_Over18_Air"
)

chemDiseaseDatasetList_Air_kids <- c(
  "non_spatial_Pediatric_ns_Air",
  "non_spatial_Youth_Air",
  "spatial_Under18_Air"
)

chemDiseaseDatasetList_Water_adults <- c(
  "non_spatial_Adult_ns_Water",
  "non_spatial_Retirement_Water",
  "non_spatial_Geriatric_Water"
)

chemDiseaseDatasetList_Water_kids <- c(
  "non_spatial_Pediatric_ns_Water",
  "non_spatial_Youth_Water"
)


# Default plot height and width for the combined disease analysis
plot_height_kids <- 400
plot_height_adults <- 400


split_middle_hyphen <- function(text) {
  # Find positions of all hyphens
  hyphen_pos <- gregexpr("-", text)[[1]]
  
  # If no hyphen, return as-is
  if (length(hyphen_pos) == 1 && hyphen_pos == -1) return(text)
  
  # Find the one closest to the middle of the string
  middle <- nchar(text) / 2
  best_split <- hyphen_pos[which.min(abs(hyphen_pos - middle))]
  
  # Split and insert <br> tag
  paste0(
    substr(text, 1, best_split),
    "<br>",
    substr(text, best_split + 1, nchar(text))
  )
}

# Settings for enrichR
# listEnrichrDbs() # To see the available databases
dbs <- c(
  "GO_Biological_Process_2018", 
  "GO_Molecular_Function_2018",
  "KEGG_2019_Human",
  "WikiPathways_2019_Human"
)

