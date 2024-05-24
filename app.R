#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries
library(shiny)
library(shinythemes)
library(shinybusy)
library(dplyr)
library(ggplot2)
library(metaheuristicOpt)
library(nsga2R)

# load my library for finding optimal designs
# source all R files in nlodm directory
directory <- "R"
# List all files with the .R extension in the directory
r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
# Source each .R file
for (file in r_files) {
  source(file)
}

# load Shiny related code
source('R/app/wizard.R')
source('R/app/pages.R')
source('R/app/ui.R')
source('R/app/server.R')

# Run the application
shinyApp(ui = ui, server = server)
