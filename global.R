suppressMessages(library(openCyto))
suppressMessages(library(data.table))
suppressMessages(library(ggcyto))
suppressMessages(library(ggplot2))
suppressMessages(library(flowAI))
suppressMessages(library(D3partitionR))
suppressMessages(library(magrittr))
suppressMessages(library(RColorBrewer))
suppressMessages(library(gridExtra))
suppressMessages(library(ggrepel))
suppressMessages(library(dplyr))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(rmarkdown))
suppressMessages(library(Rtsne))
suppressMessages(library(caret))
suppressMessages(library(shinycssloaders))


# Sourced Files
source("plots.R")
source('functions.R')
source('multireport.R')

options(spinner.color = '#E47C23')

# OpenCyto Gating Plugins 
tmix2DGate.wrapper <- function(fr, pp_res = NULL, channels, ...){
  cytoUtils::tmix2DGate(fr, channels, ...)
}
registerPlugins(tmix2DGate.wrapper, "tmix2DGate.wrapper")
#----
