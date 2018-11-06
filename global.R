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

source("sunburst.R")
source('functions.R')

# OpenCyto Gating Plugins 
tmix2DGate.wrapper <- function(fr, pp_res = NULL, channels, ...){
  cytoUtils::tmix2DGate(fr, channels, ...)
}
registerPlugins(tmix2DGate.wrapper, "tmix2DGate.wrapper")
#----