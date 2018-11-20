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

myGate <- function(fr, pp_res, channels=NA, filterId="ppgate", parent){
  # Find dim names (channel only gives full channel name and you can't find gates with it)
  d
  fmos <- fcs$fmos
  gtable <- fcs$gtable
  print('37')
  # fmos <- GatingSetList(d)
  # gtable <- fread('TCellPanel1/testfmopanel2.csv')
  # 
  gd <- gtable$dims
  l <- sapply(1:length(gd), function(x){
    strsplit(gd[x], ',')
  })
  # gd
  # channels <- 'APC CCR7'
  # print('43')
  
  g <- unique(unlist(l))
  for(i in 1:length(g)){
    if(length(grep(g[i], channels)) == 1){
      chnl <<- g[i]
    }
  }
  g
  print('51')
  # Find fmo file for this channel
  idx <- which(grepl(chnl, sampleNames(fmos)))
  print(channels)
  print(sampleNames(fmos))
  print(idx)
  fmo <- fmos[idx]
  fmo
  # name <- sampleNames(fmo)
  
  # # Rewrite gating template so fmos gate with a 0.99 quantile gate
  gtb <- gtable
  ix <- which(gtb$gating_method == 'myGate')
  print('60')
  gtb$gating_method[ix] <- 'quantileGate'
  gtb$gating_args[ix] <- 'probs=0.99'
  print('63')
  gtb[ix, c('collapseDataForGating', 'groupBy', 'preprocessing_method', 'preprocessing_args')] <- NA
  print('62')
  write.csv(gtb, 'fmo.csv')
  print('64')
  gfmo <- gatingTemplate('fmo.csv')
  plot(gfmo)

  # Clean, transform and gate fmo (quantile gate on fmo population)
  fmogs <- fmocleaning(fmo, sampleNames(fmos)[idx], gfmo)
  # sampleNames(fmogs) <- name
  gating(gfmo, fmogs)
  # 
  # 
  print('cleaned')
  # print(getNodes(fmogs))
  # # Get fmo gate 
  fmogate <- getGate(fmogs, paste(parent, '/',chnl, '+', sep = ''))
  
  print('gate')
  # mindensity gate the flow frame given 
  autogate <- mindensity2(fr, channels, filterId = filterId)
  print('autogate')
  fmomin <- fmogate[[1]]@min
  automin <- autogate@min
  
  print('comparison')
  
  # Check that autogate minimum is greater than fmo minimum and can be used, if not, use the fmo gating 
  if(automin > fmomin){
    print('auto')
    return(autogate)
  } else {
    print('rect')
    minmax <- list(c(fmogate[[1]]@min, fmogate[[1]]@max))
    names(minmax) <- channels
    return(rectangleGate(filterId = filterId, minmax))
  }
}
registerPlugins(fun=myGate,methodName='myGate',dep=NA)
#----
