library(openCyto)
library(data.table)
library(ggcyto)
library(ggplot2)
library(flowAI)
library(D3partitionR)
library(magrittr)
library(RColorBrewer)
library(gridExtra)
library(ggrepel)
library(dplyr)
library(stringr)


source("plots.R")

#Plugins ----
tmix2DGate.wrapper <- function(fr, pp_res = NULL, channels, ...){
  cytoUtils::tmix2DGate(fr, channels, ...)
}
registerPlugins(tmix2DGate.wrapper, "tmix2DGate.wrapper")
#----

multireport <- function(fs, g, filenames){

  xqc <- vector(mode = 'list', length = length(fs))
  
  fs <- flowSet(sapply(1:length(fs), function(x){
    qcout <- capture.output(
      ft <- flowAI::flow_auto_qc(fs[[x]], 
                                 remove_from = 'all', 
                                 ChExcludeFS = c("SSC", "FSC", "Time"), output = 1,
                                 mini_report = FALSE, 
                                 html_report = FALSE, 
                                 fcs_QC = FALSE))
    xqc[[x]] <<- qcout
    ft
  }))
  sampleNames(fs) <- filenames
  
  # Compensation 
  # Input: - fcsSet: flowSet of samples
  # Output: flowSet of samples that have been compensated on "channels"
  apply_comp_existing <- function(fcsSet) {
    # iterate through flowSet and apply compensation matrix stored with flowFrame
    fcsSet.comp <- fsApply(fcsSet, function(ff) {
      print(identifier(ff))
      spill.search <- try(spillover(ff))
      # Get rid of any NULL entries in list of spillover keyword search results
      spill.mat <- try(spill.search[sapply(spill.search, function(x) !is.null(x))])
      # Grab the spillover matrix from the list
      spill.mat <- try(get(names(spill.mat)[1], spill.mat))
      comp <- tryCatch(compensate(ff, spill.mat),
                       error = function(e) {
                         print (e)
                         print (paste0("compensation failed for: ", identifier(ff)))
                         return (ff)
                       })
      return (comp)
    })
  }
  fs <- apply_comp_existing(fs)
  
  #Setting marker names to dye names instead of FL1-etc. 
  # newMarkers <- unname(gs[[1]]@data[[1]]@parameters$desc)
  newMarkers <- unname(fs[[1]]@parameters$desc)
  #For naming inconsistencies
  newMarkers <- gsub("CD8\\s", "CD8a ", newMarkers)
  cols <- colnames(fs)
  
  #Keeping only '-A' markers and leaving '-H' markers as FL-etc 
  #ONLY FOR CYTOFLEX - FOR GUAVA -HLIN VS -HLOG
  for(i in 1:length(cols)){
    if(grepl('-H', newMarkers[[i]])){
      newMarkers[[i]] <-  cols[[i]]
    }
  }
  #Renaming markers and columns 
  newMarkers <- gsub("\\[\\d+\\]", "", newMarkers)
  colnames(fs) <- c(newMarkers)
  names(newMarkers) <- newMarkers
  markernames(fs) <- newMarkers
  
  #Transformation
  #Default m = 5.3 because 4.5 seems to mess up data 
  chnls <- colnames(fs[[1]])[!colnames(fs[[1]]) %in% c("Time", "FSC-Width")]
  trans <- NULL
  m <- 5.3
  
  fstrans <- fsApply(fs, function(ff){
    catch <- TRUE 
    while(catch){
      tryCatch({
        print('trans')
        trans <- estimateLogicle(ff, chnls, m=m)
        fftrans <<- transform(ff, trans)
        catch = FALSE
      }, error = function(err){
        m <<- m + 0.1
      })
    }
    fftrans
  })
  
  gs <- GatingSet(fstrans)
  gating(g,gs)
  
  if(tcell == TRUE){
    # TCELLPANEL
    # Hide nodes (gets rid of intermediate steps/gates when graphing)
    hideNodes <- c('CD4+',
                   'CD8a+',
                   'CD4+CD8a+',
                   'CD4-CD8a-',
                   'CD4+CD8a-/CCR7+',
                   'CD4+CD8a-/CD45RA+',
                   'CD4-CD8a+/CCR7+',
                   'CD4-CD8a+/CD45RA+')
    lapply(hideNodes, function(thisNode)setNode(gs, thisNode, FALSE))
  }
  
  nodes <- data.table(hierarchy = getNodes(gs)[2:length(getNodes(gs))])
  nodes$Parent <- getPopStats(gs)[1:length(nodes$hierarchy)]$Parent
  nodes$Pop <- getPopStats(gs)[1:length(nodes$hierarchy)]$Population
  test <- unique(nodes$Parent)
  
  gatePlots <- lapply(1:length(test), function(y){
    node <- nodes[y]
    
    plots <- lapply(1:length(gs), function(x){
      gates <- nodes[nodes$Parent == test[y],]$hierarchy
      pop <- nodes[nodes$Parent == test[y],]$Pop
      pop <<- gsub('\\+|-', '', pop)
      gate <- getGate(gs[[1]],gates[1])
      dims <- gate@parameters
      
      if(length(dims) == 1){
        xdim <<- 'FSC-A'
        ydim <<- dims[[1]]@parameters
        as.ggplot(ggcyto(gs[[x]], aes_(x = xdim, y = ydim)) + geom_hex(bins = 200) + geom_gate(gates) + geom_stats() + 
                    facet_grid(cols=vars(eval(identifier(getFlowFrame(gs[[x]]))))) + 
                    ggtitle('') +xlab('') + ylab('')
        )
      } else if (length(dims) == 2){
        xdim <<- dims[[1]]@parameters
        ydim <<- dims[[2]]@parameters
        print(xdim)
        as.ggplot(ggcyto(gs[[x]], aes_(x = xdim, y = ydim)) + geom_hex(bins = 200) + geom_gate(gates) + geom_stats() + 
                    facet_grid(cols=vars(eval(identifier(getFlowFrame(gs[[x]]))))) + 
                    ggtitle('') +xlab('') + ylab('')
        )
      }
    })
    
    arrangeGrob(grobs = plots, ncol = 3, left = ydim, bottom = xdim, top = pop)
  })
  
  # POP TABLE 
  dtPop <- getPopStats(gs)
  names(dtPop)[names(dtPop) == 'name'] <- 'File'
  dtPop$Percent <- round((dtPop$Count/dtPop$ParentCount)*100,1)
  names(dtPop)[names(dtPop) == 'Percent'] <- 'Percent of Parent'
  dtPop$percentTotal <- round((dtPop$Count/dt$ParentCount[which(dtPop$Population == 'nonDebris')])*100,1)
  names(dtPop)[names(dtPop) == 'Percent'] <- 'Percent of Sample'
  dtPop
  
  # QC REPORT
  percentages <-function(xqc, linenum){
    sapply(1:length(xqc), function(x){
      str_extract(xqc[[x]][linenum], '.*%')
    })
  } 
  
  print('180')
  
  qc <- data.table('File' = filenames, 
                   '% anomalous flow rate' = percentages(xqc, linenum = 2), 
                   '% anomalous signal acquisition' = percentages(xqc, linenum = 3), 
                   '% anomalous dynamic range' = percentages(xqc, linenum = 4)
  )
  
  # RMARKDOWN RENDER
  params <- list(plots = gatePlots,
                 QC = qc,
                 popTable = dtPop,
                 filenames = filenames)
  
  return(params)
}





















