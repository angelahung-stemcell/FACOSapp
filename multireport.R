source("plots.R")
source('functions.R')

#Plugins ----
tmix2DGate.wrapper <- function(fr, pp_res = NULL, channels, ...){
  cytoUtils::tmix2DGate(fr, channels, ...)
}
registerPlugins(tmix2DGate.wrapper, "tmix2DGate.wrapper")
#----

multireport <- function(fs, g, filenames, tcell = FALSE){
  print(tcell)
  withProgress(message = 'Generating Report...', 
               value = 0,{
                 n <- 7
                 
                 xqc <- vector(mode = 'list', length = length(fs))
                 
                 incProgress(amount = 1/n, detail = 'Running QC')
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
                 
                 incProgress(amount = 1/n, detail = 'Applying Compensation')
                 # Compensation 
                 fs <- apply_comp_existing(fs)
                 
                 incProgress(amount = 1/n, detail = 'Setting marker names')
                 #Setting marker names to dye names instead of FL1-etc. 
                 fs <- renameMarkers(fs)
                 
                 #Transformation
                 incProgress(amount = 1/n, detail = 'Applying logicle transformation')
                 
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
                 
                 incProgress(amount = 1/n, detail = 'Gating data')
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

                 incProgress(amount = 1/n, detail = 'Generating plots')
                 
                 gatePlots <- plots(gs)
                 
                 incProgress(amount = 1/n, detail = 'Prepping report')
                 # QC REPORT
                 percentages <-function(xqc, linenum){
                   sapply(1:length(xqc), function(x){
                     str_extract(xqc[[x]][linenum], '.*%')
                   })
                 } 
                 
                 qc <- data.table('File' = filenames, 
                                  '% anomalous flow rate' = percentages(xqc, linenum = 2), 
                                  '% anomalous signal acquisition' = percentages(xqc, linenum = 3), 
                                  '% anomalous dynamic range' = percentages(xqc, linenum = 4)
                 )
                 
                 # RMARKDOWN RENDER
                 params <- list(plots = gatePlots,
                                QC = qc,
                                # popTable = dtPop,
                                filenames = filenames)
                 
                 return(params)
               })  
}

