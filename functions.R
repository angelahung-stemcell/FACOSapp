# Input: - fcsSet: flowSet of samples
# Output: flowSet of samples that have been compensated on "channels"
apply_comp_existing <- function(fcsSet) {
  # iterate through flowSet and apply compensation matrix stored with flowFrame
  fcsSet.comp <- fsApply(fcsSet, function(ff) {
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

renameMarkers <- function(fs){
  fs <- tryCatch({
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
    fs
  }, error = function(e){
    print(e)
    return(e)
  })
  return (fs)
}

transformGating <- function(fs, g){
  chnls <- colnames(fs[[1]])[!colnames(fs[[1]]) %in% c("Time", "FSC-Width")]
  trans <- NULL
  m <- 5.3
  outercatch <- TRUE
  count <- 0
  
  while(outercatch && count < 10 ){
    tryCatch({
      fstrans <- fsApply(fs, function(ff){
        catch <- TRUE
        while(catch){
          tryCatch({
            print('trans')
            trans <- estimateLogicle(ff, chnls, m=m)
            print('transforming')
            fftrans <<- transform(ff, trans)
            catch = FALSE
          }, error = function(err){
            count <<- count + 1
            m <<- m + 0.1
          })
        }
        fftrans
      })
      print('gating')
      gs <- GatingSet(fstrans)
      # gating(g,gs)
      outercatch <- FALSE
    }, error = function(e){
      count <<- count + 1
      m <<- m + 0.1
    })
    print(count)
  }
  
  if(count == 10){
    print(
      'Failed to transform and gate'
    )
  }
  
  return(gs)
}

fmocleaning <- function(fs, filenames, g){
  fs <- flowSet(sapply(1:length(fs), function(x){
    ft <- flowAI::flow_auto_qc(fs[[x]], 
                               remove_from = 'all', 
                               ChExcludeFS = c("SSC", "FSC", "Time"), output = 1,
                               mini_report = FALSE, 
                               html_report = FALSE, 
                               fcs_QC = FALSE)
    
    ft
  }))
  sampleNames(fs) <- filenames
  # Compensation
  print('comp')
  fs <- apply_comp_existing(fs)
  
  #Setting marker names to dye names instead of FL1-etc. 
  print('markers')
  fs <- renameMarkers(fs)
  
  #Transformation
  print('transform')
  gstrans <- transformGating(fs, g)
  return(gstrans)
}







