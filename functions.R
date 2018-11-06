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

renameMarkers <- function(gs){
  gs <- tryCatch({
    #Setting marker names to dye names instead of FL1-etc. 
    newMarkers <- unname(gs[[1]]@data[[1]]@parameters$desc)
    #For naming inconsistencies
    newMarkers <- gsub("CD8\\s", "CD8a ", newMarkers)
    cols <- colnames(gs)
    
    #Keeping only '-A' markers and leaving '-H' markers as FL-etc 
    #ONLY FOR CYTOFLEX - FOR GUAVA -HLIN VS -HLOG
    for(i in 1:length(cols)){
      if(grepl('-H', newMarkers[[i]])){
        newMarkers[[i]] <-  cols[[i]]
      }
    }
    #Renaming markers and columns 
    newMarkers <- gsub("\\[\\d+\\]", "", newMarkers)
    colnames(gs) <- c(newMarkers)
    names(newMarkers) <- newMarkers
    markernames(gs) <- newMarkers
    gs
  }, error = function(e){
    print(e)
    return(e)
  })
  return (gs)
}

make_plot <- function(gt, x, y, gates){
  p <-  tryCatch({
    # Title (ugly but allows CCR7/CD45RA to have parent behind it)
    parts <- unlist(strsplit(gates[1], '/'))
    l <- length(parts)
    title <- paste(parts[l-1], '/',gsub('(\\+|-)', ' ', parts[l]))
    # Make ggcyto plots 
    ggcyto(gt[[1]], aes_(x = x, y = y)) + geom_hex(bins = 200) +
      geom_gate(gates) + geom_stats(size = 4, adjust = 0.90, negated = TRUE) +
      xlab(x) + ylab(y) + ggtitle('') + facet_grid(cols=vars(eval(title)))
  }, error = function(e){
    print(e)
    return(e)
  })
  return(as.ggplot(p))
}


plots <- function(plotdata, gt, nodes){
  plots <- tryCatch({
    lapply(1:length(plotdata$alias), function(x){
      if(!grepl(',', plotdata$dims[x])){
        # For mindensity when plotted against default.y (FSC-A) // ex. 7AAD
        make_plot(gt, x = 'FSC-A', y = plotdata$dims[x][[1]], gates = plotdata$alias[x])
      }else {
        # Plotting against 2 channels
        # Splitting dims based on ',' delimiter
        d <- plotdata$dims[x]
        chnls <- unlist(strsplit(d, ','))
        
        if(plotdata$alias[x] == '*'){
          # For expanded populations
          parentname <- gsub('+','(\\+)', plotdata$parent[x],fixed = TRUE) #Make parent name regex friendly
          
          gatereg <- paste(parentname,'/',chnls[[1]], '(\\+|-)', chnls[[2]], '(\\+|-)$', sep = '') #Regex exp. to find all gates
          
          gates <- grep(gatereg, nodes, value = TRUE) #Find gates through gt nodes
          
          make_plot(gt, x = chnls[[1]], y = chnls[[2]], gates = gates)
        }else if(grepl(',', plotdata$alias[x])){
          gates <- unlist(strsplit(plotdata$alias[x], ','))
          make_plot(gt, x = chnls[[1]], y = chnls[[2]], gates = gates)
        }else{
          # Simple graphs/ normally the first 3
          make_plot(gt, x = chnls[[1]], y = chnls[[2]], gates = plotdata$alias[x])
        }
      }
    })
  }, error = function(e){
    print(e)
    return(e)
  })
}




