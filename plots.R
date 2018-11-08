sunburstZoom <- function(gt){
  stats <- getPopStats(gt)
  stats$nodes <- getNodes(gt)[2:length(getNodes(gt))]
  stats$gatenum <- lengths(gregexpr("/", stats$nodes))
  
  #Building table for leafnodes (longest paths)
  leafnodes <- stats[stats$gatenum == max(stats$gatenum),]
  
  #Preserving population names for column namesbefore count/percent get added 
  popClean <- gsub('\\+|-', '', unlist(strsplit(leafnodes$nodes[1], '/')))
  popClean <- popClean[which(popClean != '')]
  
  #2D list where each row is a node, and the columns are the hierarchical steps of that node (ex. nonDebris, singlets...etc)
  a <- lapply(1:length(leafnodes$nodes), function(x){
    #Separate node hierachy by '/' & get rid of empty strings (s is each hierarchical step)
    s <- unlist(strsplit(leafnodes$nodes[x], '/'))
    s <- s[which(s != '')]
    
    #sc is a list of all hierarchy counts/percentages 
    sc <- lapply(1:length(s), function(x){
      #idx is the index in the stats table of the grepped population - if there's more than one, add parent and grep 
      idx <- grep(paste(gsub('+', '\\+', s[x], fixed=TRUE), '$', sep=''), stats$Population)
      if (length(idx) > 1){
        idx <- grep(paste(gsub('+', '\\+', paste(s[x-1], '/', s[x], sep=''),
                               fixed=TRUE), '$', sep=''), stats$Population)
      }
      #Switch count to K, calculate % and return as sc
      c <- paste(round(stats$Count[idx]/1000, 1),'K',sep='') 
      p <- paste(round((stats$Count[idx]/stats$ParentCount[idx])*100, 1), "%", sep='')
      paste(p,c)
    })
    #Paste hierarchy step with count + percent 
    paste(s, sc)
  })
  
  #Convert from list to datatable, add colnames and population count 
  a <- data.table(do.call(rbind, a))
  colnames(a) <- popClean
  a$Count <- leafnodes$Count
  
  #Build table for ungated population 
  ungated <- stats[stats$gatenum != max(stats$gatenum),]
  ugCount <- ungated$ParentCount - ungated$Count
  #Add count/percentage to ungated nodes 
  ungated$nodes <- paste(paste(ungated$nodes, '_ug', sep = ''), 
                         paste(round((ugCount/ungated$ParentCount)*100,1), '%', sep=""),
                         paste(round(ugCount/1000, 1), 'K', sep = ''))
  
  #Similar to a, but modified so count/percentage isn't added to ungated element (added above)
  aug <- sapply(1:length(ungated$nodes), function(x){
    s <- unlist(strsplit(ungated$nodes[x], '/'))
    s <- s[which(s != '')]
    
    sc <- lapply(1:length(s), function(x){
      idx <- grep(paste(gsub('+', '\\+', s[x], fixed=TRUE), '$', sep=''), stats$Population)
      ret <- ''
      
      if (length(idx) > 1){
        idx <- grep(paste(gsub('+', '\\+', paste(s[x-1], '/', s[x], sep=''),
                               fixed=TRUE), '$', sep=''), stats$Population)
      }
      
      if(length(idx) != 0){
        c <- paste(round(stats$Count[idx]/1000, 1),'K',sep='') 
        p <- paste(round((stats$Count[idx]/stats$ParentCount[idx])*100, 1), "%", sep='')
        ret <- paste(p, c)
      }
      ret
    })
    paste(s, sc)
  })
  
  #Fill empty spots in list with NA to allow for data.frame conversion 
  maxElements <- max(leafnodes$gatenum) 
  aug <- lapply(1:length(aug), function(x){
    repn <- maxElements - length(aug[[x]])
    c(aug[[x]], rep(NA, times = repn))
  })
  
  
  aug <- data.table(do.call(rbind, aug))
  colnames(aug) <- popClean
  aug$Count <- ugCount
  
  #sb = almagamated leafnode and ungated data for sunburst plotting
  sb <- rbind(a, aug)
  
  #Colour palette 
  pp <- c('#ffffff',brewer.pal(7, 'Set2'), brewer.pal(9, 'Pastel1'), brewer.pal(8, 'Pastel2'), brewer.pal(8,'Set3'))
  # The number elements of the sunburst (all names/ everything that's not a number)
  elements <- length(which(is.na(as.numeric(unique(unlist(sb))))))
  # Bandaid fix for weird legend bug
  pp <- c(pp[1:elements-1])
  
  D3partitionR()%>%
    add_data(sb, count = 'Count', steps = colnames(sb)[1:max(stats$gatenum)]) %>%
    set_discrete_color_scale(pp) %>%
    # A lot of css that doesn't seem to do much
    set_tooltip_parameters(
      style = '
    font-family: Geneva, Verdana, sans-serif;
    font-size:0.75em;
    text-shadow:none;
    border-color:white;
    opacity:0.85;
    border-radius:4px;
    border:1px;
    padding:3px;
    text-shadow: #ffffff 0px 0;'
    ) %>%
    set_nodes_styles(idle_style = 'stroke : white; stroke-width : 2;',
                     hovered_style = 'stroke:white; stroke-width:3.5') %>%
    plot()
}

# TODO: repel text ----
# TODO: scale_x_discrete limits (make gate level names)
# TODO: Multiple sunbursts?/choice implementation
sunburst_static <- function(popStats, nodes){
  ps <- popStats
  psplot <- ps[, c("Population", "Count", 'ParentCount')]
  psplot$percent <- round((psplot$Count/psplot$ParentCount)*100, 1)
  psplot$nodes <- nodes[2:length(nodes)]
  psplot$gateNum <- lengths(gregexpr("/", psplot$nodes))
  psplot$pop_clean <- sapply(psplot$population, function(x) gsub("\\+|-", "", x))
  
  # Color palette
  pp <- c(brewer.pal(9, 'Pastel1'), brewer.pal(7, 'Set2'), brewer.pal(9, 'Pastel2'))
  pp <- pp[1:length(psplot$Population)]
  
  psplot %>% ggplot(aes(x = gateNum, y = Count, fill = Population))+
    geom_col(width = 0.99, color = "white", size = 0.25, position = position_stack())+
    geom_text_repel(aes(label = paste(round(Count/1000, 1), "K", sep = " ")) ,
                    size = 3.1, position = position_stack(vjust = 0.5), hjust=0.5, force = 7) +
    scale_fill_manual(values = pp,
                      name = 'Cell Population') +
    scale_x_discrete(limits = psplot$gateNum)+
    labs(title = "Cell Hierarchy") +
    coord_polar(theta = "y") +
    theme_classic() +
    theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5))
}

tSNEplot <- function(gt){
  nodes <- getNodes(gt)[2:length(nodes)]

  pops <- lapply(1:length(nodes), function(x){
    flowWorkspace::getIndices(gt[[1]], nodes[x])
  })
  
  names(pops) <- nodes
  pops <- pops[order(sapply(names(pops), function(x) length(unlist(gregexpr("/",x)))))]
  
  exp <- data.table(gt[[1]]@data[[1]]@exprs)
  exp <- select(exp,contains('-A'))
  exp$Population <- 'Debris'
  
  for(i in 1:length(pops)){
    pop_idx <- pops[[i]]
    exp$Population[pop_idx] <- rep(names(pops[i]), length(pop_idx))
  }

  indx <- caret::createDataPartition(exp$Population, p = 0.1)
  exp <- exp[unlist(indx),]
  
  tsne <-Rtsne(as.matrix(exp[,1:15]), max_iter = 1000, verbose = TRUE, perplexity = 40)$Y
  tsne <- data.frame(tsne)
  tsne$Population <- as.factor(exp$Population)
  
  tsne_plot <- ggplot(data=tsne, aes(x=tsne[,1], y=tsne[,2])) +
    geom_point(aes(color=Population)) +
    ggtitle("2D t-SNE Projection") +
    theme_minimal(base_size=14)
  return(tsne_plot)
}

