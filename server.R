shinyServer <- function(input, output, session)
{
  # Increase the maximum input file size to 100MB to accomodate GUAVA FCS files
  options(shiny.maxRequestSize=100*1024^2)
  
  fcs <- reactiveValues(ff = NULL, 
                        QC = NULL, 
                        gs = NULL, 
                        g = NULL, 
                        gtable = NULL, 
                        plots = NULL, 
                        gtemp = NULL, 
                        popTable = NULL,
                        fileName = NULL, 
                        tsne = NULL)
  
  observeEvent(input$fileSelectFCS, {
    output$selectPlots <- renderUI({
      ids <- input$fileSelectFCS$name
      selectInput(inputId="fileSelector",
                  label="Select a file for analysis",
                  choices=ids)
    })
    
    observeEvent(input$fileSelector,{
      fcs$ff <- tryCatch({
        inputFiles <- data.table(input$fileSelectFCS)
        idx <- match(input$fileSelector, inputFiles$name)
        dataPath <- inputFiles$datapath[idx]
        
        # TODO: naming is weird why always V1
        f <- read.flowSet(files=dataPath, pattern="*.fcs")
        fcs$fileName <- inputFiles$name[idx]
        f
      },error = function(e){
        print('Failed to read in FCS files')
        return(e)
      })
    })
  })# END OF fileSelect
  
  observeEvent(input$runGating, {
    withProgress(message = 'Gating Data...',
                 value = 0, 
                 {
                   # Number of events for withProgress incrementation
                   n <- 11
                   sampleNames(fcs$ff) <- fcs$fileName
                   
                   if('1' %in% input$groupCheck1 & '2' %in% input$groupCheck1){
                     incProgress(amount = 1/n, detail = 'Running QC')
                     
                     x <- capture.output(
                       fcs$ff <- tryCatch({ flowSet(flowAI::flow_auto_qc(fcs$ff[[1]],
                                                                         remove_from = 'all',
                                                                         ChExcludeFS = c("SSC", "FSC", "Time"), output = 1,
                                                                         mini_report = FALSE,
                                                                         html_report = FALSE,
                                                                         fcs_QC = FALSE))
                       }, error = function(e){
                         print('Failed to run QC')
                         return(e)
                       })
                     )
                     # Character list output from flowAI
                     fcs$QC <- x
                   } else if('1' %in% input$groupCheck1 & !('2' %in% input$groupCheck1)){
                     x <- capture.output(
                       tryCatch({ flowSet(flowAI::flow_auto_qc(fcs$ff,
                                                               remove_from = 'all',
                                                               ChExcludeFS = c("SSC", "FSC", "Time"), output = 1,
                                                               mini_report = FALSE,
                                                               html_report = FALSE,
                                                               fcs_QC = FALSE))
                       }, error = function(e){
                         print('Failed to run QC')
                         return(e)
                       })
                     )
                     # Character list output from flowAI
                     fcs$QC <- x
                   }
                   
                   # Makes sure file name is in QC instead of index in flowset
                   fcs$QC[[1]] <- paste("Quality control for the file:", fcs$fileName, sep = ' ')
                   
                   incProgress(amount = 1/n, detail = 'Making Gating Template')
                   # Load preset gating templates (tcell, msc, dave)
                   if('1' %in% input$panelType){
                     tryCatch({
                       fcs$g <- gatingTemplate('TcellPanel1.csv')
                       fcs$gtable <- fread('TcellPanel1.csv')
                     }, error = function(e){
                       print('Failed for TCellPanel1 gating template')
                       return(e)
                     })
                   }
                   else if('2' %in% input$panelType){
                     tryCatch({
                       fcs$g <- gatingTemplate('MSCPanel.csv')
                       fcs$gtable <- fread('MSCPanel.csv')
                     }, error = function(e){
                       print('Failed for MSCPanel gating template')
                       return(e)
                     })
                   } else if('3' %in% input$panelType){
                     tryCatch({
                       fcs$g <- gatingTemplate('DavePanel.csv')
                       fcs$gtable <- fread('DavePanel.csv')
                     }, error = function(e){
                       print('Failed to make Luminal/Basal gating template')
                       return(e)
                     })
                   }
                   
                   # User uploaded gating template
                   observeEvent(input$gatingHierarchy, {
                     tryCatch({
                       fcs$g <- gatingTemplate(input$gatingHierarchy$datapath)
                       fcs$gtable <- fread(input$gatingHierarchy$datapath)
                     }, error = function(e){
                       print('Failed to make gating template')
                       return(e)
                     })
                   })
                   
                   # Compensation
                   incProgress(amount = 1/n, detail = 'Applying Compensation')
                   fcs$ff <- apply_comp_existing(fcs$ff)
                   
                   incProgress(amount = 1/n, detail = 'Making Gating Set')
                   fcs$gs <- tryCatch({
                     gs <- GatingSet(fcs$ff)
                     gs <- renameMarkers(gs)
                     gs
                   }, error = function(e){
                     print('Failed to make a gatingSet')
                     return(e)
                   })
                   
                   #Transformation
                   incProgress(amount = 1/n, detail = 'Applying Logicle Transformation')
                   #Default m = 5.3 because 4.5 seems to mess up data
                   chnls <- colnames(fcs$gs[[1]])[!colnames(fcs$gs[[1]]) %in% c("Time", "FSC-Width")]
                   trans <- NULL
                   m <- 5.3
                   catch <- TRUE
                   
                   while(catch){
                     tryCatch({
                       trans <- estimateLogicle(fcs$gs[[1]], chnls, m = m)
                       fcs$gs <- transform(fcs$gs, trans)
                       
                       tryCatch({
                         incProgress(amount = 1/n, detail = 'Applying Gating Template')
                         gating(fcs$g, fcs$gs)
                       }, error = function(e){
                         print('Failed to gate on gating template')
                       })
                       catch = FALSE
                     }, error = function(err){
                       m <<- m + 0.1
                     })
                   }
                   
                   if('1' %in% input$panelType){
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
                     lapply(hideNodes, function(thisNode)setNode(fcs$gs, thisNode, FALSE))
                   }
                   
                   incProgress(amount = 1/n, detail = 'Plotting Gates')
                   fcs$plots <- tryCatch({
                     # Get part of the gating template for plotting & nodes
                     plotdata <- fcs$gtable[,c('alias', 'parent', 'dims')]
                     nodes <- getNodes(fcs$gs)
                     # Make list of plots using openCyto
                     plots <- plots(plotdata, fcs$gs, nodes)
                     rows <- ceiling(length(plots)/3)
                     # Render plot 
                     output$multiplot <- renderPlot({
                       grid.arrange(grobs = plots, nrow= rows)
                     }, height = 1000)
                     plots
                   }, error = function(e){
                     print('Failed to make gating plots')
                     return(e)
                   })
                   
                   incProgress(amount = 1/n, detail = 'Making tSNE Plot')
                   fcs$tsne <- tSNEplot(fcs$gs)
                   
                   # SHINY OUTPUTS
                   output$downloadPlotBttn <- renderUI(downloadButton("downloadPlots", "Download Gating Plots"))
                   
                   output$downloadPlots <- downloadHandler(
                     filename = function() {
                       f <- gsub(".fcs", "", input$plotSelector, ignore.case=TRUE)
                       paste0(f,"_",Sys.Date(),"_plots.pdf")
                     },
                     content = function (file) {
                       # Plot printing
                       ag <-  arrangeGrob(grobs = plots)
                       ggsave(file = file, ag, width = 8, height = 11)
                     })
                   
                   incProgress(amount = 1/n, detail = 'Creating Sunburst Plots')
                   output$sunburst <- renderD3partitionR(sunburstZoom(fcs$gs))
                   output$downloadSunburstBttn <- renderUI(downloadButton("downloadSunburst", "Download Sunburst Plot"))
                   
                   sb <- sunburst_static(getPopStats(fcs$gs), getNodes(fcs$gs))
                   output$downloadSunburst <- downloadHandler(
                     filename = function() {
                       f <- gsub(".fcs", "", input$plotSelector, ignore.case=TRUE)
                       paste0(f,"_",Sys.Date(),"_sunburstplot.pdf")
                     },
                     content = function (file) {
                       # Plot printing
                       ggsave(sb, file = file, height = 11, width = 8)
                     })
                   
                   output$tSNEplot <- renderPlot(fcs$tsne)

                   incProgress(amount = 1/n, detail = 'Generating Population Table')
                   # fcs$popTable for population table and results summary
                   dt <- getPopStats(fcs$gs)
                   dt$name <-fcs$fileName
                   names(dt)[names(dt) == 'name'] <- 'File Name'
                   dt$Percent <- round((dt$Count/dt$ParentCount)*100,1)
                   names(dt)[names(dt) == 'Percent'] <- 'Percent of Parent'
                   dt$percentTotal <- round((dt$Count/dt$ParentCount[which(dt$Population == 'nonDebris')])*100,1)
                   names(dt)[names(dt) == 'Percent'] <- 'Percent of Sample'
                   fcs$popTable <- dt
                   
                   output$populationTable <- DT::renderDataTable({
                     fcs$popTable 
                   },options = list(pageLength = 10))
                   
                   # Results Summary
                   incProgress(amount = 1/n, detail = 'Summarizing Data')
                   output$populationTableSummary <- DT::renderDataTable({
                     fcs$popTable 
                   },options = list(pageLength = 10))
                   
                   output$QCreport <- renderUI({
                     HTML(paste(fcs$QC, collapse ="<br/>"))
                   })
                   
                   output$multiplotSummary <- renderPlot({
                     rows <- ceiling(length(fcs$plots)/3)
                     # Render plot 
                     grid.arrange(grobs = fcs$plots, nrow= rows)
                   }, height = 1000)
                   
                   output$sunburstSummary <- renderPlot({
                     sb
                   }, height = 1000)
                   
                  
                 })# END OF withProgress 
  })# END OF runGating
  
  output$downloadSummary <- downloadHandler (
    filename=function(){paste('STEMCELL_FACS_report_',
                              Sys.Date(),
                              input$sumTabType,
                              sep='')},
    content=function(file) {
      # if(input$sumFile == 'This file' && input$sumTabType == '.html'){
        params <- list(QC = fcs$QC, 
                       popTable = fcs$popTable,
                       nodes = getNodes(fcs$gs),
                       plots = fcs$plots)
        
        # withProgress({
          rmarkdown::render('html-report.rmd', 
                            output_format = 'all',
                            output_file = file,
                            params=params,
                            quiet=TRUE,
                            envir=new.env(parent = globalenv()))
        # }, message = 'Compiling Report')   
      # }
    }
  )# END OF summaryDownload
  
  observeEvent(input$gatingtempfile, {
    gtemp <- gatingTemplate(input$gatingtempfile$datapath)
    
    observeEvent(input$gatingTempBttn,{
      output$gttestplot <- renderPlot({
        openCyto::plot(gtemp)
      }, height = 600, width = 1300)
    })
  })
  
  
}# END OF SHINY SERVER
