shinyServer <- function(input, output, session)
{
  # Increase the maximum input file size to 100MB to accomodate GUAVA FCS files
  options(shiny.maxRequestSize=100*1024^2)

  fcs <- reactiveValues(ff = NULL, 
                        QC = NULL, 
                        gs = NULL, 
                        gt = NULL, 
                        g = NULL, 
                        gtable = NULL, 
                        plots = NULL, 
                        gtemp = NULL)
  
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
        sampleNames(f) <- inputFiles$name[idx]
        f
      },error = function(e){
        print('Failed to read in FCS files')
        return(e)
      })
    })
    
  })# END OF fileSelect

    observeEvent(input$runGating, {
      x <- capture.output(
        fcs$ff <- tryCatch({ flowSet(flowAI::flow_auto_qc(fcs$ff,
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

     tryCatch({
       fcs$g <- gatingTemplate(input$gatingHierarchy$datapath)
       fcs$gtable <- fread(input$gatingHierarchy$datapath)
      }, error = function(e){
        print('Failed to make gating template')
        return(e)
      })
      
      # Compensation
      fcs$ff <- apply_comp_existing(fcs$ff)
      
      fcs$gs <- tryCatch({
        gs <- GatingSet(fcs$ff)
        gs <- renameMarkers(gs)
        gs
      }, error = function(e){
        print('Failed to make a gatingSet')
        return(e)
      })

      #Transformation
      #Default m = 5.3 because 4.5 seems to mess up data
      chnls <- colnames(fcs$gs[[1]])[!colnames(fcs$gs[[1]]) %in% c("Time", "FSC-Width")]
      trans <- NULL
      m <- 5.3
      catch <- TRUE

      while(catch){
        tryCatch({
          trans <- estimateLogicle(fcs$gs[[1]], chnls, m = m)
          fcs$gt <- transform(fcs$gs, trans)
          
          tryCatch({
            gating(fcs$g, fcs$gt)
          }, error = function(e){
            print('Failed to gate on gating template')
          })
          catch = FALSE
        }, error = function(err){
          m <<- m + 0.1
        })
      }
      
      fcs$plots <- tryCatch({
        # Get part of the gating template for plotting & nodes
        plotdata <- fcs$gtable[,c('alias', 'parent', 'dims')]
        nodes <- getNodes(fcs$gt)
        # Make list of plots using openCyto
        plots <- plots(plotdata, fcs$gt, nodes)
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

      print('end')
      
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
      
      output$sunburst <- renderD3partitionR(sunburstZoom(fcs$gt))
      output$downloadSunburstBttn <- renderUI(downloadButton("downloadSunburst", "Download Sunburst Plot"))
      sb <- sunburst_static(fcs$gt)
      output$downloadSunburst <- downloadHandler(
        filename = function() {
          f <- gsub(".fcs", "", input$plotSelector, ignore.case=TRUE)
          paste0(f,"_",Sys.Date(),"_sunburstplot.pdf")
        },
        content = function (file) {
          # Plot printing
          ggsave(sb, file = file, height = 11, width = 8)
        })
      
      output$populationTable <- DT::renderDataTable({
       DT::datatable(getPopStats(fcs$gt)) 
      },options = list(pageLength = 25))

      observeEvent(input$gatingtempfile, {
        print('hi')
        fcs$gtemp <- gatingTemplate(input$gatingtemptest$datapath)
        save(fcs$gtemp, file = 'gtemp.rda')
        })
      
      observeEvent(input$gatingTemp,{
        print('hi')
        output$gttestplot <- renderPlot({
          openCyto::plot(fcs$gtemp)
        }, height = 500)
        fcs$gtemp
        openCyto::plot(fcs$gtemp)
      })

    
      
    })# END OF runGating
  }
