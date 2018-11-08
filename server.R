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
                        fileName = NULL)
  
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
      sampleNames(fcs$ff) <- fcs$fileName
      
      if('1' %in% input$groupCheck1 & '2' %in% input$groupCheck1){
        
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
          fcs$gs <- transform(fcs$gs, trans)
          
          tryCatch({
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
      if('2' %in% input$panelType){
        # MSC PANEL
        # Hide nodes (gets rid of intermediate steps/gates when graphing)
        hideNodes <- c('CD73+',
                       'CD90+',
                       'CD73+CD90-',
                       'CD73-CD90+',
                       'CD73-CD90-',
                       'CD146+',
                       'CD105+')
        lapply(hideNodes, function(thisNode)setNode(gt, thisNode, FALSE))
      }
      
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
      
      # fcs$popTable for population table and results summary
      dt <- getPopStats(fcs$gs)
      dt$name <-fcs$fileName
      names(dt)[names(dt) == 'name'] <- 'File Name'
      dt$Percent <- round((dt$Count/dt$ParentCount)*100,1)
      fcs$popTable <- dt
      
      output$populationTable <- DT::renderDataTable({
        fcs$popTable 
      },options = list(pageLength = 10))
      
      # Results Summary
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
      
      output$downloadSummary <- downloadHandler (
        filename=function(){paste('STEMCELL_FACS_report_',
                                  Sys.Date(),
                                  input$sumTabType,
                                  sep='')},
        content=function(file) {
          if(input$sumFile == 'This file' && input$sumTabType == '.html'){
            params <- list(QC = fcs$QC, 
                           popTable = fcs$popTable,
                           nodes = getNodes(fcs$gs),
                           plots = fcs$plots)
            
            rmarkdown::render('html-report.rmd', 
                              output_format = 'all',
                              output_file = file,
                              params=params,
                              quiet=TRUE,
                              envir=new.env(parent = globalenv()))
          }
        }
      )# END OF summaryDownload
    })# END OF runGating
    
    observeEvent(input$gatingtempfile, {
      gtemp <- gatingTemplate(input$gatingtempfile$datapath)
      
      observeEvent(input$gatingTempBttn,{
        output$gttestplot <- renderPlot({
          openCyto::plot(gtemp)
        }, height = 600, width = 1300)
      })
    })
    
    
    
    
    
}# END OF SHINY SERVER
