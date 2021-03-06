shinyUI <- fluidPage(
  tags$head(HTML('
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/default/css/styles.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/base/default/css/widgets.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/styles_sc.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/header.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/footer.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/responsive.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/custom_grid.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/default/css/grid_responsive.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/default/css/font-awesome.min.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/default/css/skin.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/default/css/custom.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/default/css/fancybox.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/workflow.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/brand.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/research_areas.css" media="all" />
                 <link rel="stylesheet" type="text/css" href="https://cdn.stemcell.com/skin/frontend/blacknwhite/stemcell/css/print.css" media="print" />')),
  tags$style(type='text/css', '.sc-button {margin-top: 0px;}'),
  tags$style(".wrapper {
             background-color: #F7F7F5; 
             padding-top: 25px; 
             padding-bottom: 25px; 
             }"),
  headerPanel(windowTitle="STEMCELL Technologies Flow Cytometry Tool", 
              HTML("<div class='row'>
                   <div class='wrapper top'>
                   <div class='col-sm-2'>
                   </div>
                   <div class='col-sm-5'>
                   <a href='/' class='logo'>
                   <img src='//tbcdn.talentbrew.com/company/8172/v1_0/img/stemcell_logo.png' 
                   alt='STEMCELL Technologies'>
                   </a>
                   </div>
                   <div class='col-sm-3'>
                   <a class='sc-button' 
                   target='_blank' 
                   href='https://www.stemcell.com ', 
                   style='float: right;'>Explore Products
                   </a>
                   </div>
                   <div class='clear'>
                   </div>
                   </div>
                   <ul class='link-list' aria-hidden='false' aria-expanded='true'>
                   </ui>
                   </div>"
              )),
  
  
  # Sidebar Panel 
  sidebarPanel(#style="position:fixed;width:inherit;", 
    width=4,
    #Inherit CSS  
    
    tags$style(type="text/css", 
               "form.well {max-width: 300px;
               background: #F7F7F7;
               width: 350px;
               float: right;}"),
    tags$style(".navbar-default {background-color:#FFF;}
               .navbar-brand {padding:0;}
               .navbar .container-fluid {padding:0;}"),
    tags$style(HTML(".multicol .shiny-options-group{
                    -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                    -moz-column-count: 2;    /* Firefox */ 
                    column-count: 2; 
                    -moz-column-fill: balanced;
                    -column-fill: balanced;
                    }
                    .checkbox{
                    margin-top: 3px !important;
                    -webkit-margin-after: 3px !important;
                    }")),
    tags$head(tags$style(".progress-bar{background-color:#E47C23;}")),
    
    # STYLE: makes toggle sliders orange
    tags$style(HTML(".bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-StemCellOrange,
                    .bootstrap-switch .bootstrap-switch-handle-off.bootstrap-switch-StemCellOrange {
                    color: #fff;
                    background:#E47C23;}")),
    
    #Change color for dropdown menu for customer data email
    tags$style(HTML(' .dropdown-toggle:hover{color:#E47C23; background:#fff;}')),
    tags$style(HTML('.dropdown-toggle {color: #fff; background:#E47C23;}')),
    tags$style(HTML('.dropdown-toggle:active, .open .dropdown-toggle{color: #fff !important; background:#E47C23 !important;}')),
    tags$style(HTML('.dropdown-toggle:focus, .open .dropdown-toggle{color: #fff !important; background:#E47C23 !important;}')),
    
    tags$style(HTML(".tooltip > .tooltip-inner {background-color: #E47C23;}")),
    tags$style(HTML(".tooltip + .tooltip.top > .tooltip-arrow {border-top-color: #E47C23;}")),
    
    
    # STYLE: makes radio buttons orange
    tags$style(HTML("input[type=radio]:checked + label:before,
                    input[type=checkbox]:checked + label:before {background: #E47C23 !important; 
                    border-color: #53585A !important; 
                    border-radius: 100px !important;
                    -webkit-transition: all .5s ease;
                    -moz-transition: all .5s ease;
                    -o-transition: all .5s ease;
                    -ms-transition: all .5s ease;
                    transition: all .5s ease;}")), 
    tags$style(HTML("input[type=radio] + label:before,
                    input[type=checkbox] + label:before {border-color: #53585A !important;
                    border-radius: 100px !important;
                    -webkit-transition: all .5s ease;
                    -moz-transition: all .5s ease;
                    -o-transition: all .5s ease;
                    -ms-transition: all .5s ease;
                    transition: all .5s ease;}")),
    
    
    tags$div(h1(tags$b("Flow Cytometry Analysis App"))),
    hr(),
    uiOutput("selectPlots"),
    h2(tags$b("Step 1: Upload")),
    h3("Upload Sample Files"),
    fileInput(inputId="fileSelectFCS", label="Select FCS Files", 
              multiple = TRUE, accept = c(".fcs", ".FCS")),
    
    h3("(Optional): Upload FMO Files"),
    fileInput(inputId="fileSelectFMO", label="Select FCS Files", 
              multiple = TRUE, accept = c(".fcs", ".FCS")),
    h2(tags$b("Step 2: Select Parameters")),
    
    awesomeCheckboxGroup(inputId="groupCheck1", label="QC Settings:", 
                         choices=c("Run QC"=1, 
                                   "Exclude Failed QC Events from Analysis"=2), 
                         inline = FALSE, status = "primary",
                         selected=c(1,2)),
    # awesomeCheckboxGroup(inputId="groupCheck2", label="Other Settings:",
    #                      choices=c("Include FMOs in Gating (in progress)"=1,  
    #                                "GUAVA Files (also in progress)" = 2
    #                      ),
    #                      inline = FALSE, status = "primary"),
    h2(tags$b("Step 3: Select a Gating Template")),
    h3(tags$b('Choose a Preset Gating template')),
    selectInput(inputId="panelType", label="Preset panels",
                choices=c('Select a panel' = 4, 
                          "T-Cell Panel 1 (CCR7|CD45RA)"=1,  
                          "MSC Panel" = 2, 
                          'Luminal/Basal' = 3
                ),
                selected = NULL, multiple = FALSE),
    br(),
    h3(tags$b('Or upload your own gating template')),
    fileInput(inputId="gatingHierarchy", label="Upload Gating Template (.csv)", 
              multiple = FALSE, accept = c(".csv", ".CSV")),
    br(),
    actionButton(inputId="runGating",
                 label="Run Gating",
                 class='sc-button')
    
  ),# END SIDEPANEL
  
  mainPanel(
    tags$style(".navbar-default {background-color:#FFF;}
               .navbar-brand {padding:0;}
               .navbar .container-fluid {padding:0;}"), 
    navbarPage(title='', id="autoGate", collapsible=TRUE,
               tabPanel(title="Check Gating Template", value="setup",
                        h1("Check .csv Gating Template"),
                        hr(),
                        fluidRow(column(3,
                                        fileInput(inputId="gatingtempfile", label="Upload .csv file", 
                                                  multiple = FALSE, accept = c(".csv", ".CSV"))),
                                 column(3, 
                                        br(),
                                        actionButton(inputId="gatingTempBttn",
                                                     label="Check Gating Template",
                                                     class='sc-button'))
                        ),
                        hr(),
                        plotOutput('gttestplot'),
                        hr()
               ),
               
               tabPanel(title="Gate Plots", value="gatePlots",
                        fluidRow(column(2,
                                        h1("Gating Plots")),
                                 column(2,
                                        offset=8,
                                        uiOutput("downloadPlotBttn"))),
                        hr(),
                        conditionalPanel(condition = 'input.runGating', 
                                         withSpinner({plotOutput("multiplot",
                                                                 height=750)}, type = 6, color = '#E47C23'))
                        ),
               
               tabPanel(title="Population Table", value="popTable",
                        fluidRow(column(6,
                                        h1("Cell Populations"))),
                        hr(),
                        DT::dataTableOutput("populationTable")),
               tabPanel(title="Sunburst", value="sunburst",
                        h1("Sunburst Plot"),
                        column(2,
                               offset=8,
                               uiOutput("downloadSunburstBttn")),
                        hr(),
                        D3partitionROutput('sunburst', height = 700),
                        hr()
               ),
               tabPanel(title="t-SNE Plot", value="tsnePlot",
                        h1("t-Distributed Stochastic Neighbor Embedding Plot"),
                        hr(),
                        h3("t-SNE Parameters"),
                        fluidRow(column(3,
                                        numericInput(inputId="subsetProportion",
                                                     label="Downsample proportion (improves speed)",
                                                     step=0.05,
                                                     value=0.3,
                                                     min=0.05,
                                                     max=1
                                        )),
                                 column(3, 
                                        numericInput(inputId="tsne_iter",
                                                     label="Number of iterations",
                                                     step=50,
                                                     value=1000,
                                                     min=500,
                                                     max=5000 # TODO: Adjust max depending on the number of rows in the sample (< nrow(data)-2)
                                        )
                                 ),
                                 column(3,
                                        numericInput(inputId="tsne_perplex",
                                                     label="Perplexity",
                                                     step=1,
                                                     value=30,
                                                     min=2,
                                                     max=100 # TODO: Adjust max depending on the number of rows in the sample (3*perplexity < nrow(data)-1)
                                        ))
                        ),
                        fluidRow(column(3, 
                                        actionButton(inputId="runTSNE",
                                                     label="Generate T-SNE",
                                                     class='sc-button')),
                                 column(9,
                                        uiOutput("populationExclude"))),
                        br(), 
                        conditionalPanel(condition = 'input.runTSNE', 
                                         withSpinner({plotOutput("tSNEplot",
                                                    height=750)}, type = 6, color = '#E47C23'))
               ),
               
               tabPanel(title="Results Summary", value="resSum", 
                        fluidRow(column(width=12,
                                        tags$div(id="sumDwnld",
                                                 fluidRow(column(width=2,
                                                                 textInput("sumTabName",
                                                                           label=NULL,
                                                                           value=paste('STEMCELL_FACS_report_',
                                                                                       Sys.Date(),
                                                                                       sep='')),
                                                                 offset=0,
                                                                 tags$style(type="text/css", 
                                                                            "#tabName { text-align:right; font-size: 10px;}")),
                                                          
                                                          column(width=2,
                                                                 offset=0.5,
                                                                 selectInput('sumFile',
                                                                             label=NULL,
                                                                             choices=list('This file', 'All files'),
                                                                             selected="This file",
                                                                             width='100%')),
                                                          column(width=2,
                                                                 offset=0,
                                                                 selectInput('sumTabType',
                                                                             label=NULL,
                                                                             choices=list(".html"), #,".docx", ".pdf"
                                                                             selected=".html",
                                                                             width='60%')),
                                                        
                                                          column(width=3,
                                                                 offset = 0, 
                                                                 downloadButton("downloadSummary",
                                                                                "Download Summary Report"
                                                                 )), 
                                                          
                                                          column(width=3,
                                                                 offset = 0, 
                                                                 downloadButton("downloaddata",
                                                                                "Download Raw Data as .csv"
                                                                 )))))), #yikes
                        br(),
                        h2('QC Report'), 
                        hr(), 
                        uiOutput('QCreport'),
                        br(),
                        h2('Population Table'),
                        hr(),
                        DT::dataTableOutput("populationTableSummary"),
                        br(),
                        h2('Gating Plots'),
                        hr(),
                        plotOutput("multiplotSummary", height = 1000),
                        br(),
                        h2('Sunburst Plot'),
                        hr(),
                        plotOutput('sunburstSummary', height = 1000),
                        hr(),
                        br()
               )
    )# NAVBAR PAGE
  )# MAINPANEL
)# END UI







