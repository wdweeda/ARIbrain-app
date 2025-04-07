## --------------- (0) Preparation --------------- ##

# By default, Shiny limits file uploads to 5MB per file. This limit 
# is modified by using the shiny.maxRequestSize option to 100MB.
options(shiny.maxRequestSize = 100*1024^2)

# Load required libraries
packages <- c("shiny", 
              "shinyjs",
              "shinydashboard",
              "DT",
              "RNifti", 
              "Rcpp",
              "hommel",
              "ARIbrain")
# Install missing packages and load packages
pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(pkgs)) install.packages(pkgs, dependencies = TRUE) 
suppressPackageStartupMessages(invisible(sapply(packages, require, character.only = TRUE)))

# Source necessary scripts dynamically
scripts <- c("utils.R", 
             "menu1.R", 
             "menu2.R", 
             "menu3.R", 
             "menu4.R", 
             "menu5.R")
lapply(scripts, source)

# Define package version
appVersion <- "0.1.5"

## --------------- (1) Define UI --------------- ##
## using "shinydashboard" ##

# Define header
header <- dashboardHeader(
  #title = "ARIbrain"
  title = paste0("ARIbrain v", appVersion)
)

# Define sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", sidebarMenuOutput("menu"))
)

# Define body
body <- dashboardBody(
  ## Using "shinyjs" ##
  shinyjs::useShinyjs(),
  
  tabItems(
    # Tab 1: menu1
    tabItem(tabName = "menu1", 
            fluidRow(uiOutput("dataBox"), uiOutput("infoBox")),
            fluidRow(uiOutput("buttonBox"))),
    
    # Tab 2: menu2
    tabItem(tabName = "menu2", 
            fluidRow(uiOutput("autoBox"), uiOutput("maskBox")),
            fluidRow(uiOutput("sagBox"), uiOutput("corBox"), uiOutput("axiBox"))),
    
    # Tab 3: menu3
    tabItem(tabName = "menu3",
            fluidRow(uiOutput("setBox")),
            fluidRow(uiOutput("runBox"))),
    
    # Tab 4: menu4
    tabItem(tabName = "menu4",
            fluidRow(uiOutput("thresBox"), uiOutput("cftBox")),
            fluidRow(uiOutput("sagResultBox"), uiOutput("corResultBox"), uiOutput("axiResultBox")),
            fluidRow(uiOutput("dlBox")),
            fluidRow(uiOutput("tblBox")),
            fluidRow(uiOutput("interBox"))),
    
    # Tab 5: menu5
    tabItem(tabName = "menu5",
            fluidRow(uiOutput("boundBox")),
            fluidRow(uiOutput("sagResBox"), uiOutput("corResBox"), uiOutput("axiResBox")),
            fluidRow(uiOutput("dlBox2")),
            fluidRow(uiOutput("tableBox")))
    )
)

# Define UI
ui <- dashboardPage(header, sidebar, body)


## --------------- (2) Define server logic --------------- ##
server <- function(input, output, session) {
  
  # Define reactive values for fileInfo
  fileInfo <- reactiveValues(type = "u", selected = "unknown", df = 0, twosided = TRUE, valid = FALSE, 
                             flip_x = NULL, flip_y = NULL, flip_z = NULL, filename = NULL, header = NULL,
                             conc_thres = NULL, mintdp = NULL, data = NULL, pval = NULL, mask = NULL, 
                             map_grad = NULL, aricluster = NULL, tdpclusters = NULL, tdpchanges = NULL)
  
  # Define reactive values for xyz
  xyz <- reactiveValues(x = NULL, y = NULL, z = NULL, img_tdps = NULL, img_clus = NULL, tblARI = NULL, tblXYZ = NULL)
  
  # Observe event after updating fileInfo
  observeEvent(fileInfo$header$dim, {
    req(fileInfo$header$dim)
    xyz$x <- round((fileInfo$header$dim[2]+1)/2)
    xyz$y <- round((fileInfo$header$dim[3]+1)/2)
    xyz$z <- round((fileInfo$header$dim[4]+1)/2)
  })
  
  # Create menus
  makeMenu1(input, output, session, fileInfo, xyz)
  makeMenu2(input, output, session, fileInfo, xyz)
  makeMenu3(input, output, session, fileInfo, xyz)
  makeMenu4(input, output, session, fileInfo, xyz)
  makeMenu5(input, output, session, fileInfo, xyz)
  
}


## --------------- (3) Run the app --------------- ##
shinyApp(ui = ui, server = server)
# app <- shinyApp(ui, server)
# runApp(app)
