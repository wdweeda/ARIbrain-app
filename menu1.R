# -------------------- menu1 -------------------- #

makeMenu1 <- function(input, output, session, fileInfo, xyz) {
  
  # Render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Input data", tabName = "menu1", icon = icon("file-import"))
    )
  })
  
  # Render data box (UI)
  output$dataBox <- renderUI({
    box(title = "Import Data", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 6, height = "100%", 
        fileInput("dataFile", label = "Load statistic file"), 
        selectInput("dataType", "File type:", choices = "", selected = ""),
        helpText("ARI supports z-maps, t-maps, and p-maps as input data. ARI will attempt to automatically detect the file type. You can also manually select the file type if needed."))
  })
  
  # Render button box (UI)
  output$buttonBox <- renderUI({
    box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
        tags$div(style = "display: inline-block;", actionButton("toMaskButton", "To import mask >>", style = "background-color: #3c8dbc; color: #fff; border-color: #717878;")),
        tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
        tags$div(style = "display: inline-block;", helpText("NOTE: The input data cannot be changed after this button is pressed.")))
  })
  
  # Set input & button boxes to disable based on file input
  observe({
    shinyjs::toggleState("dataType", !is.null(input$dataFile))
  })
  observe({
    shinyjs::toggleState("toMaskButton", !is.null(input$dataFile))
  })
  
  # Observe event after data import
  observeEvent(input$dataFile, {
    req(input$dataFile)  # ensure data file is provided
    
    # Update fileInfo
    checkFileType(input$dataFile, fileInfo)
    
    # Check if a valid file is selected
    if (!(fileInfo$valid)) {
      shinyjs::disable("dataType")
      shinyjs::disable("toMaskButton")
      showModal(modalDialog(title = "Invalid File", "Unsupported file type selected."))
      return(NULL)
    }
    shinyjs::enable("dataType")
    
    # Set selected file type if unknown
    if (fileInfo$selected == "unknown") {
      updateSelectInput(session, "dataType", choices = c("Please choose", "z-map", "t-map", "p-map"), selected = "Please choose")  
    } else {
      updateSelectInput(session, "dataType", choices = c("z-map", "t-map", "p-map"), selected = fileInfo$selected)
    }
  })
  
  # Observe event after selecting data type
  observeEvent(input$dataType, {
    req(input$dataType)  # ensure a valid selection
    
    # Define mapping of data types to their properties
    dataTypeConfig <- list(
      "z-map" = list(type = "z", title = "Z-map", content = "Z-statistics are used in the ARI analysis."),
      "t-map" = list(type = "t", title = "T-map", content = numericInput("tdf", "Please enter the degrees of freedom of t-stats:", value = 0)),
      "p-map" = list(type = "p", title = "P-map", content = selectInput("twosided", "Input p-values:", choices = c("one-sided", "two-sided"), selected = "two-sided")),
      "Please choose" = list(type = "u", title = "Unknown file type", content = "ARI could not determine the file type. Please select the input file type.")
    )
    
    # Get selected configuration
    config <- dataTypeConfig[[input$dataType]]
    
    # Update fileInfo
    fileInfo$selected <- input$dataType
    fileInfo$type <- config$type
    
    # Update UI
    output$infoBox <- renderUI({
      box(solidHeader = TRUE, collapsible = FALSE, title = config$title, width = 6, config$content)
    })
    
    # Enable or disable button "toMaskButton"
    if (fileInfo$type == "u")
      shinyjs::disable("toMaskButton")
    else
      shinyjs::enable("toMaskButton")
  })
  
  # Observe event after changing some input parameters
  observeEvent(input$tdf, {
    fileInfo$df <- input$tdf
  })
  observeEvent(input$twosided, {
    fileInfo$twosided <- (input$twosided == "two-sided")
  })
  
  # Observe event after pressing button "toMaskButton"
  observeEvent(input$toMaskButton, {
    shinyjs::disable("dataFile")
    shinyjs::disable("dataType")
    shinyjs::disable("toMaskButton")
    if (input$dataType == "p-map") shinyjs::disable("twosided")
    if (input$dataType == "t-map") shinyjs::disable("tdf")
  })
  
}
