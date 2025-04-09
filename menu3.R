# -------------------- menu3 -------------------- #

observeMenu3 <- function(input, output, session, fileInfo, xyz) {
  
  # Render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Input data",        tabName = "menu1", icon = icon("file-import")),
      menuItem("Brain mask",        tabName = "menu2", icon = icon("sliders-h")),
      menuItem("Analysis settings", tabName = "menu3", icon = icon("cogs"))
    )
  })
  isolate({updateTabItems(session, "tabs", "menu3")})
  
  # Render setting box (UI)
  output$setBox <- renderUI({
    box(title = "Analysis Settings", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%", 
        numericInput("alpha", label = "Significance level", value = 0.05, min = 0, max = 1),
        tags$div(style = "margin-bottom: -1em", selectInput("conn", "Connectivity criterion", choices = c(6, 18, 26), selected = 18, multiple = FALSE)),
        helpText("Face connectivity (6), edge connectivity (18), vertex connectivity (26)."),
        selectInput("simes", "Local test", choices = c("Simes", "Robust variant of Simes"), selected = "Simes", multiple = FALSE),
        # textInput("path", "Please specify the full path to output directory:", value = "/my/path/to/app"),
        tags$div(style = "margin-bottom: -1em", selectInput("twosidedTest", "Statistical test:", choices = c("one-sided", "two-sided"), selected = ifelse(!fileInfo$twosided, "one-sided", "two-sided"), multiple = FALSE)),
        helpText("Please choose either a one-sided or two-sided test for statistical inference.",
                 "NOTE: If the input file is a p-map, the choice should have been made automatically."))
  })
  
  # Render button box (UI)
  output$runBox <- renderUI({
    box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%", 
        tags$div(style = "display: inline-block;", actionButton("runButton", "To run analysis >>", style = "background-color: #3c8dbc; color: #fff; border-color: #717878;")),
        tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
        tags$div(style = "display: inline-block;", helpText("NOTE: The settings above cannot be changed after pressing this button.")))
  })
  
  # # Observe event after changing output path
  # observeEvent(input$path, {
  #   fileInfo$outputDir <- input$path
  # })
  
  # Observe event after pressing button "runButton"
  observeEvent(input$runButton, {
    shinyjs::disable("alpha")
    shinyjs::disable("conn")
    shinyjs::disable("simes")
    shinyjs::disable("twosidedTest")
    shinyjs::disable("runButton")
    
    # Running preparations...
    withProgress(message = NULL, value = 0, {
      
      setProgress(message = "Preparing...", detail = "This may take a few seconds.")
      
      # Check input parameters
      simes <- ifelse(input$simes == "Simes", TRUE, FALSE)
      conn  <- as.integer(input$conn)
      alpha <- as.numeric(input$alpha)
      
      # Compute p-values
      pval <- fileInfo$data
      if (fileInfo$type != "p") {
        pval[fileInfo$mask == 0] <- 1
        if (fileInfo$twosided) {  # two-sided test
          if (fileInfo$type == "t") {
            if (fileInfo$df > 0) {
              pval <- 2*pt(-abs(pval), df = fileInfo$df)
            } else if (fileInfo$df == 0) {
              pval <- 2*pnorm(-abs(pval))
            }
          } else if (fileInfo$type == "z") {
            pval <- 2*pnorm(-abs(pval))
          }
        } else {  # one-sided test
          if (fileInfo$type == "t") {
            if (fileInfo$df > 0) {
              pval <- 1 - pt(pval, df = fileInfo$df)
            } else if (fileInfo$df == 0) {
              pval <- 1 - pnorm(pval)
            }
          } else if (fileInfo$type == "z") {
            pval <- 1 - pnorm(pval)
          }
        }
        pval <- pmax(0, pmin(1, pval))
        pval <- array(pval, dim = fileInfo$header$dim[2:4])
      }
      
      # Compute the whole-brain TDP, i.e. min(TDP)
      hom    <- hommel::hommel(pval[fileInfo$mask!=0], simes = simes)
      mintdp <- hommel::tdp(hom, alpha = alpha)
      
      # Return warning if there are no activation
      if (mintdp == 0) {
        shinyjs::disable("CFTthres")
        shinyjs::disable("dlButton")
        showModal(modalDialog(title = "No Activations:", "No significant brain activations were detected."))
        return(NULL)
      }
      
      # Compute the concentration set threshold
      conc_thres <- hommel::concentration(hom, alpha = alpha)
      rm(hom)
      
      # Create an ARIBrainCluster-class object
      aricluster <- ARIbrain::ARIBrainCluster(Pmap = pval, mask = fileInfo$mask, conn = conn, alpha = alpha)
      
      # Compute TDP gradient map (with a precision of 0.01)
      gammas <- seq(0, 1, 0.01)  # TDP thresholds
      map_grad <- array(0, fileInfo$header$dim[2:4])
      for (g in gammas) {
        # Form clusters
        tdpclusters <- ARIbrain::TDPQuery(aricluster = aricluster, threshold = g)
        # Update gradient map
        map_grad[tdpclusters@aricluster@indexp[unlist(tdpclusters@clusterlist)+1]] <- g
        # Show progress
        incProgress(1/length(gammas), message = "Running analysis...", detail = paste0(round(g*100), "% completed"))
      }
      
      # Update fileInfo with necessary arguments
      if (fileInfo$type != "p") fileInfo$pval <- pval
      fileInfo$conc_thres <- conc_thres
      fileInfo$mintdp     <- mintdp
      fileInfo$aricluster <- aricluster
      fileInfo$map_grad   <- map_grad
    })
  })
  
  # Observe event after changing input parameter "twosidedTest"
  observeEvent(input$twosidedTest, {
    req(input$twosidedTest)
    if (fileInfo$type == "p") {
      if ((!fileInfo$twosided && input$twosidedTest == "two-sided") || (fileInfo$twosided && input$twosidedTest == "one-sided"))
        showModal(modalDialog(title = "Mismatch between input p-map and selected test", paste0("The input p-map is ", ifelse(fileInfo$twosided, "two-sided", "one-sided"), ", but the selected statistical test is ", input$twosidedTest, ". The follow-up analysis will be based on the information from the p-map.")))
    } else {
      fileInfo$twosided <- (input$twosidedTest == "two-sided")
    }
  })
  
}

makeMenu3 <- function(input, output, session, fileInfo, xyz) {
  observeEvent(input$toAnalysisButton, {
    observeMenu3(input, output, session, fileInfo, xyz)
  })
}
