# -------------------- menu2 -------------------- #

observeMenu2 <- function(input, output, session, fileInfo, xyz) {
  
  # Render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Input data", tabName = "menu1", icon = icon("file-import")),
      menuItem("Brain mask", tabName = "menu2", icon = icon("sliders-h"))
    )
  })
  isolate({updateTabItems(session, "tabs", "menu2")})

  # Render checkbox "autoMask" (UI)
  output$autoBox <- renderUI({
    box(title = "Import Brain Mask", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 6, height = "100%", 
        checkboxInput("autoMask", label = "Brain mask is auto-determined from the non-NA data. Please uncheck the box if you want to import a custom brain mask.", value = TRUE),
        tags$div(style = "display: inline-block;", actionButton("toAnalysisButton", "To analysis settings >>", style = "background-color: #3c8dbc; color: #fff; border-color: #717878;"), tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
                 tags$div(style = "display: inline-block;", helpText("NOTE: The brain mask cannot be changed after this button is pressed."))))
  })
  
  # Render image boxes (sagittal, coronal & axial views) (UI)
  output$sagBox <- renderUI({
    req(xyz$x, fileInfo$header)
    box(width = 4, background = "black", 
        sliderInput("slider_x", label = NULL, step = 1, min = 1, max = fileInfo$header$dim[2], value = xyz$x),
        plotOutput("maskSagittal", click = "click_sag_yz"))
  })
  output$corBox <- renderUI({
    req(xyz$y, fileInfo$header)
    box(width = 4, background = "black",
        sliderInput("slider_y", label = NULL, step = 1, min = 1, max = fileInfo$header$dim[3], value = xyz$y),
        plotOutput("maskCoronal", click = "click_cor_xz"))
  })
  output$axiBox <- renderUI({
    req(xyz$z, fileInfo$header)
    box(width = 4, background = "black",
        sliderInput("slider_z", label = NULL, step = 1, min = 1, max = fileInfo$header$dim[4], value = xyz$z),
        plotOutput("maskAxial", click = "click_axi_xy"))
  })
  
  # Update mask plots
  output$maskSagittal <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$mask)
    plotImage(fileInfo$mask, fileInfo$header$dim[2:4], xyz$x, xyz$y, xyz$z, gray.colors(64, 0, 1), FALSE, zlim = c(0,1), views = c("sag"))
    abline(h = xyz$z, v = xyz$y, col = "green")
  })
  output$maskCoronal <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$mask)
    plotImage(fileInfo$mask, fileInfo$header$dim[2:4], xyz$x, xyz$y, xyz$z, gray.colors(64, 0, 1), FALSE, zlim = c(0,1), views = c("cor"))
    abline(h = xyz$z, v = xyz$x, col = "green")
  })
  output$maskAxial <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$mask)
    plotImage(fileInfo$mask, fileInfo$header$dim[2:4], xyz$x, xyz$y, xyz$z, gray.colors(64, 0, 1), FALSE, zlim = c(0,1), views = c("axi"))
    abline(h = xyz$y, v = xyz$x, col = "green")
  })

  # Observe event after unchecking checkbox "autoMask"
  observeEvent(input$autoMask, {
    if (input$autoMask == FALSE) {
      shinyjs::enable("maskFile")
      shinyjs::disable("toAnalysisButton")
      output$maskBox <- renderUI({
        box(solidHeader = TRUE, collapsible = FALSE, width = 6, fileInput("maskFile", label = "Load mask file"))
      })
    } else {
      shinyjs::disable("maskFile")
      shinyjs::enable("toAnalysisButton")
      output$maskBox <- NULL
      # Update brain mask
      fileInfo$mask <- !is.na(fileInfo$data)
    }
  })

  # Observe event after loading brain mask
  observeEvent(input$maskFile, {
    
    # Rename mask for easier handling
    file.rename(input$maskFile$datapath, file.path(dirname(input$maskFile$datapath), input$maskFile$name))
    
    # Try reading the mask file
    mask <- tryCatch(
      suppressWarnings(RNifti::readNifti(file.path(dirname(input$maskFile$datapath), input$maskFile$name))),
      error = function(e) {  # return an error message if an error occurs
        message("Error reading mask: ", e$message)
        return(NULL)
      }
    )
    if (is.null(mask)) {
      shinyjs::disable("toAnalysisButton")
      showModal(modalDialog(title = "Invalid File Type", "You selected an invalid mask."))
      return(NULL)
    }
    
    # Read the mask header
    header <- RNifti::niftiHeader(mask)
    if (header$dim[1] != 3) {
      shinyjs::disable("toAnalysisButton")
      showModal(modalDialog(title = "Invalid Dimensions", "The mask file has invalid dimensions."))
      return(NULL)
    } else if (any(header$dim[2:4] != fileInfo$header$dim[2:4])) {
      shinyjs::disable("toAnalysisButton")
      showModal(modalDialog(title = "Inconsistent Dimensions", "The brain mask and the input data have different dimensions."))
      return(NULL)
    }
    shinyjs::enable("toAnalysisButton")
    
    # Update brain mask
    fileInfo$mask <- (!is.na(fileInfo$data)) & (mask!=0)
  })
  
  # Observe event after pressing button "toAnalysisButton"
  observeEvent(input$toAnalysisButton, {
    shinyjs::disable("autoMask")
    shinyjs::disable("toAnalysisButton")
    if (input$autoMask == FALSE) shinyjs::disable("maskFile")
  })
  
  # Observe event based on slider changes
  observeEvent(input$slider_x, {
    xyz$x <- input$slider_x
  })
  observeEvent(input$slider_y, {
    xyz$y <- input$slider_y
  })
  observeEvent(input$slider_z, {
    xyz$z <- input$slider_z
  })
  
  # Observe event based on clicking image
  observeEvent(input$click_sag_yz, {
    xyz$y <- round(input$click_sag_yz$x)
    xyz$z <- round(input$click_sag_yz$y)
  })
  observeEvent(input$click_cor_xz, {
    xyz$x <- round(input$click_cor_xz$x)
    xyz$z <- round(input$click_cor_xz$y)
  })
  observeEvent(input$click_axi_xy, {
    xyz$x <- round(input$click_axi_xy$x)
    xyz$y <- round(input$click_axi_xy$y)
  })

}

makeMenu2 <- function(input, output, session, fileInfo, xyz) {
  observeEvent(input$toMaskButton, {
    observeMenu2(input, output, session, fileInfo, xyz)
  })
}
