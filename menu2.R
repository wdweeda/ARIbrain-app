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
        sliderInput("slider_x", label = NULL, step = 1, min = 1, max = fileInfo$header$dim[2], value = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x)),
        plotOutput("maskSagittal", click = "click_sag_yz"))
  })
  output$corBox <- renderUI({
    req(xyz$y, fileInfo$header)
    box(width = 4, background = "black",
        sliderInput("slider_y", label = NULL, step = 1, min = 1, max = fileInfo$header$dim[3], value = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y)),
        plotOutput("maskCoronal", click = "click_cor_xz"))
  })
  output$axiBox <- renderUI({
    req(xyz$z, fileInfo$header)
    box(width = 4, background = "black",
        sliderInput("slider_z", label = NULL, step = 1, min = 1, max = fileInfo$header$dim[4], value = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z)),
        plotOutput("maskAxial", click = "click_axi_xy"))
  })
  
  # Update mask plots - sagittal view (x-plane)
  output$maskSagittal <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$mask)
    plotImage(fileInfo$mask, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64, 0, 1), FALSE, zlim = c(0,1), views = c("sag"))
    abline(h = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), v = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), col = "green")
  })
  # Update mask plots - coronal view (y-plane)
  output$maskCoronal <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$mask)
    plotImage(fileInfo$mask, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64, 0, 1), FALSE, zlim = c(0,1), views = c("cor"))
    abline(h = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), v = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), col = "green")
  })
  # Update mask plots - axial view (z-plane)
  output$maskAxial <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$mask)
    plotImage(fileInfo$mask, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64, 0, 1), FALSE, zlim = c(0,1), views = c("axi"))
    abline(h = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), v = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), col = "green")
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
      error = function(e) {
        message("Error reading mask: ", e$message)
        return(NULL)
      }
    )
    if (is.null(mask)) {
      stop("Input mask is NULL. Please upload a valid NIfTI file.")
    }
    
    # Read & check mask header
    hdr <- RNifti::niftiHeader(mask)
    shinyjs::disable("toAnalysisButton")
    if (is.null(hdr))
      stop("Invalid Mask: Header information is NULL.")
    if (hdr$dim[1] != 3)
      stop("Invalid Mask Dimensions: The NIfTI file must be 3D; it has invalid dimensions.")
    if (any(hdr$dim[2:4] != fileInfo$header$dim[2:4]))
      stop("Inconsistent Dimensions: Mismatch between data and mask dimensions.")
    # if (!"srow_x" %in% names(hdr) || !"srow_y" %in% names(hdr) || !"srow_z" %in% names(hdr))
    #   stop("Invalid Mask Header: Missing sform affine matrix components (srow_x, srow_y, srow_z).")
    # if (!all.equal(hdr$srow_x, fileInfo$header$srow_x) || !all.equal(hdr$srow_y, fileInfo$header$srow_y) || !all.equal(hdr$srow_z, fileInfo$header$srow_z))
    #   stop("Inconsistent sform Matrix: Mismatch between data and mask headers. Potential misalignment...")
    if (!identical(RNifti::xform(hdr), RNifti::xform(fileInfo$header)))
      stop("Inconsistent sform Matrix: Mismatch between data and mask headers. Potential misalignment...")
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
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-input$slider_x+1, input$slider_x)
  })
  observeEvent(input$slider_y, {
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-input$slider_y+1, input$slider_y)
  })
  observeEvent(input$slider_z, {
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-input$slider_z+1, input$slider_z)
  })
  
  # Observe event based on clicking image
  observeEvent(input$click_sag_yz, {
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-round(input$click_sag_yz$x)+1, round(input$click_sag_yz$x))
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-round(input$click_sag_yz$y)+1, round(input$click_sag_yz$y))
  })
  observeEvent(input$click_cor_xz, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-round(input$click_cor_xz$x)+1, round(input$click_cor_xz$x))
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-round(input$click_cor_xz$y)+1, round(input$click_cor_xz$y))
  })
  observeEvent(input$click_axi_xy, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-round(input$click_axi_xy$x)+1, round(input$click_axi_xy$x))
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-round(input$click_axi_xy$y)+1, round(input$click_axi_xy$y))
  })

}

makeMenu2 <- function(input, output, session, fileInfo, xyz) {
  observeEvent(input$toMaskButton, {
    observeMenu2(input, output, session, fileInfo, xyz)
  })
}
