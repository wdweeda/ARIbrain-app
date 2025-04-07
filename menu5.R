# -------------------- menu5 -------------------- #

observeMenu5 <- function(input, output, session, fileInfo, xyz) {
  
  # Render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Input data",          tabName = "menu1", icon = icon("file-import")),
      menuItem("Brain mask",          tabName = "menu2", icon = icon("sliders-h")),
      menuItem("Analysis settings",   tabName = "menu3", icon = icon("cogs")),
      menuItem("Initial results",     tabName = "menu4", icon = icon("brain")),
      menuItem("Interactive results", tabName = "menu5", icon = icon("list-alt"))
    )
  })
  isolate({updateTabItems(session, "tabs", "menu5")})
  
  # Render interactive results box (UI)
  output$boundBox <- renderUI({
    box(title = "Interactive Results", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
        tags$div(style = "margin-bottom: 1em", "Please click the image or select a row in the cluster table for choosing a cluster. To adjust the cluster size, press the \"+\" buttons to increase it, or the \"-\" buttons to decrease it. Increasing the cluster size reduces the TDP, while decreasing it increases the TDP."))
  })
  
  # Render download button box (UI)
  output$dlBox2 <- renderUI({
    req(xyz$img_clus)
    box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
        tags$div(style = "display: inline-block;", downloadButton("dlButton2", "Download")), tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
        tags$div(style = "display: inline-block;", helpText("You can press the button to download the current cluster image.")))
  })
  # Download cluster image
  output$dlButton2 <- downloadHandler(
    filename = "clusimg_adj.nii",
    # function() paste0(fileInfo$outputDir, .Platform$file.sep, "clusimg.nii"),
    content = function(file) {
      RNifti::writeNifti(xyz$img_clus, file, template = fileInfo$data)
    }
  )
  
  # Render sagittal, coronal & axial result boxes (UI)
  output$sagResBox <- renderUI({
    req(xyz$x, fileInfo$header)
    box(width = 4, height = "100%", background = "black",
        sliderInput("xslider", label = NULL, step = 1, value = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), min = 1, max = fileInfo$header$dim[2]),
        plotOutput("imageSag", click = "sag_click"))
  })
  output$corResBox <- renderUI({
    req(xyz$y, fileInfo$header)
    box(width = 4, height = "100%", background = "black",
        sliderInput("yslider", label = NULL, step = 1, value = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), min = 1, max = fileInfo$header$dim[3]),
        plotOutput("imageCor", click = "cor_click"))
  })
  output$axiResBox <- renderUI({
    req(xyz$z, fileInfo$header)
    box(width = 4, height = "100%", background = "black",
        sliderInput("zslider", label = NULL, step = 1, value = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), min = 1, max = fileInfo$header$dim[4]),
        plotOutput("imageAxi", click = "axi_click"))
  })
  
  # Render cluster images - sagittal view (x-plane)
  output$imageSag <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$map_grad, fileInfo$header)
    plotImage(fileInfo$map_grad, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64,0,1), FALSE, zlim = c(0,1), views = c("sag"))
    if (!is.null(xyz$img_clus)) plotImage(xyz$img_clus, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), views = c("sag"))
    abline(h = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), v = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), col = "green")
  })
  # Render cluster images - coronal view (y-plane)
  output$imageCor <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$map_grad, fileInfo$header)
    plotImage(fileInfo$map_grad, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64,0,1), FALSE, zlim = c(0,1), views = c("cor"))
    if (!is.null(xyz$img_clus)) plotImage(xyz$img_clus, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), views = c("cor"))
    abline(h = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), v = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), col = "green")
  })
  # Render cluster images - axial view (z-plane)
  output$imageAxi <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$map_grad, fileInfo$header)
    plotImage(fileInfo$map_grad, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64,0,1), FALSE, zlim = c(0,1), views = c("axi"))
    if (!is.null(xyz$img_clus)) plotImage(xyz$img_clus, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), views = c("axi"))
    abline(h = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), v = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), col = "green")
  })
  
  # Render result table box (UI)
  output$tableBox <- renderUI({
    req(xyz$tblARI)
    box(title = "Cluster Table", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, height = "100%",
        # Consistent font size, height, and background color styling
        tags$style(HTML(".btn-custom { font-family: Arial, sans-serif; font-size: 13px; font-weight: bold; padding: 7px 10px; height: 35px; background-color: #3c8dbc; color: white; }
                         .btn-action { font-family: Arial, sans-serif; font-size: 13px; font-weight: bold; padding: 7px 10px; height: 35px; background-color: #3c8dbc; color: white; }
                         .btn-lg { height: 35px; font-size: 13px; padding: 7px 10px; }
                         .btn-sm { height: 35px; font-size: 13px; padding: 7px 10px; }")),
        # Table output
        tags$div(style = "margin-bottom: 1em", DT::dataTableOutput("resTable")),
        # Action buttons with consistent styling and blue background matching dashboard blue
        tags$div(style = "display:inline-block", width = 2, actionButton("clearRows", "Clear Selection", icon = icon("eraser"), class = "btn-action btn-primary btn-sm")),
        tags$div(style = "display:inline-block", width = 2, actionButton("sizePP", "Big Increase", icon = icon("plus-circle"), class = "btn-action btn-primary btn-lg")),
        tags$div(style = "display:inline-block", width = 2, actionButton("sizePlus", "Increase Size", icon = icon("plus"), class = "btn-action btn-primary btn-sm")),
        tags$div(style = "display:inline-block", width = 2, actionButton("sizeMinus", "Decrease Size", icon = icon("minus"), class = "btn-action btn-primary btn-sm")),
        tags$div(style = "display:inline-block", width = 2, actionButton("sizeMM", "Big Decrease", icon = icon("minus-circle"), class = "btn-action btn-primary btn-lg")),
        tags$div(style = "display:inline-block", width = 2, actionButton("lastStep", "Go Back", icon = icon("undo"), class = "btn-action btn-primary btn-sm")),
        tags$div(style = "display:inline-block", width = 2, actionButton("redoAnal", "Redo Analysis", icon = icon("redo"), class = "btn-action btn-primary btn-sm"))
    )
  })
  
  # Render ARI result table
  output$resTable <- DT::renderDataTable({
    req(xyz$tblARI)
    DT::datatable(xyz$tblARI, options = list(columnDefs = list(list(className = "dt-center", targets = 1:7)), pageLength = 5, lengthMenu = c(5, 10, 15, 20)), escape = FALSE, selection = "single", editable = "cell") %>% DT::formatRound(columns = c(4,5), digits = 3)
  })
  # Create a proxy object for "resTable"
  DTproxy <- DT::dataTableProxy("resTable", session = session)
  
  # Observe event based on slider changes
  observeEvent(input$xslider, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-input$xslider+1, input$xslider)
  })
  observeEvent(input$yslider, {
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-input$yslider+1, input$yslider)
  })
  observeEvent(input$zslider, {
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-input$zslider+1, input$zslider)
  })
  
  # Observe event based on clicking image
  observeEvent(input$sag_click, {
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-round(input$sag_click$x)+1, round(input$sag_click$x))
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-round(input$sag_click$y)+1, round(input$sag_click$y))
  })
  observeEvent(input$cor_click, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-round(input$cor_click$x)+1, round(input$cor_click$x))
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-round(input$cor_click$y)+1, round(input$cor_click$y))
  })
  observeEvent(input$axi_click, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-round(input$axi_click$x)+1, round(input$axi_click$x))
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-round(input$axi_click$y)+1, round(input$axi_click$y))
  })
  
  # Observe event based on the changes of xyz
  observeEvent(c(xyz$x, xyz$y, xyz$z), {
    req(xyz$x, xyz$y, xyz$z, xyz$tblARI, xyz$img_clus)
    if (!is.na(xyz$img_clus[xyz$x, xyz$y, xyz$z])) {
      n <- ifelse(is.null(dim(xyz$tblARI)), 1, dim(xyz$tblARI)[1])
      # Check if a cluster is selected or if it's different from the current selection
      if (is.null(input$resTable_rows_selected) ||
          n - xyz$img_clus[xyz$x, xyz$y, xyz$z] + 1 != input$resTable_rows_selected)
        DT::selectRows(DTproxy, selected = n - xyz$img_clus[xyz$x, xyz$y, xyz$z] + 1)
    } else {
      DT::selectRows(DTproxy, selected = NULL)
    }
  })
  
  # Observe event based on selecting a row (cluster) in table
  observeEvent(input$resTable_rows_selected, {
    req(input$resTable_rows_selected, xyz$x, xyz$y, xyz$z, xyz$img_clus, xyz$tblARI, xyz$tblXYZ)
    n <- ifelse(is.null(dim(xyz$tblARI)), 1, dim(xyz$tblARI)[1])
    # Check if a cluster is selected or if it's different from the current selection
    if (is.na(xyz$img_clus[xyz$x, xyz$y, xyz$z]) || 
        n - xyz$img_clus[xyz$x, xyz$y, xyz$z] + 1 != input$resTable_rows_selected) {
      # Update xyz based on selection
      xyz$x <- xyz$tblXYZ[input$resTable_rows_selected, 1]
      xyz$y <- xyz$tblXYZ[input$resTable_rows_selected, 2]
      xyz$z <- xyz$tblXYZ[input$resTable_rows_selected, 3]
      shinyjs::enable("sizePlus")
      shinyjs::enable("sizePP")
      shinyjs::enable("sizeMinus")
      shinyjs::enable("sizeMM")
    }
  })
  
  # Observe event after pressing button "clearRows"
  observeEvent(input$clearRows, {
    DT::selectRows(DTproxy, selected = NULL)
    xyz$x <- round((fileInfo$header$dim[2]+1)/2)
    xyz$y <- round((fileInfo$header$dim[3]+1)/2)
    xyz$z <- round((fileInfo$header$dim[4]+1)/2)
  })
  
  # Observe event after pressing button "redoAnal"
  observeEvent(input$redoAnal, {
    DT::selectRows(DTproxy, selected = NULL)
    xyz$x <- round((fileInfo$header$dim[2]+1)/2)
    xyz$y <- round((fileInfo$header$dim[3]+1)/2)
    xyz$z <- round((fileInfo$header$dim[4]+1)/2)
    shinyjs::enable("sizePlus")
    shinyjs::enable("sizePP")
    shinyjs::enable("sizeMinus")
    shinyjs::enable("sizeMM")
    
    # Form clusters
    tdpclusters <- ARIbrain::TDPQuery(fileInfo$aricluster, threshold = input$tdpthres)
    # Convert result clusters to result table
    res <- ari2tbl(tdpclusters@clusterlist, fileInfo)
    n <- length(tdpclusters@clusterlist)
    # Update cluster & TDP maps
    img_clus <- array(NA, fileInfo$header$dim[2:4])
    img_tdps <- array(NA, fileInfo$header$dim[2:4])
    for (i in 1:n) {
      indices <- tdpclusters@aricluster@indexp[tdpclusters@clusterlist[[i]]+1]
      img_clus[indices] <- n-i+1
      img_tdps[indices] <- res$tblARI[i,4]
    }
    # Update xyz
    xyz$img_clus <- img_clus
    xyz$img_tdps <- img_tdps
    xyz$tblARI   <- res$tblARI
    xyz$tblXYZ   <- res$tblXYZ
    # Update fileInfo
    fileInfo$tdpclusters <- tdpclusters
    fileInfo$tdpchanges  <- NULL
  })
  
  # Observe event after pressing button "lastStep"
  observeEvent(input$lastStep, {
    if (is.null(fileInfo$tdpclusters)) {
      showModal(modalDialog(title = "Invalid request:", "You are only allowed to go back one step."))
      return(NULL)
    }
    if (is.null(fileInfo$tdpchanges)) {
      showModal(modalDialog(title = "Invalid request:", "There is no previous step to go back to."))
      return(NULL)
    }
    shinyjs::enable("sizePlus")
    shinyjs::enable("sizePP")
    shinyjs::enable("sizeMinus")
    shinyjs::enable("sizeMM")
    
    # Get results from last step
    clusterlist <- fileInfo$tdpclusters@clusterlist
    # Compute result table
    res <- ari2tbl(clusterlist, fileInfo)
    # Compute img_clus & img_tdps
    n <- length(clusterlist)
    img_clus <- array(NA, fileInfo$header$dim[2:4])
    img_tdps <- array(NA, fileInfo$header$dim[2:4])
    for (i in 1:n) {
      indices <- fileInfo$aricluster@indexp[clusterlist[[i]]+1]
      img_clus[indices] <- n-i+1
      img_tdps[indices] <- res$tblARI[i,4]
    }
    if (!is.null(xyz$x) && !is.null(xyz$y) && !is.null(xyz$z) && !is.na(img_clus[xyz$x, xyz$y, xyz$z])) {
      DT::selectRows(DTproxy, selected = n - img_clus[xyz$x, xyz$y, xyz$z] + 1)
    } else {
      DT::selectRows(DTproxy, selected = NULL)
    }
    
    # Update reactive values
    xyz$img_clus <- img_clus
    xyz$img_tdps <- img_tdps
    xyz$tblARI   <- res$tblARI
    xyz$tblXYZ   <- res$tblXYZ
    # Update fileInfo
    fileInfo$tdpchanges  <- fileInfo$tdpclusters
    fileInfo$tdpclusters <- NULL
  })
  
  # ----------------------------------------------------------------------------------- #
  # ---------- NOTE: Each change corresponds to a TDP change of 0.001 / 0.01 ---------- #
  # ----------------------------------------------------------------------------------- #
  
  # Observe event after pressing button "sizePlus" (increase size / decrease TDP)
  observeEvent(input$sizePlus, {
    req(xyz$x, xyz$y, xyz$z, xyz$img_clus)
    sizeInc(xyz, fileInfo, DTproxy, 0.001)
  })
  # Observe event after pressing button "sizePP" (largely increase size / decrease TDP)
  observeEvent(input$sizePP, {
    req(xyz$x, xyz$y, xyz$z, xyz$img_clus)
    sizeInc(xyz, fileInfo, DTproxy, 0.1)
  })
  # Observe event after pressing button "sizeMinus" (decrease size / increase TDP)
  observeEvent(input$sizeMinus, {
    req(xyz$x, xyz$y, xyz$z, xyz$img_clus)
    sizeDec(xyz, fileInfo, DTproxy, 0.001)
  })
  # Observe event after pressing button "sizeMM" (largely decrease size / increase TDP)
  observeEvent(input$sizeMM, {
    req(xyz$x, xyz$y, xyz$z, xyz$img_clus)
    sizeDec(xyz, fileInfo, DTproxy, 0.1)
  })
  
}

makeMenu5 <- function(input, output, session, fileInfo, xyz) {
  observeEvent(input$interButton, {
    observeMenu5(input, output, session, fileInfo, xyz)
  })
}
