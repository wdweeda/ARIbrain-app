# -------------------- menu4 -------------------- #

observeMenu4 <- function(input, output, session, fileInfo, xyz) {
  
  # Render sidebar tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Input data",        tabName = "menu1", icon = icon("file-import")),
      menuItem("Brain mask",        tabName = "menu2", icon = icon("sliders-h")),
      menuItem("Analysis settings", tabName = "menu3", icon = icon("cogs")),
      menuItem("Initial results",   tabName = "menu4", icon = icon("brain"))
    )
  })
  isolate({updateTabItems(session, "tabs", "menu4")})
  
  # Render threshold box (UI)
  output$thresBox <- renderUI({
    box(title = "Initial Results", status = "primary", solidHeader = TRUE, collapsible = FALSE, width = 6, height = "100%",
        tags$div(style = "margin-bottom: 1em", "The grayscale background image below is a TDP gradient map, which shows the maximum true discovery proportion (TDP) each voxel can reach, with a precision of 0.01. You can download the image by pressing the download button. To generate clusters please select either a test statistic or TDP threshold."),
        tags$div(style = "margin-bottom: -1em", selectInput("CFTthres", "Cluster-forming threshold:", choices = c("Please choose", "stats", "TDP"), selected = "Please choose", multiple = FALSE)))
  })
  
  # Render sagittal, coronal & axial result boxes (UI)
  output$sagResultBox <- renderUI({
    req(xyz$x, fileInfo$header)
    box(width = 4, background = "black", 
        sliderInput("x_slider", label = NULL, step = 1, value = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), min = 1, max = fileInfo$header$dim[2]),
        plotOutput("mapSag", click = "click_sag"))
  })
  output$corResultBox <- renderUI({
    req(xyz$y, fileInfo$header)
    box(width = 4, background = "black",
        sliderInput("y_slider", label = NULL, step = 1, value = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), min = 1, max = fileInfo$header$dim[3]),
        plotOutput("mapCor", click = "click_cor"))
  })
  output$axiResultBox <- renderUI({
    req(xyz$z, fileInfo$header)
    box(width = 4, background = "black",
        sliderInput("z_slider", label = NULL, step = 1, value = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), min = 1, max = fileInfo$header$dim[4]),
        plotOutput("mapAxi", click = "click_axi"))
  })
  
  # Render gradient/cluster maps - sagittal view (x-plane)
  output$mapSag <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$map_grad)
    plotImage(fileInfo$map_grad, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64,0,1), FALSE, zlim = c(0,1), views = c("sag"))
    if (!is.null(xyz$img_clus)) plotImage(xyz$img_clus, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), views = c("sag"))
    abline(h = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), v = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), col = "green")
  })
  # Render gradient/cluster maps - coronal view (y-plane)
  output$mapCor <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$map_grad)
    plotImage(fileInfo$map_grad, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64,0,1), FALSE, zlim = c(0,1), views = c("cor"))
    if (!is.null(xyz$img_clus)) plotImage(xyz$img_clus, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), views = c("cor"))
    abline(h = ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-xyz$z+1, xyz$z), v = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), col = "green")
  })
  # Render gradient/cluster maps - axial view (z-plane)
  output$mapAxi <- renderPlot({
    req(xyz$x, xyz$y, xyz$z, fileInfo$header, fileInfo$map_grad)
    plotImage(fileInfo$map_grad, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, gray.colors(64,0,1), FALSE, zlim = c(0,1), views = c("axi"))
    if (!is.null(xyz$img_clus)) plotImage(xyz$img_clus, fileInfo$header, xyz$x, xyz$y, xyz$z, fileInfo$flip_x, fileInfo$flip_y, fileInfo$flip_z, rainbow(max(xyz$img_clus[!is.na(xyz$img_clus)])), TRUE, zlim = c(1, max(xyz$img_clus[!is.na(xyz$img_clus)])), views = c("axi"))
    abline(h = ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-xyz$y+1, xyz$y), v = ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-xyz$x+1, xyz$x), col = "green")
  })
  
  # Download gradient map & cluster image
  output$gradButton <- downloadHandler(
    filename = "gradmap.nii",
    # function() paste0(fileInfo$outputDir, .Platform$file.sep, "gradmap.nii"),
    content = function(file) {
      RNifti::writeNifti(fileInfo$map_grad, file, template = fileInfo$data)
    }
  )
  output$clusButton <- downloadHandler(
    filename = "clusimg.nii",
    content = function(file) {
      RNifti::writeNifti(xyz$img_clus, file, template = fileInfo$data)
    }
  )
  
  # Render ARI result table
  output$resTBL <- DT::renderDataTable({
    req(xyz$tblARI)
    DT::datatable(xyz$tblARI, options = list(columnDefs = list(list(className = "dt-center", targets = 1:7)), pageLength = 5, lengthMenu = c(5, 10, 15, 20)), escape = FALSE, selection = "single", editable = "cell") %>% DT::formatRound(columns = c(4,5), digits = 2)
  })
  # Create a proxy object for "resTBL"
  TBLproxy <- DT::dataTableProxy("resTBL", session = session)
  
  # Observe event after selecting CFT
  observeEvent(input$CFTthres, {
    
    if (input$CFTthres == "Please choose") {
      
      xyz$tblARI <- NULL
      xyz$tblXYZ <- NULL
      xyz$img_clus <- NULL
      xyz$img_tdps <- NULL
      output$cftBox <- NULL
      output$tblBox <- NULL
      output$interBox <- NULL
      
      # Render download button box (UI)
      output$dlBox <- renderUI({
        box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
            tags$div(style = "display: inline-block;", downloadButton("gradButton", "Download")), tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
            tags$div(style = "display: inline-block;", helpText("You can press the button to download the TDP gradient map with a precision of 0.01.")))
      })
      
    } else if (input$CFTthres == "stats") {  # select the test statistic threshold
      
      xyz$tblARI <- NULL
      xyz$tblXYZ <- NULL
      xyz$img_clus <- NULL
      xyz$img_tdps <- NULL
      output$interBox <- NULL
      
      # Compute maximum statistic
      z_max <- ifelse(fileInfo$type == "p", round(-qnorm(min(fileInfo$data[fileInfo$mask != 0 & fileInfo$data > 0])), 2), round(max(fileInfo$data[fileInfo$mask != 0]), 2))
      # Get the concentration set threshold
      z_conc <- round(-qnorm(fileInfo$conc_thres), 2)
      # Render cftBox for selecting "stats"
      output$cftBox <- renderUI({
        box(solidHeader = TRUE, collapsible = FALSE, width = 6,
            sliderInput("zthres", label = "Test statistic threshold", min = min(round(-qnorm(0.001),2), z_conc), max = z_max, step = 0.01, value = z_conc),
            helpText("Applying a test statistic threshold results in conventional supra-threshold clusters. These clusters are overlaid on the TDP gradient map, with different colors representing different clusters."),
            actionButton("ZclusButton", "To form clusters >>", style = "background-color: #3c8dbc; color: #fff; border-color: #717878;"))
      })
      # Render download button box (UI)
      output$dlBox <- renderUI({
        req(xyz$tblARI)
        box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
            tags$div(style = "display: inline-block;", downloadButton("clusButton", "Download")), tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
            tags$div(style = "display: inline-block;", helpText("You can press the button to download the current cluster image.")))
      })
      # Render result table box (UI)
      output$tblBox <- renderUI({
        req(xyz$tblARI)
        box(title = "Cluster Table", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, height = "100%",
            tags$div(style = "margin-bottom: 1em", DT::dataTableOutput("resTBL")))
      })
      
    } else if (input$CFTthres == "TDP") {  # select the TDP threshold
      
      xyz$tblARI <- NULL
      xyz$tblXYZ <- NULL
      xyz$img_clus <- NULL
      xyz$img_tdps <- NULL
      
      # Render cftBox for selecting "TDP"
      output$cftBox <- renderUI({
        box(solidHeader = TRUE, collapsible = FALSE, width = 6,
            sliderInput("tdpthres", label = "TDP threshold", min = round(fileInfo$mintdp, 2), max = 1, step = 0.001, value = 0.7),
            helpText("Applying a TDP threshold is equivalent to using varying test statistic thresholds for cluster generation. Each generated cluster has a TDP that is not below the given threshold. These clusters are overlaid on the TDP gradient map, with different colors representing different clusters."),
            actionButton("TDPclusButton", "To form clusters >>", style = "background-color: #3c8dbc; color: #fff; border-color: #717878;"))
      })
      # Render download button box (UI)
      output$dlBox <- renderUI({
        req(xyz$tblARI)
        box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
            tags$div(style = "display: inline-block;", downloadButton("clusButton", "Download")), tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
            tags$div(style = "display: inline-block;", helpText("You can press the button to download the cluster image.")))
      })
      # Render result table box (UI)
      output$tblBox <- renderUI({
        req(xyz$tblARI)
        box(title = "Cluster Table", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12, height = "100%",
            tags$div(style = "margin-bottom: 1em", DT::dataTableOutput("resTBL")))
      })
      # Render to interactive clustering button box (UI)
      output$interBox <- renderUI({
        req(xyz$tblARI)
        box(solidHeader = TRUE, collapsible = FALSE, width = 12, height = "100%",
            tags$div(style = "display: inline-block;", actionButton("interButton", "To interactive analysis >>", style = "background-color: #3c8dbc; color: #fff; border-color: #717878;")), tags$div(style = "display: inline-block; width: 1em;", HTML("<br>")),
            tags$div(style = "display: inline-block;", helpText("For TDP-based thresholding, press this button to proceed to interactive thresholding, where you can adjust the size of any arbitrary cluster.")))
      })
    }
  })
  
  # Observe event based on selecting a row (cluster) in table
  observeEvent(input$resTBL_rows_selected, {
    req(input$resTBL_rows_selected, xyz$x, xyz$y, xyz$z, xyz$img_clus, xyz$tblARI, xyz$tblXYZ)
    n <- ifelse(is.null(dim(xyz$tblARI)), 1, dim(xyz$tblARI)[1])
    # Check if a cluster is selected or if it's different from the current selection
    if (is.na(xyz$img_clus[xyz$x, xyz$y, xyz$z]) || 
        n - xyz$img_clus[xyz$x, xyz$y, xyz$z] + 1 != input$resTBL_rows_selected) {
      # Update xyz based on selection
      xyz$x <- xyz$tblXYZ[input$resTBL_rows_selected, 1]
      xyz$y <- xyz$tblXYZ[input$resTBL_rows_selected, 2]
      xyz$z <- xyz$tblXYZ[input$resTBL_rows_selected, 3]
    }
  })
  
  # Observe event after pressing button "ZclusButton"
  observeEvent(input$ZclusButton, {
    
    withProgress(message = "Calculation in progress.", value = 1, detail = "This may take a moment...", {
      
      # Create clusters with a test statistic threshold
      img_clus <- tryCatch({
        suppressWarnings({
          if (fileInfo$type == "p") {
            ARIbrain::cluster_threshold(fileInfo$data > 0 & fileInfo$data < pnorm(-input$zthres) & fileInfo$mask != 0)
          } else {
            if (fileInfo$twosided) {
              ARIbrain::cluster_threshold((fileInfo$data > input$zthres | fileInfo$data < -input$zthres) & fileInfo$mask != 0)
            } else {
              ARIbrain::cluster_threshold(fileInfo$data > input$zthres & fileInfo$mask != 0)
            }
          }
        })
      },
      error = function(e) {
        message("Error forming clusters: ", e$message)
        return(NULL)
      })
      # Find cluster labels
      labels_clus <- sort(unique(as.vector(img_clus[img_clus>0])))
      
      # Run ARI analysis
      if (fileInfo$type == "p") {
        tblARI <- ARIbrain::ARI(Pmap = fileInfo$data, clusters = img_clus, mask = fileInfo$mask, Statmap = -qnorm(fileInfo$data), silent = TRUE)
      } else {
        tblARI <- ARIbrain::ARI(Pmap = fileInfo$pval, clusters = img_clus, mask = fileInfo$mask, Statmap = fileInfo$data, silent = TRUE)
      }
      tblARI <- tblARI[labels_clus,]
      
      # Update result table & add MNI coordinates
      if (length(labels_clus) == 1) {  # one cluster only
        Vox_xyzs <- tblARI[5:7]
        tblXYZ <- Vox_xyzs
        # Adjust coordinates for axis flipping
        if (fileInfo$flip_x) Vox_xyzs[1] <- fileInfo$header$dim[2]-Vox_xyzs[1]+1
        if (fileInfo$flip_y) Vox_xyzs[2] <- fileInfo$header$dim[3]-Vox_xyzs[2]+1
        if (fileInfo$flip_z) Vox_xyzs[3] <- fileInfo$header$dim[4]-Vox_xyzs[3]+1
        # Compute MNI coordinates
        MNI_xyzs <- xyz2MNI(tblXYZ, fileInfo$header)
        # Format voxel & MNI coordinates "(x, y, z)"
        xyzV <- paste0("(", as.integer(Vox_xyzs[1]), ", ", as.integer(Vox_xyzs[2]), ", ", as.integer(Vox_xyzs[3]), ")")
        xyzM <- paste0("(", as.integer(MNI_xyzs[1]), ", ", as.integer(MNI_xyzs[2]), ", ", as.integer(MNI_xyzs[3]), ")")
        # Restructure tblARI
        tblARI <- data.frame(size = as.integer(tblARI[1]), falseH = as.integer(tblARI[2]), trueH = as.integer(tblARI[3]), tdps = tblARI[4], maxT = tblARI[8], xyzV = xyzV, xyzM = xyzM)  # fileInfo$data[tblXYZ]
      } else {  # >1 clusters
        Vox_xyzs <- matrix(as.integer(tblARI[,5:7]), length(labels_clus), 3)
        tblXYZ <- Vox_xyzs
        # Adjust coordinates for axis flipping
        if (fileInfo$flip_x) Vox_xyzs[,1] <- fileInfo$header$dim[2]-Vox_xyzs[,1]+1
        if (fileInfo$flip_y) Vox_xyzs[,2] <- fileInfo$header$dim[3]-Vox_xyzs[,2]+1
        if (fileInfo$flip_z) Vox_xyzs[,3] <- fileInfo$header$dim[4]-Vox_xyzs[,3]+1
        # Compute MNI coordinates
        MNI_xyzs <- xyz2MNI(tblXYZ, fileInfo$header)
        # Format voxel & MNI coordinates "(x, y, z)"
        xyzV <- paste0("(", as.integer(Vox_xyzs[,1]), ", ", as.integer(Vox_xyzs[,2]), ", ", as.integer(Vox_xyzs[,3]), ")")
        xyzM <- paste0("(", as.integer(MNI_xyzs[,1]), ", ", as.integer(MNI_xyzs[,2]), ", ", as.integer(MNI_xyzs[,3]), ")")
        # Restructure tblARI
        tblARI <- data.frame(size = as.integer(tblARI[,1]), falseH = as.integer(tblARI[,2]), trueH = as.integer(tblARI[,3]), tdps = tblARI[,4], maxT = tblARI[,8], xyzV = xyzV, xyzM = xyzM)  # fileInfo$data[tblXYZ]
      }
      # Update img_tdps & img_clus
      img_tdps <- array(NA, fileInfo$header$dim[2:4])
      for (i in labels_clus) {
        img_tdps[img_clus == i] <- tblARI[length(labels_clus)-i+1, 4]
      }
      img_clus[img_clus == 0] <- NA
    })
    
    # Add row & column names
    rownames(tblARI) <- paste0("cl", rev(labels_clus))
    colnames(tblARI) <- c("Size", "TDN", "TrueNull", "TDP", "max(Stat)", "VOX coordinates<br/>(x, y, z)", "MNI coordinates<br/>(x, y, z)")
    
    # Update xyz
    xyz$img_clus <- img_clus
    xyz$img_tdps <- img_tdps
    xyz$tblARI   <- tblARI
    xyz$tblXYZ   <- tblXYZ
    # Update fileInfo
    fileInfo$tdpclusters <- NULL
  })
  
  # Observe event after pressing button "TDPclusButton"
  observeEvent(input$TDPclusButton, {
    
    withProgress(message = "Calculation in progress.", value = 0, detail  = "This may take a moment...", {
      
      # Form clusters
      tdpclusters <- ARIbrain::TDPQuery(fileInfo$aricluster, threshold = input$tdpthres)
      # Convert result clusters to result table
      res <- ari2tbl(tdpclusters@clusterlist, fileInfo)
      n <- length(tdpclusters@clusterlist)
      # Return warning if no clusters were found
      if (is.null(res)) {
        showModal(modalDialog(title = "No Clusters Detected", "The current threshold is too high to form clusters. Please try reducing it."))
        return(NULL)
      }
      # Update cluster & TDP maps
      img_clus <- array(NA, fileInfo$header$dim[2:4])
      img_tdps <- array(NA, fileInfo$header$dim[2:4])
      for (i in 1:n) {
        indices <- tdpclusters@aricluster@indexp[tdpclusters@clusterlist[[i]]+1]
        img_clus[indices] <- n-i+1
        img_tdps[indices] <- res$tblARI[i,4]
      }
    })
    
    # Update xyz
    xyz$img_clus <- img_clus
    xyz$img_tdps <- img_tdps
    xyz$tblARI   <- res$tblARI
    xyz$tblXYZ   <- res$tblXYZ
    # Update fileInfo
    fileInfo$tdpclusters <- tdpclusters
  })
  
  # Observe event based on slider changes
  observeEvent(input$x_slider, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-input$x_slider+1, input$x_slider)
  })
  observeEvent(input$y_slider, {
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-input$y_slider+1, input$y_slider)
  })
  observeEvent(input$z_slider, {
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-input$z_slider+1, input$z_slider)
  })
  
  # Observe event based on clicking image
  observeEvent(input$click_sag, {
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-round(input$click_sag$x)+1, round(input$click_sag$x))
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-round(input$click_sag$y)+1, round(input$click_sag$y))
  })
  observeEvent(input$click_cor, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-round(input$click_cor$x)+1, round(input$click_cor$x))
    xyz$z <- ifelse(fileInfo$flip_z, fileInfo$header$dim[4]-round(input$click_cor$y)+1, round(input$click_cor$y))
  })
  observeEvent(input$click_axi, {
    xyz$x <- ifelse(fileInfo$flip_x, fileInfo$header$dim[2]-round(input$click_axi$x)+1, round(input$click_axi$x))
    xyz$y <- ifelse(fileInfo$flip_y, fileInfo$header$dim[3]-round(input$click_axi$y)+1, round(input$click_axi$y))
  })
  
  # Observe event based on the changes of xyz
  observeEvent(c(xyz$x, xyz$y, xyz$z), {
    req(xyz$x, xyz$y, xyz$z, xyz$tblARI, xyz$img_clus)
    if (!is.na(xyz$img_clus[xyz$x, xyz$y, xyz$z])) {
      n <- ifelse(is.null(dim(xyz$tblARI)), 1, dim(xyz$tblARI)[1])
      # Check if a cluster is selected or if it's different from the current selection
      if (is.null(input$resTBL_rows_selected) ||
          n - xyz$img_clus[xyz$x, xyz$y, xyz$z] + 1 != input$resTBL_rows_selected)
        DT::selectRows(TBLproxy, selected = n - xyz$img_clus[xyz$x, xyz$y, xyz$z] + 1)
    } else {
      DT::selectRows(TBLproxy, selected = NULL)
    }
  })
  
}

makeMenu4 <- function(input, output, session, fileInfo, xyz) {
  observeEvent(input$runButton, {
    observeMenu4(input, output, session, fileInfo, xyz)
  })
}
