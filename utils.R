# ARIbrain Shiny app utility functions

# ---------- (1) Check files and return status/fields ----------
checkFileType <- function(inFile, fileInfo) {
  
  # Update fileInfo
  fileInfo$filename <- inFile$name
  
  # Rename file for easier handling
  file.rename(inFile$datapath, file.path(dirname(inFile$datapath), inFile$name))
  
  # Try reading the NIfTI file
  data <- tryCatch(
    suppressWarnings(RNifti::readNifti(file.path(dirname(inFile$datapath), inFile$name))),
    error = function(e) {
      message("Error reading NIfTI file: ", e$message)
      return(NULL)
    }
  )
  
  if (!is.null(data)) {
    fileInfo$valid <- TRUE
    fileInfo$data <- data
    fileInfo$mask <- !is.na(data)
    
    # Extract header
    header <- RNifti::niftiHeader(data)
    fileInfo$header <- header
    
    # Determine type based on intent_code
    fileInfo$type <- switch(as.character(header$intent_code), "3" = "t", "5" = "z", "22" = "p", "u")
    
    # If intent_code is unknown, try using descrip field
    if (fileInfo$type == "u" && !is.null(header$descrip)) {
      if (grepl("SPM\\{T", header$descrip)) fileInfo$type <- "t"
    }
    
    # Attempt to extract degrees of freedom (df) from descrip
    df <- tryCatch(
      as.numeric(strsplit(strsplit(header$descrip, "\\[")[[1]], "\\]")[[2]][1]),
      error = function(e) {
        message("Error extracting df from the header: ", e$message)
        return(NA)
      }
    )
    if (!is.na(df)) fileInfo$df <- df
    
    # Set selected type for UI dropdown
    fileInfo$selected <- switch(fileInfo$type, "t" = "t-map", "z" = "z-map", "p" = "p-map", "unknown")
  }
}


# ---------- (2) Define plotting functions ----------
plotImage <- function(data, dims, x, y, z, colrng, overlay, zlim, views = c("sag", "cor", "axi")) {
  par(mar = c(0,0,0,0), oma = c(0,0,0,0))
  if ("sag" %in% views) 
    image(1:dims[2], 1:dims[3], data[x,,], col = colrng, add = overlay, zlim = zlim, axes = FALSE, xlab = "", ylab = "")
  if ("cor" %in% views) 
    image(1:dims[1], 1:dims[3], data[,y,], col = colrng, add = overlay, zlim = zlim, axes = FALSE, xlab = "", ylab = "")
  if ("axi" %in% views) 
    image(1:dims[1], 1:dims[2], data[,,z], col = colrng, add = overlay, zlim = zlim, axes = FALSE, xlab = "", ylab = "")
}


# ---------- (3) Convert voxel location to MNI coordinates ----------
# vox - (nrow x 3) matrix
xyz2MNI <- function(vox, hdr) {
  if (is.null(hdr) || !"srow_x" %in% names(hdr) || !"srow_y" %in% names(hdr) || !"srow_z" %in% names(hdr)) 
    stop("Invalid header object: It must contain srow_x, srow_y, and srow_z fields.")
  transMatrix <- rbind(hdr$srow_x, hdr$srow_y, hdr$srow_z)  # transformation matrix
  if (is.null(dim(vox))) {
    return(transMatrix %*% c(vox-1, 1))
  } else {
    return(t(transMatrix %*% t(cbind(vox-1, rep(1, nrow(vox))))))
  }
}


# ---------- (4) Convert clusterlist to result table ----------
ari2tbl <- function(clusterlist, fileInfo) {
  n <- length(clusterlist)  # number of clusters found
  
  # Sort clusters by descending size
  if (n == 0) return(NULL)  # return NULL if found no clusters
  if (n > 1) {  # >1 clusters
    cluster_sizes <- sapply(clusterlist, length)
    d             <- diff(range(cluster_sizes))
    maxsize       <- max(cluster_sizes)
    if (n < 200 || d < 100000 || n*log2(d) <= sum(cluster_sizes)) {
      clusterlist <- clusterlist[order(cluster_sizes, decreasing = TRUE)]
    } else {
      cluster_ord <- ARIbrain:::counting_sort(n, maxsize, cluster_sizes)
      clusterlist <- clusterlist[cluster_ord + 1]
    }
  }
  
  # Compute cluster statistics & tblARI
  tblARI <- plyr::laply(1:n, function(i) {
    if (fileInfo$type == "p") {
      clus_stat <- -qnorm(fileInfo$data[fileInfo$aricluster@indexp[clusterlist[[i]]+1]])
    } else {
      clus_stat <- fileInfo$data[fileInfo$aricluster@indexp[clusterlist[[i]]+1]]
    }
    id_clus   <- which.max(clus_stat)
    id_max    <- clusterlist[[i]][id_clus]+1
    xyz_max   <- ARIbrain:::ids2xyz(as.integer(fileInfo$aricluster@indexp[id_max]-1), fileInfo$header$dim[2:4])
    
    clus_size <- length(clusterlist[[i]])
    clus_tdp  <- fileInfo$aricluster@tdps[clusterlist[[i]][clus_size]+1]
    unlist(c(Size       = clus_size, 
             FalseNull  = round(clus_size*clus_tdp), 
             TrueNull   = round(clus_size*(1-clus_tdp)), 
             ActiveProp = clus_tdp,
             x_max      = xyz_max[,1],
             y_max      = xyz_max[,2],
             z_max      = xyz_max[,3],
             maxZ       = clus_stat[id_clus]))
  })
  if (is.null(dim(tblARI))) tblARI <- t(as.matrix(tblARI))
  
  # Update result table & add MNI coordinates
  if (n == 1) {  # one cluster only
    Vox_xyzs <- tblARI[5:7]
    xyzV     <- paste0("(", as.integer(Vox_xyzs[1]), ", ", as.integer(Vox_xyzs[2]), ", ", as.integer(Vox_xyzs[3]), ")")
    MNI_xyzs <- xyz2MNI(Vox_xyzs, fileInfo$header)
    xyzM     <- paste0("(", as.integer(MNI_xyzs[1]), ", ", as.integer(MNI_xyzs[2]), ", ", as.integer(MNI_xyzs[3]), ")")
    tblARI   <- data.frame(size = as.integer(tblARI[1]), falseH = as.integer(tblARI[2]), trueH = as.integer(tblARI[3]), tdps = tblARI[4], maxT = tblARI[8], xyzV = xyzV, xyzM = xyzM)
  } else { # >1 clusters
    Vox_xyzs <- matrix(as.integer(tblARI[,5:7]), n, 3)
    xyzV     <- paste0("(", as.integer(Vox_xyzs[,1]), ", ", as.integer(Vox_xyzs[,2]), ", ", as.integer(Vox_xyzs[,3]), ")")
    MNI_xyzs <- xyz2MNI(Vox_xyzs, fileInfo$header)
    xyzM     <- paste0("(", as.integer(MNI_xyzs[,1]), ", ", as.integer(MNI_xyzs[,2]), ", ", as.integer(MNI_xyzs[,3]), ")")
    tblARI   <- data.frame(size = as.integer(tblARI[,1]), falseH = as.integer(tblARI[,2]), trueH = as.integer(tblARI[,3]), tdps = tblARI[,4], maxT = tblARI[,8], xyzV = xyzV, xyzM = xyzM)
  }
  
  # Write row & column names
  rownames(tblARI) <- paste0("cl", n:1)
  colnames(tblARI) <- c("Size", "TDN", "TrueNull", "TDP", "max(Z)", "VOX coordinates<br/>(x, y, z)", "MNI coordinates<br/>(x, y, z)")
  
  return(list(tblARI = tblARI, tblXYZ = Vox_xyzs))
}


# ---------- (5) Increase size function ----------
sizeInc <- function(xyz, fileInfo, DTproxy, tdpchg) {
  
  shinyjs::enable("sizeMinus")
  shinyjs::enable("sizeMM")
  removeModal()
  
  if (is.null(fileInfo$tdpchanges))
    tdpchanges <- fileInfo$tdpclusters
  else
    tdpchanges <- fileInfo$tdpchanges
  
  # Check if a valid cluster has been selected
  if (!is.na(xyz$img_clus[xyz$x, xyz$y, xyz$z])) {
    
    # Adjust chosen cluster based on query
    mintdp <- fileInfo$mintdp
    curtdp <- xyz$img_tdps[xyz$x, xyz$y, xyz$z]
    if (curtdp == mintdp) {
      shinyjs::disable("sizePlus")
      shinyjs::disable("sizePP")
      showModal(modalDialog(title = NULL, "The minimum TDP (or maximum size) has already been reached for the selected cluster!"))
    } else if (curtdp - mintdp < tdpchg) {
      tdpchanges <- ARIbrain::TDPChange(tdpchanges, v = c(xyz$x, xyz$y, xyz$z), tdpchg = mintdp-curtdp)
      shinyjs::disable("sizePlus")
      shinyjs::disable("sizePP")
      showModal(modalDialog(title = NULL, "The minimum TDP (or maximum size) has been reached for the selected cluster!"))
    } else {
      tdpchanges <- ARIbrain::TDPChange(tdpchanges, v = c(xyz$x, xyz$y, xyz$z), tdpchg = -tdpchg)
    }
    
    # Get result table
    res <- ari2tbl(tdpchanges@clusterlist, fileInfo)
    # Update img_clus & img_tdps
    n <- length(tdpchanges@clusterlist)
    img_clus <- array(NA, fileInfo$header$dim[2:4])
    img_tdps <- array(NA, fileInfo$header$dim[2:4])
    for (i in 1:n) {
      indices <- fileInfo$aricluster@indexp[tdpchanges@clusterlist[[i]] + 1]
      img_clus[indices] <- n-i+1
      img_tdps[indices] <- res$tblARI[i, 4]
    }
    DT::selectRows(DTproxy, selected = n - img_clus[xyz$x, xyz$y, xyz$z] + 1)
    
    # Update reactive values
    xyz$img_clus <- img_clus
    xyz$img_tdps <- img_tdps
    xyz$tblARI   <- res$tblARI
    xyz$tblXYZ   <- res$tblXYZ
    # Update fileInfo
    if (!is.null(fileInfo$tdpchanges)) fileInfo$tdpclusters <- fileInfo$tdpchanges
    fileInfo$tdpchanges <- tdpchanges
  } else {
    showModal(modalDialog(title = NULL, "Please choose a valid cluster!"))
    return(NULL)
  }
  
}


# ---------- (6) Decrease size function ----------
sizeDec <- function(xyz, fileInfo, DTproxy, tdpchg) {
  
  shinyjs::enable("sizePlus")
  shinyjs::enable("sizePP")
  removeModal()
  
  if (is.null(fileInfo$tdpchanges))
    tdpchanges <- fileInfo$tdpclusters
  else
    tdpchanges <- fileInfo$tdpchanges
  
  # Check if a valid cluster has been selected
  if (!is.na(xyz$img_clus[xyz$x, xyz$y, xyz$z])) {
    
    # Adjust chosen cluster based on query
    maxtdp <- fileInfo$aricluster@tdps[fileInfo$aricluster@stcs[length(fileInfo$aricluster@stcs)]+1]
    curtdp <- xyz$img_tdps[xyz$x, xyz$y, xyz$z]  
    if (maxtdp == curtdp) {
      shinyjs::disable("sizeMinus")
      shinyjs::disable("sizeMM")
      showModal(modalDialog(title = NULL, "The maximum TDP (or minimum size) has already been reached for the selected cluster!"))
    } else if (maxtdp - curtdp < tdpchg) {
      tdpchanges <- ARIbrain::TDPChange(tdpchanges, v = c(xyz$x, xyz$y, xyz$z), tdpchg = maxtdp-curtdp)
      shinyjs::disable("sizeMinus")
      shinyjs::disable("sizeMM")
      showModal(modalDialog(title = NULL, "The maximum TDP (or minimum size) has been reached for the selected cluster!"))
    } else {
      tdpchanges <- ARIbrain::TDPChange(tdpchanges, v = c(xyz$x, xyz$y, xyz$z), tdpchg = tdpchg)
    }
    
    # Get result table
    res <- ari2tbl(tdpchanges@clusterlist, fileInfo)
    # Update img_clus & img_tdps
    n <- length(tdpchanges@clusterlist)
    img_clus <- array(NA, fileInfo$header$dim[2:4])
    img_tdps <- array(NA, fileInfo$header$dim[2:4])
    for (i in 1:n) {
      indices <- fileInfo$aricluster@indexp[tdpchanges@clusterlist[[i]]+1]
      img_clus[indices] <- n-i+1
      img_tdps[indices] <- res$tblARI[i,4]
    }
    DT::selectRows(DTproxy, selected = n - img_clus[xyz$x, xyz$y, xyz$z] + 1)
    
    # Update reactive values
    xyz$img_clus <- img_clus
    xyz$img_tdps <- img_tdps
    xyz$tblARI   <- res$tblARI
    xyz$tblXYZ   <- res$tblXYZ
    # Update fileInfo
    if (!is.null(fileInfo$tdpchanges)) fileInfo$tdpclusters <- fileInfo$tdpchanges
    fileInfo$tdpchanges <- tdpchanges
  } else {
    showModal(modalDialog(title = NULL, "Please choose a valid cluster!" ))
  }
  
}
