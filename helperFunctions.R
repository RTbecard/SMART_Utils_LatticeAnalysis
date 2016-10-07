createLattice <- function(map,cellDiameter,cellType,zone,regionName,cellLattice){
  #################################################
  ####### Create cells over patrol regions ########
  #################################################
  require(raster)
  
  if(!regionName %in% names(map)){
    message('Region name not found.  Entire CA is treated as a single region.  Avaialble columns in attribute table: ',paste(names(map),collapse = ', '))
    regionName <- ''
  }
  
  ## Check if grid/lattice supplied by user
  if(is.null(cellLattice)){ ## Create new grid/lattice
    ## Set projection for map (no projection)
    if(cellType == "Hexagon Lattice"){
      proj <- CRS("+proj=longlat +datum=WGS84")
    }else{
      proj <- CRS(paste0("+proj=utm +zone=",zone,"+datum=WGS84"))
    }
    
    ## Create bounding rectangle for CA (in lat/long)
    map <- spTransform(map,CRSobj = proj)  ## Project to correct units
    ext <- extent(map) ## Extract extents
    ## Add margins to extents
    ext@xmin <- ext@xmin - cellDiameter
    ext@xmax <- ext@xmax + cellDiameter
    ext@ymin <- ext@ymin - cellDiameter
    ext@xmax <- ext@xmax + cellDiameter
    bb <- as(ext, "SpatialPolygons")
    proj4string(bb) <- proj@projargs
    
    ## Create grid/lattice within bounding box
    if(cellType == "Hexagon Lattice"){
      #Create hexagons for whole area
      HexPts <-spsample(bb,type="hexagonal",cellsize = cellDiameter)
      HexPols <- HexPoints2SpatialPolygons(HexPts)
    }else if(cellType == "Square Grid"){
      if(missing(zone)){stop("Must enter UTM 
                           \"zone\" for Square Grid analysis")}
      proj <- CRS(paste0("+proj=utm +zone=",zone,"+datum=WGS84"))
      ## Create square grid for area
      grd <-spsample(bb,type="regular",cellsize = cellDiameter)
      HexPols <- as.SpatialPolygons.GridTopology(points2grid(grd),
                                                 proj4string = proj) 
    }
  }else{  ## Use user defind grid
    HexPols <- cellLattice
  }
  
  ### Create regions for clipping
  if(regionName == ''){ ## Define a single region
    regionName <- 'name'
    map@data$name <- '1'
  }
  ### Dissolve features with similar regionNames
  map <- unionSpatialPolygons(map,IDs = map@data$name)
  map <- SpatialPolygonsDataFrame(map,data = data.frame(name = names(map),row.names = names(map)))
  names(map)[1] <- regionName
  
  ## Chop grid\lattice based on region ID's
  cell.list <- list()
  message("Creating cells for each region...")
  for(i in unique(map@data[,regionName])){
    region <- map[map@data[,regionName] == i,]
    #spplot(region,zcol = "name")
    hex.crop <- gIntersection(HexPols, region, byid=TRUE)
    #plot(hex.crop)
    hpdf <- SpatialPolygonsDataFrame(hex.crop,
                                     data = data.frame(name = factor(rep(i,length(hex.crop))),
                                                       row.names = sapply(slot(hex.crop,
                                                                               "polygons"),
                                                                          function(x) slot(x, "ID"))),
                                     match.ID = T)
    ## Save region cells into list object
    cell.list[[length(cell.list) + 1]] <- hpdf
  }
  ## Merge regions into single object
  message("Merging regions into single object")
  cellLattice <- cell.list[[1]]
  if(length(cell.list) > 1){
    for(i in 2:length(cell.list)){
      cellLattice <- rbind(cellLattice,cell.list[[i]])
    }
  }
  
  ## Calculate area for each cell
  cellLattice@data <- as.data.frame(cbind(cellLattice@data,
                                          area = gArea(cellLattice,byid = T)))
  ## Show size of each cell (proofing)
  #spplot(cellLattice,zcol = "area")
  
  ## Add ID column to celllattice
  cellLattice$fID <- 1:NROW(cellLattice)
  
  ## Rename region attribute to proper name
  names(cellLattice@data)[1] <- regionName
  
  message(NROW(cellLattice), ' squares/cells created.')
  return(list(lattice = cellLattice,region = regionName))
}

patrolVisitsPerCell <- function(patrols, cellLattice,cellType,zone){

  require(raster)
  ## Calc patrol coverage metrics
  patrol.list <- list()
  # loop through each patrol and count cells visited
  message("Counting cell visits by patrols...")
  
  cellVisits <- c()
  prgs <- 1
  patrol.id <- unique.patrols(patrols)
  
  if(any(is.na(patrol.id))){
    stop('NA values detected in ',names(patrols@data)[2],' column.  Cannot identify unique patrols!!!')
  }
  
  for(i in patrol.id){
    setTxtProgressBar(txtProgressBar(min = 0,max = length(patrol.id),style = 3,width = 20),value = prgs)
    ## Subset single patrol
    patrol <- subset.patrols(patrols,i)
    ## Load single patrol info into lattice
    patrol.cover <- over(cellLattice,patrol)
    if(NROW(patrol.cover) > 1){ ## Skip if patrol was outside of lattice
      ## append FID value from lattice
      patrol.cover$fID <- cellLattice$fID
      ## Filter out NA patrols
      patrol.cover.filtered <- patrol.cover[which(!is.na(patrol.cover$fid)),]
      ## Save IDs of cells visited
      cellVisits <- append(cellVisits,patrol.cover.filtered$fID)
    }
    prgs <- prgs + 1
  }
  
  visits.table <- as.data.frame(table(cellVisits))
  names(visits.table) <- c('fID','visits')
  
  temp.data <- base::merge(cellLattice@data,visits.table,by = 'fID',all.x = T)
  if(any(is.na(temp.data$visits))){
    temp.data$visits[which(is.na(temp.data$visits))] <- 0
  }
  cellLattice@data <- temp.data
  
  return(cellLattice)
}

patrolCoverage <- function(cellLattice,regionName){
  
  require(raster)
  cellLattice@data$Percent.coverage <- NA
  ## Convert region names to character values
  cellLattice@data[,regionName] <- as.character(cellLattice@data[,regionName])  
  
  regions <- unique(cellLattice@data[,regionName])
  for(i in regions){
    # Subset region
    idx <- which(cellLattice@data[,regionName] == i)
    region <- cellLattice@data[idx,]
    ## Find total region area
    area.total <- sum(region$area)
    ## Find total region area with at least 1 patrol
    area.patrolled <- sum(subset(region,visits > 0)$area)
    coverage <- area.patrolled*100/area.total
    #Save results to lattice
    cellLattice[idx,"Percent.coverage"] <- coverage
  }
  
  # Calc percent coverage in total area
  area.total <- sum(cellLattice@data$area)
  idx <- which(cellLattice@data$visits > 0)
  area.patrolled <- sum(cellLattice@data$area[idx])
  coverage <- area.patrolled*100/area.total
  # Save results to value
  TotalAreaCovered <- coverage
  
  #spplot(cellLattice,zcol = "Percent.coverage",
  #                         scales = list(draw = T))
 resultsTable <- unique(cellLattice@data[,c(regionName,'Percent.coverage')])
 resultsTable[NROW(resultsTable) + 1,1] <- "All Regions"
 resultsTable[NROW(resultsTable),2] <- coverage
 
 print(resultsTable)
 return(list(cellLattice = cellLattice,resultsTable = resultsTable))
}

distancePatrolled <- function(patrols, cellLattice){
  require(raster)
  ## Make empty results table
  distancePatrolled <- data.frame(fID = vector(mode = 'character'),distance = vector(mode = 'numeric'))
  ## Loop through each patrol (saves memory for analyses with many patrols)
  names(patrols@data)
  patrol.ids <- unique.patrols(patrols)
  prgs <- 1
  
  message('Chopping patrols with the grid/lattice and measuring the resulting distances...')
  for(i in patrol.ids){
    setTxtProgressBar(txtProgressBar(min = 0,max = length(patrol.ids),style = 3,width = 20),value = prgs)
    patrol.sub <- subset.patrols(patrols,i)
      
    # Split lines by patrol lines according to grid/lattice
    lines.split <- gIntersection(cellLattice,
                                 patrol.sub,byid = T,
                                 drop_lower_td = F)
    if(!is.null(lines.split)){ # skip if patrols is outside CA
      ## Get grid cell value for each patrol line
      ids <- over(lines.split,cellLattice)$fID
      ## Append cell/grid id to line segments
      lines.split$fID <- ids
      
      # Calculate line lengths
      lines.split$distance <- sapply(lines.split@lines,
                                     FUN = function(x){
                                       length = LinesLength(x,longlat = T)
                                     })
      
      #spplot(lines.split,zcol = "distance")  Use this to check line distances are correct
      
      # Save line lenghts
      distancePatrolled <- rbind(distancePatrolled,lines.split@data)
      prgs = prgs + 1
    }
  }
  
  message('Summing patrol distance per grid/lattice cell')
  
  ## Aggregate line lengths (sum values for common cells/squares)
  distancePatrolled.aggr <- stats::aggregate(distance ~ fID,data = distancePatrolled,FUN = 'sum')
  
  ## Copy results to lattice/grid
  cellLattice@data <- merge(cellLattice@data, distancePatrolled.aggr[,c('fID','distance')],by = 'fID',all.x = T)
  
  ## Set NA values to 0
  cellLattice@data$distance[is.na(cellLattice@data$distance)] <- 0
  
  return(cellLattice)
}

PatrolCoverage <- function(patrols,map,cellDiameter,cellType,zone,regionName,cellLattice){
  # Create gridded/latticed cells over conservation area
  message('---Create Lattice/Grid---')
  out <- createLattice(map,cellDiameter,cellType,zone,regionName,cellLattice)
  cellLattice <- out$lattice
  regionName <- out$region
  
  message('---Standardizing projections---')
  message('Projection: ', proj4string(cellLattice))
  if(!identicalCRS(patrols,cellLattice)){
    message('Transforming patrols layer to correct projection...')
    patrols <- spTransform(patrols,CRSobj = CRS(proj4string(cellLattice)))
  }
  if(!identicalCRS(map,cellLattice)){
    message('Transforming basemap layer to correct projection...')
    map <- spTransform(map,CRSobj = CRS(proj4string(cellLattice)))
  }
  
  # Calculate patrol visits per cell
  message('---Calculate patrol visits per cell---')
  cellLattice <- patrolVisitsPerCell(patrols, cellLattice, cellType, zone)
  # Calculate percent patrol cover per region
  message('---Calculate Patrol Coverage per region---')
  results <- patrolCoverage(cellLattice,regionName)
  cellLattice <- results$cellLattice
  resultsTable <- results$resultsTable

  # Calculate distance patrolled per cell
  message('---Calculate distance patroled in each cell/square---')
  cellLattice <- distancePatrolled(patrols, cellLattice)
  message('---Analysis Complete---')
  
  return(list(shapeFile = cellLattice,resultsTable = resultsTable))
}

## Load patrol data from a vector of ziplife locations
## This function will extract waypoint info and dates from an archived SMART patrol file.
loadPatrolData <- function(zipFile){
  require(XML)
  
  ## Create empty results table
  results <- data.frame()
 
  for(i in zipFile){
    ## Create empty table for current patrol data
    patrol.points <- data.frame(id = vector(mode = 'character'),
                                date = vector(mode = 'numeric'),
                                lat = vector(mode = 'numeric'),
                                lon = vector(mode = 'numeric'),
                                type = vector(mode = 'character'))
    
    ## Unzip patrol file to temp directory
    tempName <- strsplit(zipFile,'/')[[1]]
    tempName <- tempName[length(tempName)]
    tempName <- paste0(substr(tempName,1,nchar(tempName)-4),'.xml')
    unzip(zipFile,
          files = tempName,
          exdir = 'temp/patrols',overwrite = T)
    
    ## Load xml file
    xml.tree <- xmlTreeParse(paste0("temp/patrols/",tempName))
    xml.root <- xmlRoot(xml.tree)
    xml.attributes <- xmlAttrs(xml.root)
    
    ## Extract waypoints
    patrol.id <- xmlAttrs(xml.root)['id']
  
    patrol.legs <- xmlElementsByTagName(xml.root,'legs')
    ## Loop through all patrol legs
    for(i in patrol.legs){
      
      patrol.days <- xmlElementsByTagName(i,'days')
      ## Loop through patrol days
      for(j in patrol.days){
        ## save patrol date as days since jan.01 1970
        patrol.date <-as.Date(xmlAttrs(j)['date'],"%Y-%m-%d")
        patrol.date.linear <- as.numeric(as.POSIXct(patrol.date))/(60*60*24) 
        
        patrol.waypoints <- xmlElementsByTagName(j,'waypoints')
        ## Loop through waypoints
        for(k in patrol.waypoints){
          patrol.x <- as.numeric(xmlAttrs(k)['x'])
          patrol.y <- as.numeric(xmlAttrs(k)['y'])
          patrol.observations <- xmlElementsByTagName(k,"observations")
          patrol.time <- strptime(xmlAttrs(k)['time'],format = "%H:%M:%S")
          for(l in patrol.observations){
            patrol.obs <- xmlAttrs(l)['categoryKey']
            ## Append waypoint info to results table
            patrol.points <- rbind(patrol.points,
                                   data.frame(id = patrol.id,
                                              date = patrol.date,
                                              date.linear = patrol.date.linear,
                                              time = patrol.time,
                                              lat = patrol.y,
                                              lon = patrol.x,
                                              type = patrol.obs))
          }
        }
      }
    }
    ## Append results to results data.frame
    if(nrow(results) > 0){
      rbind(results,patrol.points)
    }else{
      results <- patrol.points
    }
  }
  return(results)
}

createPatrolTracks <- function(patrol.points){
  head(patrol.points)
  
  ## Create empty list object for holding patrols
  lines.list <- list()

  ## Loop through patrols
  patrols <- unique(patrol.points$id)
  for(i in patrols){
    ## Subset data for a single patrol
    patrols.sub <- subset(patrol.points,id = i)
    ## Sort data by date and time
    patrols.sub.sort <- patrols.sub[order(patrols.sub$date,patrols.sub$time),]
    lines.list[[length(lines.list)+1]] <- Lines(list(Line(patrols.sub.sort[,c("lat","lon")])),ID = i)
  }
  
  # Return spatial lines object
  return(SpatialLines(lines.list,proj <- CRS("+proj=longlat +datum=WGS84")))
}

## Renames shapefiles to sepcified names and loads them into temp directory
renameShapeFiles <- function(newNames,filePaths,tempFolder = "./temp"){
  if(!dir.exists(tempFolder)){
    unlink(tempFolder,recursive = T)
    dir.create(tempFolder)
  }
  
  ## Load basemap data
  for(i in 1:length(newNames)){
    ## Must move shape files into a common folder before loading
    file.rename(filePaths[i],
                paste(tempFolder,newNames[i],sep = "/"))
  }
}

loadShapeFile <- function(shapeFiles,tempFolder = "./temp"){
  ## Input is a vector of paths to shape files (4 of 5)
  ## This function will copy all files to a shared temporary directory,
  ## give them a common name, then load them into memory

  require(rgdal)
  ## Load shape file into memory
  map <- readOGR(dsn=tempFolder,
                 layer=substr(shapeFiles[1],1,
                              nchar(shapeFiles[1])-4),
                 encoding = 'utf8' , # Necessary for reading column names with special characters
                 use_iconv = T)      # Necessary for reading column names with special characters
  return(map)
}

## Extract unique patrol IDs (using column index 2)
unique.patrols <- function(patrols){
  return(
    unique(
      as.character(
        patrols@data[,2])))
}

## Subset a specific patrol ID (omit NA patrols)
subset.patrols <- function(patrols,id){
  return(
    na.omit(patrols[which(as.character(patrols@data[,2]) == id),])
  )
}
