createLattice <- function(map,cellDiameter,cellType,zone){
  #################################################
  ####### Create cells over patrol regions ########
  #################################################
  require(raster)
  
  ## Set projection for map
  proj <- CRS("+proj=longlat +datum=WGS84")
  
  if(cellType == "Hexagon Lattice"){
    #Create hexagons for whole area
    HexPts <-spsample(map,type="hexagonal",cellsize = cellDiameter)
    HexPols <- HexPoints2SpatialPolygons(HexPts)
  }else if(cellType == "Square Grid"){
    if(missing(zone)){stop("Must enter UTM 
                           \"zone\" for Square Grid analysis")}
    proj <- CRS(paste0("+proj=utm +zone=",zone,"+datum=WGS84"))
    map <- spTransform(map,proj)
    ## Create square grid for area
    grd <-spsample(map,type="regular",cellsize = cellDiameter)
    HexPols <- as.SpatialPolygons.GridTopology(points2grid(grd),
                                               proj4string = proj) 
  }
  cell.list <- list()
  # loop through each named region of map
  message("Creating cells for each region...")
  for(i in unique(map@data$name)){
    region <- subset(map,name == i)
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
  #spplot(cellLattice)
  
  ## Calculate area for each cell
  cellLattice@data <- as.data.frame(cbind(cellLattice@data,
                                          area = gArea(cellLattice,byid = T)))
  ## Show size of each cell (proofing)
  #spplot(cellLattice,zcol = "area")
  return(cellLattice)
}

patrolVisitsPerCell <- function(patrols, cellLattice,cellType,zone){
  
  ########################################
  ######## Patrol visits per cell ########
  ########################################
  
  require(raster)
  
  ## Transform patrols into matching projection with base map
  proj <- CRS("+proj=longlat +datum=WGS84")
  if(cellType == "Square Grid"){patrols <- spTransform(patrols,proj)}

    ## Calc patrol coverage metrics
  #spplot(patrols,zcol = "Patrol_ID",scales = list(draw = T))
  patrol.list <- list()
  # loop through each named region of map
  message("Creating cells for each patrol...")
  for(i in unique(patrols@data$Patrol_ID)){
    ## Subset single patrol
    patrol <- subset(patrols,Patrol_ID == i)
    #  spplot(patrol,zcol = "Patrol_ID")
    #  spplot(patrols,zcol = "Patrol_ID")
    ## Load single patrol info into lattice data.frame
    patrol.cover <- over(cellLattice,patrol)
    temp <- cellLattice
    temp@data <- as.data.frame(cbind(name = cellLattice@data$name,
                                     Patrol_ID = patrol.cover[,c("Patrol_ID")],
                                     Team = patrol.cover[,c("Team")]),
                               row.names = sapply(slot(cellLattice, "polygons"),
                                                  function(x) slot(x, "ID")))
    patrol.list[[1 + length(patrol.list)]] <- subset(temp,!is.na(Patrol_ID))
  }
  
  #spplot(patrol.list[[1]],zcol = "Patrol_ID",scales = list(draw = T))
  #spplot(patrol.list[[2]],zcol = "Patrol_ID",scales = list(draw = T))
  
  message("Merging all patrol coverages into single object")
  patrol.cover.all <- patrol.list[[1]]
  patrol.cover.merge <- patrol.list[[1]]
  if(length(patrol.list) > 1){
    for(i in 2:length(patrol.list)){
      patrol.cover.all <- rbind(patrol.cover.all,
                                patrol.list[[i]],
                                makeUniqueIDs = TRUE)
      patrol.cover.merge <- gUnion(patrol.cover.all,
                                   patrol.list[[i]])
    }
  }
  
  ## Count number of visits to each cell by all patrols
  centroids <- SpatialPoints(getSpPPolygonsLabptSlots(cellLattice),
                             proj4string = CRS(proj4string(cellLattice)))
  temp <- as.numeric(sapply(over(centroids,patrol.cover.all,
                                 returnList = TRUE),
                            FUN = function(x){nrow(x)}))
  Number.of.visits <- cellLattice
  cellLattice@data <- as.data.frame(cbind(name = cellLattice@data,
                                          Patrol.Visits = temp),
                                    row.names = sapply(slot(cellLattice, "polygons"),
                                                       function(x) slot(x, "ID")))
  #spplot(cellLattice,zcol = "Patrol.Visits",main = "Visits by patrols")
  
  return(cellLattice)
}

patrolCoverage <- function(cellLattice){
  
  require(raster)
  
  cellLattice@data$Percent.coverage <- NA
  for(i in unique(cellLattice@data$name.name)){
    # calc percent coverage per region
    temp <- subset(cellLattice@data,name.name == i)
    area.total <- sum(temp$name.area)
    area.patrolled <- sum(subset(temp,Patrol.Visits > 0)$name.area)
    coverage <- area.patrolled*100/area.total
    #Save results to lattice
    idx <- which(cellLattice$name.name == i)
    cellLattice[idx,"Percent.coverage"] <- coverage
  }
  
  # Calc percent coverage in total area
  area.total <- sum(cellLattice@data$name.area)
  area.patrolled <- sum(subset(temp,Patrol.Visits > 0)$name.area)
  coverage <- area.patrolled*100/area.total
  # Save results to value
  TotalAreaCovered <- coverage
  
  #spplot(cellLattice,zcol = "Percent.coverage",
  #                         scales = list(draw = T))
  return(cellLattice)
}

distancePatrolled <- function(patrols, cellLattice){

  require(raster)
  
  # Split lines by polygons
  lines.split <- gIntersection(cellLattice,
                               patrols,byid = T,
                               drop_lower_td = F)
  #plot(lines.split)
  #plot(patrols)
  
  # Calculate line lengths
  lineLengths <- sapply(lines.split@lines,
                        FUN = function(x){cbind(length = LinesLength(x,
                                                                     longlat = T), ID = x@ID)})
  lines.distances <- SpatialLinesDataFrame(lines.split,
                                           data = data.frame(length =as.numeric(lineLengths[1,]),
                                                             row.names = lineLengths[2,]))
  #spplot(lines.distances,zcol = "length")
  # Sum line lengths for each cell
  lines.distances.cell <- over(cellLattice,lines.distances,returnList = T)
  lines.distances.cell.sum <- sapply(lines.distances.cell,
                                     FUN = function(x){sum(x$length)})
  cellLattice@data <- cbind(cellLattice@data,
                            distance.patrolled = lines.distances.cell.sum)
  #spplot(cellLattice,zcol = "distance.patrolled",
  #             main = "Distance Patrolled per Cell",scales = list(draw = T))
  
  return(cellLattice)
}

PatrolCoverage <- function(patrols,map,cellDiameter,cellType,zone){
  ## Calculate patrol coverage and effort per cell

  # Create gridded/latticed cells over conservation area
  cellLattice <- createLattice(map,cellDiameter,cellType,zone)
  # Calculate patrol visits per cell
  cellLattice <- patrolVisitsPerCell(patrols, cellLattice, cellType, zone)
  # Calculate percent patrol cover per region
  cellLattice <- patrolCoverage(cellLattice)
  # Calculate distance patrolled per cell
  cellLattice <- distancePatrolled(patrols, cellLattice)
  return(list(cellLattice))
}

EncounterRates <- function(patrol.points,patrol.tracks,map,cellDiameter,cellType,zone){
  ## Calculate patrol coverage and effort per cell
  
  # Create gridded/latticed cells over conservation area
  cellLattice <- createLattice(map,cellDiameter,cellType,zone)
  # Calculate patrol visits per cell
  cellLattice <- patrolVisitsPerCell(patrol.tracks, cellLattice, cellType, zone)
  # Calculate percent patrol cover per region
  cellLattice <- patrolCoverage(cellLattice)
  # Calculate distance patrolled per cell
  cellLattice <- distancePatrolled(patrols, cellLattice)
  
  return(list(cellLattice))
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
                              nchar(shapeFiles[1])-4))
  return(map)
}