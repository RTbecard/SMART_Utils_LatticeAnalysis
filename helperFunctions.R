LatticedAnalysis <- function(patrols,map,cellDiameter){
  ## Create hexagon grid over each region
  # Use hexagons for 2 reasons
  # -Lower edge to volume ratio when using hexagons, so edge effects are reduced
  # -Hexagons can cover spherical geometry without distortion
  
  #Create hexagons for whole area
  HexPts <-spsample(map,type="hexagonal",cellsize = cellDiameter)
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  #plot(HexPols)
  
  cell.list <- list()
  # loop through each named region of map
  message('Creating cells for each region...')
  for(i in unique(map@data$name)){
    region <- subset(map,name == i)
    #spplot(region,zcol = 'name')
    hex.crop <- gIntersection(HexPols, region, byid=TRUE)
    #plot(hex.crop)
    hpdf <- SpatialPolygonsDataFrame(hex.crop,
                                     data = data.frame(name = factor(rep(i,length(hex.crop))),
                                                       row.names = sapply(slot(hex.crop, "polygons"), function(x) slot(x, "ID"))),
                                     match.ID = T)
    ## Save region cells into list object
    cell.list[[length(cell.list) + 1]] <- hpdf
  }
  
  ## Merge regions into single object
  message('Merging regions into single object')
  cellLattice <- cell.list[[1]]
  if(length(cell.list) > 1){
    for(i in 2:length(cell.list)){
      cellLattice <- rbind(cellLattice,cell.list[[i]])
    }
  }
  #spplot(cellLattice)
  
  ## Calculate area for each cell
  cellLattice@data <- as.data.frame(cbind(cellLattice@data,area = gArea(cellLattice,byid = T)))
  ## Show size of each cell (proofing)
  #spplot(cellLattice,zcol = 'area')
  
  ########################################
  ######## Patrol visits per cell ########
  ########################################
  
  ## Calc patrol coverage metrics
  #spplot(patrols,zcol = 'Patrol_ID',scales = list(draw = T))
  patrol.list <- list()
  # loop through each named region of map
  message('Creating cells for each patrol...')
  for(i in unique(patrols@data$Patrol_ID)){
    ## Subset single patrol
    patrol <- subset(patrols,Patrol_ID == i)
    #  spplot(patrol,zcol = 'Patrol_ID')
    #  spplot(patrols,zcol = 'Patrol_ID')
    ## Load single patrol info into lattice data.frame
    patrol.cover <- over(cellLattice,patrol)
    temp <- cellLattice
    temp@data <- as.data.frame(cbind(name = cellLattice@data$name,
                                     Patrol_ID = patrol.cover[,c('Patrol_ID')],
                                     Team = patrol.cover[,c('Team')]),
                               row.names = sapply(slot(cellLattice, "polygons"), function(x) slot(x, "ID")))
    patrol.list[[1 + length(patrol.list)]] <- subset(temp,!is.na(Patrol_ID))
  }
  
  #spplot(patrol.list[[1]],zcol = 'Patrol_ID',scales = list(draw = T))
  #spplot(patrol.list[[2]],zcol = 'Patrol_ID',scales = list(draw = T))
  
  message('Merging all patrol coverages into single object')
  patrol.cover.all <- patrol.list[[1]]
  patrol.cover.merge <- patrol.list[[1]]
  if(length(patrol.list) > 1){
    for(i in 2:length(patrol.list)){
      patrol.cover.all <- rbind(patrol.cover.all,patrol.list[[i]],makeUniqueIDs = TRUE)
      patrol.cover.merge <- gUnion(patrol.cover.all,patrol.list[[i]])
    }
  }
  
  ## Count number of visits to each cell by all patrols
  centroids <- SpatialPoints(getSpPPolygonsLabptSlots(cellLattice),proj4string = CRS(proj4string(cellLattice)))
  temp <- as.numeric(sapply(over(centroids,patrol.cover.all, returnList = TRUE), FUN = function(x){nrow(x)}))
  Number.of.visits <- cellLattice
  cellLattice@data <- as.data.frame(cbind(name = cellLattice@data,
                                          Patrol.Visits = temp),
                                    row.names = sapply(slot(cellLattice, "polygons"), function(x) slot(x, "ID")))
  #spplot(cellLattice,zcol = 'Patrol.Visits',main = 'Visits by patrols')
  
  ##################################################
  ######## Percent area covered (by region) ########
  ##################################################
  cellLattice@data$Percent.coverage <- NA
  for(i in unique(cellLattice@data$name.name)){
    # calc percent coverage  
    temp <- subset(cellLattice@data,name.name == i)
    area.total <- sum(temp$name.area)
    area.patrolled <- sum(subset(temp,Patrol.Visits > 0)$name.area)
    coverage <- area.patrolled*100/area.total
    #Save results to lattice
    idx <- which(cellLattice$name.name == i)
    cellLattice[idx,'Percent.coverage'] <- coverage
  }
  #spplot(cellLattice,zcol = 'Percent.coverage',scales = list(draw = T))
  
  #############################################
  ######## Distance patrolled per cell ########
  #############################################
  # Split lines by polygons
  lines.split <- gIntersection(cellLattice,patrols,byid = T,drop_lower_td = F)
  #plot(lines.split)
  #plot(patrols)
  
  # Calculate line lengths
  lineLengths <- sapply(lines.split@lines,FUN = function(x){cbind(length = LinesLength(x,longlat = T), ID = x@ID)})
  lines.distances <- SpatialLinesDataFrame(lines.split,data = data.frame(length =as.numeric(lineLengths[1,]),
                                                                         row.names = lineLengths[2,]))
  #spplot(lines.distances,zcol = 'length')
  # Sum line lengths for each cell
  lines.distances.cell <- over(cellLattice,lines.distances,returnList = T)
  lines.distances.cell.sum <- sapply(lines.distances.cell,FUN = function(x){sum(x$length)})
  cellLattice@data <- cbind(cellLattice@data,distance.patrolled = lines.distances.cell.sum)
  #spplot(cellLattice,zcol = 'distance.patrolled',main = 'Distance Patrolled per Cell',scales = list(draw = T))
  
  return(cellLattice)
}
