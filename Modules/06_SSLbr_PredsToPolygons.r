#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/SSLbr_PredsToPolygons.r")

library(tools)
library(doMC) 
library(EBImage)
library(stringr)
library(sp)
library(terra)
library(raster)
library(sf)
library(dplyr)

#######################################################
Preddir  =  "/mnt/adata8tb/SSL_DB_Tiles"
ssldir  =  "/mnt/adata8tb/SSL_DB"
#########################################################
deleteResult=F # check rhe doone imgs  and skip them
checkBlob =F
num_physical_cores <- detectCores(logical = FALSE)
num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
batch_size <- num_cores_to_registr 
registerDoMC(cores = num_cores_to_registr)
computer_name <- Sys.info()["nodename"]			
##############################################################################  
empty_sf <- st_sf(
    img = character(0),
    tile = character(0), 
    blob = character(0),
    blb = character(0),
    geometry = st_sfc()
  )
###############################################################################
listsites = list.files(Preddir, full.names=T, pattern="Search256")
for (stsyear in 1:length(listsites)) {
  sitedir = listsites[stsyear]
  bsnm = basename(sitedir)
  site = strsplit(bsnm, "_")[[1]][2]
  year = strsplit(bsnm, "_")[[1]][1]
  daysdir = list.files(sitedir, full.names=T)
  
  for (y in 1:length(daysdir)) {
    predict_dir = daysdir[y]
    day = basename(predict_dir)
    print(predict_dir)
    listImage_glob <- list.files(predict_dir, full.names = T, recursive = T, include.dirs = F, pattern="png|JPG|jpg|jpeg|JPEG")
    listImage_glob <- listImage_glob[!grepl("check", listImage_glob)]
	listImage_glob <- listImage_glob[!grepl("geojson", listImage_glob)]
    listImage_glob <- listImage_glob[!grepl("CHECK", listImage_glob)]	
    ImgDone1 =  list.files(predict_dir, pattern="geojson")
	ImgDone = gsub(".geojson","",ImgDone1)
	
    listImage_dt = data.frame(listImage_glob = listImage_glob)
    listImage_dt$tile = basename(listImage_dt$listImage_glob)
    listImage_dt$img = sub(".*#", "", listImage_dt$tile)
    listImage = unique(listImage_dt$img)
    
	if (deleteResult==T) {
     check  =  list.files(predict_dir, pattern="CHECK", full.names=T); unlink(check) 
	 geojson = list.files(predict_dir, pattern="geojson", full.names=T); unlink(geojson)
}	
	
if (deleteResult==F) {
 #if (length(ImgDone1) != 0){
listImage = listImage[!listImage %in% ImgDone]
if (length(listImage)==0) {print(paste0("SKIP      ",  predict_dir)); next }

}
#}
	
	
    result_batch <- mclapply(listImage, function(imgpth) {
	#imgpth=listImage[1]
     #####################################################################
	 lsttiles = listImage_dt$listImage_glob[listImage_dt$img == imgpth]
      ImgOcFin = NULL
      for (w in 1:length(lsttiles)) {
        pth = lsttiles[w]
        msk = readImage(pth)
        msk = bwlabel(msk)
        nmask4 = fillHull(msk)
        
        if (max(nmask4) != 0) {   
          oc = ocontour(nmask4)
          
          ImgOc = NULL
          for (o in 1:length(oc)) {
            pre = data.frame(
              x = as.numeric(oc[o][[1]][,1]),
              y = as.numeric(oc[o][[1]][,2]), 
              id = paste0(basename(pth), "#", o),
              img = imgpth, 
              tile = pth
            )
            ImgOc = rbind(pre, ImgOc)
          }
          ImgOcFin = rbind(ImgOcFin, ImgOc)
        }
      }
      ################################3
	  if (length(ImgOcFin)== 0) {
	  pth = paste0(imgpth, ".geojson")
      kmlPathSave = file.path(predict_dir, pth)
      write_sf(empty_sf, kmlPathSave)
	  }
	  if (length(ImgOcFin) != 0) {
      # Process coordinates
      coords <- str_match(ImgOcFin$tile, "xstart_(\\d+)_xend_(\\d+)_ystart_(\\d+)_yend_(\\d+)")
      
      refcoords = data.frame(
        xstart = as.numeric(coords[, 2]),
        xend = as.numeric(coords[, 3]),
        ystart = as.numeric(coords[, 4]),
        yend = as.numeric(coords[, 5])
      )
      
      ImgOcFin$lat = refcoords$xstart + ImgOcFin$x 
      ImgOcFin$lon = refcoords$yend + ImgOcFin$y 
      
      coordinates(ImgOcFin) <- ~ lat+lon
      srPolygons = list()
      ListPol = unique(ImgOcFin$id)
      
      for(s in 1:length(ListPol)) {
        srPolygons[[s]] = Polygons(list(Polygon(ImgOcFin[ImgOcFin$id == ListPol[s],])), paste0(ListPol[s]))
        #print(paste0("Processing ", s, "/", length(ListPol), " blobs"))
      }
      
      PRJ = NA
      SpP = SpatialPolygons(srPolygons)
      
      # Create polygon info
      info = data.frame(id = row.names(SpP))
      info1 = strsplit(x = as.character(info$id), split = "#")
      info2 = NULL
      for (i in 1:length(info1)) {
        info2$img[i] = paste0(info1[[i]][2])
        info2$tile[i] = paste0(info1[[i]][1])
        info2$blob[i] = info1[[i]][3]
      }
      
      info3 = data.frame(
        img = paste0(info2$img, ".JPG"),  
        tile = info2$tile,  
        blob = info2$blob
      )
      row.names(info3) = paste0(info2$tile, "#", info2$img, "#", info2$blob)
      info3$blb = paste0(info2$tile, "#", info2$img, "#", info2$blob)
      
      SppBLB = SpatialPolygonsDataFrame(SpP, info3)
      polygons <- st_as_sf(SppBLB)
      
      # Process polygons
      valid_polygons1 <- st_make_valid(polygons)
      valid_polygons1 <- valid_polygons1[!st_is_empty(valid_polygons1), ]
      clean_polygons1 <- st_simplify(valid_polygons1, preserveTopology = TRUE, dTolerance = 0.001)
      
      buffered <- st_buffer(clean_polygons1, dist = 10)
      merged_polygons <- st_union(buffered)
      
      final_result <- merged_polygons %>% 
        st_collection_extract("POLYGON") %>% 
        st_cast("POLYGON")
      
      valid_polygons <- st_make_valid(final_result)
      valid_polygons <- valid_polygons[!st_is_empty(valid_polygons), ]
      clean_polygons <- st_simplify(valid_polygons, preserveTopology = TRUE, dTolerance = 0.001)
      clean_polygons <- clean_polygons %>% st_cast("POLYGON") 
      
      areas <- st_area(clean_polygons)
      ar = as.numeric(areas)
      index = which(ar > 2000) 
      filter_pol = clean_polygons[index]
      
      buff <- st_buffer(filter_pol, dist = 10)
      pth = paste0(imgpth, ".geojson")
      kmlPathSave = file.path(predict_dir, pth)
      write_sf(buff, kmlPathSave)
      
      if (checkBlob == T) {
        mapdir = paste0(year, "_", site, "_Map")
        Origimgpth = file.path(ssldir, mapdir, day, imgpth)
        savename = paste0("CHECK_", imgpth)
        pthsave = file.path(predict_dir, savename)
        
        img = readImage(Origimgpth)
        width = dim(img)[1]
        height = dim(img)[2]
        
        png(pthsave, width = width, height = height, res = 300)
        plot(img)
        plot(st_geometry(buff), add = TRUE, border = "red", lwd = 2, col = rgb(1, 0, 0, 0.3))
        dev.off()
      }
	  }
      ##################################
    })  # End of mclapply
  }  # End of daysdir loop
}  # End of listsites loop