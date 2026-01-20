#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/SSLbr_CropTavroByPol.r")

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
siteselect=c(145)
#########################################################
deleteResult=T 
num_physical_cores <- detectCores(logical = FALSE)
num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
batch_size <- num_cores_to_registr 
registerDoMC(cores = num_cores_to_registr)
computer_name <- Sys.info()["nodename"]			
###############################################################################
# Функция для добавления отступа к bounding box
add_buffer_to_bbox <- function(bbox, img_width, img_height,buffer_percent = 0.01) {
  x_range <- bbox[3] - bbox[1]
  y_range <- bbox[4] - bbox[2]
  
  x_buffer <- x_range * buffer_percent
  y_buffer <- y_range * buffer_percent
  
  return(c(
    xmin = max(0, bbox[1] - x_buffer),
    ymin = max(0, bbox[2] - y_buffer),
    xmax = min(img_width, bbox[3] + x_buffer),
    ymax = min(img_height, bbox[4] + y_buffer)
  ))
}
# Функция для создания вырезки
create_crop <- function(img, bbox) {
  xmin <- round(bbox[1])
  xmax <- round(bbox[3])
  ymin <- round(bbox[2])
  ymax <- round(bbox[4])
  
  # EBImage использует [y, x, channel] индексацию
  crop <- img[xmin:xmax, ymin:ymax, ]
  return(crop)
}
#################################################################################
listsites = list.files(Preddir, full.names=T, pattern="Search256")
for (stsyear in 1:length(listsites)) {
  sitedir = listsites[stsyear]
  bsnm = basename(sitedir)
  site = strsplit(bsnm, "_")[[1]][2]
  if (site %in% siteselect == F) next
  year = strsplit(bsnm, "_")[[1]][1]
  map=paste0(year,"_",site,"_Map")
  daysdir = list.files(sitedir, full.names=T)
  
  for (y in 1:length(daysdir)) {
    predict_dir = daysdir[y]
    day = basename(predict_dir)
    print(predict_dir)
    geojsonList =  list.files(predict_dir, pattern="geojson", full.names=T)
    if (length(geojsonList)==0){next}
	
	if (deleteResult==T){
    ExistsCrop = list.files(predict_dir,pattern="CROP")
	unlink(ExistsCrop)
  #  Exists <- unique(str_match(ExistsCrop, "#(.+)$")[,2])
   # geojsonDT=data.frame(geojsonList=geojsonList, bsnm= gsub(".geojson","",basename(geojsonList)))
#	geojsonList =  geojsonDT$geojsonList[geojsonDT$bsnm %in% Exists]
#	if (length(geojsonList)==0){next}
	}
    result_batch <- mclapply(geojsonList, function(geojsonpth) {
	#geojsonpth=geojsonList[2]
     #####################################################################
	geojson =read_sf(geojsonpth)
	if (nrow(geojson) !=0) {
	imgname=gsub(".geojson","", basename(geojsonpth))
	Origimgpth = file.path(ssldir,map,day,imgname)
	orig_img <- readImage(Origimgpth)
	img_height <- dim(orig_img)[1]
    img_width <- dim(orig_img)[2]
	
	for (k in 1:nrow(geojson)) {
         bbox <- round(st_bbox(geojson[k, ]))
        bbox_with_buffer <- add_buffer_to_bbox(bbox,img_width,img_height)
         crop_img <- create_crop(orig_img, bbox_with_buffer)
		 
    	   xmin   = round(as.numeric(bbox[1]))
		   ymin  =  round(as.numeric(bbox[2]))
		   xmax  =  round(as.numeric(bbox[3]))
		   ymax =  round(as.numeric(bbox[4]))
		  
		 
          crop_filename <- paste0("CROP_xmin_", xmin,
		                                                 "_ymin_", ymin,
														 "_xmax_", xmax,
														 "_ymax_", ymax,
														 "#",
		                                                imgname)
		  
          crop_path <- file.path(predict_dir, crop_filename)
		  
         writeImage(crop_img, crop_path)
}
}
      ##################################
    })  # End of mclapply
  }  # End of daysdir loop
}  # End of listsites loop