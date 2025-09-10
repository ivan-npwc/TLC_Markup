
#source("/home/npwc/GIT/TLC_Markup/Modules/tiling.r")
#source("/home/npwc/GIT/TLC_Markup/Modules/reconstract_image_from_tiles.r")
  library(EBImage)
  library(sp)
  library(sf)
  library(tools)
  library(doMC) 
  library(futile.logger)

  ###########################
   num_cores <- detectCores()  - 10
   registerDoMC(cores = num_cores)
   flog.appender(appender.file("parallel.log"))
  ##########################
  indir= "/home/npwc/NAS_TITAN/SSL_DB"
  outdir =  "/media/npwc/Seagate Portable Drive/SSL_DB_Tiles"
  RDSpth = "/home/npwc/GIT/TLC_Markup/image_tiles.rds"
  imgsdtpth = "/home/npwc/image_data.csv"
  control_tmp_pth="control_tmp.rds"
 ######################################### 
  RDSdata = readRDS(RDSpth)
  imgs_dt=read.csv(imgsdtpth)
  if (dir.exists(indir)==F) {stop("NO IN DIR FOUND")}
  if (dir.exists(outdir)==F) {stop("NO OUT DIR FOUND")}
  if (file.exists(control_tmp_pth)==T){control_tmp=readRDS(control_tmp_pth)} else {control_tmp=list()}
  ##########################
  head(imgs_dt)
  imgs_dt=imgs_dt[imgs_dt$status == "success",]
 ###########################################chesk 
  source("/home/ivan/GIT_HUB/TLC_Markup/Modules/RDStoTable.r")
	 RDStbl =RDStoTable(RDSpth)
	 RDStbl$sitepoly=paste0("site_",RDStbl$site,"#","poly_", RDStbl$poly)
	 imgs_dt$sitepoly=paste0(imgs_dt$site,"#",imgs_dt$poly) 
	 err=imgs_dt[!imgs_dt$sitepoly %in% RDStbl$sitepoly,]
	 length(err$image_path)
     uniqerr=unique(err$sitepoly)
 if(length(uniqerr>0)){
 errDir =paste0(outdir,"/NEDD_MARKUP"); dir.create(errDir)
   for (i in 1:length(uniqerr)){
   cam=uniqerr[i]
   imgsNeedMarkUp =imgs_dt[imgs_dt$sitepoly == cam,]
   index=sample(1 : length(imgsNeedMarkUp$sitepoly))[1:30]
   imgspth =imgsNeedMarkUp[index,]
      for(y in 1:length(imgspth)){
	  row1 = imgspth[y,]
	  site= gsub("site_","",row1$site)
	  day=row1$day
	  year= substr(basename(row1$image_path),1,4)
	  Mapyearfold =paste0(errDir,"/",year,"_",site,"_Map");dir.create(Mapyearfold,showWarnings = F)
	  dayfold= paste0(Mapyearfold,"/",day);dir.create(dayfold,showWarnings = F)
    file.copy(row1$image_path,dayfold)
 }
 }
 paste0("PLEASE  REVIEW AND  MAKE MARCKUP FOR   IMAGES IN FOLDER  " ,errDir  )
 stop(paste0("No markUp found for ",   data.frame(uniqerr)))
 } 
#######################################################################
 if (file.exists(imgs_dt$image_path[1])==F){
    for (i in 1:length(imgs_dt$image_path)){  # for case if SSL_db pth changed
	pth=imgs_dt$image_path[i]
	bspth=strsplit(pth,"SSL_DB")
	newpth=paste0(indir,bspth[[1]][2])
	imgs_dt$image_path[i]=newpth
	}}
###############################################################################################
imgs_dt$day=substr(basename(imgs_dt$image_path),1,8)
##############	
uniqsites =unique(imgs_dt$site)	
uniqcam =unique(imgs_dt$poly)
uniqday =unique(imgs_dt$day)

 for (sts in 1: length(uniqsites)){ #
    site = uniqsites[sts]
	print(site)
	site_no_site =gsub("site_","",site)
	#site_site =paste0("site_",site)
	site_data = imgs_dt[imgs_dt$site==site,]
	site_days =unique(site_data$day)
	  for (ds in 1:length(site_days)){
	  day1 =site_days[ds]
	  print(paste0("     ",day1))
	  site_day_data = site_data[site_data$day==day1,]
	  site_days_cam =unique(site_day_data$poly)
	      for (pl in 1:length(site_days_cam)){
		  
		  poly1 = site_days_cam[pl]
		  print(paste0("          ",poly1))
		  cam = gsub("poly","",poly1)
		  cam=paste0(cam,".JPG")
		  
	#	   if (site  %in%  control_tmp$site &
#		      poly1 %in% control_tmp$cam &
#			  day1 %in% control_tmp$day
#			  ) next

		    site_days_cam_data =site_day_data[site_day_data$poly == poly1,]
			tmp = basename(site_days_cam_data$image_path)[1]
            daydir =gsub(tmp,"", site_days_cam_data$image_path[1])
			lstimgs =list.files(daydir,full.names=T,pattern = cam)			
 ###############################################################
 saved_tiles <- list()
 result_day=mclapply(lstimgs, function(imgpth) {
   library(EBImage)
  library(sp)
  library(sf)
  library(tools)
  library(doMC) 
  library(futile.logger)
#  for (imgs in 1:length(lstimgs)){
  #  imgpth = lstimgs[d]
   ##################################################################
    img <- readImage(imgpth)
    img_dims = dim(img)
    img_width <- img_dims[1]
    img_height <- img_dims[2]
	##########################################################
RDSi = RDSdata[[site]][[poly1]]
			tile_sizes = RDSi[[1]]
            mask_x =  c(RDSi$Mask$x)
            mask_y=  c(RDSi$Mask$y)
            mask_x =c( mask_x , mask_x[1])
            mask_y =c( mask_y , mask_y[1])
		
           mask_polygon=NULL
           mask_polygon <- st_polygon(list(cbind(mask_x, mask_y))) %>%
           st_sfc() %>%
           st_sf()
         tile_sizes_rounded <- round(tile_sizes)
 
 # Создаем кумулятивную сумму для получения Y-координат начала каждого ряда тайлов
y_starts <- c(1, cumsum(tile_sizes_rounded)[-length(tile_sizes_rounded)] + 1)
y_ends1 <- cumsum(tile_sizes_rounded)

	# Корректируем последний тайл, чтобы он не выходил за границы изображения
    y_ends <- pmin(y_ends1, img_height)
	valid_rows <- which(y_starts <= img_height)
	
    bsnme= basename(imgpth)
    year=substr(bsnme,1,4)
    day <- substr(bsnme,1,8)
    tilsDir =paste0(outdir,"/",year,"_",site_no_site,"_Tiles");dir.create(tilsDir,showWarnings = F)
   output_dir =paste0(tilsDir,"/",day);dir.create(output_dir,showWarnings = F)

##########################################################
# 4. Функция для проверки пересечения тайла с маской
check_tile_corners <- function(x_start, y_start, tile_size, mask_polygon) { 
  # Определяем координаты всех четырех углов тайла
  x_coords <- c(
    x_start,              # левый верхний
    x_start + tile_size,  # правый верхний  
    x_start + tile_size,  # правый нижний
    x_start               # левый нижний
  )
  
  y_coords <- c(
    y_start,              # левый верхний
    y_start,              # правый верхний
    y_start + tile_size,  # правый нижний
    y_start + tile_size   # левый нижний
  )
  
  # Создаем точки для всех углов
  corner_points <- st_multipoint(cbind(x_coords, y_coords)) %>%
    st_sfc() %>%
    st_sf()
  
  # Проверяем, находится ли хотя бы один угол внутри полигона
  intersections <- st_intersects(corner_points, mask_polygon, sparse = FALSE)
  return(any(intersections))
}
# 6. Нарезка и сохранение тайлов
########################################################################
saved_tiles <- list()
for (rows in 1:length(valid_rows)) {
  # Получаем размер тайла для текущего ряда
  tile_size <- tile_sizes_rounded[rows]
   # Пропускаем если размера тайла мал
  if (tile_size < 120) next
  
  # Координаты Y для текущего ряда
  y_start <- y_starts[rows]
  y_end <- y_ends[rows]
  
  # Пропускаем если высота ряда меньше размера тайла
  if ((y_end - y_start + 1) < tile_size) next
  
  # Рассчитываем количество тайлов в текущем ряду по горизонтали
  n_tiles_x <- floor(img_width / tile_size)
    
   # print(paste("started row  ", rows))
  #  print(paste("number tiles in the row", n_tiles_x))
  for (tls in 1:n_tiles_x) {
    # Координаты X для текущего тайла
    x_start <- (tls - 1) * tile_size + 1
    x_end <- x_start + tile_size - 1
    
    # Пропускаем если тайл выходит за границы по X
    if (x_end < img_width) {
    
    # Проверяем пересечение с маской
    if (check_tile_corners(x_start, y_start, tile_size, mask_polygon)==T) {
      
   	  img_width
       img_height
		EBIy_start = img_height-y_start
		EBIy_end = EBIy_start- tile_size + 1
		
		 if (EBIy_end>img_height){EBIy_end=img_height}
		 
        # Вырезаем квадратный тайл
        tile <- img[x_start:x_end, EBIy_start:EBIy_end, ]
		tile=flip(tile)
        
  
							
			tile_name=paste0("row",rows,
			                "_col", tls,
							"_xstart_", x_start,
							"_xend_", x_end,
							"_ystart_", EBIy_start,
							"_yend_",EBIy_end,
							"#",basename(imgpth))
			
							
        
        output_path <- file.path(output_dir, tile_name)
        writeImage(tile, output_path, quality = 85)
		saved_tiles <- c(saved_tiles, tile_name)
    }
    }
    }
    }
  return(saved_tiles)
	
#	}
  }, mc.cores = num_cores)
#################################################################  
 }
#  control_site_days_cam = data.frame(site=site,day=day1,cam=poly1)
# control_tmp =rbind(control_site_days_cam,control_tmp)
# write.csv(control_tmp,control_tmp_pth)
 }
 control_tmp=c(control_tmp,result_day)
 }

head(result)
