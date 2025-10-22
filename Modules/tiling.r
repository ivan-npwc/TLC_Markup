#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/tiling.r")
#source("/home/npwc/GIT/TLC_Markup/Modules/tiling.r")

#source("/home/npwc/GIT/TLC_Markup/Modules/reconstract_image_from_tiles.r")
library(EBImage)
library(sp)
library(sf)
library(tools)
library(doMC) 
library(futile.logger)
library(parallel)
###########################
num_physical_cores <- detectCores(logical = FALSE)
num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
batch_size <- num_cores_to_registr 

registerDoMC(cores = num_cores_to_registr)
flog.appender(appender.file("parallel.log"))
##########################
 indir = "/mnt/adata8tb/SSL_DB"
 outdir =  "/mnt/adata8tb/SSL_DB_Tiles"
 control_tmp_pth="control_tmp.csv"
 imgsdtpth = "image_data.csv"
 task_coordination_pth = "/mnt/adata8tb/task_coordination/task_coordination.csv"
#############################################
 RDSpth = "/home/ivan/GIT_HUB/TLC_Markup/image_tiles.rds"
 source("/home/ivan/GIT_HUB/TLC_Markup/Modules/RDStoTable.r")
######################################### 
#RDSpth = "/home/npwc/GIT/TLC_Markup/image_tiles.rds"

#source("/home/npwc/GIT/TLC_Markup/Modules/RDStoTable.r")
#########################################
computer_name <- Sys.info()["nodename"]
RDSdata = readRDS(RDSpth)
imgs_dt_all = read.csv(imgsdtpth)
totallcount=length(list.files(indir, recursive=T))
if (dir.exists(indir)==F) {stop("NO IN DIR FOUND")}
if (dir.exists(outdir)==F) {stop("NO OUT DIR FOUND")}
#################################################
if (file.exists(imgs_dt_all$image_path[1])==F){
print("SSL_DB path changed, please waite to restore the sistem info")
  for (i in 1:length(imgs_dt_all$image_path)){  # for case if SSL_db pth changed
    pth=imgs_dt_all$image_path[i]
    bspth=strsplit(pth,"SSL_DB")
    newpth=paste0(indir,bspth[[1]][2])
    imgs_dt_all$image_path[i]=newpth
  }
  write.csv(imgs_dt_all,imgsdtpth,row.names=F)
  }
#####################################
if (file.exists(control_tmp_pth)==T){control_tmp =read.csv(control_tmp_pth)} else {
#create_control_tmp
tlsdone = data.frame(list.files(outdir,recursive=T,pattern=".JPG"))
tlsdone$img=gsub(".*#", "", basename(tlsdone[,1]))
control_tmp = data.frame(img=unique(tlsdone$img))
  if (length(control_tmp$img)>1){write.csv(control_tmp, control_tmp_pth, row.names=F)} else {
control_tmp = data.frame(img="")}}
################################################################
imgs_dt_all$img=basename(imgs_dt_all$image_path)
imgs_dt=imgs_dt_all[imgs_dt_all$status == "success",]  # filter to exlude bad imgs 
imgs_dt=imgs_dt[!imgs_dt$img %in%   control_tmp$img,]# filter to exlude imgs done
##########################################################
if (file.exists(task_coordination_pth)==F) {

imgs_dt_done = imgs_dt_all[imgs_dt_all$img %in%   control_tmp$img,]
imgs_dt_done$year=substr(imgs_dt_done$img,1,4)
imgs_dt_done=imgs_dt_done[order(imgs_dt_done$site),]
imgs_dt_done$siteyear = paste0(imgs_dt_done$site,"_", imgs_dt_done$year)
task_coordination = data.frame(siteyear=unique(imgs_dt_done$siteyear),computer_name=paste0(computer_name))
write.csv(task_coordination,task_coordination_pth, row.names=F)

}
#########################################################
if (file.exists(task_coordination_pth)) {
task_coordination =read.csv(task_coordination_pth)
imgs_dt$year =substr(imgs_dt$img,1,4)
imgs_dt$siteyear = paste0(imgs_dt$site,"_", imgs_dt$year)
task_exlude = task_coordination$siteyear[!task_coordination$computer_name == computer_name]
imgs_dt = imgs_dt[!imgs_dt$siteyear %in% task_exlude,]  # filter to exlude 
}
#################################################
donepercent = length(control_tmp$img)/totallcount*100
print(donepercent)
###########################################chesk 
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
imgs_dt$day=substr(basename(imgs_dt$image_path),1,8)
##############	
uniqsites =unique(imgs_dt$site)	
uniqcam =unique(imgs_dt$poly)
uniqday =unique(imgs_dt$day)

# Функция для проверки пересечения тайла с маской
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

# Основной цикл обработки
for (sts in 1: length(uniqsites)){ #
  site = uniqsites[sts]
 # print(site)
  site_no_site =gsub("site_","",site)
  site_data = imgs_dt[imgs_dt$site==site,]
  site_days =unique(site_data$day)
  
  for (ds in 1:length(site_days)){
    day1 = site_days[ds]
  #  print(paste0("     ",day1))
    site_day_data = site_data[site_data$day==day1,]
    site_days_cam =unique(site_day_data$poly)
    
    #########################
    year=substr(day1,1,4)
    day=day1
    tilsDir =paste0(outdir,"/",year,"_",site_no_site,"_Tiles"); dir.create(tilsDir,showWarnings = F)
    output_dir =paste0(tilsDir,"/",day);dir.create(output_dir,showWarnings = F)
  #  lsttlspresence=list.files(output_dir); if (length(lsttlspresence) > 5000) {print(paste0("SKIP   ", output_dir)); next  }
    ###############################################################
    
    for (pl in 1:length(site_days_cam)){
	start_site_days_cam = as.numeric(Sys.time())
      poly1 = site_days_cam[pl]
    #  print(paste0("          ",poly1))
      cam = gsub("poly","",poly1)
      cam=paste0(cam,".JPG")
      
      site_days_cam_data = site_day_data[site_day_data$poly == poly1,]
      tmp = basename(site_days_cam_data$image_path)[1]
      daydir = gsub(tmp,"", site_days_cam_data$image_path[1])
      lstimgs = list.files(daydir, full.names=T, pattern = cam)
      
      # Получаем параметры для этого site/poly один раз
      RDSi = RDSdata[[site]][[poly1]]
      tile_sizes = RDSi[[1]]
      mask_x = c(RDSi$Mask$x)
      mask_y = c(RDSi$Mask$y)
      mask_x = c(mask_x, mask_x[1])
      mask_y = c(mask_y, mask_y[1])
      
      mask_polygon <- st_polygon(list(cbind(mask_x, mask_y))) %>%
        st_sfc() %>%
        st_sf()
      
      tile_sizes_rounded <- round(tile_sizes)
      num_batches <- ceiling(length(lstimgs) / batch_size)
      
      for (batch in 1:num_batches) {
        start_idx <- (batch - 1) * batch_size + 1
        end_idx <- min(batch * batch_size, length(lstimgs))
        batch_imgs <- lstimgs[start_idx:end_idx]
         
        if (length(batch_imgs)==0){ break	}	 
		
        result_batch <- mclapply(batch_imgs, function(imgpth) {
          tryCatch({
            # Загружаем изображение
            img <- readImage(imgpth)
            img_dims = dim(img)
            img_width <- img_dims[1]
            img_height <- img_dims[2]
            
            # Создаем кумулятивную сумму для получения Y-координат
            y_starts <- c(1, cumsum(tile_sizes_rounded)[-length(tile_sizes_rounded)] + 1)
            y_ends1 <- cumsum(tile_sizes_rounded)
            
            # Корректируем последний тайл
            y_ends <- pmin(y_ends1, img_height)
            valid_rows <- which(y_starts <= img_height)
            
            saved_tiles <- list()
            
            # Нарезка и сохранение тайлов
            for (rows in 1:length(valid_rows)) {
              tile_size <- tile_sizes_rounded[rows]
              if (tile_size < 120) next
              
              y_start <- y_starts[rows]
              y_end <- y_ends[rows]
              
              if ((y_end - y_start + 1) < tile_size) next
              
              n_tiles_x <- floor(img_width / tile_size)
              
              for (tls in 1:n_tiles_x) {
                x_start <- (tls - 1) * tile_size + 1
                x_end <- x_start + tile_size - 1
                
                if (x_end <= img_width) {
                  if (check_tile_corners(x_start, y_start, tile_size, mask_polygon)) {
                    EBIy_start = img_height - y_start
                    EBIy_end = EBIy_start - tile_size + 1
                    
                    if (EBIy_end > img_height) { EBIy_end = img_height }
                    
                    # Вырезаем квадратный тайл
                    tile <- img[x_start:x_end, EBIy_start:EBIy_end, ]
                    tile = flip(tile)
                    
                    tile_name = paste0("row", rows, "_col", tls,
                                      "_xstart_", x_start, "_xend_", x_end,
                                      "_ystart_", EBIy_start, "_yend_", EBIy_end,
                                      "#", basename(imgpth))
                    
                    output_path <- file.path(output_dir, tile_name)
                    writeImage(tile, output_path, quality = 85)
                    saved_tiles <- c(saved_tiles, tile_name)
                    
                    # Очищаем память после сохранения каждого тайла
                    rm(tile)
                    gc()
                  }
                }
              }
            }
            
            # Очищаем память после обработки изображения
            rm(img)
            gc()
            
            return(saved_tiles)
            
          }, error = function(e) {
            flog.error(paste("Error processing", imgpth, ":", e$message))
            return(NULL)
          })
        }, mc.cores = min(num_cores_to_registr, length(batch_imgs)))  # Ограничиваем количество ядер  
        
        # Сохраняем результаты батча
		resbtc=unlist(result_batch)
	
		resbtc=data.frame(resbtc)
		tlscount=length(resbtc[,1])
		resbtc=data.frame(basename(resbtc[,1]))
		imgdn =gsub(".*#", "", resbtc[,1])
		dn=data.frame(img=unique(imgdn))
		tilsperimgs = tlscount/length(dn$img)
		
		
        control_tmp <- rbind(control_tmp, dn)

		
		done = length(control_tmp$img)/totallcount*100
		print(paste0(done,"       ", site, "      ", day, "      " , poly1))
		
        write.csv(control_tmp, control_tmp_pth,row.names=F)
        
        # Очищаем память после батча
        rm(result_batch)
        gc()
      }
	  
	stop_site_days_cam = as.numeric(Sys.time())
	processing_times <- stop_site_days_cam - start_site_days_cam
	countimgs=length(lstimgs)
	speed = processing_times/countimgs
	print(paste0("SPEED     " , speed, "  SECONDS per 1 IMG",  "Tiles per 1 img    ",tilsperimgs))
	img_future= totallcount - length(control_tmp$img)
	timetofinish = speed*img_future/60/60 # hours
	 print(paste0("Time to finish   ",  timetofinish, " hours"))
	  
	  
    }
	
	
  }
}