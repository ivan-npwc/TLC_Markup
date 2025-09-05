

  tiling=function(outdir,imgdata,RDSdata){
  library(EBImage)
  library(sp)
  library(sf)


  outdir="E:\\SSL_DB_Tiles"
  # RDSdata=readRDS("C:\\Users\\usato\\SSL_DB\\TLC_markUp\\image_tiles.rds")
 # imgdata=read.csv( "C:\\Users\\usato\\SSL_DB\\TLC_markUp\\images_data.csv")
 # imgdata=imgdata[imgdata$site=="71"]
   
   site = gsub("site_","",imgdata$site)
   RDSi=RDSdata[[imgdata$site]][[imgdata$poly]]

  tile_sizes = RDSi[[1]]
  img_dims = as.numeric(strsplit(imgdata$dimensions, " ")[[1]])
  img_width <- img_dims[2]
  img_height <- img_dims[1]
  mask_x =  c(RDSi$Mask$x)
  mask_y=  c(RDSi$Mask$y)
  mask_x =c( mask_x , mask_x[1])
  mask_y =c( mask_y , mask_y[1])
  image_path <- imgdata$image_path
  bsnme= basename(image_path)
  year=substr(bsnme,1,4)
  day = substr(bsnme,1,8)
  tilsDir=paste0(outdir,"\\",year,"_",site,"_Tiles");dir.create(tilsDir,showWarnings = F)
  output_dir =paste0(tilsDir,"\\",day);dir.create(output_dir,showWarnings = F)
##############################################################################################
# 4. Функция для проверки пересечения тайла с маской
check_tile_center <- function(x_start, y_start, tile_size, mask_polygon) {
	  center_x <- x_start + tile_size / 2
	  center_y <- y_start + tile_size / 2
	  center_point <- st_point(c(center_x, center_y)) %>%
		st_sfc() %>%
		st_sf()
  
  # Проверяем, находится ли центр тайла внутри полигона
  return(st_intersects(center_point, mask_polygon, sparse = FALSE)[1, 1])
}
###############################################################################################
# Создаем объект полигона для маски используя sf
mask_polygon=NULL
mask_polygon <- st_polygon(list(cbind(mask_x, mask_y))) %>%
  st_sfc() %>%
  st_sf()
  
 img <- readImage(image_path)
 tile_sizes_rounded <- round(tile_sizes)
 
 # Создаем кумулятивную сумму для получения Y-координат начала каждого ряда тайлов
y_starts <- c(1, cumsum(tile_sizes_rounded)[-length(tile_sizes_rounded)] + 1)
y_ends <- cumsum(tile_sizes_rounded)
 
# Корректируем последний тайл, чтобы он не выходил за границы изображения
y_ends <- pmin(y_ends, img_height)
valid_rows <- which(y_starts <= img_height)
print(paste("Валидных рядов (в пределах изображения):", length(valid_rows), "\n"))


# 6. Нарезка и сохранение тайлов
########################################################################
for (i in 1:length(valid_rows)) {
  # Получаем размер тайла для текущего ряда
  tile_size <- tile_sizes_rounded[i]
  
  # Координаты Y для текущего ряда
  y_start <- y_starts[i]
  y_end <- y_ends[i]
  
  # Пропускаем если высота ряда меньше размера тайла
  if ((y_end - y_start + 1) < tile_size) next
  
  # Рассчитываем количество тайлов в текущем ряду по горизонтали
  n_tiles_x <- floor(img_width / tile_size)
    
    print(paste("started row  ", i))
    print(paste("number tiles in the row", n_tiles_x))
  for (y in 1:n_tiles_x) {
    # Координаты X для текущего тайла
    x_start <- (y - 1) * tile_size + 1
    x_end <- x_start + tile_size - 1
    
    # Пропускаем если тайл выходит за границы по X
    if (x_end < img_width) {
    
    # Проверяем пересечение с маской (используем быстрый метод по центру)
    if (check_tile_center(x_start, y_start, tile_size, mask_polygon)==T) {
  
  
       img_width
       img_height
  
    
		EBIy_start = img_height-y_start
		EBIy_end = EBIy_start- tile_size + 1
		
		 if (EBIy_end>img_height){EBIy_end=img_height}
		 
        # Вырезаем квадратный тайл
        tile <- img[x_start:x_end, EBIy_start:EBIy_end, ]
		#tile=flop(tile)
		tile=flip(tile)
        
  
							
			tile_name=paste0("row",i,
			                "_col", y,
							"_xstart_", x_start,
							"_xend_", x_end,
							"_ystart_", EBIy_start,
							"_yend_",EBIy_end,
							"#",basename(image_path))
			
							
        
        output_path <- file.path(output_dir, tile_name)
        # Сохраняем тайл
        writeImage(tile, output_path, quality = 85)

    
    }
    }
    }
    }
    }
##############################################################
    

