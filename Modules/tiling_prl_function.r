
#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/tiling_prl_function.r")


results <- mclapply(lstimgs, function(d) {
  #for (imgs in 1:length(lstimgs)){
    imgpth = lstimgs[d]
   ##################################################################
    img <- readImage(imgpth)
    img_dims = dim(img)
    img_width <- img_dims[1]
    img_height <- img_dims[2]
	# Корректируем последний тайл, чтобы он не выходил за границы изображения
    y_ends <- pmin(y_ends1, img_height)
	valid_rows <- which(y_starts <= img_height)
	
    bsnme= basename(imgpth)
    year=substr(bsnme,1,4)
    day <<- substr(bsnme,1,8)
    tilsDir =paste0(outdir,"/",year,"_",site_no_site,"_Tiles");dir.create(tilsDir,showWarnings = F)
   output_dir =paste0(tilsDir,"/",day);dir.create(output_dir,showWarnings = F)
##########################################################
# 6. Нарезка и сохранение тайлов
########################################################################
for (rows in 1:length(valid_rows)) {
  # Получаем размер тайла для текущего ряда
  tile_size <- tile_sizes_rounded[rows]
  
  # Координаты Y для текущего ряда
  y_start <- y_starts[rows]
  y_end <- y_ends[rows]
  
  # Пропускаем если высота ряда меньше размера тайла
  if ((y_end - y_start + 1) < tile_size) next
  
  # Рассчитываем количество тайлов в текущем ряду по горизонтали
  n_tiles_x <- floor(img_width / tile_size)
    
   # print(paste("started row  ", rows))
    print(paste("number tiles in the row", n_tiles_x))
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
		#tile=flop(tile)
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
    }
    }
    }
    }
 
    #return(saved_count)
#	}
  }, mc.cores = num_cores)
