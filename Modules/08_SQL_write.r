#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/DATA crate_sql.r")
#source("/home/npwc/GIT/TLC_Markup/Modules/DATA crate_csv_sql.r")
#write sql
library(RSQLite)
library(dplyr)
library(tools)
library(sf)
library(stringr)
##########################
dir_tiles =  "C:\\Users\\usato\\Documents\\SSL_DB"
basesqlpth =   "C:\\Users\\usato\\Documents\\SSL_DB\\base_20251020.db"
#####################################################################
sqlite    <- dbDriver("SQLite")
#############################################################
if (dir.exists(dir_tiles)==F) {stop("NO dir_tiles FOUND")}
###############################################################################################	
lstdirData = list.files(dir_tiles, pattern = "Search256", full.names=T) 
for (siteyear in lstdirData){ 
  
 # siteyear = lstdirData[1]
  
  bsnm = basename(siteyear)
  year = strsplit(bsnm, "_")[[1]][1]
  site = strsplit(bsnm, "_")[[1]][2]
  SQLite_pth = file.path(dir_tiles,  paste0(year, "_", site, "_predictions.db"))
  unlink(SQLite_pth)
  file.copy(basesqlpth, SQLite_pth)

	  # Ищем geojson файлы (по аналогии с кодом №1)
#	  geojson_files = list.files(siteyear, pattern = "\\.geojson$",recursive = TRUE, full.names = TRUE)
      crpslst = list.files(siteyear, pattern="CROP",recursive=T, full.names=T) 
    
	  
	   all_predictions = data.frame()
	  for (crop_file in crpslst) {
		
		#  crop_file = crpslst[2]		 
	#   crop_file =	CROP_xmin_1045_ymin_1262_xmax_1120_ymax_1325#20220811_195427_45c.JPG"
	
		imgname = strsplit(crop_file,"#")[[1]][2]  
		tlname =  basename(strsplit(crop_file,"#")[[1]][1])  
		tlsnmspit= strsplit(tlname,"_")
		  # Извлекаем координаты
		  x1 = as.numeric( tlsnmspit[[1]][3]) 
		  y1 =as.numeric( tlsnmspit[[1]][5])
		  x2 =as.numeric( tlsnmspit[[1]][7]) 
		  y2 = as.numeric(tlsnmspit[[1]][9]) 
		  
		  # Округляем координаты (как в коде №1)
		  x1 = round(x1)
		  y1 = round(y1)
		  x2 = round(x2)
		  y2 = round(y2)
		  
		
		  tilename1 = paste0(tools::file_path_sans_ext(imgname),"_res_",basename(crop_file))
		  
		  # Создаем запись для SQL
		  prediction_row = data.frame(
			species = "SSL",
			r_year = year,
			site = site,
			x1 = x1,
			x2 = x2,
			y1 = y1,
			y2 = y2,
			tiles =tilename1 ,
			file_name = imgname,
			ommited = 0,
			datecreated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
			dateupdated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
			stringsAsFactors = FALSE
		  )
		  
		  all_predictions = rbind(all_predictions, prediction_row)
		}
	  

  
  # Проверяем и корректируем нулевые координаты
  all_predictions$x1[all_predictions$x1 == 0] = 1
  all_predictions$x2[all_predictions$x2 == 0] = 1
  all_predictions$y1[all_predictions$y1 == 0] = 1
  all_predictions$y2[all_predictions$y2 == 0] = 1
  
  # Удаляем строки с NA значениями
  all_predictions = all_predictions[complete.cases(all_predictions$x1, all_predictions$x2, 
                                                   all_predictions$y1, all_predictions$y2), ]
  
  # Проверяем, что координаты корректны
  if (any(all_predictions$x2 <= all_predictions$x1)) {
    warning(paste("Некорректные координаты X для", site, year, "- x2 должно быть больше x1. Found", 
                  sum(all_predictions$x2 <= all_predictions$x1), "invalid records"))
  }
  
  if (any(all_predictions$y2 <= all_predictions$y1)) {
    warning(paste("Некорректные координаты Y для", site, year, "- y2 должно быть больше y1. Found", 
                  sum(all_predictions$y2 <= all_predictions$y1), "invalid records"))
  }
  

  
  # Подключаемся к базе данных и записываем данные
  SSL <- dbConnect(sqlite, SQLite_pth)
  
  dbWriteTable(SSL, "id_prediction", all_predictions, append = TRUE)
  dbDisconnect(SSL)
  
  print(paste("Successfully added", nrow(all_predictions), "records for", site, year))
  
}  # End of loop
