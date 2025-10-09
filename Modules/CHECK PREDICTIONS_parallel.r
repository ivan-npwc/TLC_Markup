#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/CHECK PREDICTIONS_parallel.r")
#tcltk::tk_choose.files()
#watch -n 1 sensors

library(magick)
library(parallel)

Preddir = "/media/ivan/USATOV_2024/SSL_DB_Tiles"
SSL_DB_dir = "/media/ivan/2023_ HD2/SSL_DB"

###################################################
listsites_presence = list.files(Preddir, full.names=T, pattern="Presence")
###################################################

# Функция для обработки одного изображения и добавления боксов
process_single_image <- function(img_data) {
  library(magick)
  
  img_pth <- img_data$img_pth
  presence_tiles <- img_data$presence_tiles
  branded_tiles <- img_data$branded_tiles
  SaveDir <- img_data$SaveDir
  day_folder <- img_data$day_folder
  
  
      # Создание пути для сохранения
    output_dir <- file.path(SaveDir, basename(day_folder))
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_path <- file.path(output_dir, paste0("annotated_", basename(img_pth)))
	
	if(file.exists(output_path)==T){next}
  
  
  tryCatch({
    # Проверка существования исходного изображения
    if (!file.exists(img_pth)) {
      warning(paste0("No orig img found: ", img_pth))
      return(NULL)
    }
    
    # Чтение исходного изображения
    img <- image_read(img_pth)
    
    # Создание графического устройства для рисования
    img_draw <- image_draw(img)
    
    # Добавление боксов для Presence (например, зеленый цвет)
    if (nrow(presence_tiles) > 0) {
      for (w in 1:nrow(presence_tiles)) {
        x1 <- as.numeric(presence_tiles$xstart[w])
        y1 <- as.numeric(presence_tiles$ystart[w])
        x2 <- as.numeric(presence_tiles$xend[w])
        y2 <- as.numeric(presence_tiles$yend[w])
        
        # Рисуем прямоугольник
        rect(x1, y1, x2, y2, border = "green", lwd = 3, col = NA)

      }
    }
    
    # Добавление боксов для Branded (например, красный цвет)
    if (nrow(branded_tiles) > 0) {
      for (r in 1:nrow(branded_tiles)) {
        x1 <- as.numeric(branded_tiles$xstart[r])
        y1 <- as.numeric(branded_tiles$ystart[r])
        x2 <- as.numeric(branded_tiles$xend[r])
        y2 <- as.numeric(branded_tiles$yend[r])
        
        # Рисуем прямоугольник
        rect(x1, y1, x2, y2, border = "red", lwd = 3, col = NA)
        
     
      }
    }
    
    dev.off()
    

    image_write(img_draw, output_path, quality = 90)
    cat("Completed img:", output_path, "\n")
    # Освобождение памяти
    rm(img, img_draw)
    gc()
    
    return(paste("Processed:", basename(img_pth)))
    
  }, error = function(e) {
    warning(paste("Error processing image", img_pth, ":", e$message))
    return(NULL)
  })
}

# Функция для обработки одного дня
process_single_day <- function(day_data) {
  day_presence <- day_data$day_presence
  day_branded <- day_data$day_branded
  SaveDir <- day_data$SaveDir
  year <- day_data$year
  site <- day_data$site
  
  listImgPresence <- list.files(day_presence, full.names = T, pattern = "JPG|jpg|JPEG|jpeg|PNG|png")
  listImgBranded <- list.files(day_branded, full.names = T, pattern = "JPG|jpg|JPEG|jpeg|PNG|png")
  
  if (length(listImgPresence) == 0) listImgPresence <- NULL
  if (length(listImgBranded) == 0) listImgBranded <- NULL
  
  # Создаем data.frame с информацией о тайлах
  listImgPresenceDT <- data.frame(category = "Presence", tiles = listImgPresence, stringsAsFactors = FALSE)
  listImgBrandedDT <- data.frame(category = "Branded", tiles = listImgBranded, stringsAsFactors = FALSE)
  
  lsttils <- rbind(listImgPresenceDT, listImgBrandedDT)
  
  if (nrow(lsttils) == 0) {
    return(NULL)
  }
  
  # Извлекаем координаты и информацию об изображениях
  for (e in 1:nrow(lsttils)) {
    tile <- basename(lsttils$tiles[e])
    info <- strsplit(tile, "#")
    img_bsname <- info[[1]][2]
    coords <- info[[1]][1]
    info_coords <- strsplit(coords, "_")
    
    xstart <- info_coords[[1]][4]
    xend <- info_coords[[1]][6]
    ystart <- info_coords[[1]][8]
    yend <- info_coords[[1]][10]
    
    lsttils$img_bsname[e] <- img_bsname
    lsttils$xstart[e] <- xstart
    lsttils$xend[e] <- xend
    lsttils$ystart[e] <- ystart
    lsttils$yend[e] <- yend
  }
  
  # Создаем полный путь к исходным изображениям
  lsttils$img_pth <- file.path(SSL_DB_dir, 
                               paste0(year, "_", site, "_Map"), 
                               basename(day_presence), 
                               lsttils$img_bsname)
  
  # Получаем уникальные исходные изображения
  lstimgs <- unique(lsttils$img_pth)
  
  # Подготавливаем данные для параллельной обработки
  img_data_list <- list()
  
  for (w in 1:length(lstimgs)) {
    img_pth <- lstimgs[w]
    tiles_for_img <- lsttils[lsttils$img_pth == img_pth, ]
    presence_tiles <- tiles_for_img[tiles_for_img$category == "Presence", ]
    branded_tiles <- tiles_for_img[tiles_for_img$category == "Branded", ]
    
    img_data_list[[w]] <- list(
      img_pth = img_pth,
      presence_tiles = presence_tiles,
      branded_tiles = branded_tiles,
      SaveDir = SaveDir,
      day_folder = day_presence
    )
  }
  
  # Параллельная обработка изображений с помощью mclapply
  results <- mclapply(img_data_list, process_single_image, 
                     mc.cores = detectCores() - 1,
                     mc.preschedule = FALSE)
  
  return(results)
}

# Основной цикл обработки сайтов
for (i in 1:length(listsites_presence)) {
  sitedir_presence <- listsites_presence[i]
  site_info <- strsplit(basename(sitedir_presence), "_")
  year <- site_info[[1]][1]
  site <- site_info[[1]][2]
  
  SaveDir <- gsub("Presence", "CHECK", sitedir_presence)
  dir.create(SaveDir, showWarnings = FALSE, recursive = TRUE)
  daysdir_presence <- list.files(sitedir_presence, full.names = TRUE)
  
  # Подготавливаем данные для параллельной обработки дней
  day_data_list <- list()
  
  for (y in 1:length(daysdir_presence)) {
    day_presence <- daysdir_presence[y]
    day_branded <- gsub("Presence", "Branded", day_presence)
    
    # Проверяем существование директории branded
    if (!dir.exists(day_branded)) {
      warning(paste("Branded directory not found:", day_branded))
      next
    }
    
    day_data_list[[y]] <- list(
      day_presence = day_presence,
      day_branded = day_branded,
      SaveDir = SaveDir,
      year = year,
      site = site
    )
  }
  
  # Параллельная обработка дней
  cat("Processing site:", site, "Year:", year, "\n")
  cat("Number of days to process:", length(day_data_list), "\n")
  
  day_results <- mclapply(day_data_list, process_single_day,
                         mc.cores = max(1, detectCores() - 12),
                         mc.preschedule = TRUE)
  
  cat("Completed site:", site, "\n")
  print(unlist(day_results))
}

cat("All sites processed successfully!\n")
