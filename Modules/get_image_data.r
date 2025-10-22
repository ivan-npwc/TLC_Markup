

#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/get_image_data.r")




get_image_data_parallel_unix <- function(image_directory) {
  library(EBImage)
  library(tools)
  library(parallel)
  library(future.apply)
  library(pbapply)
  
  # Находим все JPG файлы
  jpeg_files <- list.files(image_directory, full.names = TRUE, 
                          ignore.case = TRUE, recursive = TRUE
        )
  

  
  # Функция для обработки одного изображения
  process_single_image <- function(image_path) {
    tryCatch({
      # Пытаемся прочитать изображение
      img <- readImage(image_path)
      
      # Получаем размеры
      width <- dim(img)[2]  # исправлено: width обычно второй dimension
      height <- dim(img)[1] # height обычно первый
      
      # Извлекаем информацию из пути
      file_name <- basename(image_path)
      dtime <- substr(file_name, 1, 16)
      ext <- file_ext(file_name)
      ext1 <- paste0(".", ext)
      poly0 <- gsub(dtime, "", file_name)
      poly <- gsub(ext1, "", poly0)
      
      site0 <- gsub(file_name, "", image_path)
      date <- basename(site0)
      site1 <- gsub(date, "", site0)
      site2 <- basename(site1)
      site3 <- ifelse(length(strsplit(site2, "_")[[1]]) >= 2, 
                     strsplit(site2, "_")[[1]][2], 
                     "unknown")
      
      # Возвращаем результат
      list(
        image_path = image_path,
        site = paste0("site_", site3),
        poly = paste0("poly_", poly),
        width = width,
        height = height,
        dimensions = paste0(width, "x", height),
        status = "success"
      )
      
    }, error = function(e) {
      # В случае ошибки возвращаем запись с ошибкой
      list(
        image_path = image_path,
        site = "NA_character_",
        poly = "NA_character_",
        width = "error",
        height = "error",
        dimensions = "error",
        status = "error"
      )
    })
  }
  
  # Вариант 1: Использование mclapply (только для Unix)
  if (.Platform$OS.type == "unix") {
    cat("Using mclapply for parallel processing on Unix...\n")
    
    results_list <- mclapply(jpeg_files, function(img_path) {
      process_single_image(img_path)
    }, mc.cores = 25, mc.preschedule = TRUE)
    
  } else {
    # Для Windows используем обычный lapply
    warning("Running on Windows, using single-core processing")
    results_list <- lapply(jpeg_files, process_single_image)
  }
  
  # Преобразуем список в data.frame
  results_df <- do.call(rbind, lapply(results_list, as.data.frame, stringsAsFactors = FALSE))
  
  return(results_df)
}



# Пример использования:
 result1 <- get_image_data_parallel_unix("/mnt/adata8tb/SSL_DB")
 
 write.csv(result1,"image_data1.csv",row.names=F)