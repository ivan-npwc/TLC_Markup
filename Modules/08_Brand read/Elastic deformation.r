
#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/Brand read/Elastic deformation.r")


library(EBImage)

dirfrom = "/home/ivan/TRAIN/SSL/Brand read/orig"
dirto = "/home/ivan/TRAIN/SSL/Brand read/elastic_perspective_deformation"



###############################################################
   elastic_def=function(                             
								  image_path,
                                     #alpha = 9, #  alpha - интенсивность деформации (300-1000)
									 sigma = 15#   sigma - гладкость деформации (5-15)
									  ){


  alpha = sample(2:9)[1]
		
  img <- readImage(image_path)
 
  

  maxdim=max(dim(img))
  img = resize(img,maxdim,maxdim)
  
  channels <- dim(img)[3]
  w <- dim(img)[1]
  h <- dim(img)[2]
  
  # Создаем случайные поля смещения
  #set.seed(rnorm(1))
  dx <- matrix(rnorm(h * w, sd = 1), nrow = h, ncol = w)
  dy <- matrix(rnorm(h * w, sd = 1), nrow = h, ncol = w)
  
  # Применяем Гауссово размытие
  gaussian_kernel <- makeBrush(size = sigma*2+1, shape = 'gaussian', sigma = sigma)
  dx_blur <- filter2(dx, gaussian_kernel)
  dy_blur <- filter2(dy, gaussian_kernel)
  
  # Масштабируем
  max_disp <- max(abs(c(dx_blur, dy_blur)))
  dx_norm <- dx_blur * (alpha / max_disp)
  dy_norm <- dy_blur * (alpha / max_disp)
  
  # Создаем координатные сетки
  x_grid <- matrix(rep(1:w, each = h), nrow = h, ncol = w)
  y_grid <- matrix(rep(1:h, times = w), nrow = h, ncol = w)
  
  # Новые координаты
  x_new <- x_grid + dx_norm
  y_new <- y_grid + dy_norm
  
  # Ограничиваем границы
  x_new <- pmin(pmax(x_new, 1), w)
  y_new <- pmin(pmax(y_new, 1), h)
  
  # Функция для интерполяции с использованием approx2
  interpolate_channel <- function(channel_data, x_coords, y_coords) {
    # Создаем матрицу для результата
    result <- matrix(0, nrow = h, ncol = w)
    
    # Интерполируем каждый пиксель
    for (i in 1:h) {
      for (j in 1:w) {
        x <- x_coords[i, j]
        y <- y_coords[i, j]
        
        # Билинейная интерполяция
        x1 <- floor(x)
        x2 <- ceiling(x)
        y1 <- floor(y)
        y2 <- ceiling(y)
        
        # Проверяем границы
        if (x1 >= 1 & x2 <= w & y1 >= 1 & y2 <= h) {
          # Веса для интерполяции
          wx <- x2 - x
          wy <- y2 - y
          
          # Интерполяция
          result[i, j] <- 
            channel_data[y1, x1] * wx * wy +
            channel_data[y1, x2] * (1 - wx) * wy +
            channel_data[y2, x1] * wx * (1 - wy) +
            channel_data[y2, x2] * (1 - wx) * (1 - wy)
        }
      }
    }
    return(result)
  }
  
  # Применяем трансформацию для каждого канала
  transformed_channels <- list()
  
  for (channel in 1:channels) {
    channel_data <- imageData(img)[,,channel]
    transformed_channel <- interpolate_channel(channel_data, t(x_new), t(y_new))
    transformed_channels[[channel]] <- transformed_channel
  }
  
  # Собираем обратно в изображение
  if (channels == 1) {
    transformed_img <- Image(transformed_channels[[1]])
  } else {
    transformed_array <- array(0, dim = c(h, w, channels))
    for (channel in 1:channels) {
      transformed_array[,,channel] <- transformed_channels[[channel]]
    }
    transformed_img <- Image(transformed_array, colormode = Color)
	
	transformed_img1 = flop(rotate(transformed_img,angle=90))
	
	
  }
  return(transformed_img1)


#combined_images <- combine(img, transformed_img1)
#display(combined_images, method = "raster", all = TRUE)

}
perspective_transform  <- function(image_path, type = "random", intensity = 0.3) {
 maxin = intensity*1000
 intensity1= sample(100:maxin)[1]/1000
 intensity1
 img <- readImage(image_path)
 
 
 maxdim=max(dim(img))
  img = resize(img,maxdim,maxdim)
 
  w <- dim(img)[1]
  h <- dim(img)[2]
  
  # Базовые точки (только 3 точки для аффинного преобразования)
  src_points <- matrix(c(
    1, 1,      # левый верхний
    w, 1,      # правый верхний  
    w, h       # правый нижний
  ), ncol = 2, byrow = TRUE)
  
  # Случайное смещение точек
  shift_x <- runif(3, -w * intensity1, w * intensity1)
  shift_y <- runif(3, -h * intensity1, h * intensity1)
  
  # Целевые точки
  dst_points <- src_points + cbind(shift_x, shift_y)
  
  # Создаем матрицу аффинного преобразования
  A <- cbind(src_points, 1)
  b <- dst_points
  
  # Решаем систему уравнений для матрицы преобразования
  affine_matrix <- solve(A) %*% b
  
  # Применяем аффинное преобразование
  transformed <- affine(img, affine_matrix, filter = "bilinear")
 
 # combined_images <- combine(img, transformed)
#display(combined_images, method = "raster", all = TRUE)
  
  
  return(transformed)
}

#########################################################################

lstdird =list.dirs(dirfrom, recursive=T)

for (i in 1: length(lstdird)){

brdr = lstdird[i]
percent = round(i/length(lstdird) *100)
print(paste0("DONE    ", percent, "           percent"))
lstimgs = list.files(brdr, full.names=T,   pattern="png|JPG|jpg|jpeg|JPEG")

if (length(lstimgs)==0) next
dirtobr = gsub(dirfrom,dirto,brdr)
dir.create(dirtobr, recursive=T,showWarnings=F)
#file.copy(lstimgs,dirtobr)  # i need delete orig files froo check loop length dim
presence=list.files(dirtobr)
if (length(presence)>2000) next


  for (g in 1:1000){
  index=sample(1:length(lstimgs))[1]
  pth=lstimgs[index]
  if (length(dim(readImage(pth))) != 2){
  elasticdef = elastic_def(pth)
  newname = paste0(g,"Elastic#",basename(pth))
  savepth=file.path(dirtobr,newname)
  writeImage(elasticdef,savepth)
  }
 }


lstimgs1=list.files(dirtobr, full.names=T)

  for (d in 1:1000){
  index=sample(1:length(lstimgs1))[1]
  pth=lstimgs1[index]
    if (length(dim(readImage(pth))) != 2){
  elasticdef = perspective_transform(pth,intensity = 0.2)
  newname = paste0(d,"Perspective#",basename(pth))
  savepth=file.path(dirtobr,newname)
  writeImage(elasticdef,savepth)
  }
 }

  for (v in 1:1000){
  index=sample(1:length(lstimgs))[1]
  pth=lstimgs[index]
    if (length(dim(readImage(pth))) != 2){
  elasticdef = perspective_transform(pth)
  newname = paste0(v,"Perspective#",basename(pth))
  savepth=file.path(dirtobr,newname)
  writeImage(elasticdef,savepth)
  }
  }


}







