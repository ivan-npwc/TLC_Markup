tile_optimization=function(imgdata, pnts, scl=1){

#2 pair of coordinates: 1 sets hwight of SSL bull at the bottom, 2 at the top.
# first bull at the bottom
# second bull at the top
   pnts
   imgdata
   scl=1
   dimensions = as.numeric(strsplit(imgdata$dimensions, " ")[[1]])
   height= dimensions[1] # высота изображения

  # Измеряем размеры животных в пикселях
 
  
    size1 <- sqrt((pnts$x[2] - pnts$x[1])^2 + (pnts$y[2] - pnts$y[1])^2)
    size2 <- sqrt((pnts$x[4] - pnts$x[3])^2 + (pnts$y[4] - pnts$y[3])^2)
    sealsize=c(size1,size2)
  
    down_animal_size <-max(sealsize)
    up_animal_size  <-min(sealsize)
  
  coeficientDown = 1- (pnts$y[1] / height)
  coeficientUp = pnts$y[3] / height
  
  bottom_animal_size = down_animal_size / coeficientDown
  top_animal_size = up_animal_size * coeficientUp *0.1
  # Линейная интерполяция размеров тайлов
  n_rows <- round(height / mean(c(bottom_animal_size, top_animal_size)))
  
  # Создаем последовательность размеров тайлов
  tile_sizes <- seq(bottom_animal_size, top_animal_size, length.out = n_rows)
  
  # Корректируем чтобы сумма точно равнялась высоте изображения
  total_current <- sum(tile_sizes)
  correction_factor <- height / total_current
  tile_sizes <- tile_sizes * correction_factor
  
  return(tile_sizes)
   
  
}
##############################################################################
 
	
	
	
	