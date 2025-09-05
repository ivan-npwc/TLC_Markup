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
  down_animal_size <- abs(pnts$x[1] - pnts$x[2]) 
  up_animal_size <- abs(pnts$x[3] - pnts$x[4]) 
  
  
   size1 <- sqrt((pnts$x[2] - pnts$x[1])^2 + (pnts$y[2] - pnts$y[1])^2)
   size2 <- sqrt((pnts$x[4] - pnts$x[3])^2 + (pnts$y[4] - pnts$y[3])^2)
   
   measurements1= data.frame(x1 = pnts$x[1], y1 = pnts$y[1], x2 = pnts$x[2], y2 = pnts$y[2], size = size1)
   measurements2= data.frame(x1 = pnts$x[3], y1 = pnts$y[3], x2 = pnts$x[4], y2 = pnts$y[4], size = size2)
   measurements <- rbind(measurements1, measurements2)                     
   measurements$y_center <- (measurements$y1 + measurements$y2) / 2
   y_positions <- seq(1, height, length.out = 100)

   model_glm <- glm(size ~ y_center, data = measurements, 
                family = gaussian(link = "log"))
				
 
   predicted_sizes <- predict(model_glm, 
                          newdata = data.frame(y_center = y_positions),
                          type = "response")  # Предсказания в исходной шкале
 
  ##########################################
  # Вспомогательная функция для точной корректировки суммы
adjust_sum_to_exact_height <- function(heights, target_height) {
  current_sum <- sum(heights)
  diff <- target_height - current_sum
  
  if (diff != 0) {
    if (diff > 0) {
      # Добавляем пиксели к самым большим тайлам
      indices <- order(heights, decreasing = TRUE)[1:diff]
      heights[indices] <- heights[indices] + 1
    } else {
      # Убираем пиксели у самых маленьких тайлов
      indices <- order(heights)[1:abs(diff)]
      heights[indices] <- heights[indices] - 1
    }
  }
  
  return(heights)
}
  ############################################
  optimize_tiling_advanced <- function(predicted_sizes, height) {
  # Функция для минимизации
  objective_function <- function(params) {
    n_tiles <- round(params[1])  # Количество тайлов
    scale_factor <- params[2]   # Масштабный коэффициент
    
    if (n_tiles < 2 || n_tiles > 30) return(Inf)
    
    # Границы тайлов
    boundaries <- seq(1, length(predicted_sizes), length.out = n_tiles + 1)
    boundaries <- round(boundaries)
    
    # Высоты тайлов на основе predicted_sizes
    base_heights <- diff(boundaries)
    scaled_heights <- base_heights * scale_factor
    
    # Ошибка суммы высот
    sum_error <- abs(sum(scaled_heights) - height)
    
    # Ошибка соответствия predicted_sizes
    size_error <- 0
    for (i in 1:n_tiles) {
      y_start <- boundaries[i]
      y_end <- boundaries[i + 1]
      avg_predicted <- mean(predicted_sizes[y_start:y_end])
      size_error <- size_error + abs(scaled_heights[i] - avg_predicted)
    }
    
    # Общая ошибка (взвешенная сумма)
    total_error <- sum_error * 10 + size_error  # Увеличиваем вес ошибки суммы
    
    return(total_error)
  }
  
  # Оптимизация
  result <- optim(
    par = c(5, 1.0),  # Начальные значения: 5 тайлов, масштаб 1.0
    fn = objective_function,
    method = "L-BFGS-B",
    lower = c(2, 0.5),
    upper = c(30, 2.0)
  )
  
  # Извлекаем оптимальные параметры
  n_tiles <- round(result$par[1])
  scale_factor <- result$par[2]
  
  # Рассчитываем финальные высоты
  boundaries <- seq(1, length(predicted_sizes), length.out = n_tiles + 1)
  boundaries <- round(boundaries)
  base_heights <- diff(boundaries)
  scaled_heights <- base_heights * scale_factor
  
  # Масштабируем к точной высоте изображения
  final_scale <- height / sum(scaled_heights)
  final_heights <- scaled_heights * final_scale
  final_heights <- round(final_heights)
  
  # Корректируем сумму
  adjust_sum_to_exact_height(final_heights, height)
  
  return(list(
    tile_count = n_tiles,
    heights = final_heights,
    boundaries = boundaries,
    total_error = result$value
  ))
}
optimize_tiling_advanced(predicted_sizes, height)
#########################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    # Определение линий разрезов (горизонтальных полос)
  # Находим точки, где размер уменьшается на значительную величину
  breaks <- c()
  current_size <- predicted_sizes[1]
  
  for (i in 2:length(predicted_sizes)) {
    size_ratio <- predicted_sizes[i] / current_size
    if (size_ratio < 1.5) {  # Порог для создания новой линии
      breaks <- c(breaks, y_positions[i])
      current_size <- predicted_sizes[i]
    }
  }
  
  breaks
  
    # Создание линий разрезов
  lines <- c(1, breaks, height)
  lines <- sort(unique(round(lines)))
   tile_system <- data.frame()
   
   expected_size <-    predicted_sizes <- predict(model_glm, 
                          newdata = data.frame(y_center = lines),
                          type = "response")  # Предсказания в исходной шкале
  
 # Определение размера тайла на основе ожидаемого размера сивуча
    min_tile_size=50
    tile_size <- max(min_tile_size, round(expected_size * 1.5))  # Коэффициент 1.5 для запаса
   
  
}
##############################################################################
 draft=function(){
 scale=1.8
find_width=function(nOb)
	{
      sealSize1 <- (sqrt((pnts$x[2] - pnts$x[1])^2 + (pnts$y[2] - pnts$y[1])^2))*scale
      sealSize2 <- (sqrt((pnts$x[4] - pnts$x[3])^2 + (pnts$y[4] - pnts$y[3])^2))*scale
		sz=c(sealSize1,sealSize2)
		delta=abs(diff(sz))
		start=min(sz);stop=max(sz)
		vect.1=seq(start,stop, delta/nOb)
		y=abs(height-sum(vect.1))
		return(y)
	}
	optim_result=optim(par=5,fn=find_width,method="Brent",lower=2,upper=50)
	
#///////////////////////////////////////////////
	optim_strips=function(params,nOb)
	{
		sealSize1 <- (sqrt((pnts$x[2] - pnts$x[1])^2 + (pnts$y[2] - pnts$y[1])^2)*scale)+params[1]
		sealSize2 <- (sqrt((pnts$x[4] - pnts$x[3])^2 + (pnts$y[4] - pnts$y[3])^2)*scale)+params[2]
		sz=c(sealSize1,sealSize2)
		delta=abs(diff(sz))
	    start=min(sz);stop=max(sz)
		vect.1=seq(start,stop,delta/nOb)
		y=abs(height-sum(vect.1))
		return(y)
	}
	optim_result2=optim(par=c(0,0),fn=optim_strips,method="L-BFGS-B",lower=-20,upper=20,nOb=optim_result$par)
#/////////////////////////////////////////////////	
	
	return_sections=function(nOb,params)
	{

	   sealSize1 <- (sqrt((pnts$x[2] - pnts$x[1])^2 + (pnts$y[2] - pnts$y[1])^2)*scale)+params[1]
	   sealSize2 <- (sqrt((pnts$x[4] - pnts$x[3])^2 + (pnts$y[4] - pnts$y[3])^2)*scale)+params[2]
	   sz=c(sealSize1,sealSize2)
		delta=abs(diff(sz))
	    start=min(sz);stop=max(sz)
		vect.1=seq(start,stop,delta)/nOb
	}

	a=return_sections(optim_result$par,optim_result2$par)
	a
	
	
	
	
	
	
	
	
	