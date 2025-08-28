
########################################

quick_point_collector <- function(image_path, imgdata, RDSdata) {
library(jpeg)
library(magick)

#image_path="E:\\Image uniq sites folders\\2022_33_Map\\20221030\\20221030_174817_45c.JPG"
	  img <- readJPEG(image_path)
	  blankImg <<- readJPEG("IMAGE_BLANK.jpg") 
	  dims <- dim(img)
	  imgdata
	  site <- paste0("site_",imgdata$site)  
	  poly <-  paste0("poly_",imgdata$poly)             
	  sectors <- RDSdata[[site]][[poly]][[1]]
  ################################
  dev.new(width = dims[2]/100, height = dims[1]/100, 
          noRStudioGD = TRUE, xpos = 100, ypos = 100)
  par(mar = rep(0.5, 4), bg = "black")
  plot(NA, xlim = c(1, dims[2]), ylim = c(1, dims[1]), 
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  rasterImage(img, 1, 1, dims[2], dims[1])
  ###########################
  if (is.null(sectors)==F){
transparent_red <- adjustcolor("red", alpha.f = 0.2)    
current_y <- 1  
 for(i in 1:length(sectors)) {
    sector_height <- sectors[i]
	column_width = sector_height
    n_columns = round(dims[2]/column_width)+1
    for(col in 1:n_columns) {
      x_left <- (col - 1) * column_width + 1
      x_right <- col * column_width
      y_bottom <- current_y
      y_top <- current_y + sector_height
   
      rect(xleft = x_left, ybottom = y_bottom,
           xright = x_right, ytop = y_top,
           border  = transparent_red, lwd = 0.5, lty = 1)  
    }
    current_y <- current_y + sector_height
  }
  }
##########################################################
  points <- locator(n=4,type = "p", pch = 19, col = "red", cex = 1.2)
  dev.off()
  ###########################
#   dev.new(width = dims[2]/100, height = dims[1]/100, 
#          noRStudioGD = TRUE, xpos = 100, ypos = 100)
#  par(mar = rep(0.5, 4), bg = "black")
#  plot(NA, xlim = c(1, dims[2]), ylim = c(1, dims[1]), 
#       axes = FALSE, xlab = "", ylab = "", asp = 1)
#	    rasterImage(blankImg, 1, 1, dims[2], dims[1])  
#	    dev.off()
  ###########################
  
  
  
  return(points)
}
########################################################################
get_image_data <<- function(image_directory) {
 library(magick)
 library(tools)
# image_directory="E:\\Image uniq sites folders"
  jpeg_files <- list.files(image_directory, full.names = TRUE, ignore.case = TRUE, recursive=T)
  results <- NULL
  for (i in 1:length(jpeg_files)) {
  
    image_path=jpeg_files[i]
    img <- image_read(image_path)

     img_info <- image_info(img)
     width <- img_info$width
     height <- img_info$height
      file_name <- basename(image_path)
	  dtime = substr(file_name,1,16)
	  ext = file_ext(file_name)
	  ext1=paste0(".",ext)
	  poly0 = gsub(dtime,"",file_name)
	  poly = gsub(ext1,"",poly0)

      site0=gsub(file_name,"",image_path)
      date=basename(site0)
      site1=gsub(date,"",site0)
      site2=basename(site1)
	  site3=strsplit(site2,"_")[[1]][2]
	  
      pre=data.frame(
	    image_path = image_path,
        site = site3,
        poly = poly,
        dimensions = paste0(height," ", width),
		stringsAsFactors = FALSE
        
      )
	results = rbind(pre,results)
	  
    }

  return(results)
}
###################################################################
 RDStoTable=function(rds_path){
 library(dplyr)
library(tidyr)
library(purrr)
fin=NULL
data <- readRDS(rds_path)
sites = names(data)
  for (i in 1:length(sites)){
   site = sites[i]
   sitedata= data[[site]]
   polys=names(sitedata)
     for (y in 1:length(polys)){
	 poly=polys[y]
	 polydata= sitedata[[poly]]
	 dimensions=polydata$dimensions
	 dimensions = paste(dimensions, collapse = " ")
	 points= polydata[[1]]
	 points <- paste(points, collapse = " ")
	 site1=gsub("site_","",site)
	 poly1=gsub("poly_","",poly)
    df1 = data.frame(site=site1,poly=poly1,dimensions=dimensions,points=points)
	fin=rbind(fin,df1) 
}}
return(fin)
}
##########################################################
tile_optimization=function(imgdata, pnts, scl=1){

#2 pair of coordinates: 1 sets hwight of SSL bull at the bottom, 2 at the top.
# first bull at the bottom
# second bull at the top
   pnts
   imgdata
   scl=1
   dimensions = as.numeric(strsplit(imgdata$dimensions, " ")[[1]])
   height= dimensions[2] # высота изображения

  # Измеряем размеры животных в пикселях
  down_animal_size <- abs(pnts$x[1] - pnts$x[2]) * 0.9
  up_animal_size <- abs(pnts$x[3] - pnts$x[4]) * 0.1
  
  coeficientDown = 1- (pnts$y[1] / height)
  coeficientUp = pnts$y[3] / height
  
  bottom_animal_size = down_animal_size / coeficientDown
  top_animal_size = up_animal_size * coeficientUp
  # Линейная интерполяция размеров тайлов
  n_tiles <- round(height / mean(c(bottom_animal_size, top_animal_size)))
  
  # Создаем последовательность размеров тайлов
  tile_sizes <- seq(bottom_animal_size, top_animal_size, length.out = n_tiles)
  
  # Корректируем чтобы сумма точно равнялась высоте изображения
  total_current <- sum(tile_sizes)
  correction_factor <- height / total_current
  tile_sizes <- tile_sizes * correction_factor
  
  return(tile_sizes)
   
  
}
##############################################################################
display_tile_grid <- function(RDSdata, imgdata) {
  library(jpeg)
  library(magick)
  library(RColorBrewer)
  
  
  imgdata
  
  image_path = imgdata$image_path
  site = paste0("site_",imgdata$site)  
  poly =  paste0("poly_",imgdata$poly)             
  
  sectors = RDSdata[[site]][[poly]][[1]]
  # dim = RDSdata[[site]][[poly]][[2]]
  # dims <- as.numeric(strsplit(dim," ")[[1]])

  
  # Читаем изображение
#  img <- readJPEG(image_path)
#  dims <- dim(img)

  ##########################################################################################
   dev.new(width = dims[2]/100, height = dims[1]/100, 
          noRStudioGD = TRUE, xpos = 100, ypos = 100)
  par(mar = rep(0.5, 4), bg = "black")
  plot(NA, xlim = c(1, dims[2]), ylim = c(1, dims[1]), 
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  rasterImage(img, 1, 1, dims[2], dims[1])
   ########################################################  
transparent_red <- adjustcolor("red", alpha.f = 0.3)    
current_y <- 1  # Начинаем СНИЗУ
 for(i in 1:length(sectors)) {
    sector_height <- sectors[i]
	column_width = sector_height
    n_columns = round(dims[2]/column_width)
    for(col in 1:n_columns) {
      # Координаты тайла (снизу вверх)
      x_left <- (col - 1) * column_width + 1
      x_right <- col * column_width
      y_bottom <- current_y
      y_top <- current_y + sector_height
      
      # Рисуем границы тайла
      rect(xleft = x_left, ybottom = y_bottom,
           xright = x_right, ytop = y_top,
           border  = transparent_red, lwd = 0.5, lty = 1)  
    }
    # Обновляем текущую позицию Y (двигаемся ВВЕРХ)
    current_y <- current_y + sector_height
  }
  }
##########################################################
draft=function(){
 
   
  # width
   #nOb
 #########  
	xy.1 <- list(
                x = pnts$x[1:2],
                y = pnts$y[1:2]
                )
	xy.2 <- list(
                x = pnts$x[3:4],
                y = pnts$y[3:4]
                )
##########
	find_width=function(nOb){
		hv.begin=abs(xy.1$y[1]-xy.1$y[2])*scl
		hv.end=abs(xy.2$y[1]-xy.2$y[2])*scl
		delta=abs(hv.begin-hv.end)
		   vect.1=seq(hv.begin,hv.end,-((delta)/nOb))
		y=abs(height-sum(vect.1))
		return(y)
	}
#########	
	optim_result=optim(par=5, fn=find_width,method="Brent",lower=2,upper=50)
####
	optim_strips=function(params,nOb){
		hv.begin=(abs(xy.1$y[1]-xy.1$y[2])*scl)+params[1]
		hv.end=(abs(xy.2$y[1]-xy.2$y[2])*scl)+params[2]
		delta=abs(hv.begin-hv.end)
		vect.1=seq(hv.begin,hv.end,-((delta)/nOb))
		y=abs(height-sum(vect.1))
		return(y)
	}
#####	
	optim_result2=optim(par=c(0,0),fn=optim_strips,method="L-BFGS-B",lower=-20,upper=20,nOb=optim_result$par)
###
	return_sections=function(nOb,params){
		
		hv.begin=(abs(xy.1$y[1]-xy.1$y[2])*scl)+params[1]
		hv.end=(abs(xy.2$y[1]-xy.2$y[2])*scl)+params[2]
		delta=abs(hv.begin-hv.end)
		vect.1=seq(hv.begin,hv.end,-((delta)/nOb))
	
		
		vect.1
	}

	return_sections(optim_result$par,optim_result2$par)

}














