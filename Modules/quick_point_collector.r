
quick_point_collector <- function(image_path, imgdata, RDSdata,n) {
library(jpeg)
library(magick)

#image_path="E:\\Image uniq sites folders\\2022_33_Map\\20221030\\20221030_174817_45c.JPG"
	  img <- readJPEG(image_path)
	  blankImg <<- readJPEG("IMAGE_BLANK.jpg") 
	  dims <- dim(img)
	  imgdata
	  site <- imgdata$site
	  poly <-  imgdata$poly             
	  sectors <- RDSdata[[site]][[poly]][[1]]
	  mask = RDSdata[[site]][[poly]]$Mask
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
   lines(as.numeric(mask$x),as.numeric(mask$y),col=4,lwd=2)	
  }
##########################################################
  points <- locator(n=n,type = "p", pch = 19, col = "red", cex = 1.2)
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