########################################################################
get_image_data <<- function(image_directory) {
 library(magick)
 library(tools)
# image_directory="E:\\Image uniq sites folders"
  jpeg_files <- list.files(image_directory, full.names = TRUE, ignore.case = TRUE, recursive=T,  pattern = "\\.(jpg|jpeg|png|gif|bmp|tiff|tif)$")
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
        site = paste0("site_",site3),
        poly = paste0("poly_",poly),
        dimensions = paste0(height," ", width),
		stringsAsFactors = FALSE
        
      )
	results = rbind(pre,results)
	  
    }

  return(results)
}
###################################################################