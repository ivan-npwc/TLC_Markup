  reconstruct_image_from_tiles=function(imgdata, dirtiles){

      dirtiles =  "E:\\SSL_DB_Tiles\\2024_71_Tiles\\20240511"
      lsttiles= list.files(dirtiles, full.names=T, recursive=T, pattern=basename(imgdata$image_path))

      img_dims = as.numeric(strsplit(imgdata$dimensions, " ")[[1]])
      img_width <- img_dims[2]
      img_height <- img_dims[1]
      reconstructed <- array(0, dim = c(img_width, img_height, 3))
	   for (i in 1:length(lsttiles)){
	   tileP = lsttiles[i]
	   tile=readImage(tileP)
	   tile=flip(tile)
	   info=basename(tileP)
	   info1=gsub(basename(imgdata$image_path),"",info)
	   info2=gsub("#","",info1)
	   info3=strsplit(info2,"_")
	   
	   
	   x_start=as.numeric(info3[[1]][4])
	   x_end=as.numeric(info3[[1]][6])
	   
	   EBIy_start=as.numeric(info3[[1]][8])
	   EBIy_end=as.numeric(info3[[1]][10])
	      
        reconstructed[x_start:x_end, EBIy_start:EBIy_end, ] <- tile#[,,1:3]
		



}

}