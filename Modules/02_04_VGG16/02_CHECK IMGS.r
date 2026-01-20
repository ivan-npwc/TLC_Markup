      
 #source("/home/npwc/GIT/TLC_Markup/Modules/VGG16/CHECK IMGS.r")
 #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/VGG16/CHECK IMGS.r")

	    library(keras)
        library(magick)
        library(tidyr)
        library(tfdatasets)
	    library(tidyverse)
    	library(reticulate)
		
		
		
		
    ssldir =  "/mnt/adata8tb/SSL_DB_Tiles"
    dirforimgserr = "/mnt/adata8tb/SSL_DB_Tiles/err_imgs"
	
    file_size=2000
    vision_dimensions=2
  #  DONE=read.csv("listImgPred_DONE_GOOD.csv")
    err=data.frame(imgs="")
	
	if (dir.exists(dirforimgserr)==F){dir.create(dirforimgserr)}

listsites = list.files(ssldir, full.names=T,pattern="Tiles")
 for (e in 1:length(listsites)){
    #e=3
     sitedir=listsites[e]
	  daysdir=list.files(sitedir,full.names=T)
	   for (y in 1:length(daysdir)){
            #y=10
             day = daysdir[y]
             pth_imgs_err = paste0(day,"/imgs_err.csv")
			  if(file.exists(pth_imgs_err)==T) {print(paste0("SKIP     ", day)); next}
              listImgPred <- list.files(day, full.names = T,pattern="JPG")
			 
			  # listImgPredcheck =listImgPred[!listImgPred %in% DONE$img]
			  # if (length(listImgPredcheck)==0){print(paste0("SKIP     ", day)); next}
			   print(length(listImgPred))
         

            inf=data.frame(listImgPred=listImgPred,file_size=as.numeric(file.size(listImgPred)))
			listImgPred=inf$listImgPred[inf$file_size > file_size]
			exl=length(inf$listImgPred[inf$file_size < file_size])
			print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImgPred) , "   Images"))
			errimgs=inf$listImgPred[inf$file_size < file_size]
			file.copy(errimgs, dirforimgserr)
			unlink(errimgs)


##############################
if(!exists("model")) {
  model <- keras_model_sequential() %>%
    layer_flatten(input_shape = c(vision_dimensions, vision_dimensions, 3)) %>%
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = "accuracy"
  )
}
########################################
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$io$read_file(.x$img)
	))  %>%  
	     dataset_map(~.x %>% list_modify(
		                            img = tf$image$decode_jpeg(.x$img,channels = 3)
									)) 	%>%  
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float64)
		)) 	%>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions))
		))   %>% 
	                                            dataset_batch(batch_size) %>%
                        	                  #  dataset_cache() %>%
	                                             dataset_map(unname)  # Keras needs an unnamed output.
	                                           #dataset_shuffle(buffer_size = batch_size*vision_dimensions)
	}
	#################################### level 1
	    batch_size=1000
	    globalBatch = 1000
	
	fin=NULL
	glob_step = round(length(listImgPred)/globalBatch)+1
	  for (i in 1:glob_step){
	   if (i==1){start1=1;stop1=globalBatch}
	     index=c(start1:stop1)
		  listImgBtch = listImgPred[index]
	      listImgBtch=listImgBtch[is.na(listImgBtch)==F]
		  if (length(listImgBtch)==0) break
	      dataBtch <- tibble::tibble(img = listImgBtch)
	      Btch_data_set <- create_dataset(dataBtch, train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
	  
	  predBtch =tryCatch({
	                                  model %>% predict(Btch_data_set)
						 fin=rbind(dataBtch,	fin	)	  
	  
	    }, error = function(e) {
     message("✗ ", i, " - ", e$message)
	 })
        start1=start1+globalBatch
	     stop1=stop1+globalBatch
   }
################################################# level 2
errimgs = listImgPred[!listImgPred %in%   fin$img]

if(length(errimgs)==0){write.csv(err, pth_imgs_err, row.names=F)}

if(length(errimgs>0)){

globalBatch1=100
batch_size1=100
fin2=NULL
	glob_step = round(length(errimgs)/globalBatch1)+1
	  for (i in 1:glob_step){
	   if (i==1){start1=1;stop1=globalBatch1}
	     index=c(start1:stop1)
		  listImgBtch1 = errimgs[index]
	      listImgBtch1=listImgBtch1[is.na(listImgBtch1)==F]
		  if (length(listImgBtch1)==0) break
	      dataBtch1 <- tibble::tibble(img = listImgBtch1)
	      Btch_data_set1 <- create_dataset(dataBtch1, train = FALSE, batch_size=batch_size1, vision_dimensions=vision_dimensions)
	  
	  predBtch =tryCatch({
	                                  model %>% predict(Btch_data_set1)
						        fin2=rbind(dataBtch1,	fin2)	  
	  
	    }, error = function(e) {
     message("✗ ", i, " - ", e$message)
	 })
        start1=start1+globalBatch1
	     stop1=stop1+globalBatch1
   }
############################################################# level 3
errimgs1 =errimgs[!errimgs %in%   fin2$img]


if(length(errimgs1)==0){write.csv(err, pth_imgs_err, row.names=F)}

  if(length(errimgs1>0)){
quick_individual_predict <- function(model, listImgPred, vision_dimensions ) {
  results <- data.frame()
  
  for (img_path in listImgPred) { 
    tryCatch({
      img <- magick::image_read(img_path)
      img <- magick::image_resize(img, paste0(vision_dimensions, "x", vision_dimensions))
      img_array <- as.numeric(img[[1]]) / 255.0
      img_batch <- array(img_array, dim = c(1, vision_dimensions, vision_dimensions, 3))
      pred <- model %>% predict(img_batch)
      
      results <- rbind(results, data.frame(
        img_path = img_path,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      message("✗ ", basename(img_path), " - ", e$message)
    })
  }
  
  return(results)
}

good= quick_individual_predict(model, listImgPred=errimgs1, vision_dimensions) 

err = errimgs1[!errimgs1 %in% good$img_path]

print(err)
write.csv(err, pth_imgs_err, row.names=F)

file.copy(err, dirforimgserr)
unlink(err)

}
}
}
}