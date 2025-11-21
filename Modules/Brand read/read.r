
  #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/Brand read/read.r")

#tcltk::tk_choose.files()
#watch -n 1 sensors

         
	 
        library(keras)
        library(magick)
        library(tidyr)
        library(tfdatasets)
	    library(tidyverse)
    	library(reticulate)
       
		
Yar_Path_model_br =  "/home/ivan/GIT_HUB/TLC MarkUp System data/Models/Brand read/Yar_read.h5"
Yar_name_pth =         "/home/ivan/GIT_HUB/TLC MarkUp System data/Models/Brand read/Yar_Name.csv"

Preddir  =  "/mnt/adata8tb/SSL_DB_Tiles"
file_size=2000
batch_size=128
vision_dimensions = 95
siteselect=c()
####################################################################
listsites = list.files(Preddir, full.names=T,pattern="Search256")
for (i in 1:length(listsites)){
 sitedir =listsites[i]
  bsnm = basename(sitedir)
  site = strsplit(bsnm, "_")[[1]][2]
  if (site %in% siteselect == F) next
  year = strsplit(bsnm, "_")[[1]][1]
  map=paste0(year,"_",site,"_Map")
 if (site==145){model_read_brand =load_model_hdf5(Yar_Path_model_br)
                       Pred_Name= read.csv(Yar_name_pth)

 } else {next}
 
 savesitedir = gsub("Search256","Brand_read",basename(sitedir))
 savesitedir1 = file.path(Preddir,savesitedir)
 #unlink(savesitedir1, recursive=T)
 dir.create(savesitedir1,showWarnings=F)
 
 daysdir=list.files(sitedir,full.names=T)
   for (y in 1:length(daysdir)){
 day = daysdir[y]
 
 #BrandedDir = paste0(savesitedir1,"/",basename(day))
 #dir.create(BrandedDir,showWarnings=F)

 listImgPred <- list.files(day, full.names=T,pattern="CROP")
 if (length(listImgPred)==0) {print("No Imgs Found");next}
 print(day)

 ###############################################################
            inf=data.frame(listImgPred=listImgPred,file_size=as.numeric(file.size(listImgPred)))
			listImgPred=inf$listImgPred[inf$file_size > file_size]
			exl=length(inf$listImgPred[inf$file_size < file_size])
			print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImgPred) , "   Images"))
			 data <- tibble::tibble(img = listImgPred)
############################################################################
	create_dataset <- function(data, train, batch_size = batch_size, vision_dimensions) {
	 
	dataset <- data %>% 
		tensor_slices_dataset() %>% 
		dataset_map(~.x %>% list_modify(
		  img = tf$io$read_file(.x$img)
	)) %>%  
	 dataset_map(~.x %>% list_modify(
		                   img = tf$image$decode_jpeg(.x$img,channels = 3)
					)) 	%>%  
		dataset_map(~.x %>% list_modify(
		  img = tf$image$convert_image_dtype(.x$img, dtype = tf$float64)
		)) 	%>%  dataset_map(~.x %>% list_modify(
		  img = tf$image$resize(.x$img, size = shape(vision_dimensions, vision_dimensions))
		)) %>% 
	                                            dataset_batch(batch_size) %>%
                        	                 #   dataset_cache() %>%
	                                             dataset_map(unname) # Keras needs an unnamed output.
	                                        #   dataset_shuffle(buffer_size = batch_size*vision_dimensions)
	}
	################################
	if (length(listImgPred) <2 ) next
		pred_data_set <- create_dataset(data, train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
		pred = model_read_brand %>% predict(pred_data_set)
###############################################################################################
   predicted_classes <- apply(pred, 1, which.max)
   predicted_names <- Pred_Name[predicted_classes,]
   preddt =data.frame(listImgPred=listImgPred,predicted_names=predicted_names)
   unuqbrand = unique(predicted_names)
    for (b in 1:length(unuqbrand)){
     brand = unuqbrand[b]
	 imgs = preddt$listImgPred[preddt$predicted_names == brand]
	 copydirto=file.path(savesitedir1,brand)
	 dir.create(copydirto,showWarnings=F)
   file.copy(imgs,copydirto)
#write.csv(preds3,PesAbsPth, row.names=F)


}
}
print(paste0("DONE sealion_read brand:   ",  day))
}






