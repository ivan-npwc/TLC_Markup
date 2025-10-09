
  #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/VGG16/PREDICT_BRANDED-NONBRANDED_VGG16.r")

#tcltk::tk_choose.files()
#watch -n 1 sensors

         
	 
        library(keras)
        library(magick)
        library(tidyr)
        library(tfdatasets)
	    library(tidyverse)
    	library(reticulate)
       
		
Path_model_br = "/home/ivan/GIT_HUB/TLC MarkUp System data/Models/branded_not_branded_128_2025-10-07_accuracy_0.81_epoch_133.h5"
Preddir =  "/media/ivan/USATOV_2024/SSL_DB_Tiles"
file_size=2000
batch_size=128
vision_dimensions = 256
Pred_Name= c( "BRENDED","NONBRENDED")
#######################################################################
if(exists("model_split_brand")==F) {
model_split_brand =load_model_hdf5(Path_model_br)
}
####################################################################
listsites = list.files(Preddir, full.names=T,pattern="Presence")
for (i in 1:length(listsites)){
 sitedir=listsites[i]
 
 savesitedir = gsub("Presence","Branded",basename(sitedir))
 savesitedir1 = paste0(Preddir,"/",savesitedir)
 dir.create(savesitedir1,showWarnings=F)
 
 daysdir=list.files(sitedir,full.names=T)
   for (y in 1:length(daysdir)){
 day = daysdir[y]
 BrandedDir = paste0(savesitedir1,"/",basename(day))
 dir.create(BrandedDir,showWarnings=F)
 PesAbsPth =paste0(BrandedDir,"/sealion_branded_nonbranded.csv")
 listImgPred <- list.files(day, full.names=T,pattern="JPG")
 if (length(listImgPred)==0) {print("No Imgs Found");next}
 data <- tibble::tibble(img = listImgPred)
 ###############################################################
            inf=data.frame(listImgPred=listImgPred,file_size=as.numeric(file.size(listImgPred)))
			listImgPred=inf$listImgPred[inf$file_size > file_size]
			exl=length(inf$listImgPred[inf$file_size < file_size])
			print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImgPred) , "   Images"))
 ###############################################################
 if (file.exists(PesAbsPth))  {tbl1=read.csv(PesAbsPth)
 if (length(listImgPred)== length(tbl1[,1])){print(paste0("SKIP    ",BrandedDir)); next}
     }
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
		pred = model_split_brand %>% predict(pred_data_set)
###############################################################################################
    preds3 <<- data.frame(pred)
	names(preds3) <- Pred_Name
	preds3$link=listImgPred
	preds3$BRENDED= preds3$BRENDED
for (w in 1:length(listImgPred)) {
   if(preds3[,1][w]> preds3[,2][w]){preds3$name[w]=Pred_Name[1]}
   if(preds3[,1][w]< preds3[,2][w]){preds3$name[w]=Pred_Name[2]}
   }
   
pres=preds3$link[preds3$name=="BRENDED"]

file.copy(pres,BrandedDir)
write.csv(preds3,PesAbsPth, row.names=F)
print(paste0("DONE sealion_presence:   ",  length(pres)," / ", length(listImgPred),"                    ",     day))

}
}







