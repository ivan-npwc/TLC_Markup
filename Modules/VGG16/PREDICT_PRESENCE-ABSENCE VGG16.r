
  #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/VGG16/PREDICT_PRESENCE-ABSENCE VGG16.r")

#tcltk::tk_choose.files()
#watch -n 1 sensors

         
	 
        library(keras)
        library(magick)
        library(tidyr)
        library(tfdatasets)
	    library(tidyverse)
    	library(reticulate)
       
		
Path_model = "/home/ivan/TRAIN/SSL/sealion_presence_absence_checkpoints/sealion_presence_absence_128_2025-09-29_accuracy_0.97_epoch_48.h5"
Preddir =  "/media/ivan/USATOV_2024/SSL_DB_Tiles"
file_size=2000
batch_size=128
vision_dimensions = 128
Pred_Name= c( "ABSANCE","PRESENCE")
#######################################################################
if(exists("model_split")==F) {
model_split=load_model_hdf5(Path_model)
}
####################################################################
listsites = list.files(Preddir, full.names=T,pattern="Tiles")
for (i in 1:length(listsites)){
 sitedir=listsites[i]
 
 savesitedir = gsub("Tiles","Presence",basename(sitedir))
 savesitedir1 = paste0(Preddir,"/",savesitedir)
 dir.create(savesitedir1,showWarnings=F)
 
 daysdir=list.files(sitedir,full.names=T)
   for (y in 1:length(daysdir)){
 day = daysdir[y]
 PresenceDir = paste0(savesitedir1,"/",basename(day))
 dir.create(PresenceDir,showWarnings=F)
 PesAbsPth =paste0(PresenceDir,"/sealion_presence_absence.csv")
 listImgPred <- list.files(day, full.names=T,pattern="JPG")
 if (length(listImgPred)==0) {print("No Imgs Found");next}
 ###############################################################
            inf=data.frame(listImgPred=listImgPred,file_size=as.numeric(file.size(listImgPred)))
			listImgPred=inf$listImgPred[inf$file_size > file_size]
			exl=length(inf$listImgPred[inf$file_size < file_size])
			print(paste0("Exlude  ", exl, " Images and for predict available  ", length (listImgPred) , "   Images"))
 ###############################################################
 if (file.exists(PesAbsPth))  {tbl1=read.csv(PesAbsPth)
 if (length(listImgPred)== length(tbl1[,1])){print(paste0("SKIP    ",PresenceDir)); next}
     }
############################################################################
data <- tibble::tibble(img = listImgPred)
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
    pred_data_set <- create_dataset(data, train = FALSE, batch_size=batch_size, vision_dimensions=vision_dimensions)
    pred = model_split %>% predict(pred_data_set)
###############################################################################################
    preds3 <<- data.frame(pred)
	names(preds3) <- Pred_Name
	preds3$link=listImgPred
	preds3$PRESENCE= preds3$PRESENCE
for (i in 1:length(listImgPred)) {
   if(preds3[,1][i]> preds3[,2][i]){preds3$name[i]=Pred_Name[1]}
   if(preds3[,1][i]< preds3[,2][i]){preds3$name[i]=Pred_Name[2]}
   }
   
pres=preds3$link[preds3$name=="PRESENCE"]
file.copy(pres,PresenceDir)
write.csv(preds3,PesAbsPth, row.names=F)
print(paste0("DONE sealion_presence:   ",  length(pres)," / ", length(listImgPred),"                    ",     day))

}
}






