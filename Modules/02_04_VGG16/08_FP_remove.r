
  #source("/home/npwc/GIT/TLC_Markup/Modules/08_FP_remover.r")

#tcltk::tk_choose.files()
#watch -n 1 sensors



        library(keras)
        library(magick)
        library(tidyr)
        library(tfdatasets)
		library(tidyverse)
    	library(reticulate)


Path_model_br = "/home/npwc/GIT/TLC MarkUp System data/Models/branded_not_branded_128_2025-10-13_accuracy_0.92_epoch_97.h5"
Preddir  =  "/mnt/adata8tb/SSL_DB_Tiles"
batch_size=128
vision_dimensions = 256
Pred_Name= c( "BRENDED","NONBRENDED")
#######################################################################
if(exists("model_split_brand")==F) {
model_split_brand =load_model_hdf5(Path_model_br)
}
####################################################################
listsites = list.files(Preddir, full.names=T,pattern="Search256")
for (i in 1:length(listsites)){

 listImgPred <- list.files(listsites[i], full.names=T,pattern="CROP", recursive=T)
# if (length(listImgPred)==0) {print("No Imgs Found");next}
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
	# if (length(listImgPred) <2 ) next
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

FP = preds3$link[preds3$name=="NONBRENDED"]

FPpth = gsub("CROP","FP",FP)
file.copy(FP,FPpth)
#unlink(FP)
print(paste0("DONE sealion_FP:   ", listsites[i] )



}








