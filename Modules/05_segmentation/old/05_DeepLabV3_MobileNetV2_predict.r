  
  #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/SEARCH ML/DeepLabV3_MobileNetV2_predict.r")

  
  library(reticulate)
  library("tensorflow")
  library("abind")
  library("doParallel")
  library("foreach")
  library("tfdatasets")
  library(tools)
  library(doMC) 
  library(EBImage)
  library(keras)
  library(parallel)
  library(futile.logger)
  
                     deleteDONE=F
					 check = T
                     Preddir  =  "/mnt/adata8tb/SSL_DB_Tiles"
					 weight_pth =  "/home/ivan/GIT_HUB/TLC MarkUp System data/Models/deeplabv3_val_dice_0.897_epoch_134"
                     vision_dimensions = 256L	
					
###########################
num_physical_cores <- detectCores(logical = FALSE)
num_cores_to_registr  = round(num_physical_cores - 0.2 *  num_physical_cores)
batch_size <- num_cores_to_registr 
batch_size_global = batch_size*10 
registerDoMC(cores = num_cores_to_registr)
computer_name <- Sys.info()["nodename"]				
##############################################################################  
if(exists("model_search_brand")==F) {
source("/home/ivan/GIT_HUB/TLC_Markup/Modules/SEARCH ML/DeepLabV3_MobileNetV2_create.r")
weight=readRDS(weight_pth)
set_weights(model,weight)
}
 ############################################################################  
 listsites = list.files(Preddir, full.names=T,pattern="Branded")
 for (i in 1:length(listsites)){
 sitedir=listsites[i]

 savesitedir = gsub("Branded","Search256",basename(sitedir))
 savesitedir1 = paste0(Preddir,"/",savesitedir)
 if (deleteDONE == T){unlink(savesitedir1, recursive=T)}
 dir.create(savesitedir1,showWarnings=F)
 
 daysdir=list.files(sitedir,full.names=T)
   for (y in 1:length(daysdir)){
 predict_dir = daysdir[y]
  print(predict_dir)
 savePredMskDir = paste0(savesitedir1,"/",basename(predict_dir))
 dir.create(savePredMskDir,showWarnings=F)
#pthcsv_search256 =paste0(savePredMskDir,"/sealion_branded_search256.csv")
 #if (file.exists(pthcsv_search256)) {print(paste0("SKIP   ", savePredMskDir));next}
 
 ###################################################################
  listImage_glob <<-list.files(predict_dir, full.names = T,  recursive = T, include.dirs = F,pattern="png|JPG|jpg|jpeg|JPEG")
  presencePredMsk = length(list.files(savePredMskDir,pattern="png|JPG|jpg|jpeg|JPEG"))
  
   if (length(listImage_glob) == presencePredMsk ){print(paste0("SKIP   ", savePredMskDir));next}
   if (length(listImage_glob)==0)                            {print("No Imgs Found"); next}
   
  global_steps <<- round(length(listImage_glob)/batch_size_global)
  if(length(listImage_glob) > (global_steps*batch_size_global)) {global_steps=global_steps+1}
 ###################################################### 
 create_dataset <- function(data1, batch_size = batch_size, vision_dimensions) {  
  dataset <- data1 %>% 
    tensor_slices_dataset() %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$decode_image(tf$io$read_file(.x$img), channels = 3, expand_animations = FALSE)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)
    )) %>% 
    dataset_map(~.x %>% list_modify(
      img = tf$image$resize(.x$img, size = c(vision_dimensions, vision_dimensions))
    )) 
#	 dataset_map(~.x %>% list_modify(
#    image <-tf$keras$applications$resnet$preprocess_input(.x$img) 	
#	 ))
  dataset <- dataset %>% 
    dataset_batch(batch_size)
  dataset %>% 
    dataset_map(unname) # Keras needs an unnamed output.
}
  #################################################################################
  for (e in 1:global_steps) { 
  batch_ind_global <- c(1:length(listImage_glob))[1:batch_size_global]
  listImage <- listImage_glob[batch_ind_global]
  listImage=listImage[is.na(listImage)==F]
  if (length(listImage_glob) > length(listImage)) {
 # batch_ind_global=batch_ind_global[is.na(batch_ind_global)==F]
    listImage_glob <<- listImage_glob[-batch_ind_global] 
  }
  data1 <<- tibble::tibble(img = listImage)
  if (length(data1$img) == 1){break}
  #######################################################################################
  pred_dataset <- create_dataset(data1, batch_size=min(batch_size,length(listImage)), vision_dimensions=vision_dimensions)
  preds=keras:::predict.keras.engine.training.Model(object=model,
                                                    x=pred_dataset)
    #######################################################################################

    result_batch <- mclapply(listImage, function(imgpth) {
    #imgpth = listImage[1]
     pred_order <- which(listImage == imgpth)
     name=basename(imgpth)
	 MskName=name
     mask0=preds[pred_order, , , ]
	 mask0=t(mask0)
     img = readImage(imgpth)
     width=dim(img)[1]
	 hight=dim(img)[2]
     dim(mask0) <- c(vision_dimensions, vision_dimensions, 1)
     mask1 = getFrame(mask0, 1)
	 mask2=resize(mask1, w = width, h = hight)
	 PthSave=file.path(paste0(savePredMskDir, "/", MskName))
	 writeImage(mask2,PthSave)
	 #################################
	 if (check==T){
	 PthSaveCheck=file.path(paste0(savePredMskDir, "/check_", MskName))
    # Нормализуем маску для визуализации
      #    mask_vis <- normalize(mask2)
mask_vis  <- mask2
# Создаем цветную маску (например, красную)
colored_mask <- array(0, dim = c(dim(mask2)[1:2], 3))
colored_mask[,,1] <- mask_vis  # Красный канал

# Наложение с прозрачностью
alpha <- 0.3  # Прозрачность маски
result <- img * (1 - alpha) + colored_mask * alpha
result_color <- Image(result, colormode = "Color")
	 
	 writeImage(result_color,PthSaveCheck, quality = 90)
	 

	}
	######################################### 
    	
}, mc.cores = min(num_cores_to_registr, length(listImage))  
)


       rm(result_batch)
        gc()
		}
		}
		}
		

