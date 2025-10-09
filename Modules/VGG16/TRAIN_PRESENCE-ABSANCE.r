        #source("/home/ivan/GIT_HUB/TLC_Markup/Modules/VGG16/TRAIN_PRESENCE-ABSANCE.r")
        
        
        
        
        library(keras)
        library(magick)

	#tcltk::tk_choose.files()
	train_Gen_dir=   "/home/ivan/TRAIN/SSL/branded_not_branded"
        train_dir=train_Gen_dir
	checkpoint_dir = "/home/ivan/TRAIN/SSL/branded_not_branded"				
	NewModelCreate=T
	BaseModel_pth =  ""
	Species="branded_not_branded_128"
	bth_size =64
	trgt_size =256
	#epochs=100
	validation_split=0.8
							
   dateTrain = substr(Sys.time(),1,10)				
   AllImg=length(list.files(train_dir, recursive=T))/2
   train_step=round(AllImg/bth_size*1* (1-validation_split))
   val_step = round(AllImg/bth_size*1* (validation_split))
#####################################
dir.create(checkpoint_dir,showWarnings=F)	
   early_stopping <<- callback_early_stopping(patience = 5)   # 5 epoch for check if regress exists
   cp_callback <- callback_model_checkpoint( 
   filepath = paste0(checkpoint_dir, "/",Species, "_",dateTrain,"_", basename(file.path(checkpoint_dir, "accuracy_{val_accuracy:.2f}_epoch_{epoch:02d}.h5"))),
   period = 2,
   verbose = 1)  
############################################
	train_datagen = image_data_generator(
	  rescale = 1/255,
	 horizontal_flip = T, vertical_flip = T,
	  rotation_range = 15,
	  width_shift_range = 0.2,
	  height_shift_range = 0.2,
	  shear_range = 0.2,
	 zoom_range = 0.2,
    #     brightness_range=c(1,3),
	  fill_mode = "nearest",
	  validation_split=validation_split
	)
	val_datagen=image_data_generator(rescale = 1/255,validation_split=validation_split)
	##########################################################
	train_generator <- flow_images_from_directory(
	  train_dir,
	  train_datagen,
	  target_size = c(trgt_size, trgt_size),
	  batch_size = bth_size,
	  class_mode = "categorical",
	 subset="training")
	#############################################################
	validation_generator <- flow_images_from_directory(
	  train_dir,
	  val_datagen,
	  target_size = c(trgt_size, trgt_size),
	  batch_size = bth_size,
	  class_mode = "categorical",
	  subset="validation"
	)
	##############################################################################################
		  conv_base <- application_vgg16(
		   weights = "imagenet",
		   include_top = FALSE,
		   input_shape = c(trgt_size, trgt_size, 3))
		   
    modelTrain <<- keras_model_sequential() %>%
    conv_base %>%
    layer_dropout(rate = 0.3) %>% 
    layer_flatten() %>%  
    layer_dense(units = 512, activation = "relu",name = "fc3") %>% 
    layer_batch_normalization() %>% 
	layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 256, activation = "relu",name = "fc4") %>%   
    layer_batch_normalization() %>%   
	layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 128, activation = "relu",name = "fc5") %>%
    layer_batch_normalization() %>%     
    layer_dense(units = train_generator$num_classes, activation = "softmax",name = "predictions")
	 
	freeze_weights(conv_base)
	
	modelTrain %>% compile(
		       optimizer =    optimizer_adam(learning_rate= 0.0001 ), 
		       loss = "categorical_crossentropy",
		       metrics = c("accuracy"))
	modelTrain
	###########################
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch = train_step,
	  epochs = 10,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(cp_callback)) 

	#################################
	unfreeze_weights(conv_base, from = "block3_conv1")
	modelTrain
	modelTrain %>% compile(
		       optimizer =    optimizer_adam(learning_rate= 0.0001), 
		       loss = "categorical_crossentropy",
		       metrics = c("accuracy"))					
##########################################################								
	 modelTrain %>% fit(
	  train_generator,
	  steps_per_epoch =   train_step,
	  epochs = 200,
	  validation_data = validation_generator,
	  validation_steps = val_step,
	  callbacks = list(cp_callback))
	  						 
