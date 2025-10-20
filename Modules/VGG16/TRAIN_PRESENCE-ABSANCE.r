#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/VGG16/TRAIN_PRESENCE-ABSANCE.r")

library(keras)
library(magick)

#tcltk::tk_choose.files()
train_Gen_dir = "/home/ivan/TRAIN/SSL/branded_not_branded"
train_dir = train_Gen_dir
checkpoint_dir = "/home/ivan/TRAIN/SSL/branded_not_branded"				
NewModelCreate = T
BaseModel_pth = ""
Species = "branded_not_branded_384"
bth_size = 32
trgt_size = 384
#epochs=100
validation_split = 0.2
							
dateTrain = substr(Sys.time(),1,10)				
AllImg = length(list.files(train_dir, recursive=T))/2
train_step = round(AllImg/bth_size*1* (1-validation_split))
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
  # Базовые трансформации
 # horizontal_flip = T, 
  #vertical_flip = T,
  #rotation_range = 25,  # увеличен угол вращения
  #width_shift_range = 0.25,  # увеличен сдвиг
  #height_shift_range = 0.25,
  #shear_range = 0.3,  # увеличен сдвиг
  #zoom_range = 0.3,   # увеличен зум
  #brightness_range = c(0.3, 1.8),  # расширен диапазон яркости
  #channel_shift_range = 0.3,       # увеличен сдвиг каналов
  # Новые аугментации для затенения и бликов
  #fill_mode = "reflect",
  validation_split = validation_split
)



val_datagen = image_data_generator(rescale = 1/255, validation_split=validation_split)

##########################################################
train_generator <- flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(trgt_size, trgt_size),
  batch_size = bth_size,
  class_mode = "categorical",
  #preprocessing_function = custom_augmentation,  # раскомментировать для сложной аугментации
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
create_augmented_model <- function(input_shape) {
  
  inputs <- layer_input(shape = input_shape)
  
  # ВСТРОЕННЫЕ СЛОИ АУГМЕНТАЦИИ
  augmented <- inputs %>%
    # Аугментации геометрии
    layer_random_flip(mode = "horizontal_and_vertical") %>%
    layer_random_rotation(factor = 0.2) %>%
    layer_random_zoom(height_factor = 0.2, width_factor = 0.2) %>%
    layer_random_translation(height_factor = 0.2, width_factor = 0.2) %>%
    
    # Аугментации цвета и освещения
    layer_random_brightness(factor = 0.3) %>%
    layer_random_contrast(factor = 0.3)
  
  # Базовая модель ResNet
  conv_base <- application_resnet50(
    weights = "imagenet",
    include_top = FALSE,
    input_tensor = augmented
  )
  
  # Классификационная головка
  outputs <- conv_base$output %>%
    layer_global_average_pooling_2d() %>% 
    layer_dense(units = 512, activation = "relu", name = "fc3") %>% 
    layer_batch_normalization() %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 256, activation = "relu", name = "fc4") %>%   
    layer_batch_normalization() %>%  
    layer_dropout(rate = 0.3) %>% 
    layer_dense(units = 128, activation = "relu", name = "fc5") %>%
    layer_batch_normalization() %>%     
    layer_dense(units = length(list.dirs(train_dir, recursive = FALSE)), 
    activation = "softmax", name = "predictions")
  
  model <- keras_model(inputs, outputs)
  return(model)
}

# Создание модели с аугментацией
modelTrain = create_augmented_model(input_shape=c(trgt_size, trgt_size, 3))


for(i in 8:181) {
  freeze_weights(modelTrain$layers[[i]])
}



modelTrain %>% compile(
	       optimizer = optimizer_adam(learning_rate = 0.0001), 
	       loss = "categorical_crossentropy",
	       metrics = c("accuracy"))
##########################
modelTrain %>% fit(
  train_generator,
  steps_per_epoch = train_step,
  epochs = 10,
  validation_data = validation_generator,
  validation_steps = val_step,
  callbacks = list(cp_callback)) 

################а
for(i in 88:181) {
  unfreeze_weights(modelTrain$layers[[i]])
}
################################

modelTrain %>% compile(
	       optimizer = optimizer_adam(learning_rate = 0.0001), 
	       loss = "categorical_crossentropy",
	       metrics = c("accuracy"))					
								
modelTrain %>% fit(
  train_generator,
  steps_per_epoch = train_step,
  epochs = 200,
  validation_data = validation_generator,
  validation_steps = val_step,
  callbacks = list(cp_callback))