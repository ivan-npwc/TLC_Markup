
#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/Brand read/train.r")
  library(tensorflow)
  library(keras)
  
  shape=95 
  
  epochs=100
  train_dir= "/home/ivan/TRAIN/SSL/Brand read/elastic_perspective_deformation/Yar_standartized_cor"
  batch_size = length(list.dirs(train_dir))
  filepath = file.path(train_dir,"Yar_read.h5")
###############################################################################################

create_model_ensemble <- function(input_shape = c(shape, shape, 3), num_classes) {
  
  # Input layer
  input_layer <- layer_input(shape = input_shape)
  
  # Model 1: EfficientNetB4
  effnet <- application_efficientnet_b4(
    include_top = FALSE,
    weights = "imagenet",
    input_tensor = input_layer
  )
  effnet_output <- effnet$output %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(512, activation = "relu")
  
  # Model 2: ResNet50
  resnet <- application_resnet50(
    include_top = FALSE,
    weights = "imagenet", 
    input_tensor = input_layer
  )
  resnet_output <- resnet$output %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(512, activation = "relu")
  
  # Concatenate and create final layers
  concatenated <- layer_concatenate(list(effnet_output, resnet_output)) %>%
    layer_dropout(0.5) %>%
    layer_dense(256, activation = "relu") %>%
    layer_dropout(0.3) %>%
    layer_dense(num_classes, activation = "softmax")
  
  model <- keras_model(inputs = input_layer, outputs = concatenated)
  return(model)
}


###############################################################################################

  # Data augmentation for training
  train_datagen <- image_data_generator(
    rescale = 1/255,
    rotation_range = 10,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    shear_range = 0.1,
    zoom_range = 0.2,
    horizontal_flip = TRUE,
	brightness_range = c(0.6, 1.5),  
   channel_shift_range = 0.2,       
    fill_mode = "nearest",
    validation_split = 0.2
  )
  
  # Validation data generator (only rescaling)
  val_datagen <- image_data_generator(rescale = 1/255)
  
  # Create generators
  train_generator <- flow_images_from_directory(
    directory = train_dir,
    generator = train_datagen,
    target_size = c(shape, shape),
    batch_size = batch_size,
    class_mode = "categorical",
    subset = "training"
  )
  
  validation_generator <- flow_images_from_directory(
    directory = train_dir,  # or val_dir if separate
    generator = train_datagen,
    target_size = c(shape, shape),
    batch_size = batch_size,
    class_mode = "categorical", 
    subset = "validation"
  )
  
###############################################################

model=create_model_ensemble(num_classes = length(list.dirs(train_dir, recursive = FALSE)))
  
  # Compile model
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "categorical_crossentropy",
    metrics = c("accuracy", "top_k_categorical_accuracy")
  )
  
  # Callbacks
  callbacks <- list(
    callback_model_checkpoint(
      filepath = filepath,
      monitor = "val_accuracy",
      save_best_only = TRUE,
      mode = "max"
    ),
    callback_reduce_lr_on_plateau(
      monitor = "val_loss",
      factor = 0.5,
      patience = 5,
      min_lr = 1e-7
    ),
    callback_early_stopping(
      monitor = "val_loss",
      patience = 15,
      restore_best_weights = TRUE
    )
  )
  
  # Train model
  history <- model %>% fit(
    train_generator,
    epochs = epochs,
    validation_data = validation_generator,
    callbacks = callbacks,
    verbose = 1
  )
  
