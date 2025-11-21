
#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/SEARCH ML/DeepLabV3_MobileNetV2_train.r")

library(reticulate)
library(tensorflow)
library(keras)
library(tfdatasets)
library(tidyverse)

# Параметры
trainDir <- "/home/ivan/TRAIN/SSL_br_searc_256"
images_dir <- file.path(trainDir, "Image")
masks_dir <- file.path(trainDir, "Mask")

epochs <- 200
batch_size <- 16L
img_height <- 256L
img_width <- 256L
validation_split <- 0.2

# Проверка данных
cat("Images found:", length(list.files(images_dir)), "\n")
cat("Masks found:", length(list.files(masks_dir)), "\n")

# Создание датафрейма с путями
image_files <- list.files(images_dir, full.names = TRUE, pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE)
mask_files <- list.files(masks_dir, full.names = TRUE, pattern = "\\.(png|jpg|jpeg)$", ignore.case = TRUE)

# Проверка соответствия имен
get_base_name <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

image_names <- sapply(image_files, get_base_name)
mask_names <- sapply(mask_files, get_base_name)

# Находим общие имена
common_names <- intersect(image_names, mask_names)
cat("Common image-mask pairs:", length(common_names), "\n")

if (length(common_names) == 0) {
  stop("No matching image-mask pairs found!")
}

# Фильтруем файлы по общим именам
image_files <- image_files[image_names %in% common_names]
mask_files <- mask_files[mask_names %in% common_names]

# Упорядочиваем по именам
image_files <- image_files[order(image_names[image_names %in% common_names])]
mask_files <- mask_files[order(mask_names[mask_names %in% common_names])]

data_df <- data.frame(
  image = image_files,
  mask = mask_files,
  stringsAsFactors = FALSE
)

# Предобработка
preprocess_image <- function(image_path) {
  image <- tf$io$read_file(image_path)
  image <- tf$image$decode_image(image, channels = 3, expand_animations = FALSE)
  image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  image <- tf$image$resize(image, size = c(img_height, img_width))
  return(image)
}

preprocess_mask <- function(mask_path) {
  mask <- tf$io$read_file(mask_path)
  mask <- tf$image$decode_image(mask, channels = 1, expand_animations = FALSE)
  mask <- tf$image$convert_image_dtype(mask, dtype = tf$float32)
  mask <- tf$image$resize(mask, size = c(img_height, img_width))
  mask <- tf$round(mask)
  return(mask)
}

# Создание датасета
create_dataset <- function(df, batch_size, shuffle = FALSE, augment = FALSE) {
  
  dataset <- tensor_slices_dataset(list(df$image, df$mask)) %>%
    dataset_map(function(image_path, mask_path) {
      image <- preprocess_image(image_path)
      mask <- preprocess_mask(mask_path)
      list(image, mask)
    }) %>%
    dataset_map(function(image, mask) {
      image <- tf$ensure_shape(image, list(img_height, img_width, 3L))
      mask <- tf$ensure_shape(mask, list(img_height, img_width, 1L))
      list(image, mask)
    })
  
  if (shuffle) {
    dataset <- dataset %>% dataset_shuffle(buffer_size = nrow(df))
  }
  
  #########################################
    if (augment) {
    dataset <- dataset %>% 
      dataset_map(function(image, mask) {
        # Случайное отражение по горизонтали
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            list(tf$image$flip_left_right(image), tf$image$flip_left_right(mask))
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]
        mask <- result[[2]]
        
        # Flip up-down
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            list(tf$image$flip_up_down(image), tf$image$flip_up_down(mask))
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]
        mask <- result[[2]]
        
        # Random zoom
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            scale_factor <- runif(1, 0.8, 1.2)  # Меньший диапазон для сохранения деталей
            new_height <- as.integer(img_height * scale_factor)
            new_width <- as.integer(img_width * scale_factor)
            downscaled_image <- tf$image$resize(image, size = c(new_height, new_width))
            downscaled_mask <- tf$image$resize(mask, size = c(new_height, new_width))
            list(
              tf$image$resize(downscaled_image, size = c(img_height, img_width)),
              tf$image$resize(downscaled_mask, size = c(img_height, img_width))
            )
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]
        mask <- result[[2]]
        
        # Яркость (только для изображения)
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_brightness(image, max_delta = 0.2),
          false_fn = function() image
        )
        
        # Контраст (только для изображения)
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_contrast(image, lower = 0.8, upper = 1.2),
          false_fn = function() image
        )
        
        list(image, mask)
      })
  }
  
  ######################################################3
  
  dataset <- dataset %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE)
  
  return(dataset)
}

# Разделение на train/validation
set.seed(123)
train_indices <- sample(1:nrow(data_df), size = round((1 - validation_split) * nrow(data_df)))
train_df <- data_df[train_indices, ]
val_df <- data_df[-train_indices, ]

cat("Training samples:", nrow(train_df), "\n")
cat("Validation samples:", nrow(val_df), "\n")

# Создание датасетов
train_dataset <- create_dataset(train_df, batch_size, shuffle = TRUE, augment = TRUE)
val_dataset <- create_dataset(val_df, batch_size, shuffle = FALSE, augment = FALSE)

################################################################
# ПОЛНАЯ АРХИТЕКТУРА DeepLabV3 с MobileNetV2
create_deeplabv3_mobilenet <- function(input_shape = c(256, 256, 3), num_classes = 1, output_stride = 16) {
  
  inputs <- layer_input(shape = input_shape)
  
  # MobileNetV2 backbone
  base_model <- application_mobilenet_v2(
    weights = "imagenet",
    include_top = FALSE,
    input_tensor = inputs
  )
  
  # Получаем выход энкодера и low-level features
  encoder_output <- base_model$output  # Для 256x256: 8x8 при output_stride=32
  low_level_features <- base_model$get_layer("block_3_expand_relu")$output  # 64x64
  
  # Если нужен output_stride=16, модифицируем последние слои
  if (output_stride == 16) {
    # Находим последние слои MobileNetV2 и меняем страйд
    x <- base_model$get_layer("block_16_project_BN")$output
    
    # Добавляем дополнительные слои без downsampling
    x <- x %>%
      layer_conv_2d(320, 1, padding = "same", use_bias = FALSE) %>%
      layer_batch_normalization() %>%
      layer_activation("relu")
    
    encoder_output <- x  # Теперь 16x16 для 256x256 входа
  }
  
  cat("DeepLabV3 Architecture:\n")
  cat("Input shape:", input_shape, "\n")
  cat("Encoder output shape:", dim(encoder_output), "\n")
  cat("Low-level features shape:", dim(low_level_features), "\n")
  cat("Output stride:", output_stride, "\n")
  
  # ASPP (Atrous Spatial Pyramid Pooling) модуль
  aspp_output <- aspp_module(encoder_output, output_stride)
  
  # DeepLabV3 (без декодера) - только ASPP
  if (output_stride == 16) {
    # Для output_stride=16, финальный upsampling 16x
    x <- aspp_output %>%
      layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
      layer_batch_normalization() %>%
      layer_activation("relu") %>%
      layer_dropout(0.1) %>%
      layer_upsampling_2d(size = 16, interpolation = "bilinear")
  } else {
    # Для output_stride=32, финальный upsampling 32x
    x <- aspp_output %>%
      layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
      layer_batch_normalization() %>%
      layer_activation("relu") %>%
      layer_dropout(0.1) %>%
      layer_upsampling_2d(size = 32, interpolation = "bilinear")
  }
  
  # Финальный слой классификации
  outputs <- x %>%
    layer_conv_2d(num_classes, 1, activation = "sigmoid")
  
  model <- keras_model(inputs = inputs, outputs = outputs)
  return(model)
}

# ASPP модуль для DeepLabV3
aspp_module <- function(input_tensor, output_stride) {
  input_filters <- dim(input_tensor)[4]
  
  # Branch 1: 1x1 convolution
  branch_1x1 <- input_tensor %>%
    layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 2: 3x3 convolution with rate = 6
  branch_3x3_r6 <- input_tensor %>%
    layer_conv_2d(256, 3, padding = "same", dilation_rate = 6, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 3: 3x3 convolution with rate = 12
  branch_3x3_r12 <- input_tensor %>%
    layer_conv_2d(256, 3, padding = "same", dilation_rate = 12, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 4: 3x3 convolution with rate = 18
  branch_3x3_r18 <- input_tensor %>%
    layer_conv_2d(256, 3, padding = "same", dilation_rate = 18, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 5: Image Pooling
  branch_pool <- input_tensor %>%
    layer_global_average_pooling_2d() %>%
    layer_reshape(c(1, 1, input_filters)) %>%
    layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_upsampling_2d(size = dim(input_tensor)[2:3], interpolation = "bilinear")
  
  # Concatenate all branches
  concatenated <- layer_concatenate(list(
    branch_1x1,
    branch_3x3_r6, 
    branch_3x3_r12,
    branch_3x3_r18,
    branch_pool
  ))
  
  # Final projection
  output <- concatenated %>%
    layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1)
  
  return(output)
}

# DeepLabV3+ архитектура (с декодером)
create_deeplabv3_plus_mobilenet <- function(input_shape = c(256, 256, 3), num_classes = 1) {
  
  inputs <- layer_input(shape = input_shape)
  
  # MobileNetV2 backbone
  base_model <- application_mobilenet_v2(
    weights = "imagenet",
    include_top = FALSE,
    input_tensor = inputs
  )
  
  # Encoder features
  encoder_output <- base_model$output  # 8x8
  low_level_features <- base_model$get_layer("block_3_expand_relu")$output  # 64x64
  
  cat("DeepLabV3+ Architecture:\n")
  cat("Input shape:", input_shape, "\n")
  cat("Encoder output shape:", dim(encoder_output), "\n")
  cat("Low-level features shape:", dim(low_level_features), "\n")
  
  # ASPP модуль
  aspp_output <- aspp_module(encoder_output, output_stride = 32)
  
  # Декодер DeepLabV3+
  
  # Обработка low-level features
  low_level_processed <- low_level_features %>%
    layer_conv_2d(48, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Апсэмплинг ASPP output до размера low-level features
  aspp_upsampled <- aspp_output %>%
    layer_upsampling_2d(size = 4, interpolation = "bilinear")  # 8x8 -> 32x32
  
  # Нужно дополнительно апсэмплить до 64x64
  aspp_upsampled <- aspp_upsampled %>%
    layer_upsampling_2d(size = 2, interpolation = "bilinear")  # 32x32 -> 64x64
  
  # Конкатенация
  concatenated <- layer_concatenate(list(aspp_upsampled, low_level_processed))
  
  # Декодерные свертки
  x <- concatenated %>%
    layer_conv_2d(256, 3, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1) %>%
    
    layer_conv_2d(256, 3, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1) %>%
    
    layer_upsampling_2d(size = 4, interpolation = "bilinear") %>%  # 64x64 -> 256x256
    layer_conv_2d(num_classes, 1, activation = "sigmoid")
  
  model <- keras_model(inputs = inputs, outputs = x)
  return(model)
}

# Упрощенная DeepLabV3 для быстрого старта
create_simple_deeplabv3 <- function(input_shape = c(256, 256, 3), num_classes = 1) {
  
  inputs <- layer_input(shape = input_shape)
  
  # MobileNetV2 backbone
  base_model <- application_mobilenet_v2(
    weights = "imagenet",
    include_top = FALSE,
    input_tensor = inputs
  )
  
  # Encoder output
  encoder_output <- base_model$output  # 8x8
  
  cat("Simple DeepLabV3 Architecture:\n")
  cat("Input shape:", input_shape, "\n")
  cat("Encoder output shape:", dim(encoder_output), "\n")
  
  # Упрощенный ASPP с меньшим количеством ветвей
  # Branch 1: 1x1
  branch1 <- encoder_output %>%
    layer_conv_2d(128, 1, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 2: 3x3 rate=6
  branch2 <- encoder_output %>%
    layer_conv_2d(128, 3, padding = "same", dilation_rate = 6) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")
  
  # Branch 3: Global pooling
  branch3 <- encoder_output %>%
    layer_global_average_pooling_2d() %>%
    layer_reshape(c(1, 1, dim(encoder_output)[4])) %>%
    layer_conv_2d(128, 1, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_upsampling_2d(size = dim(encoder_output)[2:3], interpolation = "bilinear")
  
  # Concatenate
  concatenated <- layer_concatenate(list(branch1, branch2, branch3))
  
  # Final layers
  x <- concatenated %>%
    layer_conv_2d(128, 1, padding = "same") %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_upsampling_2d(size = 32, interpolation = "bilinear") %>%  # 8x8 -> 256x256
    layer_conv_2d(num_classes, 1, activation = "sigmoid")
  
  model <- keras_model(inputs = inputs, outputs = x)
  return(model)
}

# Создание модели
cat("Creating DeepLabV3 model...\n")

# Выберите архитектуру:
# model <- create_deeplabv3_mobilenet(input_shape = c(img_height, img_width, 3), output_stride = 16)
 model <- create_deeplabv3_plus_mobilenet(input_shape = c(img_height, img_width, 3))
#model <- create_simple_deeplabv3(input_shape = c(img_height, img_width, 3))

# Функции потерь и метрик
dice_coef <- custom_metric("dice_coef", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

dice_coef_loss <- function(y_true, y_pred) -dice_coef(y_true, y_pred)

# Компиляция модели
cat("Compiling DeepLabV3 model...\n")
model %>% compile(
  optimizer = optimizer_adam(learning_rate = 1e-3),
  loss = dice_coef_loss,
  metrics = dice_coef
)

# Колбэки
checkpoint_dir <- file.path(trainDir, "checkpoints_deeplabv3")
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
filepath <- file.path(checkpoint_dir, "deeplabv3_val_dice_{val_dice_coef:.3f}_epoch_{epoch:02d}.h5")

callbacks <- list(
  callback_model_checkpoint(
    filepath = filepath,
    monitor = "val_dice_coef",
    save_best_only = TRUE,
    mode = "max",
    verbose = 1
  ),
  callback_reduce_lr_on_plateau(
    monitor = "val_dice_coef",
    factor = 0.5,
    patience = 8,
    verbose = 1,
    mode = "max",
    min_lr = 1e-6
  ),
  callback_early_stopping(
    monitor = "val_dice_coef",
    patience = 15,
    verbose = 1,
    mode = "max",
    restore_best_weights = TRUE
  )
)

# Обучение модели
cat("Starting DeepLabV3 training...\n")
cat("Model architecture: DeepLabV3 with MobileNetV2\n")
cat("Input size:", img_height, "x", img_width, "\n")

history <- model %>% fit(
  train_dataset,
  epochs = epochs,
  validation_data = val_dataset,
  callbacks = callbacks,
  verbose = 1
)

# Сохранение финальной модели
final_model_path <- file.path(checkpoint_dir, "final_deeplabv3_segmentation.h5")
model %>% save_model_hdf5(final_model_path)
cat("Training completed! Final model saved to:", final_model_path, "\n")

# Дообучение с размороженными слоями
#cat("Starting fine-tuning...\n")

# Находим и размораживаем базовую модель
#for (layer in model$layers) {
#  if (grepl("mobilenet", layer$name)) {
#    layer$trainable <- TRUE
#  }
#}

#model %>% compile(
 # optimizer = optimizer_adam(learning_rate = 1e-5),
  #loss = dice_coef_loss,
  #metrics = dice_coef
#)

#history_finetune <- model %>% fit(
#  train_dataset,
 # epochs = 20,
 # validation_data = val_dataset,
#  callbacks = callbacks,
  #verbose = 1
#)

# Сохранение дообученной модели
#finetuned_model_path <- file.path(checkpoint_dir, "finetuned_deeplabv3_segmentation.h5")
#model %>% save_model_hdf5(finetuned_model_path)
#cat("Fine-tuning completed! Model saved to:", finetuned_model_path, "\n")