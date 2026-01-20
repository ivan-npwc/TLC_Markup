
#source("/home/ivan/GIT_HUB/TLC_Markup/Modules/SEARCH ML/DeepLabV3_MobileNetV2_create.r")
library(reticulate)
library(tensorflow)
library(keras)
library(tfdatasets)
library(tidyverse)

img_height=256
 img_width = 256
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

