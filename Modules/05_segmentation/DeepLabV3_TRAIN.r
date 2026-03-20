#!/usr/bin/env Rscript
# =============================================================================
# DeepLabV3+ с MobileNetV2 backbone — бинарная сегментация животных
# R / keras3 / TensorFlow
# source("train_deeplabv3plus.r")
# Curriculum learning:
#   Фаза 1: только positive кропы (с животными)
#   Фаза 2: постепенное подмешивание negative кропов
#
# Вход:  seg_dataset/positive/{images,masks}  и  seg_dataset/negative/{images,masks}
# Выход: обученная модель .keras + логи обучения
# =============================================================================
library(reticulate)
use_condaenv("r-tf-gpu", required = TRUE)

py_run_string("
import ctypes
wsl_lib = '/usr/lib/wsl/lib'
conda_lib = '/home/ivan/miniconda3/envs/r-tf-gpu/lib'
ctypes.CDLL(f'{wsl_lib}/libcuda.so.1', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{wsl_lib}/libnvidia-ml.so.1', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcublasLt.so.12', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcublas.so.12', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcudnn.so.9', mode=ctypes.RTLD_GLOBAL)
print('All libs preloaded OK')
")

library(tensorflow)
print(tf$config$list_physical_devices("GPU"))


# ======================== НАСТРОЙКИ ==========================================

library(reticulate)
library(tensorflow)
library(keras3)
library(tfdatasets)



# ======================== НАСТРОЙКИ ==========================================

base_dir    <- "/home/ivan/TRAIN/segmentation/train"
data_dir    <- file.path(base_dir, "seg_dataset")
pos_dir     <- file.path(data_dir, "positive")
neg_dir     <- file.path(data_dir, "negative")
model_dir   <- file.path(base_dir, "model_output")
dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)

img_size       <- 224L
batch_size     <- 16L
val_split      <- 0.15

# Фаза 1: только positive, backbone заморожен
phase1_epochs  <- 15L
phase1_lr      <- 1e-3

# Фаза 2: curriculum + fine-tune
phase2_epochs  <- 30L
phase2_lr      <- 5e-4
neg_ratio_start <- 0.05
neg_ratio_end   <- 0.40

set.seed(42)

# ======================== ПРЕДОБРАБОТКА ======================================

preprocess_image <- function(image_path) {
  image <- tf$io$read_file(image_path)
  image <- tf$image$decode_image(image, channels = 3L, expand_animations = FALSE)
  image <- tf$image$convert_image_dtype(image, dtype = tf$float32)
  image <- tf$image$resize(image, size = c(img_size, img_size))
  image
}

preprocess_mask <- function(mask_path) {
  mask <- tf$io$read_file(mask_path)
  mask <- tf$image$decode_image(mask, channels = 1L, expand_animations = FALSE)
  mask <- tf$image$convert_image_dtype(mask, dtype = tf$float32)
  mask <- tf$image$resize(mask, size = c(img_size, img_size), method = "nearest")
  mask <- tf$round(mask)
  mask
}

# ======================== СОЗДАНИЕ ДАТАСЕТА ===================================

create_dataset <- function(image_files, mask_files, batch_size,
                           shuffle = FALSE, augment = FALSE) {

  df <- data.frame(image = image_files, mask = mask_files,
                   stringsAsFactors = FALSE)

  dataset <- tensor_slices_dataset(list(df$image, df$mask)) %>%
    dataset_map(function(image_path, mask_path) {
      image <- preprocess_image(image_path)
      mask  <- preprocess_mask(mask_path)
      list(image, mask)
    }) %>%
    dataset_map(function(image, mask) {
      image <- tf$ensure_shape(image, list(img_size, img_size, 3L))
      mask  <- tf$ensure_shape(mask, list(img_size, img_size, 1L))
      list(image, mask)
    })

  if (shuffle) {
    dataset <- dataset %>% dataset_shuffle(buffer_size = min(nrow(df), 10000L))
  }

  if (augment) {
    dataset <- dataset %>%
      dataset_map(function(image, mask) {

        # Flip horizontal
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() list(tf$image$flip_left_right(image),
                                    tf$image$flip_left_right(mask)),
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]; mask <- result[[2]]

        # Flip vertical
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() list(tf$image$flip_up_down(image),
                                    tf$image$flip_up_down(mask)),
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]; mask <- result[[2]]

        # Random zoom
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() {
            scale <- tf$random$uniform(shape = shape(), minval = 0.8, maxval = 1.2)
            new_h <- tf$cast(tf$cast(img_size, tf$float32) * scale, tf$int32)
            new_w <- new_h
            img_z <- tf$image$resize(image, size = c(new_h, new_w))
            msk_z <- tf$image$resize(mask, size = c(new_h, new_w), method = "nearest")
            list(
              tf$image$resize(img_z, size = c(img_size, img_size)),
              tf$image$resize(msk_z, size = c(img_size, img_size), method = "nearest")
            )
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]; mask <- result[[2]]

        # Brightness
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_brightness(image, max_delta = 0.15),
          false_fn = function() image
        )

        # Contrast
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) > 0.5,
          true_fn = function() tf$image$random_contrast(image, lower = 0.8, upper = 1.2),
          false_fn = function() image
        )

        # === Зимняя аугментация (p=0.3) ===
        result <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) < 0.3,
          true_fn = function() {
            # Десатурация
            gray <- tf$reduce_mean(image, axis = -1L, keepdims = TRUE)
            gray3 <- tf$concat(list(gray, gray, gray), axis = -1L)
            desat <- tf$random$uniform(shape = shape(), minval = 0.3, maxval = 0.7)
            img_w <- image * (1.0 - desat) + gray3 * desat

            # Холодный сдвиг (синий+, красный-)
            cold <- tf$random$uniform(shape = shape(), minval = 0.02, maxval = 0.08)
            channels <- tf$split(img_w, 3L, axis = -1L)
            r <- channels[[1]] - cold * 0.5
            g <- channels[[2]]
            b <- channels[[3]] + cold
            img_w <- tf$concat(list(r, g, b), axis = -1L)

            # Осветление
            bright <- tf$random$uniform(shape = shape(), minval = 0.05, maxval = 0.2)
            img_w <- img_w + bright

            list(img_w, mask)
          },
          false_fn = function() list(image, mask)
        )
        image <- result[[1]]; mask <- result[[2]]

        # === Туман (p=0.2) ===
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) < 0.2,
          true_fn = function() {
            fog <- tf$random$uniform(shape = shape(), minval = 0.05, maxval = 0.25)
            image * (1.0 - fog) + fog * 0.85
          },
          false_fn = function() image
        )

        # Gaussian noise (p=0.2)
        image <- tf$cond(
          tf$random$uniform(shape = shape(), minval = 0, maxval = 1) < 0.2,
          true_fn = function() {
            noise <- tf$random$normal(tf$shape(image), mean = 0.0, stddev = 0.02)
            image + noise
          },
          false_fn = function() image
        )

        image <- tf$clip_by_value(image, 0.0, 1.0)

        list(image, mask)
      })
  }

  dataset <- dataset %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE)

  dataset
}


# ======================== МОДЕЛЬ: DeepLabV3+ =================================

aspp_module <- function(x, output_stride = 32) {
  input_shape <- x$shape

  branch_1x1 <- x %>%
    layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")

  branch_r6 <- x %>%
    layer_conv_2d(256, 3, padding = "same", dilation_rate = 6L, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")

  branch_r12 <- x %>%
    layer_conv_2d(256, 3, padding = "same", dilation_rate = 12L, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")

  branch_r18 <- x %>%
    layer_conv_2d(256, 3, padding = "same", dilation_rate = 18L, use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")

  # Image-level features
  branch_pool <- x %>%
    layer_global_average_pooling_2d() %>%
    layer_reshape(c(1, 1, as.integer(input_shape[[4]]))) %>%
    layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_upsampling_2d(size = c(as.integer(input_shape[[2]]),
                                 as.integer(input_shape[[3]])),
                        interpolation = "bilinear")

  concatenated <- layer_concatenate(list(
    branch_1x1, branch_r6, branch_r12, branch_r18, branch_pool
  ))

  output <- concatenated %>%
    layer_conv_2d(256, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1)

  output
}


create_deeplabv3_plus <- function(input_shape = c(224, 224, 3), num_classes = 1) {

  inputs <- layer_input(shape = input_shape)

  base_model <- application_mobilenet_v2(
    weights      = "imagenet",
    include_top  = FALSE,
    input_tensor = inputs
  )

  encoder_output     <- base_model$output                                    # 12x12
  low_level_features <- base_model$get_layer("block_3_expand_relu")$output   # 96x96

  cat("DeepLabV3+ Architecture:\n")
  cat("  Input:", input_shape, "\n")
  cat("  Encoder output:", dim(encoder_output), "\n")
  cat("  Low-level features:", dim(low_level_features), "\n")

  # ASPP
  aspp_out <- aspp_module(encoder_output, output_stride = 32)

  # Decoder: project low-level
  low_level_proj <- low_level_features %>%
    layer_conv_2d(48, 1, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu")

  # Upsample ASPP: 7 -> 56 = x8
  aspp_up <- aspp_out %>%
    layer_upsampling_2d(size = 8, interpolation = "bilinear")

  concatenated <- layer_concatenate(list(aspp_up, low_level_proj))

  x <- concatenated %>%
    layer_conv_2d(256, 3, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1) %>%
    layer_conv_2d(256, 3, padding = "same", use_bias = FALSE) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dropout(0.1) %>%
    layer_upsampling_2d(size = 4, interpolation = "bilinear") %>%  # 56 -> 224
    layer_conv_2d(num_classes, 1, activation = "sigmoid")

  model <- keras_model(inputs = inputs, outputs = x)
  model
}


# ======================== LOSS И МЕТРИКИ =====================================

# Метрики и функция потерь
# ============================================================
py_run_string("
import tensorflow as tf

@tf.function
def dice_coef(y_true, y_pred):
    smooth = 1.0
    y_true_f = tf.reshape(y_true, [-1])
    y_pred_f = tf.reshape(y_pred, [-1])
    intersection = tf.reduce_sum(y_true_f * y_pred_f)
    return (2.0 * intersection + smooth) / (tf.reduce_sum(y_true_f) + tf.reduce_sum(y_pred_f) + smooth)

def dice_loss(y_true, y_pred):
    return 1.0 - dice_coef(y_true, y_pred)
")

dice_coef <- py$dice_coef
dice_loss <- py$dice_loss

# ============================================================

# ======================== ПОДГОТОВКА ДАННЫХ ===================================

cat("=== DeepLabV3+ Segmentation Training ===\n\n")

get_pairs <- function(dir_path) {
  img_dir  <- file.path(dir_path, "images")
  mask_dir <- file.path(dir_path, "masks")
  img_files  <- sort(list.files(img_dir, full.names = TRUE,
                                pattern = "\\.(jpg|jpeg|png)$", ignore.case = TRUE))
  mask_files <- file.path(mask_dir,
                          paste0(tools::file_path_sans_ext(basename(img_files)), ".png"))
  ok <- file.exists(mask_files)
  list(images = img_files[ok], masks = mask_files[ok])
}

pos_pairs <- get_pairs(pos_dir)
neg_pairs <- get_pairs(neg_dir)
cat("Positive пар:", length(pos_pairs$images), "\n")
cat("Negative пар:", length(neg_pairs$images), "\n")

split_data <- function(pairs, val_frac) {
  n <- length(pairs$images)
  idx <- sample(n)
  n_val <- max(1, round(n * val_frac))
  list(
    train = list(images = pairs$images[idx[-(1:n_val)]],
                 masks  = pairs$masks[idx[-(1:n_val)]]),
    val   = list(images = pairs$images[idx[1:n_val]],
                 masks  = pairs$masks[idx[1:n_val]])
  )
}

pos_split <- split_data(pos_pairs, val_split)
neg_split <- split_data(neg_pairs, val_split)

cat("Positive: train=", length(pos_split$train$images),
    ", val=", length(pos_split$val$images), "\n")
cat("Negative: train=", length(neg_split$train$images),
    ", val=", length(neg_split$val$images), "\n\n")

# Validation: pos + neg
val_images <- c(pos_split$val$images, neg_split$val$images)
val_masks  <- c(pos_split$val$masks,  neg_split$val$masks)
val_ds <- create_dataset(val_images, val_masks, batch_size,
                         shuffle = FALSE, augment = FALSE)


# ======================== ПОСТРОЕНИЕ МОДЕЛИ ==================================

cat("Создание DeepLabV3+...\n")
model <- create_deeplabv3_plus(input_shape = c(img_size, img_size, 3L))
cat("Параметров:", format(count_params(model), big.mark = ","), "\n\n")

# Замораживаем backbone
for (layer in model$layers) {
  if (grepl("^block_|^Conv1|^expanded_conv", layer$name)) {
    layer$trainable <- FALSE
  }
}


# ======================== ФАЗА 1: ТОЛЬКО POSITIVE ============================

cat("========================================\n")
cat("  ФАЗА 1: только positive (", phase1_epochs, "эпох)\n")
cat("  Backbone заморожен, lr=", phase1_lr, "\n")
cat("========================================\n\n")

model %>% compile(
  optimizer = optimizer_adam(learning_rate = phase1_lr),
  loss      = dice_loss,
  metrics   = dice_coef
)

train_ds_p1 <- create_dataset(pos_split$train$images, pos_split$train$masks,
                               batch_size, shuffle = TRUE, augment = TRUE)

checkpoint_dir <- file.path(model_dir, "checkpoints")
dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)

history_p1 <- model %>% fit(
  train_ds_p1,
  epochs          = phase1_epochs,
  validation_data = val_ds,
  callbacks = list(
    callback_model_checkpoint(
      filepath       = file.path(checkpoint_dir,
                       "phase1_dice_{val_dice_coef:.4f}_ep{epoch:02d}.keras"),
      monitor        = "val_dice_coef",
      mode           = "max",
      save_best_only = TRUE,
      verbose        = 1
    ),
    callback_reduce_lr_on_plateau(
      monitor  = "val_dice_coef",
      mode     = "max",
      factor   = 0.5,
      patience = 4,
      min_lr   = 1e-6,
      verbose  = 1
    )
  ),
  verbose = 1
)

cat("\nФаза 1 завершена.\n\n")


# ======================== ФАЗА 2: CURRICULUM =================================

cat("========================================\n")
cat("  ФАЗА 2: curriculum (", phase2_epochs, "эпох)\n")
cat("  Backbone разморожен, neg:", neg_ratio_start, "->", neg_ratio_end, "\n")
cat("========================================\n\n")

# Размораживаем backbone
for (layer in model$layers) {
  layer$trainable <- TRUE
}

model %>% compile(
  optimizer = optimizer_adam(learning_rate = phase2_lr),
  loss      = dice_loss,
  metrics   = dice_coef
)

n_pos_train <- length(pos_split$train$images)
n_neg_train <- length(neg_split$train$images)
best_dice   <- 0

for (epoch in seq_len(phase2_epochs)) {
  neg_ratio <- neg_ratio_start +
    (neg_ratio_end - neg_ratio_start) * (epoch - 1) / max(phase2_epochs - 1, 1)

  n_neg_epoch <- min(as.integer(n_pos_train * neg_ratio / (1 - neg_ratio)), n_neg_train)
  neg_idx <- sample(n_neg_train, n_neg_epoch)

  epoch_images <- c(pos_split$train$images, neg_split$train$images[neg_idx])
  epoch_masks  <- c(pos_split$train$masks,  neg_split$train$masks[neg_idx])

  cat(sprintf("Эпоха %d/%d: neg=%.0f%% (%d pos + %d neg)\n",
              epoch, phase2_epochs, neg_ratio * 100, n_pos_train, n_neg_epoch))

  train_ds_epoch <- create_dataset(epoch_images, epoch_masks,
                                    batch_size, shuffle = TRUE, augment = TRUE)

  h <- model %>% fit(
    train_ds_epoch,
    epochs          = 1L,
    validation_data = val_ds,
    verbose         = 1
  )

  current_dice <- h$metrics$val_dice_coef
  if (is.numeric(current_dice) && current_dice > best_dice) {
    best_dice <- current_dice
    model %>% save_model(file.path(checkpoint_dir, "phase2_best.keras"))
    cat(sprintf("  -> Новый лучший val_dice: %.4f\n", best_dice))
  }
}

# Финальное сохранение
model %>% save_model(file.path(model_dir, "deeplabv3plus_final.keras"))

cat("\n========================================\n")
cat("  ОБУЧЕНИЕ ЗАВЕРШЕНО\n")
cat("  Лучший val dice (Фаза 2):", best_dice, "\n")
cat("  Модели в:", checkpoint_dir, "\n")
cat("========================================\n")
