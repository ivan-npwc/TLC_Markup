# =============================================================================
# source("TRAIN_BRANDED_BINARY.r")

# Бинарная: absence (0) vs presence (1)
# Backbone полностью разморожен + keras аугментация на GPU
# safe_bce loss — защита от NaN на битых изображениях
# =============================================================================

library(reticulate)
py_run_string("
import ctypes, os
conda_lib = '/home/ivan/miniconda3/envs/tf_gpu/lib'
wsl_lib = '/usr/lib/wsl/lib'
ctypes.CDLL(f'{wsl_lib}/libcuda.so.1', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{wsl_lib}/libnvidia-ml.so.1', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcublasLt.so.12', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcublas.so.12', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcudnn.so.9', mode=ctypes.RTLD_GLOBAL)
ctypes.CDLL(f'{conda_lib}/libcudart.so.12', mode=ctypes.RTLD_GLOBAL)
print('All libs preloaded OK')
")
library(tensorflow)
print(tf$config$list_physical_devices("GPU"))
library(keras3)

# ===== ПАРАМЕТРЫ =====
train_dir      <- "/home/ivan/TRAIN/sealion_presence_absence_aug"
# Содержит: absence/  presence/

checkpoint_dir <- file.path("checkpoints")
Species        <- "SSL_presence_binary_128"
bth_size       <- 64L
trgt_size      <- 128L
validation_split <- 0.1
epochs         <- 30L

dateTrain <- substr(Sys.time(), 1, 10)
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CLASS WEIGHTS
# =============================================================================
# Алфавитный порядок: absence=0, presence=1
n_absence  <- length(list.files(file.path(train_dir, "absence"),
  pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$"))
n_presence <- length(list.files(file.path(train_dir, "presence"),
  pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$"))
n_total <- n_absence + n_presence

w_absence  <- n_total / (2.0 * n_absence)
w_presence <- n_total / (2.0 * n_presence)
class_wt <- list("0" = w_absence, "1" = w_presence)

cat(sprintf("\nabsence:  %d (label=0, weight=%.2f)\n", n_absence, w_absence))
cat(sprintf("presence: %d (label=1, weight=%.2f)\n", n_presence, w_presence))

# =============================================================================
# ДАТАСЕТЫ
# =============================================================================
cat("\n===== ABSENCE vs PRESENCE (128x128, binary) =====\n")

train_ds <- image_dataset_from_directory(
  train_dir,
  validation_split = validation_split,
  subset = "training",
  seed = 42L,
  image_size = c(trgt_size, trgt_size),
  batch_size = bth_size,
  label_mode = "binary"
)

val_ds <- image_dataset_from_directory(
  train_dir,
  validation_split = validation_split,
  subset = "validation",
  seed = 42L,
  image_size = c(trgt_size, trgt_size),
  batch_size = bth_size,
  label_mode = "binary"
)

# Аугментация + rescale для train
augmentation <- keras_model_sequential(layers = list(
  layer_rescaling(scale = 1/255),
  layer_random_flip(mode = "horizontal"),
  layer_random_flip(mode = "vertical"),
  layer_random_rotation(factor = 0.08),
  layer_random_zoom(height_factor = 0.1),
  layer_random_brightness(factor = 0.3),
  layer_random_contrast(factor = 0.3)
))

# Только rescale для val
rescale <- keras_model_sequential(layers = list(
  layer_rescaling(scale = 1/255)
))

train_ds <- train_ds$map(function(x, y) list(augmentation(x, training = TRUE), y))
train_ds <- train_ds$prefetch(buffer_size = tf$data$AUTOTUNE)

val_ds <- val_ds$map(function(x, y) list(rescale(x), y))
val_ds <- val_ds$prefetch(buffer_size = tf$data$AUTOTUNE)

# =============================================================================
# SAFE BCE LOSS — защита от NaN
# =============================================================================
safe_bce <- function(y_true, y_pred) {
  y_pred <- op_clip(y_pred, 1e-7, 1.0 - 1e-7)
  -op_mean(y_true * op_log(y_pred) + (1 - y_true) * op_log(1 - y_pred))
}

# =============================================================================
# МОДЕЛЬ
# =============================================================================
cat("\nCreating EfficientNetV2-S (fully trainable)...\n")

conv_base <- tryCatch(
  application_efficientnet_v2_s(weights = "imagenet", include_top = FALSE,
    input_shape = c(trgt_size, trgt_size, 3)),
  error = function(e) application_efficientnet_v2s(weights = "imagenet", include_top = FALSE,
    input_shape = c(trgt_size, trgt_size, 3))
)
conv_base$trainable <- TRUE

inputs <- layer_input(shape = c(trgt_size, trgt_size, 3))
x <- inputs |> conv_base() |>
  layer_global_average_pooling_2d() |>
  layer_batch_normalization() |>
  layer_dense(256, activation = "relu") |>
  layer_dropout(0.4) |>
  layer_dense(128, activation = "relu") |>
  layer_dropout(0.3) |>
  layer_dense(1, activation = "sigmoid")

model <- keras_model(inputs, x)
summary(model)

# =============================================================================
# CALLBACKS
# =============================================================================
fp <- file.path(checkpoint_dir, paste0(Species, "_", dateTrain,
  "_acc_{val_accuracy:.3f}_ep_{epoch:02d}.keras"))

callbacks <- list(
  callback_model_checkpoint(filepath = fp, monitor = "val_accuracy",
    save_best_only = TRUE, mode = "max", verbose = 1),
  callback_reduce_lr_on_plateau(monitor = "val_accuracy", factor = 0.5,
    patience = 3, verbose = 1, mode = "max", min_lr = 1e-7),
  callback_early_stopping(monitor = "val_accuracy", patience = 7,
    verbose = 1, mode = "max", restore_best_weights = TRUE)
)

# =============================================================================
# ОБУЧЕНИЕ
# =============================================================================
cat("\n===== Training (fully trainable, lr=1e-4, safe_bce) =====\n")

model |> compile(
  optimizer = optimizer_adam(learning_rate = 1e-4),
  loss = safe_bce,
  metrics = "accuracy"
)

model |> fit(train_ds, epochs = epochs,
  validation_data = val_ds, callbacks = callbacks,
  class_weight = class_wt)

# =============================================================================
# СОХРАНЕНИЕ
# =============================================================================
final_path <- file.path(checkpoint_dir, paste0(Species, "_", dateTrain, "_final.keras"))
model |> save_model(final_path)
cat("\n===== DONE =====\n")
cat("Model:", final_path, "\n")
cat("  < 0.5 = absence\n  > 0.5 = presence\n")
