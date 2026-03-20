# =============================================================================
# source("TRAIN_BRANDED_BINARY_WINTER_1.r")
#
# Бинарная: absence (0) vs presence (1)
# Backbone полностью разморожен + keras аугментация на GPU
# safe_bce loss — защита от NaN на битых изображениях
#
# НОВОЕ: winter_augment для генерализации на зимние тайлы
#        CLAHE вынесен в OFFLINE_AUGMENTATION.r (EBImage)
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

library(keras3)

# ===== ПАРАМЕТРЫ =====
# CLAHE уже применён офлайн в OFFLINE_AUGMENTATION.r (EBImage)
# В tf.data pipeline CLAHE НЕ нужен — данные уже нормализованы
train_dir      <- "/home/ivan/TRAIN/sealion_presence_absence"
# Содержит: absence/  presence/

checkpoint_dir <- file.path("checkpoints")
Species        <- "SSL_presence_binary_256_winter"
bth_size       <- 32L
trgt_size      <- 256L
validation_split <- 0.1
epochs         <- 30L

# Вероятность winter-аугментации (0.0–1.0)
WINTER_AUG_PROB    <- 0.40
# Вероятность полного обесцвечивания (0.0–1.0)
GRAYSCALE_AUG_PROB <- 0.35

dateTrain <- substr(Sys.time(), 1, 10)
dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CLASS WEIGHTS
# =============================================================================
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
# WINTER AUGMENTATION — имитация зимнего фона (на GPU через tf$)
# =============================================================================
# Применяется к каждому изображению отдельно (после rescale в [0,1]).
# С вероятностью WINTER_AUG_PROB делает:
#   1. Десатурация (blend с grayscale)
#   2. Осветление  (blend с белым)
#   3. Лёгкий шум  (имитация текстуры снега)
#
# Без winter-аугментации модель учит: "сивуч = светлый на тёмном",
# с ней: часть тайлов становится "светлый на светлом", модель
# вынуждена учить форму/текстуру, а не только яркостный контраст.
# =============================================================================

winter_augment_single <- function(image) {
  # image: [H, W, 3] float32 [0, 1]
  do_winter <- tf$random$uniform(shape = list()) < WINTER_AUG_PROB

  image <- tf$cond(do_winter, function() {
    img <- image

    # --- 1. Десатурация: blend RGB → Grayscale ---
    grey <- tf$image$rgb_to_grayscale(img)                   # [H,W,1]
    grey <- tf$concat(list(grey, grey, grey), axis = -1L)    # [H,W,3]
    desat_alpha <- tf$random$uniform(shape = list(), minval = 0.2, maxval = 0.9)
    img <- img * (1.0 - desat_alpha) + grey * desat_alpha

    # --- 2. Осветление: blend с белым (имитация снежного покрова) ---
    white_alpha <- tf$random$uniform(shape = list(), minval = 0.1, maxval = 0.70)
    img <- img * (1.0 - white_alpha) + white_alpha

    # --- 3. Лёгкий гауссов шум (текстура снега/инея) ---
    noise <- tf$random$normal(
      shape  = tf$shape(img),
      mean   = 0.0,
      stddev = 0.02
    )
    img <- img + noise

    tf$clip_by_value(img, 0.0, 1.0)
  }, function() { image })

  image
}

# Обёртка для tf$map_fn по батчу
winter_augment_batch <- function(x, y) {
  x_aug <- tf$map_fn(winter_augment_single, x)
  list(x_aug, y)
}

# =============================================================================
# GRAYSCALE AUGMENTATION — полное обесцвечивание (на GPU через tf$)
# =============================================================================
# Независимо от winter_augment, с вероятностью GRAYSCALE_AUG_PROB
# конвертирует тайл в 3-канальный grayscale.
#
# Зачем: зимние тайлы почти монохромные. Если модель видит только
# цветные данные, она опирается на цвет меха (рыжий/бурый) vs фон.
# Зимой этот признак исчезает. Grayscale заставляет модель учить
# форму и текстуру, а не цветовой канал.
#
# Применяется ко ВСЕМ тайлам (и летним, и "заснеженным" после
# winter_augment) — это расширяет домен ещё сильнее.
# =============================================================================

grayscale_augment_single <- function(image) {
  # image: [H, W, 3] float32 [0, 1]
  do_grey <- tf$random$uniform(shape = list()) < GRAYSCALE_AUG_PROB

  image <- tf$cond(do_grey, function() {
    grey <- tf$image$rgb_to_grayscale(image)              # [H,W,1]
    tf$concat(list(grey, grey, grey), axis = -1L)          # [H,W,3]
  }, function() { image })

  image
}

# Обёртка для tf$map_fn по батчу
grayscale_augment_batch <- function(x, y) {
  x_aug <- tf$map_fn(grayscale_augment_single, x)
  list(x_aug, y)
}

# =============================================================================
# ДАТАСЕТЫ
# =============================================================================
cat("\n===== ABSENCE vs PRESENCE (256x256, binary) =====\n")
cat(sprintf("CLAHE: offline (EBImage) | Winter: %.0f%% | Grayscale: %.0f%%\n",
            WINTER_AUG_PROB * 100, GRAYSCALE_AUG_PROB * 100))

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

# --- Аугментация (расширенная: brightness/contrast шире) ---
augmentation <- keras_model_sequential(layers = list(
  layer_rescaling(scale = 1/255),
  layer_random_flip(mode = "horizontal"),
  layer_random_flip(mode = "vertical"),
  layer_random_rotation(factor = 0.08),
  layer_random_zoom(height_factor = 0.1),
  layer_random_brightness(factor = 0.7),     # было 0.3 → расширено
  layer_random_contrast(factor = 0.7)         # было 0.3 → расширено
))

# Только rescale для val (без аугментации)
rescale_only <- keras_model_sequential(layers = list(
  layer_rescaling(scale = 1/255)
))

# --- TRAIN pipeline: augment → winter → grayscale → prefetch ---
# (CLAHE уже применён офлайн — не нужен в pipeline)
train_ds <- train_ds$map(function(x, y) list(augmentation(x, training = TRUE), y))
train_ds <- train_ds$map(winter_augment_batch)       # зимняя аугментация (30%)
train_ds <- train_ds$map(grayscale_augment_batch)    # полное обесцвечивание (15%)
train_ds <- train_ds$prefetch(buffer_size = tf$data$AUTOTUNE)

# --- VAL pipeline: rescale → prefetch (без аугментации, CLAHE уже в данных) ---
val_ds <- val_ds$map(function(x, y) list(rescale_only(x), y))
val_ds <- val_ds$prefetch(buffer_size = tf$data$AUTOTUNE)

# =============================================================================
# SAFE BCE LOSS — защита от NaN
# =============================================================================
safe_bce <- function(y_true, y_pred) {
  y_pred <- op_clip(y_pred, 1e-7, 1.0 - 1e-7)
  -op_mean(y_true * op_log(y_pred) + (1 - y_true) * op_log(1 - y_pred))
}

# =============================================================================
# МОДЕЛЬ — EfficientNetV2-S
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
  layer_dropout(0.6) |>
  layer_dense(128, activation = "relu") |>
  layer_dropout(0.6) |>
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
cat("\n===== Training (lr=1e-4, safe_bce, winter+grayscale aug) =====\n")

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

# =============================================================================
# ИНФЕРЕНС — пример применения CLAHE при предсказании
# =============================================================================
# library(EBImage)
# source("OFFLINE_AUGMENTATION_1.r")   # загрузит clahe_rgb()
#
# img <- readImage("path/to/winter_tile.jpg")
# img <- resize(img, w = 256, h = 256)
# img_arr <- as.array(img)                  # [H, W, 3] уже в [0,1]
# img_arr <- clahe_rgb(img_arr)             # <-- та же CLAHE что при обучении
# batch   <- array(img_arr, dim = c(1, 256, 256, 3))
# pred    <- model |> predict(batch)
# cat("presence prob:", pred[1,1], "\n")
