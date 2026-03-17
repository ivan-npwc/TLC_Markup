# =============================================================================
# source("EXTRACT_WEIGHTS_TO_RDS.r")
# Загрузка обученной .keras модели, извлечение весов всех слоёв → RDS
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
library(keras3)

# ===== ПАРАМЕТРЫ =====
# Путь к обученной модели (.keras)
model_path <- file.path("checkpoints",
  "SSL_presence_binary_128_2026-03-16_acc_0.997_ep_13.keras")

# Путь для сохранения весов (RDS)
rds_path <- sub("\\.keras$", "_weights.rds", model_path)

# =============================================================================
# ЗАГРУЗКА МОДЕЛИ
# =============================================================================
cat(sprintf("Loading model: %s\n", model_path))
stopifnot(file.exists(model_path))

model <- load_model(model_path)
cat("Model loaded OK\n")
summary(model)



a=get_weights(model)
saveRDS(a,"SSL_presence_binary_128_2026-03-16_acc_0997_ep_13")

# =============================================================================
# ИЗВЛЕЧЕНИЕ ВЕСОВ
# =============================================================================
cat("\nExtracting weights from all layers...\n")

# --- Вариант 1: Плоский список всех тензоров (как get_weights) ---
# Это то, что используется в model$set_weights() для восстановления
all_weights_flat <- model$get_weights()

# Присвоим имена для удобства (weight_001, weight_002, ...)
names(all_weights_flat) <- sprintf("weight_%03d", seq_along(all_weights_flat))

# --- Вариант 2: Структурированный список по слоям ---
layer_names <- sapply(model$layers, function(l) l$name)
n_layers    <- length(model$layers)

weights_by_layer <- vector("list", n_layers)
names(weights_by_layer) <- layer_names

for (i in seq_len(n_layers)) {
  layer  <- model$layers[[i]]
  lname  <- layer$name
  w_list <- layer$get_weights()

  if (length(w_list) == 0) {
    weights_by_layer[[i]] <- NULL
    next
  }

  # Конвертируем каждый тензор в R-массив
  w_r <- lapply(w_list, function(w) {
    if (inherits(w, "tensorflow.python.framework.ops.EagerTensor") ||
        inherits(w, "numpy.ndarray")) {
      as.array(w)
    } else {
      w
    }
  })

  # Именуем: kernel, bias, gamma, beta, moving_mean, moving_variance, ...
  var_names <- tryCatch(
    sapply(layer$weights, function(v) tail(strsplit(v$name, "/")[[1]], 1)),
    error = function(e) paste0("param_", seq_along(w_r))
  )
  names(w_r) <- var_names

  weights_by_layer[[i]] <- w_r
}

# Убираем слои без весов
weights_by_layer <- Filter(Negate(is.null), weights_by_layer)

cat(sprintf("Layers with weights: %d\n", length(weights_by_layer)))
cat(sprintf("Total weight tensors (flat): %d\n", length(all_weights_flat)))

# =============================================================================
# МЕТАДАННЫЕ
# =============================================================================
metadata <- list(
  model_path       = normalizePath(model_path),
  source_script    = "TRAIN_BRANDED_BINARY.r",
  architecture     = "EfficientNetV2-S + Dense head (binary)",
  input_size       = 384L,
  classes          = c("branded", "notbranded"),
  n_layers_total   = n_layers,
  n_layers_weights = length(weights_by_layer),
  n_tensors_flat   = length(all_weights_flat),
  extracted_at     = Sys.time()
)

# =============================================================================
# СОХРАНЕНИЕ В RDS
# =============================================================================
result <- list(
  metadata         = metadata,
  weights_by_layer = weights_by_layer,
  weights_flat     = all_weights_flat
)

cat(sprintf("\nSaving to: %s\n", rds_path))
saveRDS(result, rds_path, compress = "xz")

file_mb <- round(file.info(rds_path)$size / 1024^2, 1)
cat(sprintf("Saved OK  (%s MB)\n", file_mb))

# =============================================================================
# ПРОВЕРКА: загрузить RDS и восстановить веса в модель
# =============================================================================
cat("\n--- Verification ---\n")
loaded <- readRDS(rds_path)

cat("Metadata:\n")
str(loaded$metadata, max.level = 1)

cat(sprintf("\nLayers in weights_by_layer: %d\n",
            length(loaded$weights_by_layer)))
cat(sprintf("Tensors in weights_flat:    %d\n",
            length(loaded$weights_flat)))

# Пример: первые 5 слоёв с весами
cat("\nFirst 5 layers with weights:\n")
for (nm in head(names(loaded$weights_by_layer), 5)) {
  dims <- sapply(loaded$weights_by_layer[[nm]], function(w) paste(dim(w), collapse="x"))
  cat(sprintf("  %-40s  %s\n", nm, paste(dims, collapse = "  ")))
}

cat("\n===== DONE =====\n")

# =============================================================================
# ПРИМЕР: как потом восстановить веса из RDS в новую модель
# =============================================================================
# loaded <- readRDS("path/to/_weights.rds")
# model$set_weights(loaded$weights_flat)
