# =============================================================================
#source("/home/npwc/GIT/TLC_Markup/Modules/02_04_VGG16/04_PREDICT_BRANDED-NONBRANDED_EfficientNetV2S.r")
# Предсказание branded/nonbranded с использованием EfficientNetV2-S (384x384)
# Модель: бинарная sigmoid (< 0.5 = branded, >= 0.5 = notbranded)
# Веса загружаются из RDS
# =============================================================================

library(reticulate)
library(tensorflow)
library(keras)
library(tidyverse)
library(tfdatasets)

# ===== ПАРАМЕТРЫ =====
rds_path   <- "/home/npwc/SSL_branded_binary_384_2026-03-13_acc_099"
Preddir    <- "/mnt/adata8tb/SSL_DB_Tiles"
file_size  <- 2000
batch_size <- 32L
trgt_size  <- 384L
threshold  <- 0.5
Pred_Name  <- c("BRANDED", "NONBRANDED")

# =============================================================================
# 1. СОЗДАНИЕ МОДЕЛИ + ЗАГРУЗКА ВЕСОВ (один раз)
# =============================================================================
if (!exists("model_split_brand")) {
  cat("Building EfficientNetV2-S (384) + binary head...\n")

  conv_base <- tf$keras$applications$EfficientNetV2S(
    weights     = NULL,
    include_top = FALSE,
    input_shape = as.integer(c(trgt_size, trgt_size, 3L))
  )
  conv_base$trainable <- FALSE

  inputs <- layer_input(shape = c(trgt_size, trgt_size, 3L))
  x <- inputs %>%
    conv_base() %>%
    layer_global_average_pooling_2d() %>%
    layer_batch_normalization() %>%
    layer_dense(units = 256L, activation = "relu") %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 128L, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 1L,   activation = "sigmoid")

  model_split_brand <- keras_model(inputs, x)

  cat("Loading weights from RDS...\n")
  weights <- readRDS(rds_path)
  set_weights(model_split_brand, weights)
  cat("Model ready\n")
  summary(model_split_brand)
}

# =============================================================================
# 2. ДАТАСЕТ: чтение JPG → resize 384 → rescale [0,1]
# =============================================================================
create_dataset <- function(data, batch_size, trgt_size) {
  data %>%
    tensor_slices_dataset() %>%
    dataset_map(~.x %>% list_modify(
      img = tf$io$read_file(.x$img)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$decode_jpeg(.x$img, channels = 3L)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$resize(.x$img, size = shape(trgt_size, trgt_size))
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$cast(.x$img, tf$float32) / 255.0
    )) %>%
    dataset_batch(batch_size) %>%
    dataset_prefetch(buffer_size = tf$data$AUTOTUNE) %>%
    dataset_map(unname)
}

# =============================================================================
# 3. ЦИКЛ ПРЕДСКАЗАНИЙ
# =============================================================================
listsites <- list.files(Preddir, full.names = TRUE, pattern = "Presence")

for (i in seq_along(listsites)) {
  sitedir      <- listsites[i]
  savesitedir  <- gsub("Presence", "Branded", basename(sitedir))
  savesitedir1 <- file.path(Preddir, savesitedir)
  unlink(savesitedir1, recursive = TRUE)
  dir.create(savesitedir1, showWarnings = FALSE)

  daysdir <- list.files(sitedir, full.names = TRUE)

  for (y in seq_along(daysdir)) {
    day        <- daysdir[y]
    BrandedDir <- file.path(savesitedir1, basename(day))
    dir.create(BrandedDir, showWarnings = FALSE)
    PesAbsPth  <- file.path(BrandedDir, "sealion_branded_nonbranded.csv")

    listImgPred <- list.files(day, full.names = TRUE, pattern = "JPG")
    if (length(listImgPred) == 0) { print("No Imgs Found"); next }

    # --- Фильтр по размеру файла ---
    finfo       <- file.info(listImgPred)
    keep        <- finfo$size > file_size
    exl         <- sum(!keep)
    listImgPred <- listImgPred[keep]
    cat(sprintf("Exclude %d images, predict on %d images\n",
                exl, length(listImgPred)))

    if (length(listImgPred) < 2) next

    # --- Пропуск уже обработанных ---
    if (file.exists(PesAbsPth)) {
      tbl1 <- read.csv(PesAbsPth)
      if (nrow(tbl1) == length(listImgPred)) {
        print(paste0("SKIP    ", BrandedDir)); next
      }
    }

    # --- Предсказание ---
    data    <- tibble(img = listImgPred)
    pred_ds <- create_dataset(data, batch_size, trgt_size)
    pred_raw <- predict(model_split_brand, pred_ds, verbose = 0L)

    # sigmoid: значение = P(notbranded); branded = class 0
    p_notbranded <- as.numeric(pred_raw[, 1])

    preds3 <- data.frame(
      BRANDED    = 1 - p_notbranded,
      NONBRANDED = p_notbranded,
      link       = listImgPred,
      name       = ifelse(p_notbranded >= threshold, "NONBRANDED", "BRANDED"),
      stringsAsFactors = FALSE
    )

    # --- Копирование branded-файлов + сохранение CSV ---
    pres <- preds3$link[preds3$name == "BRANDED"]
    file.copy(pres, BrandedDir)
    write.csv(preds3, PesAbsPth, row.names = FALSE)

    cat(sprintf("DONE branded: %d / %d    %s\n",
                length(pres), length(listImgPred), day))
  }
}
