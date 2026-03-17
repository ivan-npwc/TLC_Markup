# =============================================================================
# source(""/home/npwc/GIT/TLC_Markup/Modules/02_04_VGG16/03_PREDICT_PRESENCE-ABSENCE_EfficientNetV2S.r")
# Предсказание presence/absence с использованием EfficientNetV2-S (128x128)
# Модель: бинарная sigmoid (< 0.5 = absence, >= 0.5 = presence)
# Веса загружаются из RDS
# =============================================================================

library(reticulate)
library(tensorflow)
library(keras)
library(tidyverse)
library(tfdatasets)

# ===== ПАРАМЕТРЫ =====
rds_path   <- "/home/npwc/GIT/TLC MarkUp System data/Models/SSL_presence_binary_128_2026-03-16_acc_0997_ep_13"
Preddir    <- "/mnt/adata8tb/SSL_DB_Tiles"
file_size  <- 2000
batch_size <- 128L
trgt_size  <- 128L
threshold  <- 0.5
Pred_Name  <- c("ABSENCE", "PRESENCE")

# =============================================================================
# 1. СОЗДАНИЕ МОДЕЛИ + ЗАГРУЗКА ВЕСОВ (один раз)
# =============================================================================
if (!exists("model_split")) {
  cat("Building EfficientNetV2-S + binary head...\n")

  conv_base <- tf$keras$applications$EfficientNetV2S(
    weights     = NULL,
    include_top = FALSE,
    input_shape = as.integer(c(trgt_size, trgt_size, 3L))
  )
  conv_base$trainable <- FALSE   # инференс — не нужен trainable

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

  model_split <- keras_model(inputs, x)

  cat("Loading weights from RDS...\n")
  weights <- readRDS(rds_path)
  set_weights(model_split, weights)
  cat("Model ready\n")
  summary(model_split)
}

# =============================================================================
# 2. ДАТАСЕТ: чтение JPG → resize → rescale [0,1]
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
listsites    <- list.files(Preddir, full.names = TRUE, pattern = "Tiles")
totallcount  <- 14129255

for (i in seq_along(listsites)) {
  sitedir     <- listsites[i]
  savesitedir <- gsub("Tiles", "Presence", basename(sitedir))
  unlink(savesitedir, recursibe=T)
  savesitedir1 <- file.path(Preddir, savesitedir)
  dir.create(savesitedir1, showWarnings = FALSE)

  daysdir <- list.files(sitedir, full.names = TRUE)

  for (y in seq_along(daysdir)) {
    start_time <- as.numeric(Sys.time())
    day         <- daysdir[y]
    PresenceDir <- file.path(savesitedir1, basename(day))
    dir.create(PresenceDir, showWarnings = FALSE)
    PesAbsPth   <- file.path(PresenceDir, "sealion_presence_absence.csv")

    listImgPred <- list.files(day, full.names = TRUE, pattern = "JPG")
    if (length(listImgPred) == 0) { print("No Imgs Found"); next }

    # --- Фильтр по размеру файла ---
    finfo       <- file.info(listImgPred)
    keep        <- finfo$size > file_size
    exl         <- sum(!keep)
    listImgPred <- listImgPred[keep]
    cat(sprintf("Exclude %d images, predict on %d images\n",
                exl, length(listImgPred)))

    if (length(listImgPred) == 0) next

    # --- Пропуск уже обработанных ---
    if (file.exists(PesAbsPth)) {
      tbl1 <- read.csv(PesAbsPth)
      if (nrow(tbl1) == length(listImgPred)) {
        print(paste0("SKIP    ", PresenceDir)); next
      }
    }

    # --- Предсказание ---
    data     <- tibble(img = listImgPred)
    pred_ds  <- create_dataset(data, batch_size, trgt_size)
    pred_raw <- predict(model_split, pred_ds, verbose = 0L)

    # pred_raw — матрица Nx1 (sigmoid), значение = P(presence)
    p_presence <- as.numeric(pred_raw[, 1])

    preds3 <- data.frame(
      ABSENCE  = 1 - p_presence,
      PRESENCE = p_presence,
      link     = listImgPred,
      name     = ifelse(p_presence >= threshold, "PRESENCE", "ABSENCE"),
      stringsAsFactors = FALSE
    )

    # --- Копирование presence-файлов + сохранение CSV ---
    pres <- preds3$link[preds3$name == "PRESENCE"]
    file.copy(pres, PresenceDir)
    write.csv(preds3, PesAbsPth, row.names = FALSE)

    # --- Статистика ---
    stop_time  <- as.numeric(Sys.time())
    elapsed    <- stop_time - start_time
    countimgs  <- length(listImgPred)
    speed      <- elapsed / countimgs
    hrs_remain <- speed * totallcount / 3600

    cat(sprintf("DONE presence: %d / %d    %s\n",
                length(pres), countimgs, day))
    cat(sprintf("SPEED  %.3f sec/image   ETA %.1f hours\n",
                speed, hrs_remain))
  }
}
