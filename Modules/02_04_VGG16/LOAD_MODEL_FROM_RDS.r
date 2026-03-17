# =============================================================================
# source("LOAD_MODEL_FROM_RDS.r")
# Создание модели EfficientNetV2-S (branded binary) с нуля + загрузка весов из RDS
# R пакеты: tensorflow + keras (keras2)
# =============================================================================

library(reticulate)
library(tensorflow)
library(keras)

# ===== ПАРАМЕТРЫ =====
trgt_size <- 384L
rds_path  <-  "/home/npwc/SSL_branded_binary_384_2026-03-13_acc_099"

# =============================================================================
# 1. СОЗДАНИЕ АРХИТЕКТУРЫ (идентично TRAIN_BRANDED_BINARY.r)
# =============================================================================
# Backbone — keras2 не имеет R-обёртки для EfficientNetV2,
# поэтому вызываем через tf$keras$applications
conv_base <- tf$keras$applications$EfficientNetV2S(
  weights     = NULL,
  include_top = FALSE,
  input_shape = as.integer(c(trgt_size, trgt_size, 3L))
)
conv_base$trainable <- TRUE
# Функциональное API — голова в точности как при обучении
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

model <- keras_model(inputs, x)
summary(model)

# =============================================================================
# 2. ЗАГРУЗКА ВЕСОВ ИЗ RDS
# =============================================================================

weights <- readRDS(rds_path)
set_weights(model, weights)
