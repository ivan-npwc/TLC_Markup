# =============================================================================
# source("EXTRACT_WEIGHTS_TO_RDS.r")
# Загрузка обученной .keras модели, извлечение весов всех слоёв → RDS
#
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

Sys.setenv(PATH = paste0("/home/ivan/miniconda3/envs/r-tf-gpu/bin:", Sys.getenv("PATH")))


library(tensorflow)
library(keras3)

# ===== ПАРАМЕТРЫ =====
# Путь к обученной модели (.keras)
model_path <- file.path("checkpoints",
  "SSL_presence_binary_256_2026-03-17_acc_0975_ep_04")


model <- load_model(model_path,compile = F )

summary(model)

a=get_weights(model)
saveRDS(a,"SSL_presence_binary_256_2026-03-17_acc_0975_ep_04")
