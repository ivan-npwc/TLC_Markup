# =============================================================================
# OFFLINE_AUGMENTATION.r
# Elastic + perspective для одной папки
# source_dir — папка с оригиналами
# output_dir — папка куда сохраняются оригиналы + аугментации
# =============================================================================

library(EBImage)
library(parallel)

# ===== ПАРАМЕТРЫ =====
source_dir <- "/home/ivan/TRAIN/sealion_presence_absence/presence"
output_dir <- "/home/ivan/TRAIN/sealion_presence_absence_aug/presence"

TARGET <- 400000L
trgt_size <- 256L
JPEG_QUALITY <- 85
BATCH_SAVE <- 2000L

num_cores <- max(1L, detectCores(logical = FALSE) - 2L)
cat("Using", num_cores, "cores\n")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# ПРЕДВЫЧИСЛЕННЫЕ ЯДРА
# =============================================================================
PRECOMPUTED_KERNELS <- list()
for (s in c(5, 8, 10, 12, 15, 18, 20, 25)) {
  PRECOMPUTED_KERNELS[[as.character(s)]] <- makeBrush(
    size = s * 2 + 1, shape = "gaussian", sigma = s)
}

# =============================================================================
# ELASTIC DEFORMATION
# =============================================================================
elastic_deform_array <- function(img_array, alpha = 5, sigma = 15) {
  h <- dim(img_array)[1]; w <- dim(img_array)[2]; ch <- dim(img_array)[3]
  dx <- matrix(rnorm(h*w), h, w); dy <- matrix(rnorm(h*w), h, w)
  gk <- PRECOMPUTED_KERNELS[[as.character(sigma)]]
  if (is.null(gk)) gk <- makeBrush(sigma*2+1, "gaussian", sigma=sigma)
  dx_b <- filter2(dx, gk); dy_b <- filter2(dy, gk)
  md <- max(abs(c(dx_b, dy_b))); if (md < 1e-6) return(img_array)
  dx_n <- dx_b*(alpha/md); dy_n <- dy_b*(alpha/md)
  xg <- matrix(rep(1:w, each=h), h, w); yg <- matrix(rep(1:h, times=w), h, w)
  xn <- pmin(pmax(xg+dx_n, 1), w-1); yn <- pmin(pmax(yg+dy_n, 1), h-1)
  x1 <- floor(xn); x2 <- pmin(x1+1, w); y1 <- floor(yn); y2 <- pmin(y1+1, h)
  wx <- as.vector(xn-x1); wy <- as.vector(yn-y1)
  w1 <- (1-wx)*(1-wy); w2 <- wx*(1-wy); w3 <- (1-wx)*wy; w4 <- wx*wy
  i11 <- cbind(as.vector(y1), as.vector(x1)); i12 <- cbind(as.vector(y1), as.vector(x2))
  i21 <- cbind(as.vector(y2), as.vector(x1)); i22 <- cbind(as.vector(y2), as.vector(x2))
  result <- array(0, dim = dim(img_array))
  for (c in 1:ch) {
    cd <- img_array[,,c]
    result[,,c] <- matrix(cd[i11]*w1 + cd[i12]*w2 + cd[i21]*w3 + cd[i22]*w4, h, w)
  }
  pmin(pmax(result, 0), 1)
}

# =============================================================================
# PERSPECTIVE TRANSFORM
# =============================================================================
perspective_transform_array <- function(img_array, intensity = 0.25) {
  i1 <- runif(1, 0.05, intensity); h <- dim(img_array)[1]; w <- dim(img_array)[2]
  img_ebi <- Image(img_array, colormode = Color)
  src <- matrix(c(1,1, w,1, w,h), ncol = 2, byrow = TRUE)
  dst <- src + cbind(runif(3, -w*i1, w*i1), runif(3, -h*i1, h*i1))
  am <- tryCatch(solve(cbind(src, 1)) %*% dst, error = function(e) NULL)
  if (is.null(am)) return(img_array)
  tr <- tryCatch(affine(img_ebi, am, filter = "bilinear"), error = function(e) img_ebi)
  r <- imageData(tr)
  if (length(dim(r)) == 2) r <- array(rep(r, 3), dim = c(dim(r), 3))
  pmin(pmax(r, 0), 1)
}

# =============================================================================
# ТИПЫ ДЕФОРМАЦИЙ
# =============================================================================
AUG_TYPES <- list(
  light_elastic       = list(fn = "elastic",     alpha = c(2,3),   sigma = c(15,20,25), weight = 0.25),
  medium_elastic      = list(fn = "elastic",     alpha = c(4,5,6), sigma = c(10,12,15), weight = 0.30),
  heavy_elastic       = list(fn = "elastic",     alpha = c(7,8,9), sigma = c(8,10,12),  weight = 0.15),
  perspective_light   = list(fn = "perspective", intensity = 0.15, weight = 0.10),
  perspective_heavy   = list(fn = "perspective", intensity = 0.30, weight = 0.05),
  elastic_perspective = list(fn = "both", alpha = c(3,4,5), sigma = c(10,15), intensity = 0.20, weight = 0.15)
)

apply_deformation <- function(img, aspec) {
  if (aspec$fn == "elastic") {
    elastic_deform_array(img, sample(aspec$alpha, 1), sample(aspec$sigma, 1))
  } else if (aspec$fn == "perspective") {
    perspective_transform_array(img, aspec$intensity)
  } else {
    img <- elastic_deform_array(img, sample(aspec$alpha, 1), sample(aspec$sigma, 1))
    perspective_transform_array(img, aspec$intensity)
  }
}

build_schedule <- function(n) {
  types <- character(n); pos <- 1L
  for (nm in names(AUG_TYPES)) {
    cnt <- round(n * AUG_TYPES[[nm]]$weight)
    if (pos + cnt - 1 > n) cnt <- n - pos + 1
    if (cnt > 0) { types[pos:(pos + cnt - 1)] <- nm; pos <- pos + cnt }
  }
  if (pos <= n) types[pos:n] <- "medium_elastic"
  sample(types)
}

process_one <- function(src_path, aug_type, save_path, target_size) {
  tryCatch({
    raw <- readImage(src_path)
    if (length(dim(raw)) == 2) raw <- abind::abind(raw, raw, raw, along = 3)
    if (dim(raw)[3] > 3) raw <- raw[,,1:3]
    img <- imageData(EBImage::resize(raw, w = target_size, h = target_size))
    img_aug <- apply_deformation(img, AUG_TYPES[[aug_type]])
    writeImage(Image(img_aug, colormode = Color), save_path, quality = JPEG_QUALITY)
    TRUE
  }, error = function(e) FALSE)
}

# =============================================================================
# ГЕНЕРАЦИЯ
# =============================================================================
cat("\n===== OFFLINE AUGMENTATION =====\n")
cat("Source:", source_dir, "\n")
cat("Output:", output_dir, "\n")
cat("Target:", TARGET, "\n\n")

src_files <- list.files(source_dir, pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$", full.names = TRUE)
n_src <- length(src_files)
if (n_src == 0) stop("No images found in ", source_dir)
cat("Source images:", n_src, "\n")

# Копируем оригиналы
orig_marker <- file.path(output_dir, ".originals_copied")
if (!file.exists(orig_marker)) {
  cat("Copying originals...\n")
  file.copy(src_files, output_dir, overwrite = FALSE)
  writeLines("done", orig_marker)
}

existing <- length(list.files(output_dir, pattern = "\\.(jpg|jpeg|png|JPG)$"))
remaining <- TARGET - existing
cat(sprintf("Existing: %d | Remaining: %d\n", existing, max(0, remaining)))

if (remaining <= 0) {
  cat("Already complete!\n")
  quit(save = "no")
}

schedule <- build_schedule(remaining)
counter <- existing
t0 <- Sys.time()

n_batches <- ceiling(remaining / BATCH_SAVE)
for (bi in seq_len(n_batches)) {
  b_start <- (bi - 1L) * BATCH_SAVE + 1L
  b_end <- min(bi * BATCH_SAVE, remaining)
  
  tasks <- lapply(b_start:b_end, function(gi) {
    counter <<- counter + 1L
    list(
      src_path = src_files[sample(n_src, 1)],
      aug_type = schedule[gi],
      save_path = file.path(output_dir,
        sprintf("aug_%07d_%s.jpg", counter, schedule[gi]))
    )
  })
  
  mclapply(tasks, function(t) {
    process_one(t$src_path, t$aug_type, t$save_path, trgt_size)
  }, mc.cores = num_cores)
  
  done <- b_end
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  speed <- done / max(elapsed, 1)
  eta <- (remaining - done) / max(speed, 1) / 60
  cat(sprintf("\r  %d/%d (%d%%)  %.0f img/sec  ETA %.0f min     ",
              done, remaining, round(done/remaining*100), speed, eta))
}

cat("\n\n===== COMPLETE =====\n")
final <- length(list.files(output_dir, pattern = "\\.(jpg|jpeg|png|JPG)$"))
cat(sprintf("Output: %s\nTotal: %d images\n", output_dir, final))
