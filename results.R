
read_latest <- function() {
  f <- list.files('output/results', full.names = TRUE)
  fl <- file.info(f)$ctime
  most_recent <- f[which.max(fl)]
  print(most_recent)
  readRDS(most_recent)
}


res <- read_latest()
