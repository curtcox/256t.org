#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(digest)
  library(base64enc)
})

get_base_dir <- function() {
  args <- commandArgs(FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 0) {
    stop("Unable to determine script path")
  }
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
  normalizePath(file.path(dirname(script_path), "..", ".."))
}

BASE_DIR <- get_base_dir()
EXAMPLES_DIR <- file.path(BASE_DIR, "examples")
CIDS_DIR <- file.path(BASE_DIR, "cids")

to_base64url <- function(raw_bytes) {
  encoded <- base64encode(raw_bytes)
  encoded <- sub("=+$", "", encoded)
  chartr("+/", "-_", encoded)
}

encode_length <- function(length) {
  bytes <- writeBin(as.numeric(length), raw(), size = 8, endian = "big")
  to_base64url(bytes[3:8])
}

compute_cid <- function(content) {
  prefix <- encode_length(length(content))
  suffix <- if (length(content) <= 64) {
    to_base64url(content)
  } else {
    hashed <- digest(content, algo = "sha512", serialize = FALSE, raw = TRUE)
    to_base64url(hashed)
  }
  paste0(prefix, suffix)
}

assign("BASE_DIR", BASE_DIR, envir = .GlobalEnv)
assign("EXAMPLES_DIR", EXAMPLES_DIR, envir = .GlobalEnv)
assign("CIDS_DIR", CIDS_DIR, envir = .GlobalEnv)
assign("compute_cid", compute_cid, envir = .GlobalEnv)
