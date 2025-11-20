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
  value <- as.integer(length)
  bytes <- raw(6)

  for (i in 6:1) {
    bytes[i] <- as.raw(bitwAnd(value, 0xFF))
    value <- bitwShiftR(value, 8)
  }

  to_base64url(bytes)
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

download_cid <- function(base_url, cid) {
  suppressPackageStartupMessages(library(httr))
  url <- paste0(sub("/$", "", base_url), "/", cid)
  
  tryCatch({
    response <- GET(url, timeout(10))
    
    if (status_code(response) != 200) {
      stop(sprintf("HTTP %d", status_code(response)))
    }
    
    content <- content(response, "raw")
    computed <- compute_cid(content)
    is_valid <- identical(computed, cid)
    
    list(content = content, computed = computed, is_valid = is_valid)
  }, error = function(e) {
    stop(e$message)
  })
}

assign("BASE_DIR", BASE_DIR, envir = .GlobalEnv)
assign("EXAMPLES_DIR", EXAMPLES_DIR, envir = .GlobalEnv)
assign("CIDS_DIR", CIDS_DIR, envir = .GlobalEnv)
assign("compute_cid", compute_cid, envir = .GlobalEnv)
assign("download_cid", download_cid, envir = .GlobalEnv)
