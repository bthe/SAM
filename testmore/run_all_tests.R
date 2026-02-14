#!/usr/bin/env Rscript

detect_script_dir <- function() {
  ca <- commandArgs()
  f <- ca[grepl("^--file=", ca)]
  if (length(f) == 0) return(getwd())
  normalizePath(dirname(sub("^--file=", "", f[1])), winslash = "/", mustWork = FALSE)
}

parse_args <- function(args) {
  cfg <- list(
    root = detect_script_dir(),
    r_bin = "R",
    include_ignored = FALSE,
    keep_resout = FALSE,
    timeout_sec = Inf,
    outdir = file.path("testmore_results")
  )

  for (a in args) {
    if (a == "--include-ignored") cfg$include_ignored <- TRUE
    else if (a == "--keep-resout") cfg$keep_resout <- TRUE
    else if (startsWith(a, "--root=")) cfg$root <- sub("^--root=", "", a)
    else if (startsWith(a, "--r=")) cfg$r_bin <- sub("^--r=", "", a)
    else if (startsWith(a, "--outdir=")) cfg$outdir <- sub("^--outdir=", "", a)
    else if (startsWith(a, "--timeout=")) {
      x <- suppressWarnings(as.numeric(sub("^--timeout=", "", a)))
      if (!is.na(x) && is.finite(x) && x > 0) cfg$timeout_sec <- x
    }
  }

  cfg
}

norm_lines <- function(path) {
  if (!file.exists(path)) return(character())
  x <- readLines(path, warn = FALSE)
  sub("\r$", "", x)
}

first_diff <- function(exp_path, out_path, max_lines = 8L) {
  e <- norm_lines(exp_path)
  o <- norm_lines(out_path)
  n <- min(length(e), length(o))
  idx <- which(e[seq_len(n)] != o[seq_len(n)])
  if (length(idx) > 0) {
    i <- idx[1]
    return(sprintf("First mismatch at line %d\nEXP: %s\nOUT: %s", i, e[i], o[i]))
  }
  if (length(e) != length(o)) {
    return(sprintf("Line count mismatch: exp=%d out=%d", length(e), length(o)))
  }
  ""
}

get_timeout_bin <- function() {
  t1 <- Sys.which("timeout")
  if (nzchar(t1)) return(t1)
  t2 <- Sys.which("gtimeout")
  if (nzchar(t2)) return(t2)
  ""
}

run_one <- function(case_dir, cfg, timeout_bin) {
  case_name <- basename(case_dir)
  ignore_file <- file.path(case_dir, "ignore")
  exp_file <- file.path(case_dir, "res.EXP")
  out_file <- file.path(case_dir, "res.out")

  if (file.exists(ignore_file) && !cfg$include_ignored) {
    return(list(
      case = case_name, status = "SKIP", elapsed_sec = 0,
      detail = "Skipped (ignore file present)", rc = NA_integer_
    ))
  }

  if (file.exists(out_file)) unlink(out_file)

  args <- c("--slave", "-f", "script.R")
  cmd <- cfg$r_bin

  if (is.finite(cfg$timeout_sec) && nzchar(timeout_bin)) {
    args <- c(as.character(cfg$timeout_sec), cfg$r_bin, args)
    cmd <- timeout_bin
  }

  t0 <- proc.time()[["elapsed"]]
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(case_dir)

  out <- tryCatch(
    system2(cmd, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(character(), status = 999L, errmsg = conditionMessage(e))
  )
  elapsed <- proc.time()[["elapsed"]] - t0
  rc <- attr(out, "status")
  if (is.null(rc)) rc <- 0L
  rc <- as.integer(rc)

  if (rc != 0L) {
    if (rc %in% c(124L, 137L)) {
      return(list(case = case_name, status = "TIMEOUT", elapsed_sec = elapsed,
                  detail = paste(utils::tail(out, 10), collapse = "\n"), rc = rc))
    }
    if (rc == 999L) {
      msg <- attr(out, "errmsg")
      return(list(case = case_name, status = "ERROR", elapsed_sec = elapsed,
                  detail = if (is.null(msg)) "Execution error" else msg, rc = rc))
    }
    return(list(case = case_name, status = "ERROR", elapsed_sec = elapsed,
                detail = paste(utils::tail(out, 15), collapse = "\n"), rc = rc))
  }

  if (!file.exists(out_file)) {
    return(list(case = case_name, status = "ERROR", elapsed_sec = elapsed,
                detail = "Missing res.out", rc = rc))
  }

  ok <- identical(norm_lines(exp_file), norm_lines(out_file))
  if (!cfg$keep_resout && ok && file.exists(out_file)) unlink(out_file)

  if (ok) {
    return(list(case = case_name, status = "PASS", elapsed_sec = elapsed, detail = "", rc = rc))
  }

  list(
    case = case_name, status = "FAIL", elapsed_sec = elapsed,
    detail = first_diff(exp_file, out_file), rc = rc
  )
}

main <- function() {
  cfg <- parse_args(commandArgs(trailingOnly = TRUE))
  root <- normalizePath(cfg$root, winslash = "/", mustWork = TRUE)
  setwd(root)

  all_dirs <- list.dirs(".", recursive = FALSE, full.names = TRUE)
  cases <- all_dirs[
    file.exists(file.path(all_dirs, "script.R")) &
      file.exists(file.path(all_dirs, "res.EXP"))
  ]
  cases <- sort(cases)

  if (length(cases) == 0L) stop("No testmore cases found (need script.R + res.EXP).")

  timeout_bin <- get_timeout_bin()
  if (is.finite(cfg$timeout_sec) && !nzchar(timeout_bin)) {
    message("No timeout binary found (timeout/gtimeout); running without timeout.")
  }

  dir.create(cfg$outdir, recursive = TRUE, showWarnings = FALSE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  csv_path <- file.path(cfg$outdir, paste0("summary_", stamp, ".csv"))
  txt_path <- file.path(cfg$outdir, paste0("details_", stamp, ".txt"))

  results <- lapply(cases, run_one, cfg = cfg, timeout_bin = timeout_bin)
  df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      case = x$case,
      status = x$status,
      elapsed_sec = round(x$elapsed_sec, 3),
      rc = if (is.null(x$rc)) NA_integer_ else x$rc,
      stringsAsFactors = FALSE
    )
  }))

  utils::write.csv(df, csv_path, row.names = FALSE)

  detail_lines <- c(
    sprintf("Root: %s", root),
    sprintf("R: %s", cfg$r_bin),
    sprintf("Timeout(sec): %s", if (is.finite(cfg$timeout_sec)) cfg$timeout_sec else "none"),
    sprintf("Include ignored: %s", cfg$include_ignored),
    sprintf("Keep res.out: %s", cfg$keep_resout),
    ""
  )
  for (x in results) {
    if (nzchar(x$detail)) {
      detail_lines <- c(detail_lines, sprintf("[%s] %s", x$status, x$case), x$detail, "")
    }
  }
  writeLines(detail_lines, con = txt_path, useBytes = TRUE)

  counts <- sort(table(df$status), decreasing = TRUE)
  cat("Testmore summary\n")
  print(df, row.names = FALSE)
  cat("\nCounts:\n")
  print(counts)
  cat(sprintf("\nWrote:\n- %s\n- %s\n", csv_path, txt_path))
}

main()
