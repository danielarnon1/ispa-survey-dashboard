## Parse all block-format survey CSVs into one long data frame:
## one row per (raw_name, term, class, wave, Q#, QType, QText, answer)

## NOTE: raw survey exports are not in this repo (they contain student names).
## Point these at your own local copy of the source CSVs to reproduce the pipeline.
survey_dir <- "<path to raw survey CSV exports, not included in this repo>"
out_dir    <- "<local output directory for intermediate .rds files>"

files <- list.files(survey_dir, pattern = "^(Fall|Spring).*Survey.*\\.csv$", full.names = TRUE)

term_of  <- function(f) sub(" - POL.*$", "", basename(f))
class_of <- function(f) if (grepl("POL 441", f)) "POL441" else "POL416"
wave_of  <- function(f) if (grepl("Survey III", f)) "III" else if (grepl("Survey II", f)) "II" else "I"

clean_txt <- function(x) {
  x <- gsub(" ", " ", x, fixed = TRUE)   # non-breaking space -> regular space
  x <- gsub("\\s+", " ", x, perl = TRUE)      # collapse whitespace runs
  trimws(x)
}

parse_one <- function(path) {
  df <- read.csv(path, header = TRUE, colClasses = "character",
                  check.names = FALSE, na.strings = NULL, fileEncoding = "UTF-8-BOM")
  names(df) <- c("sec","qnum","qtype","qtitle","qtext","bonus","difficulty","answer","answer_match","nresp")
  df[is.na(df)] <- ""
  df <- as.data.frame(lapply(df, clean_txt), stringsAsFactors = FALSE)

  is_name_row <- df$sec != "" & df$qnum == "" & df$qtype == "" & df$qtitle == ""
  block_id <- cumsum(is_name_row)
  student_name <- df$sec[is_name_row][block_id]
  df$block_id <- block_id
  df$student_name <- student_name

  qrows <- df[df$qnum != "" & !is_name_row, ]
  if (nrow(qrows) == 0) return(NULL)

  key <- paste(qrows$block_id, qrows$qnum, sep = "|")
  split_idx <- split(seq_len(nrow(qrows)), key)

  out <- vector("list", length(split_idx))
  i <- 0
  for (grp in split_idx) {
    i <- i + 1
    g <- qrows[grp, ]
    qtype <- g$qtype[1]
    if (qtype %in% c("MC", "M-S")) {
      sel <- g$answer[g$nresp == "1"]
      ans <- if (length(sel) == 0) NA_character_ else paste(sel, collapse = "; ")
    } else { # FIB, SA
      am <- g$answer_match[1]
      ans <- if (am == "") NA_character_ else am
    }
    out[[i]] <- data.frame(
      student_name = g$student_name[1],
      qnum  = g$qnum[1],
      qtype = qtype,
      qtext = g$qtext[1],
      answer = ans,
      stringsAsFactors = FALSE
    )
  }
  res <- do.call(rbind, out)
  res$file  <- basename(path)
  res$term  <- term_of(path)
  res$class <- class_of(path)
  res$wave  <- wave_of(path)
  res
}

all_list <- lapply(files, parse_one)
long_all <- do.call(rbind, all_list)
rownames(long_all) <- NULL

cat("Total parsed rows:", nrow(long_all), "\n")
cat("Distinct students (raw name x term x class):", length(unique(paste(long_all$student_name, long_all$term, long_all$class))), "\n")
cat("Distinct (term,class,wave) instances:", length(unique(paste(long_all$term, long_all$class, long_all$wave))), "\n")

saveRDS(long_all, file.path(out_dir, "long_all.rds"))
write.csv(long_all, file.path(out_dir, "long_all_RAW_WITH_NAMES.csv"), row.names = FALSE, na = "")
cat("Saved.\n")
