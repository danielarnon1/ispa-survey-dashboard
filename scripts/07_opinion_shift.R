## Opinion strength & shift: for each applicable ordinal question with a true
## center category, classify each matched respondent's starting position
## (Strong/Moderate/Neutral, + side) and where they end up, per grouping.

library(jsonlite)

panel_long <- readRDS("panel_long.rds")
qdict <- readRDS("qdict.rds")

PRE_TERMS  <- c("Fall 21","Fall 22","Spring 23","Fall 23")
POST_TERMS <- c("Spring 24","Fall 25")
panel_long$era <- ifelse(panel_long$term %in% PRE_TERMS, "PRE_OCT7",
                   ifelse(panel_long$term %in% POST_TERMS, "POST_OCT7", NA))

## qid -> start wave (CORE: Wave I; REPEAT: Wave II, since Wave I is only
## populated for Fall21 for these four)
qcat <- qdict[!duplicated(qdict$qid), c("qid","label","category","levels")]
CORE_CENTER   <- c("O_RIGHT_EXIST","O_APARTHEID","O_RACISM_SIMILAR","O_GENOCIDE",
                    "O_CONCERN_ANTISEMITISM","O_CONCERN_ISLAMOPHOBIA",
                    "O_SUPPORT_ISRAEL","O_SUPPORT_PALESTINIANS")
REPEAT_CENTER <- c("P_ZION_ATTACHMENT","P_ZION_JEWISH_DEM_STATE","P_ZION_PRIVILEGE","P_AID_RESTRICT")
APPLICABLE <- c(CORE_CENTER, REPEAT_CENTER)
start_wave_of <- function(qid) if (qid %in% REPEAT_CENTER) "II" else "I"

groupings <- list(ALL = list(field=NULL, val=NULL),
                   POL441 = list(field="class", val="POL441"),
                   POL416 = list(field="class", val="POL416"),
                   PRE_OCT7 = list(field="era", val="PRE_OCT7"),
                   POST_OCT7 = list(field="era", val="POST_OCT7"))

classify <- function(level, n_levels) {
  center <- (n_levels + 1) / 2
  dist <- abs(level - center)
  bucket <- if (dist == 0) "Neutral" else if (dist == center - 1) "Strong" else "Moderate"
  side <- if (level < center) "A" else if (level > center) "B" else "None"
  list(bucket = bucket, side = side)
}

pct <- function(x, n) if (n == 0) NA else round(100 * x / n, 1)

analyze_question <- function(qid, sub_all) {
  qrow <- qcat[qcat$qid == qid, ][1, ]
  levels_order <- strsplit(qrow$levels, "\\|")[[1]]
  n_levels <- length(levels_order)
  sw <- start_wave_of(qid)

  sub_q <- sub_all[sub_all$qid == qid & !is.na(sub_all$answer_level), ]

  by_group <- list()
  for (gname in names(groupings)) {
    gspec <- groupings[[gname]]
    sub_g <- if (is.null(gspec$field)) sub_q else sub_q[sub_q[[gspec$field]] == gspec$val, ]

    # outcome candidates are whichever of II/III are NOT the start wave itself,
    # preferring III -- e.g. for REPEAT qids (start = Wave II), Wave II can't
    # also serve as its own "outcome", so only Wave III counts there.
    outcome_waves <- setdiff(c("III","II"), sw)
    s <- sub_g[sub_g$wave == sw, c("enrollment_id","answer_level")]; names(s)[2] <- "start"
    m <- s
    m$outcome <- NA_real_
    for (ow in outcome_waves) {
      o <- sub_g[sub_g$wave == ow, c("enrollment_id","answer_level")]; names(o)[2] <- "cand"
      m <- merge(m, o, by = "enrollment_id", all.x = TRUE)
      m$outcome <- ifelse(is.na(m$outcome), m$cand, m$outcome)
      m$cand <- NULL
    }
    m <- m[!is.na(m$outcome), ]

    n_total <- nrow(m)
    if (n_total == 0) {
      by_group[[gname]] <- list(n = 0, composition = list(), started_strong = NULL,
                                 started_moderate = NULL, started_neutral = NULL)
      next
    }

    sc <- mapply(classify, m$start, n_levels, SIMPLIFY = FALSE)
    oc <- mapply(classify, m$outcome, n_levels, SIMPLIFY = FALSE)
    m$start_bucket <- sapply(sc, `[[`, "bucket"); m$start_side <- sapply(sc, `[[`, "side")
    m$out_bucket   <- sapply(oc, `[[`, "bucket"); m$out_side   <- sapply(oc, `[[`, "side")
    m$flipped <- m$start_side != "None" & m$out_side != "None" & m$start_side != m$out_side

    comp_tab <- table(factor(m$start_bucket, levels = c("Strong","Moderate","Neutral")))
    composition <- list(n = n_total, counts = as.list(setNames(as.integer(comp_tab), names(comp_tab))))

    mk_outcome <- function(bucket_name, outcome_levels, outcome_labels) {
      sub <- m[m$start_bucket == bucket_name, ]
      n <- nrow(sub)
      if (n == 0) return(list(n = 0, counts = list(), pct = list(), flipped_n = 0))
      cnt <- sapply(outcome_levels, function(ol) sum(sub$.outcome_class == ol))
      list(n = n,
           counts = as.list(setNames(as.integer(cnt), outcome_labels)),
           pct = as.list(setNames(round(100 * cnt / n, 1), outcome_labels)),
           flipped_n = sum(sub$flipped))
    }

    m$.outcome_class <- ifelse(m$start_bucket == "Strong",
                                ifelse(m$out_bucket == "Strong", "stayed", "moderated"),
                         ifelse(m$start_bucket == "Moderate",
                                ifelse(m$out_bucket == "Strong", "polarized",
                                ifelse(m$out_bucket == "Moderate", "stayed", "neutralized")),
                                ifelse(m$out_bucket == "Neutral", "stayed",
                                ifelse(m$out_side == "A", "sideA", "sideB"))))

    started_strong <- mk_outcome("Strong", c("stayed","moderated"), c("Stayed strong","Moderated"))
    started_moderate <- if (n_levels >= 5)
      mk_outcome("Moderate", c("polarized","stayed","neutralized"), c("Polarized","Stayed moderate","Moved to neutral"))
      else NULL
    started_neutral <- mk_outcome("Neutral", c("stayed","sideA","sideB"),
                                   c("Stayed neutral", paste0("Moved toward ", levels_order[1]),
                                     paste0("Moved toward ", levels_order[n_levels])))

    by_group[[gname]] <- list(n = n_total, composition = composition,
                               started_strong = started_strong,
                               started_moderate = started_moderate,
                               started_neutral = started_neutral)
  }

  list(label = qrow$label, category = qrow$category, start_wave = sw,
       levels = as.list(levels_order), n_levels = n_levels, data = by_group)
}

result <- list()
for (qid in APPLICABLE) result[[qid]] <- analyze_question(qid, panel_long)

json_out <- toJSON(list(opinion_shift = result), auto_unbox = TRUE, pretty = FALSE, na = "null")
writeLines(json_out, "opinion_shift_data.json")
cat("Wrote opinion_shift_data.json,", length(result), "questions,", nchar(json_out), "chars\n")

## console summary (ALL grouping) for sanity check
cat("\n================ OPINION SHIFT SUMMARY (ALL) ================\n")
for (qid in APPLICABLE) {
  d <- result[[qid]]$data$ALL
  if (d$n == 0) { cat(qid, ": no data\n"); next }
  comp <- d$composition$counts
  cat(sprintf("\n%s (n=%d, start=Wave %s)\n", result[[qid]]$label, d$n, result[[qid]]$start_wave))
  cat(sprintf("  Composition: Strong=%d (%.0f%%) Moderate=%d (%.0f%%) Neutral=%d (%.0f%%)\n",
              comp$Strong %||% 0, pct(comp$Strong %||% 0, d$n),
              comp$Moderate %||% 0, pct(comp$Moderate %||% 0, d$n),
              comp$Neutral %||% 0, pct(comp$Neutral %||% 0, d$n)))
  if (!is.null(d$started_strong) && d$started_strong$n > 0)
    cat(sprintf("  Started Strong (n=%d): %s\n", d$started_strong$n,
                paste(names(d$started_strong$pct), unlist(d$started_strong$pct), sep="=", collapse=", ")))
  if (!is.null(d$started_neutral) && d$started_neutral$n > 0)
    cat(sprintf("  Started Neutral (n=%d): %s\n", d$started_neutral$n,
                paste(names(d$started_neutral$pct), unlist(d$started_neutral$pct), sep="=", collapse=", ")))
}
