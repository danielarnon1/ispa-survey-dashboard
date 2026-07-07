## Compute wave-by-wave (I/II/III) response distributions for opinion questions,
## for ALL classes combined and for POL441/POL416 separately. Export JSON for dashboard.

panel_long <- readRDS("panel_long.rds")
qdict <- readRDS("qdict.rds")

# focus on repeated opinion battery: CORE (every wave/term) + REPEAT (II&III always, I only Fall21)
opinion_qids <- qdict[qdict$category %in% c("CORE","REPEAT") & qdict$qtype != "SA", ]
opinion_qids <- opinion_qids[!duplicated(opinion_qids$qid), c("qid","label","category","ordinal","levels")]

d <- panel_long[panel_long$qid %in% opinion_qids$qid & !is.na(panel_long$answer), ]

# expand multi-select answers (info sources) into atomic rows for counting
expand_multi <- function(df) {
  idx <- grepl(";", df$answer)
  if (!any(idx)) return(df)
  single <- df[!idx, ]
  multi <- df[idx, ]
  rows <- lapply(seq_len(nrow(multi)), function(i) {
    opts <- trimws(strsplit(multi$answer[i], ";")[[1]])
    r <- multi[rep(i, length(opts)), ]
    r$answer <- opts
    r
  })
  rbind(single, do.call(rbind, rows))
}
d <- expand_multi(d)

# era: before/after the Oct 7, 2023 Hamas attack & Israel-Hamas war.
# Fall 21 / Fall 22 / Spring 23 / Fall 23 -> PRE (Fall 23 was already underway when the
# war broke out ~7 weeks in, treated here as still a "pre-war baseline" cohort per
# user direction). Spring 24 / Fall 25 -> POST.
PRE_TERMS <- c("Fall 21","Fall 22","Spring 23","Fall 23")
POST_TERMS <- c("Spring 24","Fall 25")
d$era <- ifelse(d$term %in% PRE_TERMS, "PRE_OCT7", ifelse(d$term %in% POST_TERMS, "POST_OCT7", NA))
panel_long$era <- ifelse(panel_long$term %in% PRE_TERMS, "PRE_OCT7", ifelse(panel_long$term %in% POST_TERMS, "POST_OCT7", NA))

groupings <- list(ALL = list(field=NULL, val=NULL),
                   POL441 = list(field="class", val="POL441"),
                   POL416 = list(field="class", val="POL416"),
                   PRE_OCT7 = list(field="era", val="PRE_OCT7"),
                   POST_OCT7 = list(field="era", val="POST_OCT7"))

result <- list()
for (qid in unique(d$qid)) {
  qrow <- opinion_qids[opinion_qids$qid == qid, ][1, ]
  sub_q <- d[d$qid == qid, ]
  levels_order <- if (!is.na(qrow$levels) && qrow$levels != "") strsplit(qrow$levels, "\\|")[[1]] else NULL

  by_group <- list()
  for (gname in names(groupings)) {
    gspec <- groupings[[gname]]
    sub_g <- if (is.null(gspec$field)) sub_q else sub_q[sub_q[[gspec$field]] == gspec$val, ]

    by_wave <- list()
    for (w in c("I","II","III")) {
      sub_w <- sub_g[sub_g$wave == w, ]
      n <- length(unique(sub_w$enrollment_id[!is.na(sub_w$answer)]))
      if (nrow(sub_w) == 0) {
        by_wave[[w]] <- list(n = 0, counts = list())
        next
      }
      tab <- table(sub_w$answer)
      # order categories: use canonical level order if ordinal, else by descending freq
      cats <- if (!is.null(levels_order)) levels_order[toupper(levels_order) %in% toupper(names(tab))] else names(sort(tab, decreasing = TRUE))
      # map back to actual-cased names present in tab
      counts <- list()
      for (cat in cats) {
        hit <- names(tab)[toupper(names(tab)) == toupper(cat)]
        cnt <- if (length(hit) == 1) as.integer(tab[hit]) else 0L
        counts[[cat]] <- cnt
      }
      # mean level (only if ordinal)
      mean_level <- if (!is.null(levels_order)) {
        lv <- match(toupper(trimws(sub_w$answer)), toupper(trimws(levels_order)))
        round(mean(lv, na.rm = TRUE), 3)
      } else NA
      by_wave[[w]] <- list(n = n, counts = counts, mean_level = mean_level)
    }
    by_group[[gname]] <- by_wave
  }
  result[[qid]] <- list(
    label = qrow$label,
    category = qrow$category,
    ordinal = qrow$ordinal,
    levels = if (!is.null(levels_order)) as.list(levels_order) else list(),
    data = by_group
  )
}

# also compute enrollment counts per wave per grouping (denominator context)
n_summary <- list()
for (gname in names(groupings)) {
  gspec <- groupings[[gname]]
  sub_g <- if (is.null(gspec$field)) panel_long else panel_long[panel_long[[gspec$field]] == gspec$val, ]
  n_summary[[gname]] <- sapply(c("I","II","III"), function(w) {
    length(unique(sub_g$enrollment_id[sub_g$wave == w]))
  })
}

# term -> era lookup for the footer / methodology note
term_era <- unique(panel_long[, c("term","era")])
term_era <- term_era[order(term_era$era, term_era$term), ]

## ---- PRE vs POST Oct-7 era_stats, attached to each question for the dashboard ----
## Wave I only (pre-instruction baseline) is the clean comparison: unconfounded by
## course exposure, since neither cohort has had any class time yet.
panel_long$is_waveI <- panel_long$wave == "I"
for (qid in names(result)) {
  q <- result[[qid]]
  pre_I  <- q$data$PRE_OCT7$I
  post_I <- q$data$POST_OCT7$I
  if (isTRUE(q$ordinal)) {
    delta <- if (!is.null(pre_I$mean_level) && !is.null(post_I$mean_level)) round(post_I$mean_level - pre_I$mean_level, 3) else NA
    n_levels <- length(q$levels)
    delta_pct <- if (!is.na(delta)) round(100 * delta / (n_levels - 1), 1) else NA
    sub <- panel_long[panel_long$qid == qid & panel_long$is_waveI & !is.na(panel_long$answer_level) &
                         panel_long$era %in% c("PRE_OCT7","POST_OCT7"), ]
    pre_vals  <- sub$answer_level[sub$era == "PRE_OCT7"]
    post_vals <- sub$answer_level[sub$era == "POST_OCT7"]
    p_value <- NA
    if (length(pre_vals) >= 3 && length(post_vals) >= 3) {
      tt <- tryCatch(t.test(post_vals, pre_vals), error = function(e) NULL)
      if (!is.null(tt)) p_value <- round(tt$p.value, 4)
    }
    result[[qid]]$era_stats <- list(
      pre_n = pre_I$n, post_n = post_I$n,
      pre_mean = pre_I$mean_level, post_mean = post_I$mean_level,
      delta = delta, delta_pct_of_scale = delta_pct, p_value = p_value
    )
  } else {
    pre_c <- pre_I$counts; post_c <- post_I$counts
    pre_n <- sum(unlist(pre_c)); post_n <- sum(unlist(post_c))
    shifts <- list()
    if (pre_n > 0 && post_n > 0) {
      cats <- union(names(pre_c), names(post_c))
      for (cat_name in cats) {
        p_pre  <- 100 * (if (!is.null(pre_c[[cat_name]])) pre_c[[cat_name]] else 0) / pre_n
        p_post <- 100 * (if (!is.null(post_c[[cat_name]])) post_c[[cat_name]] else 0) / post_n
        shifts[[cat_name]] <- round(p_post - p_pre, 1)
      }
    }
    result[[qid]]$era_stats <- list(pre_n = pre_n, post_n = post_n, pct_point_shifts = shifts)
  }
}

library(jsonlite)
json_out <- toJSON(list(questions = result, n_summary = n_summary), auto_unbox = TRUE, pretty = FALSE, na = "null")
writeLines(json_out, "opinion_dashboard_data.json")
cat("Wrote opinion_dashboard_data.json,", length(result), "questions,",
    nchar(json_out), "chars\n")

# also a compact CSV summary of means by wave x grouping for quick reference
rows <- list()
for (qid in names(result)) {
  q <- result[[qid]]
  if (!isTRUE(q$ordinal)) next
  for (gname in names(q$data)) {
    for (w in c("I","II","III")) {
      bw <- q$data[[gname]][[w]]
      rows[[length(rows)+1]] <- data.frame(qid=qid, label=q$label, grouping=gname, wave=w,
                                            n=bw$n, mean_level=ifelse(is.null(bw$mean_level), NA, bw$mean_level))
    }
  }
}
means_df <- do.call(rbind, rows)
write.csv(means_df, "opinion_means_by_wave.csv", row.names = FALSE, na = "")
cat("Wrote opinion_means_by_wave.csv,", nrow(means_df), "rows\n")

## ---- PRE vs POST Oct-7 comparison report ----
## (a) baseline (Wave I, pre-instruction) comparison -- cleanest read on whether
##     incoming student attitudes shifted, unconfounded by course exposure
## (b) all-waves-pooled comparison -- robustness check using the full sample
cat("\n================ PRE vs POST OCT-7 COMPARISON ================\n")

era_report <- list()
for (qid in names(result)) {
  q <- result[[qid]]
  if (!isTRUE(q$ordinal)) next
  pre_I  <- q$data$PRE_OCT7$I
  post_I <- q$data$POST_OCT7$I
  pre_all_n  <- sum(sapply(c("I","II","III"), function(w) q$data$PRE_OCT7[[w]]$n))
  post_all_n <- sum(sapply(c("I","II","III"), function(w) q$data$POST_OCT7[[w]]$n))

  # pooled mean across waves, weighted by n at each wave
  pooled_mean <- function(era) {
    ws <- c("I","II","III")
    ns <- sapply(ws, function(w) q$data[[era]][[w]]$n)
    ms <- sapply(ws, function(w) { m <- q$data[[era]][[w]]$mean_level; if (is.null(m)) NA else m })
    if (all(is.na(ms)) || sum(ns, na.rm=TRUE) == 0) return(NA)
    sum(ms * ns, na.rm = TRUE) / sum(ns[!is.na(ms)], na.rm = TRUE)
  }

  era_report[[qid]] <- data.frame(
    qid = qid, label = q$label, n_levels = length(q$levels),
    pre_waveI_n = pre_I$n, pre_waveI_mean = ifelse(is.null(pre_I$mean_level), NA, pre_I$mean_level),
    post_waveI_n = post_I$n, post_waveI_mean = ifelse(is.null(post_I$mean_level), NA, post_I$mean_level),
    waveI_delta = ifelse(is.null(post_I$mean_level) || is.null(pre_I$mean_level), NA,
                          post_I$mean_level - pre_I$mean_level),
    pre_pooled_mean = pooled_mean("PRE_OCT7"), post_pooled_mean = pooled_mean("POST_OCT7"),
    pooled_delta = pooled_mean("POST_OCT7") - pooled_mean("PRE_OCT7"),
    stringsAsFactors = FALSE
  )
}
era_df <- do.call(rbind, era_report)
era_df$waveI_delta_pct_of_scale <- round(100 * era_df$waveI_delta / (era_df$n_levels - 1), 1)

# Welch two-sample t-test on individual-level answer_level, Wave I only, PRE vs POST
era_df$t_p_value <- NA_real_
panel_long$is_waveI <- panel_long$wave == "I"
for (i in seq_len(nrow(era_df))) {
  qid <- era_df$qid[i]
  sub <- panel_long[panel_long$qid == qid & panel_long$is_waveI & !is.na(panel_long$answer_level) &
                       panel_long$era %in% c("PRE_OCT7","POST_OCT7"), ]
  pre_vals  <- sub$answer_level[sub$era == "PRE_OCT7"]
  post_vals <- sub$answer_level[sub$era == "POST_OCT7"]
  if (length(pre_vals) >= 3 && length(post_vals) >= 3) {
    tt <- tryCatch(t.test(post_vals, pre_vals), error = function(e) NULL)
    if (!is.null(tt)) era_df$t_p_value[i] <- round(tt$p.value, 4)
  }
}

era_df <- era_df[order(-abs(era_df$waveI_delta_pct_of_scale)), ]
write.csv(era_df, "era_comparison_ordinal.csv", row.names = FALSE, na = "")
print(era_df[, c("label","pre_waveI_n","pre_waveI_mean","post_waveI_n","post_waveI_mean",
                  "waveI_delta","waveI_delta_pct_of_scale","t_p_value")], row.names = FALSE)

# nominal questions: compare category shares (percentage points) between eras, wave I only
cat("\n---- Nominal / categorical questions: Wave I category share, PRE vs POST (pct points) ----\n")
for (qid in names(result)) {
  q <- result[[qid]]
  if (isTRUE(q$ordinal)) next
  pre_c  <- q$data$PRE_OCT7$I$counts
  post_c <- q$data$POST_OCT7$I$counts
  pre_n  <- sum(unlist(pre_c)); post_n <- sum(unlist(post_c))
  if (pre_n == 0 || post_n == 0) next
  cats <- union(names(pre_c), names(post_c))
  cat("\n", q$label, " (pre n=", pre_n, ", post n=", post_n, ")\n", sep = "")
  for (cat_name in cats) {
    p_pre  <- 100 * (if (!is.null(pre_c[[cat_name]])) pre_c[[cat_name]] else 0) / pre_n
    p_post <- 100 * (if (!is.null(post_c[[cat_name]])) post_c[[cat_name]] else 0) / post_n
    cat(sprintf("   %-90s %5.1f%% -> %5.1f%%  (%+.1f pt)\n", substr(cat_name,1,90), p_pre, p_post, p_post - p_pre))
  }
}
cat("\nWrote era_comparison_ordinal.csv\n")
