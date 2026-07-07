## Assemble final anonymized panel: long format (primary deliverable) + wide format.

long_all <- readRDS("long_all.rds")
lookup   <- readRDS("id_lookup.rds")
qdict    <- readRDS("qdict.rds")

d <- merge(long_all, lookup, by = c("student_name","term","class"), all.x = TRUE)
stopifnot(all(!is.na(d$uniqueid)))

d <- merge(d, qdict[, c("raw_text","qid","label","category","ordinal","levels")],
           by.x = "qtext", by.y = "raw_text", all.x = TRUE)
stopifnot(all(!is.na(d$qid)))

d$wave_num <- ifelse(d$wave == "I", 1, ifelse(d$wave == "II", 2, 3))

# compute ordinal level code (position within the canonical level order), case-insensitive
level_code <- function(answer, ordinal, levels) {
  out <- rep(NA_integer_, length(answer))
  for (i in seq_along(answer)) {
    if (!ordinal[i] || is.na(answer[i]) || is.na(levels[i])) next
    lv <- strsplit(levels[i], "\\|")[[1]]
    pos <- which(toupper(trimws(lv)) == toupper(trimws(answer[i])))
    if (length(pos) == 1) out[i] <- pos
  }
  out
}
d$answer_level <- level_code(d$answer, d$ordinal, d$levels)
d$n_levels <- ifelse(d$ordinal, sapply(strsplit(ifelse(is.na(d$levels),"",d$levels), "\\|"), length), NA)

panel_long <- d[, c("uniqueid","enrollment_id","class","term","wave","wave_num",
                     "qid","label","category","qtype","answer","answer_level","n_levels")]
names(panel_long)[names(panel_long) == "label"] <- "question_label"
panel_long <- panel_long[order(panel_long$uniqueid, panel_long$enrollment_id, panel_long$wave_num, panel_long$qid), ]

write.csv(panel_long, "panel_long.csv", row.names = FALSE, na = "")
cat("panel_long.csv:", nrow(panel_long), "rows,", length(unique(panel_long$enrollment_id)), "enrollments,",
    length(unique(panel_long$uniqueid)), "unique respondents\n")

## ---- WIDE format: one row per enrollment x wave, one column per qid ----
wide_key <- unique(d[, c("uniqueid","enrollment_id","class","term","wave","wave_num")])
qids <- sort(unique(d$qid))
wide <- wide_key
for (q in qids) {
  sub <- d[d$qid == q, c("enrollment_id","wave_num","answer")]
  names(sub)[3] <- q
  wide <- merge(wide, sub, by = c("enrollment_id","wave_num"), all.x = TRUE)
}
wide <- wide[order(wide$uniqueid, wide$enrollment_id, wide$wave_num), ]
write.csv(wide, "panel_wide.csv", row.names = FALSE, na = "")
cat("panel_wide.csv:", nrow(wide), "rows (enrollment x wave),", ncol(wide), "columns\n")

## ---- Demographics: one row per enrollment (from wave I only) ----
demo_qids <- qdict$qid[qdict$category == "DEMO"]
demo <- d[d$qid %in% demo_qids & d$wave == "I", c("enrollment_id","qid","answer")]
demo_wide <- unique(d[d$wave == "I", c("uniqueid","enrollment_id","class","term")])
for (q in demo_qids) {
  sub <- demo[demo$qid == q, c("enrollment_id","answer")]
  names(sub)[2] <- q
  demo_wide <- merge(demo_wide, sub, by = "enrollment_id", all.x = TRUE)
}
demo_wide <- demo_wide[order(demo_wide$uniqueid), ]
write.csv(demo_wide, "demographics_wide.csv", row.names = FALSE, na = "")
cat("demographics_wide.csv:", nrow(demo_wide), "rows (one per enrollment)\n")

saveRDS(panel_long, "panel_long.rds")
saveRDS(wide, "panel_wide.rds")
