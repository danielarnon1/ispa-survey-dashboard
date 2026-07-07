## Build anonymized uniqueid crosswalk (person-level, matched by normalized full name
## across ALL files/terms/classes) and enrollment_id (person x class x term instance).

long_all <- readRDS("long_all.rds")

norm_name <- function(x) toupper(trimws(gsub("\\s+", " ", x)))

roster <- unique(long_all[, c("student_name", "term", "class")])
roster$norm <- norm_name(roster$student_name)

# sanity: does any normalized name map to >1 distinct raw spelling? (would indicate
# case/whitespace variants we should double check)
spellings <- aggregate(student_name ~ norm, data = roster, FUN = function(x) length(unique(x)))
multi <- spellings[spellings$student_name > 1, ]
if (nrow(multi) > 0) {
  cat("NOTE: normalized names with >1 raw spelling (will still be merged):\n")
  print(multi)
}

person_ids <- sort(unique(roster$norm))
id_map <- data.frame(
  norm = person_ids,
  uniqueid = sprintf("R%03d", seq_along(person_ids)),
  stringsAsFactors = FALSE
)

roster <- merge(roster, id_map, by = "norm")
roster$enrollment_id <- paste(roster$uniqueid, roster$class, gsub(" ", "", roster$term), sep = "_")

# how many people appear in more than one class/term (cross-enrolled)?
per_person <- aggregate(enrollment_id ~ uniqueid, data = unique(roster[, c("uniqueid","enrollment_id")]), FUN = length)
cross <- per_person[per_person$enrollment_id > 1, ]
cat("People with multiple enrollments (took the class more than once, or both POL441 & POL416):", nrow(cross), "\n")
if (nrow(cross) > 0) {
  detail <- roster[roster$uniqueid %in% cross$uniqueid, c("uniqueid","student_name","class","term")]
  detail <- unique(detail[order(detail$uniqueid), ])
  print(detail, row.names = FALSE)
}

cat("\nTotal unique respondents (uniqueid):", length(unique(roster$uniqueid)), "\n")
cat("Total enrollment instances (uniqueid x class x term):", length(unique(roster$enrollment_id)), "\n")

# PRIVATE crosswalk (contains PII) -- keep separate from anonymized outputs
crosswalk_private <- roster[, c("uniqueid", "enrollment_id", "student_name", "class", "term")]
crosswalk_private <- crosswalk_private[order(crosswalk_private$uniqueid), ]
write.csv(crosswalk_private, "PRIVATE_name_crosswalk.csv", row.names = FALSE)

# public lookup: student_name -> uniqueid/enrollment_id (needed to join into long_all)
lookup <- roster[, c("student_name", "term", "class", "uniqueid", "enrollment_id")]
saveRDS(lookup, "id_lookup.rds")
cat("\nSaved PRIVATE_name_crosswalk.csv and id_lookup.rds\n")
