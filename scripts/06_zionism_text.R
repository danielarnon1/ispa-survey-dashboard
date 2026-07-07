## Word cloud + rudimentary (bag-of-words, Bing lexicon) sentiment analysis for the
## open-ended "What does Zionism mean to you?" question, sliced the same way as the
## other opinion questions: by class (ALL/POL441/POL416), by era (PRE/POST Oct 7), by wave.

library(tidytext)
library(dplyr)
library(stringr)
library(jsonlite)

panel_long <- readRDS("panel_long.rds")

PRE_TERMS  <- c("Fall 21","Fall 22","Spring 23","Fall 23")
POST_TERMS <- c("Spring 24","Fall 25")
panel_long$era <- ifelse(panel_long$term %in% PRE_TERMS, "PRE_OCT7",
                   ifelse(panel_long$term %in% POST_TERMS, "POST_OCT7", NA))

d <- panel_long[panel_long$qid == "O_ZIONISM_MEANING" & !is.na(panel_long$answer), ]
d <- d[, c("uniqueid","enrollment_id","class","era","wave","answer")]
cat("Total Zionism free-text responses:", nrow(d), "\n")

groupings <- list(ALL = list(field=NULL, val=NULL),
                   POL441 = list(field="class", val="POL441"),
                   POL416 = list(field="class", val="POL416"),
                   PRE_OCT7 = list(field="era", val="PRE_OCT7"),
                   POST_OCT7 = list(field="era", val="POST_OCT7"))

bing <- get_sentiments("bing")  # word -> "positive"/"negative", bundled/cached, offline

# stopwords: standard SMART list + the tautological question terms themselves
data(stop_words)
extra_stop <- data.frame(word = c("zionism","zionist","zionists","2","5","10"), lexicon = "extra")
stopset <- bind_rows(stop_words, extra_stop)

analyze_group <- function(sub) {
  n <- length(unique(sub$enrollment_id))
  if (n == 0) return(list(n = 0, word_freq = list(), sentiment = list(), quotes = list()))

  # word frequencies for the cloud
  words <- sub %>%
    mutate(doc_id = row_number()) %>%
    unnest_tokens(word, answer) %>%
    filter(!word %in% stopset$word, str_detect(word, "[a-z]"), nchar(word) > 2)
  wf <- words %>% count(word, sort = TRUE) %>% head(50)
  word_freq <- setNames(as.list(wf$n), wf$word)

  # rudimentary sentiment: net (pos-neg) bing-word count per response
  scored <- sub %>%
    mutate(doc_id = row_number()) %>%
    unnest_tokens(word, answer) %>%
    inner_join(bing, by = "word") %>%
    count(doc_id, sentiment) %>%
    tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
  if (!"positive" %in% names(scored)) scored$positive <- 0
  if (!"negative" %in% names(scored)) scored$negative <- 0

  all_docs <- data.frame(doc_id = seq_len(nrow(sub)))
  scored <- merge(all_docs, scored, by = "doc_id", all.x = TRUE)
  scored$positive[is.na(scored$positive)] <- 0
  scored$negative[is.na(scored$negative)] <- 0
  scored$net <- scored$positive - scored$negative
  scored$label <- ifelse(scored$net > 0, "Positive", ifelse(scored$net < 0, "Negative", "Neutral"))

  tab <- table(factor(scored$label, levels = c("Positive","Neutral","Negative")))

  # which lexicon words actually drove the classification, for transparency
  sw <- sub %>% mutate(doc_id = row_number()) %>% unnest_tokens(word, answer) %>%
    inner_join(bing, by = "word") %>% count(word, sentiment, sort = TRUE)
  top_pos_words <- sw[sw$sentiment == "positive", ][1:min(8, sum(sw$sentiment=="positive")), c("word","n")]
  top_neg_words <- sw[sw$sentiment == "negative", ][1:min(8, sum(sw$sentiment=="negative")), c("word","n")]

  sentiment <- list(
    n = nrow(scored),
    counts = as.list(setNames(as.integer(tab), names(tab))),
    avg_net_score = round(mean(scored$net), 3),
    pct_positive = round(100 * tab["Positive"] / nrow(scored), 1),
    pct_negative = round(100 * tab["Negative"] / nrow(scored), 1),
    top_positive_words = if (nrow(top_pos_words) > 0) setNames(as.list(top_pos_words$n), top_pos_words$word) else list(),
    top_negative_words = if (nrow(top_neg_words) > 0) setNames(as.list(top_neg_words$n), top_neg_words$word) else list()
  )

  # NOTE: deliberately not including verbatim quotes anywhere in the output --
  # this JSON is embedded in a page intended to be publicly hosted, and even
  # anonymized free-text quotes from a small class are a re-identification risk.
  list(n = n, word_freq = word_freq, sentiment = sentiment)
}

result <- list()
for (gname in names(groupings)) {
  gspec <- groupings[[gname]]
  sub_g <- if (is.null(gspec$field)) d else d[d[[gspec$field]] == gspec$val, ]
  by_wave <- list()
  for (w in c("I","II","III")) {
    by_wave[[w]] <- analyze_group(sub_g[sub_g$wave == w, ])
  }
  result[[gname]] <- by_wave
}

json_out <- toJSON(list(zionism_text = result), auto_unbox = TRUE, pretty = FALSE, na = "null")
writeLines(json_out, "zionism_text_data.json")
cat("Wrote zionism_text_data.json,", nchar(json_out), "chars\n")

# quick console summary
for (gname in names(result)) {
  cat("\n==", gname, "==\n")
  for (w in c("I","II","III")) {
    s <- result[[gname]][[w]]$sentiment
    if (length(s) == 0) { cat(" ", w, ": no data\n"); next }
    cat(sprintf("  %s: n=%d, pos=%.1f%%, neg=%.1f%%, avg net=%.2f\n",
                w, s$n, s$pct_positive, s$pct_negative, s$avg_net_score))
  }
}
