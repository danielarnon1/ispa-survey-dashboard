# ISPA Conflict Class Survey Dashboard

Analysis of pre/mid/post-semester opinion surveys from two courses on the
Israeli-Palestinian conflict — POL 441 (Fall 2021, 2022, 2023, 2025) and
POL 416 (Spring 2023, 2024). Each course ran three survey waves per term
(entry, mid-semester, end); Survey I also collected demographics.

**Live dashboard:** _(add your GitHub Pages URL here once enabled — see below)_

## What's in the dashboard

- Wave-by-wave (Survey I → II → III) distributions for every repeated opinion
  question, shown combined and split by class (POL 441 / POL 416).
- The same trend split by a second dimension: before vs. after Oct 7, 2023,
  with a baseline (Wave I) significance test per question.
- A word cloud + rudimentary bag-of-words sentiment analysis (Bing lexicon)
  of the open-ended "What does Zionism mean to you?" responses, sliced the
  same two ways.

## Data & privacy

This repo contains only **aggregated, anonymized** data: group-level counts,
percentages, and word frequencies. It does **not** contain:

- Student names or any name-to-response crosswalk
- Row-level (per-respondent) response data
- Verbatim free-text quotes

The raw survey exports (which include full student names) and the private
name→ID crosswalk used to build the anonymized panel live outside this repo
and are not published anywhere.

## Repo layout

- `index.html` — the dashboard (self-contained; data is embedded inline)
- `data/` — the aggregated JSON/CSV backing the dashboard, for reuse outside it
- `scripts/` — the R pipeline that builds the anonymized panel and the
  aggregates above, from the raw (private) survey CSVs

## Reproducing the pipeline

Run `scripts/01_parse.R` through `06_zionism_text.R` in order against your own
local copy of the raw survey export CSVs (point `survey_dir` in
`01_parse.R` at them — they're not included here). Requires R with
`dplyr`, `readxl`, `jsonlite`, and `tidytext`.

## Hosting on GitHub Pages

Settings → Pages → Deploy from branch → `main` / `(root)`. The dashboard
will then be live at `https://<username>.github.io/<repo>/`.
