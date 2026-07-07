## Question dictionary: maps raw question text -> canonical qid/label/category/level order
## category: DEMO (wave I only, always) | CORE (asked every wave, every term)
##           REPEAT (asked wave II & III always, wave I only in Fall 21) | BASELINE (wave I only, added from Fall22+)

mk <- function(qid, label, category, qtype, ordinal, levels, raw_texts) {
  levels_str <- if (length(levels) == 1 && is.na(levels)) NA_character_ else paste(levels, collapse = "|")
  data.frame(
    raw_text = raw_texts, qid = qid, label = label, category = category,
    qtype = qtype, ordinal = ordinal,
    levels = levels_str,
    stringsAsFactors = FALSE
  )
}

dict_list <- list(
  mk("D_ETHNICITY", "Ethnic identity (open text)", "DEMO", "FIB", FALSE, NA,
     "How do you identify ethnically? _______"),
  mk("D_NATIONALITY", "National identity (open text)", "DEMO", "FIB", FALSE, NA,
     "How do you identify nationally? _______"),
  mk("D_RELIGION", "Religious identity (open text)", "DEMO", "FIB", FALSE, NA,
     "How do you identify religiously? _______"),
  mk("D_RELIGIOUS_MEMBER", "Active member of religious community (open text)", "DEMO", "FIB", FALSE, NA,
     "Are you currently an active member of a religious community? If so, which one. (If not, respond no). _______"),
  mk("D_GENDER", "Gender", "DEMO", "MC", FALSE, NA,
     "What gender do you identify with?"),
  mk("D_PERSONAL_OPINION_SOURCES", "Sources for personal opinions (multi-select)", "DEMO", "M-S", FALSE, NA,
     "When forming personal opinions, how often do you draw on the following sources for guidance (mark all that you consider correct)"),
  mk("D_POLITICAL_OPINION_SOURCES", "Sources for political opinions (multi-select)", "DEMO", "M-S", FALSE, NA,
     "When forming political opinions, how often do you draw on the following sources for guidance (mark all that you consider correct)"),
  mk("D_INCLUDE_ETHNIC", "Feel included in ethnic community", "DEMO", "MC", TRUE,
     c("Very included","Somewhat included","Neither included nor excluded","Somewhat excluded","Very excluded"),
     "How included do you feel in you ethnic community?"),
  mk("D_INCLUDE_NATIONAL", "Feel included in national community", "DEMO", "MC", TRUE,
     c("Very included","Somewhat included","Neither included nor excluded","Somewhat excluded","Very excluded"),
     "How included do you feel in your national community?"),
  mk("D_INCLUDE_RELIGIOUS", "Feel included in religious community", "DEMO", "MC", TRUE,
     c("Very included","Somewhat included","Neither included nor excluded","Somewhat excluded","Very excluded"),
     "How included do you feel in your religious community?"),
  mk("D_VOTED", "Have voted", "DEMO", "MC", FALSE, NA,
     "Have you voted?"),
  mk("D_IDEOLOGY", "Political ideology", "DEMO", "MC", FALSE, NA,
     "Do you consider yourself"),

  mk("O_INFORMED", "Feel informed about ISPA conflict", "CORE", "MC", TRUE,
     c("Very informed","Informed","Somewhat informed","Somewhat uninformed","Uninformed","Very uninformed"),
     "On the issue of the Israeli-Palestinian conflict, do you feel"),
  mk("O_INFO_SOURCES", "Main info sources on ISPA conflict (multi-select)", "CORE", "MC", FALSE, NA,
     "Your main sources of information on the ISPA conflict prior to the class are?"),
  mk("O_SUPPORT_ISRAEL", "US support for Israel should be", "CORE", "MC", TRUE,
     c("Increased","Stay the same","Decreased"),
     "Do you believe the US support for Israel should be:"),
  mk("O_SUPPORT_PALESTINIANS", "US support for Palestinians should be", "CORE", "MC", TRUE,
     c("Increased","Stay the same","Decreased"),
     c("Do you believe the US support for Palestinian should be",
       "Do you believe the US support for Palestinians should be")),
  mk("O_PROISRAEL_CRITIQUE", "Can be critical of Israeli govt and still pro-Israel", "CORE", "MC", FALSE, NA,
     "People often talk about being \"pro-Israel\". Do you think someone can be critical of Israeli government policies and still be \"pro-Israel\"?"),
  mk("O_RESOLUTION", "Best resolution of ISPA conflict", "CORE", "MC", FALSE, NA,
     "In your opinion, which of the following would be the best resolution of the Israeli -Palestinian conflict?"),
  mk("O_CONCERN_ANTISEMITISM", "Concern about antisemitism in US", "CORE", "MC", TRUE,
     c("Very concerned","Somewhat concerned","Neutral","Not too concerned","Not at all concerned"),
     "How concerned are you about antisemitism in the US?"),
  mk("O_CONCERN_ISLAMOPHOBIA", "Concern about Islamophobia in US", "CORE", "MC", TRUE,
     c("Very concerned","Somewhat concerned","Neutral","Not too concerned","Not at all concerned"),
     "How concerned are you about Islamophobia in the US?"),
  mk("O_RIGHT_EXIST", "Agree: Israel doesn't have the right to exist", "CORE", "MC", TRUE,
     c("Strongly agree","Somewhat agree","Neutral","Somewhat disagree","Strongly disagree"),
     "Do you agree or disagree with the following statement: Israel doesn't have the right to exist."),
  mk("O_APARTHEID", "Agree: Israel is an apartheid state", "CORE", "MC", TRUE,
     c("Strongly agree","Somewhat agree","Neutral","Somewhat disagree","Strongly disagree"),
     "Do you agree or disagree with the following statement: Israel is an apartheid state."),
  mk("O_RACISM_SIMILAR", "Agree: Israel's treatment of Palestinians similar to US racism", "CORE", "MC", TRUE,
     c("Strongly agree","Somewhat agree","Neutral","Somewhat disagree","Strongly disagree"),
     "Do you agree or disagree with the following statement: Israel's treatment of Palestinian is similar to racism in the US"),
  mk("O_GENOCIDE", "Agree: Israel is committing genocide against Palestinians", "CORE", "MC", TRUE,
     c("Strongly agree","Somewhat agree","Neutral","Somewhat disagree","Strongly disagree"),
     "Do you agree or disagree with the following statement: Israel is committing genocide against the Palestinians"),
  mk("O_ZIONISM_MEANING", "What Zionism means to you (open text)", "CORE", "SA", FALSE, NA,
     "What does Zionism mean to you (5-10 word response)"),

  mk("P_ZION_ATTACHMENT", "Zionist per 'attachment to Israel' definition", "REPEAT", "MC", TRUE,
     c("Definitely","Probably","Neutral","Probably not","Definitely not"),
     "According to the following definition of Zionism, are you a Zionist? Zionism means a feeling of attachment to Israel"),
  mk("P_ZION_JEWISH_DEM_STATE", "Zionist per 'Jewish and democratic state' definition", "REPEAT", "MC", TRUE,
     c("Definitely","Probably","Neutral","Probably not","Definitely not"),
     "According to the following definition of Zionism, are you a Zionist? Zionism means a belief in a Jewish and democratic state"),
  mk("P_ZION_PRIVILEGE", "Zionist per 'privileging Jewish rights' definition", "REPEAT", "MC", TRUE,
     c("Definitely","Probably","Neutral","Probably not","Definitely not"),
     "According to the following definition of Zionism, are you a Zionist? Zionism means the belief in privileging Jewish rights over non-Jewish rights in Israel"),
  mk("P_AID_RESTRICT", "Support conditioning Israel aid on settlements", "REPEAT", "MC", TRUE,
     c("Strongly support","Somewhat support","Neither support nor oppose","Somewhat oppose","Strongly oppose"),
     "Do you support or oppose the U.S. providing the same amount of financial aid that it gives Israel, but restricting it so that Israel cannot spend U.S. aid on expanding settlements in the West Bank?"),

  mk("B_BIDEN_COMPARE", "Biden admin position vs own position", "BASELINE", "MC", FALSE, NA,
     "Compared with your own position on the Israeli-Palestinian conflict, how would you describe the Biden Administration's position?"),
  mk("B_FAV_ISRAELI_GOVT", "Favorable opinion of Israeli government", "BASELINE", "MC", FALSE, NA,
     "Do you have a favorable opinion towards the Israeli government"),
  mk("B_FAV_ISRAELI_PEOPLE", "Favorable opinion of Israeli people", "BASELINE", "MC", FALSE, NA,
     "Do you have a favorable opinion towards the Israeli people?"),
  mk("B_FAV_PALESTINIAN_GOVT", "Favorable opinion of Palestinian government", "BASELINE", "MC", FALSE, NA,
     "Do you have a favorable opinion towards the Palestinian government"),
  mk("B_FAV_PALESTINIAN_PEOPLE", "Favorable opinion of Palestinian people", "BASELINE", "MC", FALSE, NA,
     "Do you have a favorable opinion towards the Palestinian people?"),
  mk("B_BDS_SUPPORT", "Support the BDS movement", "BASELINE", "MC", TRUE,
     c("Strongly support","Somewhat support","Don't know","Somewhat oppose","Strongly oppose"),
     "Do you support the BDS movement"),
  mk("B_BDS_HEARD", "Heard of BDS movement", "BASELINE", "MC", TRUE,
     c("A lot","Some","Not much","Nothing at all"),
     "Have you heard about the Boycott, Divest, Sanction (BDS) movement against Israel?")
)

qdict <- do.call(rbind, dict_list)
rownames(qdict) <- NULL

# sanity check: every raw_text in qdict should exist in the parsed data, and vice versa
long_all <- readRDS("long_all.rds")
parsed_texts <- sort(unique(long_all$qtext))
dict_texts   <- sort(unique(qdict$raw_text))

missing_in_dict <- setdiff(parsed_texts, dict_texts)
missing_in_data <- setdiff(dict_texts, parsed_texts)

cat("Q texts in parsed data but NOT in dictionary (", length(missing_in_dict), "):\n")
print(missing_in_dict)
cat("\nQ texts in dictionary but NOT found in parsed data (", length(missing_in_data), "):\n")
print(missing_in_data)

saveRDS(qdict, "qdict.rds")
write.csv(qdict, "question_dictionary.csv", row.names = FALSE)
cat("\nSaved question_dictionary.csv with", nrow(qdict), "raw-text mappings covering",
    length(unique(qdict$qid)), "canonical questions.\n")
