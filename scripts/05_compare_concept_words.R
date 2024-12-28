## Estimate impacts on the prevalence and frequency of use of a list of pre-identified
# "concept" words and phrases within each grade and subject

options(stringsAsFactors = FALSE)



load( here::here( "data-generated/meta.RData" ) )


# Data frame containing the "concept words" identified in the MORE
# study
cwords = tibble::tribble(
  ~grade,  ~subject,      ~concept_word,    ~taught,
  1, "science",          "survive",   "taught",
  1, "science",          "species",   "taught",
  1, "science",         "behavior",   "taught",
  1, "science",        "advantage",   "taught",
  1, "science",       "adaptation",   "taught",
  1, "science",          "habitat",   "taught",
  1, "science", "physical_feature",   "taught",
  1, "science",        "potential", "untaught",
  1, "science",           "unique", "untaught",
  1, "science",       "camouflage", "untaught",
  1, "science",        "diversity", "untaught",
  1,  "social",       "expedition",   "taught",
  1,  "social",        "discovery",   "taught",
  1,  "social",         "obstacle",   "taught",
  1,  "social",       "indigenous",   "taught",
  1,  "social",         "explorer",   "taught",
  1,  "social",        "community",   "taught",
  1,  "social",       "persistent",   "taught",
  1,  "social",         "ancestor", "untaught",
  1,  "social",       "navigation", "untaught",
  1,  "social",           "settle", "untaught",
  1,  "social",        "celebrate", "untaught",
  1,  "social",            "route", "untaught",
  2, "science",          "extinct",   "taught",
  2, "science",           "fossil",   "taught",
  2, "science",           "brutal",   "taught",
  2, "science",         "evidence",   "taught",
  2, "science",           "theory",   "taught",
  2, "science",           "hunter",   "taught",
  2, "science",   "paleontologist",   "taught",
  2, "science",        "carnivore", "untaught",
  2, "science",       "hypothesis", "untaught",
  2, "science",         "organism", "untaught",
  2, "science",            "trait", "untaught",
  2, "science",          "reptile", "untaught",
  2,  "social",         "inventor",   "taught",
  2,  "social",       "experiment",   "taught",
  2,  "social",        "prototype",   "taught",
  2,  "social",   "discrimination",   "taught",
  2,  "social",       "laboratory",   "taught",
  2,  "social",         "engineer",   "taught",
  2,  "social",        "ingenious",   "taught",
  2,  "social",             "hire", "untaught",
  2,  "social",          "approve", "untaught",
  2,  "social",        "establish", "untaught",
  2,  "social",      "manufacture", "untaught",
  2,  "social",       "foundation", "untaught"
)



# Analyze separately for taught and untaught terms
tmp = cwords %>%
  group_by(grade,subject,taught) %>% 
  summarise( terms = list( concept_word ), .groups="drop" ) %>%
  pivot_wider( values_from = terms, names_from = taught )
tmp

text$grade = as.numeric(text$grade)            
text1 = left_join(text, tmp, by=c("grade", "subject"))
text1$taught[[1]]

if ( FALSE ) {
  # This function returns a line of statistics for term frequencies
  textfx_terms( text$text, text$more, cwords$concept_word )
}

res.taught = text1 %>% 
  group_by(grade, subject) %>% 
  do(  rcttext::textfx_terms(.$text.sc, .$more, unique(unlist(.$taught)))) %>%
  mutate(type="taught")

res.untaught = text1 %>% 
  group_by(grade, subject) %>%
  do(rcttext::textfx_terms(.$text.sc, .$more, unique(unlist(.$untaught)))) %>% 
  mutate(type="untaught")


# combine results across taught and untaught terms and adjust for multiple comparisons
res = rbind(res.taught, res.untaught)
res$p.adj = p.adjust(res$p.value, "fdr")

res = res %>% 
  select(grade, subject, type, everything())

res

save(res, file="results/concept-tabs.RData")










