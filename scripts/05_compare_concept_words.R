## Estimate impacts on the prevalence and frequency of use of a list of pre-identified
# "concept" words and phrases within each grade and subject

options(stringsAsFactors = FALSE)



load( here::here( "data-generated/meta.RData" ) )


# Data frame containing the concept words identified in the MORE study
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


textfx_terms = function(x, Z, terms, ...){
  
  # Determine maximum number of ngrams to search based on the input terms
  ngrams.terms = stringi::stri_count_words(terms)
  max.ngrams = 1:max(ngrams.terms)
  
  # Convert ngrams to quanteda structure with underscores
  terms = gsub(" ", "_", terms, fixed=T)
  
  dfm = quanteda::dfm(quanteda::tokens_ngrams(quanteda::tokens(x,...),n=max.ngrams))
  dfm.terms = as.matrix(quanteda::dfm_match(dfm, terms))
  
  out = data.frame(Z=Z, dfm.terms, tot=rowSums(dfm.terms))
  out = out %>%
    group_by(Z) %>% 
    summarise(n=n(), termfreq=sum(tot), docfreq=sum(tot>0), prop.docs=docfreq/n) %>% 
    arrange(desc(Z))
  
  # Hypothesis test for difference in proportions between groups
  test = prop.test(x=out$docfreq, n=out$n)
  
  
  out1 = out %>% pivot_wider(names_from=Z, values_from=!Z) %>%
    mutate(diff = test$estimate[1]-test$estimate[2],
           p.value = test$p.value,
           LL = test$conf.int[1],
           UL = test$conf.int[2])
  
  return(out1)
  
}




# Analyze separately for taught and untaught terms
tmp = cwords %>%
  group_by(grade,subject) %>% 
  do(terms.taught=paste0(.$concept_word[.$taught=="taught"]),
     terms.untaught = paste0(.$concept_word[.$taught=="untaught"]))

text1 = merge(text, tmp, by=c("grade", "subject"))


res.taught = text1 %>% 
  group_by(grade, subject) %>% 
  do(rcttext::textfx_terms(.$text.sc, .$more, unique(unlist(.$terms.taught)))) %>%
  mutate(type="taught")

res.untaught = text1 %>% 
  group_by(grade, subject) %>%
  do(rcttext::textfx_terms(.$text.sc, .$more, unique(unlist(.$terms.untaught)))) %>% 
  mutate(type="untaught")


# combine results across taught and untaught terms and adjust for multiple comparisons
res = rbind(res.taught, res.untaught)
res$p.adj = p.adjust(res$p.value, "fdr")

res = res %>% 
  select(grade, subject, type, everything())

res

save(res, file="results/concept-tabs.RData")










