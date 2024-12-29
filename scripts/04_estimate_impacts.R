#
# Estimate impacts on top-line results (human-coded essay quality) and
# across the machine-generated measures of the text.
# 

options(stringsAsFactors = FALSE)

source( "scripts/00_setup.R" )

load(here::here( "data-generated/meta.RData") )


# Standardize pretest
meta$maprit_std = as.numeric( scale(meta$s_maprit_1819w) )

meta = filter( meta, subject == "science" )

# Explore the human scored outcomes ----

stats = meta %>% group_by( more ) %>%
  summarise( mn = mean( score ),
             sd = sd( score ),
             n = n(), .groups="drop" )
stats

meta$score_std = meta$score / stats$sd[[1]]

##### Estimate main impacts #####

library( estimatr )
Mod = lm_robust( score_std ~ as.factor(sch_id) + grade + maprit_std + more, data=meta,
                 cluster =  meta$t_id )
Mod_raw = lm_robust( score_std ~ as.factor(sch_id) + more, data=meta,
                     cluster =  meta$t_id )

texreg::screenreg(list(Mod, Mod_raw),
                  custom.model.names=c("Adjusted", "Unadjusted"),
                  custom.coef.map = list("(Intercept)"=NA,
                                         "grade2"="Grade 2", "maprit_std"="Pre-test score", "more"="MORE"))



# Clean up workspace
gdata::keep(text, meta, sure=TRUE)



##### Estimate treatment impacts on generated text features #####

load("data-generated/all.info.RData")
tmp = select(meta, ID, s_id, sch_id, grade, subject, more, maprit_std)
all.info = left_join(tmp, all.info, by=c("ID", "s_id", "sch_id", "subject", "grade", "more"))

# Cut down to fewer features
ncol(all.info)
dat = dplyr::select( all.info, sch_id,t_id, more, maprit_std,subject,
                     grade, spellcheck, lex_TTR:lex_ELF,
                     xxx:liwc_Sixltr, liwc_prep:liwc_AllPunc,
                     taaco_basic_connectives:taaco_addition, 
                     taaco_reason_and_purpose:taaco_all_demonstratives,
                     taaco_all_connective, taaco_pronoun_density)
ncol(dat)
names(dat)[1:20]

dat = clean_features( dat,
                      c( "sch_id", "t_id", "more", "maprit_std", "subject", "grade" ),
                      cor = 0.90 )

ncol( dat )



my_analysis <- function( feature, data ) {
  
  stopifnot( !is.null(feature) )
  
  data$.feature = feature
  mod = lm( .feature ~ maprit_std + grade + more, data=data)
  vc = sandwich::vcovCL(mod, data$sch_id)
  
  est = lmtest::coeftest( mod, vcov. = vc )
  CI = lmtest::coefci(mod, vcov.=vc)
  
  res <- c( est[grep("more",rownames(est)),],
            CI = CI[grep("more",rownames(CI)),] )
  names(res) = c( "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high" )
  res <- as_tibble_row( res )
  
  res$Grp_0 = mean( data$.feature[data$more==0] )
  res$Grp_1 = mean( data$.feature[data$more==1] )
  res$Grp_0_sd = sd( data$.feature[data$more==0] )
  res$Grp_1_sd = sd( data$.feature[data$more==1] )
  
  res
}

a <- my_analysis( dat$spellcheck, data=tmp )
a

# The default in the package is basically the same, using lm_robust
b <- simple_RCT_analysis( dat$spellcheck,
                          formula = ~ more + grade + maprit_std, data=tmp, cluster = tmp$sch_id )
b

# Minor differences in implementation of cluster robust SEs:
bind_rows(a,b)

# What variables do we specifically test, vs. the many others we do
# not care as much about?
planned.vars = c("liwc_Analytic","liwc_Authentic","liwc_Clout","liwc_Tone",
                 "liwc_WC","liwc_WPS","liwc_Sixltr","xxx",
                 "lex_TTR","lex_Flesch.Kincaid")



  out <- impacts_on_features( dat,
                              ignore = c("sch_id", "t_id", "more", 
                                         "maprit_std", "subject", "grade"),
                              analysis_function = my_analysis, 
                              planned_features = planned.vars,
                              mcp = "fdr" )
  

out
table( adjusted = out$p.adj<=0.05, raw = out$p.value<=0.05)


out


# Impacts on "concept words"

## Estimate impacts on the prevalence and frequency of use of a list of pre-identified
# "concept" words and phrases within each grade and subject

options(stringsAsFactors = FALSE)



load( here::here( "data-generated/meta.RData" ) )

cwords_untaught <- c("potential", "unique", "camouflage", "diversity", "carnivore", "hypothesis", "organism", "trait", "reptile")
cwords_taught <- c("survive", "species", "behavior", "advantage", "adaptation", "habitat", "physical_feature", "extinct", "fossil", "brutal", "evidence", "theory", "hunter", "paleontologist")


# This function returns a line of statistics for term frequencies
r1 <- textfx_terms( text$text, text$more, cwords_untaught )
r2 <- textfx_terms( text$text, text$more, cwords_taught )

bind_rows( r1, r2 ) %>%
  knitr::kable( digits=2 )









