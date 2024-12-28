#
# Estimate impacts on top-line results (human-coded essay quality) and
# across the machine-generated measures of the text.
# 

options(stringsAsFactors = FALSE)

source( "scripts/00_setup.R" )

load(here::here( "data-generated/meta.RData") )

# Characteristics of the sample
length(unique(meta$s_id))
length(unique(meta$sch_id))

sum(is.na(meta$s_maprit_1819w))

meta$grade = as.factor(meta$grade)
apply(meta,2,function(x)sum(is.na(x)))

# Standardize pretest
meta$maprit_std = as.numeric( scale(meta$s_maprit_1819w) )


# Explore the human scored outcomes ----

stats = meta %>% group_by( subject, grade, more ) %>%
  summarise( mn = mean( score ),
             sd = sd( score ),
             n = n(), .groups="drop" )
stats

avg_stats = stats %>% group_by( subject ) %>%
  summarise( sd_bar = sqrt( weighted.mean( sd^2, w=n ) ) )
avg_stats

meta %>% group_by( subject ) %>%
  summarise( mn = mean( score ),
             sd = sd( score ) )


sci = meta[meta$subject=="science",]
soc = meta[meta$subject!="science",]
sci$score_std = sci$score/avg_stats$sd_bar[[1]]
soc$score_std = soc$score/avg_stats$sd_bar[[2]]


##### Estimate main impacts #####

Mod.Sci = lm( score_std ~ as.factor(sch_id) + grade +  maprit_std + more, data=sci)
vcov_clust = sandwich::vcovCL( Mod.Sci, sci$t_id )
est.sci=lmtest::coeftest( Mod.Sci, vcov. = vcov_clust )

Mod.SS = lm( score_std ~ as.factor(sch_id) + grade +maprit_std + more, data=soc)
vcov_clust2 = sandwich::vcovCL( Mod.SS, soc$t_id )
est.soc=lmtest::coeftest( Mod.SS, vcov. = vcov_clust2 )


texreg::screenreg(list(est.sci, est.soc),
                  custom.model.names=c("Science", "Social Studies"),
                  custom.coef.map = list("(Intercept)"=NA,
                                         "grade2"="Grade 2", "maprit_std"="Pre-test score", "more"="MORE"))



# Sensitivity check: Clustering at school level (most conservative SE approach)
vcov_clust_sch = sandwich::vcovCL( Mod.Sci, sci$sch_id )
est.sci_sch=lmtest::coeftest( Mod.Sci, vcov. = vcov_clust_sch )

vcov_clust2_sch = sandwich::vcovCL( Mod.SS, soc$sch_id )
est.soc_sch=lmtest::coeftest( Mod.SS, vcov. = vcov_clust2_sch )

# Relative SEs: about the same
est.sci_sch[,2] / est.sci[,2]
est.soc_sch[,2] / est.soc[,2]


# Results basically identical.
texreg::screenreg(list(est.sci, est.sci_sch, est.soc, est.soc_sch),
                  custom.model.names=c("Science", "Sci (sch)", "Social Studies", "SS (sch)"),
                  custom.coef.map = list("(Intercept)"=NA,
                                         "grade2"="Grade 2", "maprit_std"="Pre-test score", "more"="MORE"))


save(Mod.Sci, Mod.SS, est.sci, est.soc, file="results/tx_models.RData")

# Clean up workspace
gdata::keep(text, meta, sure=TRUE)



##### Estimate treatment impacts on generated text features #####

load("data-generated/all.info.RData")
tmp = select(meta, ID, s_id, sch_id, grade, subject, more, maprit_std)
all.info = merge(tmp, all.info, by=c("ID", "s_id", "sch_id", "grade", "subject", "more"))

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
  mod = lm( .feature ~ maprit_std + more, data=data)
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
                          formula = ~ more + maprit_std, data=tmp, cluster = tmp$sch_id )
b

# Minor differences in implementation of cluster robust SEs:
bind_rows(a,b)

# What variables do we specifically test, vs. the many others we do
# not care as much about?
planned.vars = c("liwc_Analytic","liwc_Authentic","liwc_Clout","liwc_Tone",
                 "liwc_WC","liwc_WPS","liwc_Sixltr","xxx",
                 "lex_TTR","lex_Flesch.Kincaid")

if ( FALSE ) {
  # Illustration of code on full dataset, but we will do by subgroup.
  all <- impacts_on_features( dat,
                              ignore = c("sch_id", "t_id", "more", 
                                         "maprit_std", "subject", "grade"),
                              analysis_function = my_analysis, 
                              planned_features = planned.vars,
                              mcp = "fdr" )
  # one row per feature of analysis
  dim(all)
}

out <- dat %>%
  group_by( subject, grade ) %>%
  group_modify( ~impacts_on_features( .x,
                                      ignore = c("sch_id", "t_id", "more", 
                                                 "maprit_std", "subject", "grade"),
                                      analysis_function = my_analysis, 
                                      planned_features = planned.vars,
                                      standardize = TRUE,
                                      mcp = "fdr" ) )

table( adjusted = out$p.adj<=0.05, raw = out$p.value<=0.05)




# Clean up and arrange our four groups nicely ----

# Select only those features tagged by at least one of the four groups
# as significant (plus the planned comparisons)
sig.vars = unique(out$feature[which(out$p.adj<=0.05)])
all.vars = unique(c(sig.vars,planned.vars))


out2 = out %>%
  filter( feature %in% all.vars ) 


# Pretty results for printing; also renaming columns for later scripts
dd = out2 %>%
  mutate( name = feature,
          Control = Grp_0 / scale,
          MORE = Grp_1 / scale,
          est = estimate_std,
          LL = conf.low,
          UL = conf.high,
          LL.std = conf.low_std,
          UL.std = conf.high_std,
          delta = MORE - Control,
          p.raw = p.value ) 

dd.out <- dd %>%
  select( grade, subject, name, Control, MORE,
          est, LL, UL,
          delta, LL.std, UL.std,
          p.adj, p.raw)

dd.out$pretty.CI.raw = paste0("(", sprintf("%.2f",round(dd.out$LL,2)), ", ",
                              sprintf("%.2f",round(dd.out$UL,2)), ")")
dd.out$pretty.CI.std = paste0("(", sprintf("%.2f",round(dd.out$LL.std,2)), ", ",
                              sprintf("%.2f",round(dd.out$UL.std,2)), ")")

dd = dd %>%
  select(grade, subject, name, delta, LL.std, UL.std, p.raw, p.adj)

dd.out

save(dd.out, dd, file="results/LIWC_diffs_results.RData")

