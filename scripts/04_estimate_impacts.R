#
# Estimate impacts on top-line results (human-coded essay quality) and across the machine-generated measures of the text.
#

options(stringsAsFactors = FALSE)


load(here::here( "data-generated/meta.RData") )

# Characteristics of the sample
length(unique(meta$s_id))
length(unique(meta$sch_id))

sum(is.na(meta$s_maprit_1819w))

meta$grade = as.factor(meta$grade)
apply(meta,2,function(x)sum(is.na(x)))

# Standardize pretest
meta$maprit_std = scale(meta$s_maprit_1819w)


stats = meta %>% group_by( subject, grade, more ) %>%
  summarise( mn = mean( score ),
             sd = sd( score ),
             n = n() )
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
vcov_clust
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



if (FALSE){
  # Multilevel model for science scores
  mod.sci = lme4::lmer(score_std ~ 1 + grade + #mean.pretest+
                   maprit_std +
                   more + (1|sch_id) + (1|t_id),
                 data=sci)
  summary(mod.sci)
  mod.sci1 = lme4::lmer(score_std ~ 1 + grade + #mean.pretest+
                    maprit_std+
                    more + grade*more+ (1|sch_id) + (1|t_id),
                  data=sci)

  lmtest::lrtest(mod.sci, mod.sci1)

  # Multilevel model for social scores
  mod.soc = lme4::lmer(score_std ~ 1 + grade + #mean.pretest+
                   maprit_std +
                   more + (1|sch_id) + (1|t_id),
                 data=soc)

  mod.soc1 = lme4::lmer(score_std ~ 1 + grade + #mean.pretest+
                    maprit_std +
                    more + (grade*more)+ (1|sch_id) + (1|t_id),
                  data=soc )

  lmtest::lrtest(mod.soc, mod.soc1)
}
save(Mod.Sci, Mod.SS, est.sci, est.soc, file="results/tx_models.RData")




##### Estimate treatment impacts on generated text features #####

load("data-generated/all.info.RData")
tmp = select(meta, ID, s_id, grade, subject, more, maprit_std)
all.info = merge(tmp, all.info, by=c("ID", "s_id","grade", "subject", "more"))


dat = dplyr::select(all.info, sch_id,t_id,more, maprit_std,subject,
                    grade, spellcheck, lex_TTR:lex_ELF,
                    xxx:liwc_Sixltr, liwc_prep:liwc_AllPunc,
                    taaco_basic_connectives:taaco_addition, taaco_reason_and_purpose:taaco_all_demonstratives,
                    taaco_all_connective, taaco_pronoun_density)

x = dat %>% select(-sch_id, -t_id, -more, -maprit_std, -subject, -grade)
names(x)[caret::findLinearCombos(x)$remove]
caret::findCorrelation(cor(dat[,-c(1:6)]),names=T,exact=T,cutoff=0.9)

dat = select(dat, -lex_Flesch, -lex_ARI) # remove highly correlated features
caret::findCorrelation(cor(dat[,-c(1:6)]),names=T,exact=T,cutoff=0.9)
x = dat[,-c(1:6)]

# find features with near zero variance
rm=caret::nearZeroVar(x, uniqueCut = 2, freqCut=99/1, names=T)
sort(apply(dat[,names(dat)%in%rm], 2, function(x)length(unique(x))))

dat = dat %>% select( -liwc_filler, -liwc_sexual, -liwc_swear, -liwc_nonflu)


#' For each column of x, conduct an analysis of impact of MORE intervention on
#' feature represented by that column.
#'
#' Adjust all tests with FDR at end.
get_diffs = function(d, x){
  res = data.frame(var=names(x),
                   est=NA, SE=NA, stat=NA,
                   p.raw=NA, LL=NA, UL=NA )
  tmp = select(d, sch_id, t_id, more, grade, maprit_std)
  for (j in 1:ncol(x)){
    tmp$var=x[,j]
    tmp$more=as.factor(tmp$more)
    mod = lm(var ~ maprit_std + more, data=tmp)
    vc = sandwich::vcovCL(mod, tmp$sch_id)


    est= lmtest::coeftest( mod, vcov. = vc )
    CI = lmtest::coefci(mod, vcov.=vc)
    res[j,2:5]=est[grep("more",rownames(est)),]
    res[j,6:7] = CI[grep("more",rownames(CI)),]
  }
  return(res)
}


out=plyr::ddply(dat, ~subject+grade, function(d) get_diffs(d, x=d[,-c(1:6)]))
add.vars = c("liwc_Analytic","liwc_Authentic","liwc_Clout","liwc_Tone",
             "liwc_WC","liwc_WPS","liwc_Sixltr","xxx",
             "lex_TTR","lex_Flesch.Kincaid")


out$planned=1*(out$var%in%add.vars)
out.pl = out[out$planned==1,]
out.un = out[out$planned==0,]
out.pl$p.adj= p.adjust(out.pl$p.raw,"fdr")
out.un$p.adj= p.adjust(out.un$p.raw,"fdr")
out = rbind(out.pl, out.un)

table(out$p.adj<=0.05, out$p.raw<=0.05)

sig.vars = unique(out$var[which(out$p.adj<=0.05)])

all.vars = unique(c(sig.vars,add.vars))


diffs= all.info %>% group_by(subject, grade,more ) %>%
  dplyr::summarise_at(add.vars, mean)

diffs2 = all.info %>% group_by( subject, grade,more ) %>%
  dplyr::summarise_at(sort(all.vars[!all.vars%in%names(diffs)]), mean)

dd = diffs %>% pivot_longer(cols = -c(subject, grade,more ) ) %>%
  pivot_wider( names_from = more, values_from = value,
               names_prefix = "Grp_")
dd2 = diffs2 %>% pivot_longer(cols = -c(subject, grade,more ) ) %>%
  pivot_wider( names_from = more, values_from = value,
               names_prefix = "Grp_")
dd = bind_rows( dd, dd2 )
dd =  dd %>%
  mutate( delta =  Grp_1 - Grp_0 ) %>%
  dplyr::rename( MORE="Grp_1",
                 Control="Grp_0")

dd = dd %>% pivot_wider(names_from=c(subject),
                        values_from=c(Control,MORE,delta)) %>%
  select(grade, name, ends_with("science"), ends_with("social"))



dd.out = arrange(dd, grade,name)
dd.out.raw = dd.out

out2 = out[out$var%in%all.vars,]
names(out2)[grep("var",names(out2))]="name"

d2 = all.info %>% group_by( subject, grade,more ) %>%
  dplyr::summarise_at(all.vars, .funs = c( mn = mean, sd = sd ) )

dd = d2 %>% pivot_longer(cols = -c(subject,grade, more ),
                         names_to = c("name", ".value"),
                         names_pattern = "(.*)_(.*)" ) %>%
  pivot_wider( names_from =more, values_from=c(mn,sd) )

dd = mutate( dd, delta = (mn_1 - mn_0) / ((sd_1+sd_0)/2) )
dd = dd[dd$name%in%all.vars,]


dd = dd %>%
  dplyr::select(  subject, grade,name,mn_0, sd_0,  mn_1, sd_1, delta ) %>%
  dplyr::rename( MORE="mn_1",
                 Control="mn_0",
                 MORE_sd = sd_1,
                 Co_sd = sd_0 )

dd = merge(dd, out2, by=c("grade","subject","name"))
dd$LL.std = dd$LL/((dd$MORE_sd+dd$Co_sd)/2)
dd$UL.std = dd$UL/((dd$MORE_sd+dd$Co_sd)/2)


dd.out = select(dd, grade, subject, name, Control, MORE,
                est, LL, UL,
                delta, LL.std, UL.std,
                p.adj, p.raw)
dd.out$pretty.CI.raw = paste0("(", sprintf("%.2f",round(dd$LL,2)), ", ",
                              sprintf("%.2f",round(dd$UL,2)), ")")
dd.out$pretty.CI.std = paste0("(", sprintf("%.2f",round(dd$LL.std,2)), ", ",
                              sprintf("%.2f",round(dd$UL.std,2)), ")")

dd = select(dd, grade, subject, name, delta, LL.std, UL.std, p.raw, p.adj)


save(dd.out, dd, file="results/LIWC_diffs_results.RData")

