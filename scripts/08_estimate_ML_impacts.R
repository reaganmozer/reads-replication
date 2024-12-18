# Estimate treatment effects on fully machine-generated proxy
# outcomes, including:
#
# 1) The "descriptive similarity" between each essay and it's
# gold-standard reference text, and
#
# 2) The predicted quality score from a ML model trained on pilot data
# and compare these estimates to those based on full human coding


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = FALSE)



source( here::here( "scripts/utils.R") )

load( here::here( "data-generated/all.info.RData") )

load( here::here( "data-generated/all.ML.scores.RData") )

simil.scores = select(all.info, s_id, t_id, sch_id, grade, subject, tdm.raw.cosine)

all.ML.scores = merge(all.ML.scores, simil.scores,
                      by=c("s_id","t_id","sch_id","grade","subject"))

load( here::here( "data-generated/meta.RData") )

rownames(meta)=rownames(all.ML.scores)=NULL
meta = merge(meta, all.ML.scores,
             by=c("s_id","t_id","sch_id","grade","subject"))
names(meta)[grep("score",names(meta))]="Yobs"


rm(all.ML.scores, text, simil.scores, all.info)




# Impact estimates for human-coded outcomes
out.Yobs = get_impact_est(meta, yhat=meta$Yobs, interact =T)

out.simil = get_impact_est(meta, yhat=1-meta$tdm.raw.cosine, interact =T)

out.ML = get_impact_est(meta, meta$stack, interact = T)

meta$cos = 1-meta$tdm.raw.cosine
meta %>% group_by( grade, subject ) %>%
    summarise( to_sim = cor( Yobs, cos ),
               to_ML = cor( Yobs, stack ),
               sim_ML = cor( cos, stack) )




save(out.Yobs, out.simil, out.ML, file="results/overall_impact_ests.RData")


