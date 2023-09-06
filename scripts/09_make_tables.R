setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = FALSE)


#### Table 1: Differences between use of taught and untaught concept words  ####
load("results/concept-tabs.RData")
sums.out = res %>% arrange(grade, subject) %>%
  mutate(diff=prop.docs_1-prop.docs_0)

stars = rep("",nrow(sums.out))

stars[sums.out$p.adj<=0.05]="*"
stars[sums.out$p.adj<=0.01]="**"
stars[sums.out$p.adj<=0.001]="***"
sout1 = sums.out %>% mutate(freq.diff=termfreq_1-termfreq_0,
                            prop.docs_0=paste0(docfreq_0, " (",round(100*prop.docs_0,digits=1),"%)"),
                            prop.docs_1=paste0(docfreq_1," (",round(100*prop.docs_1,digits=1),"%)"),
                            diff=paste0(docfreq_1-docfreq_0," (",round(100*diff,1),"%)"))

sout1 = select(sout1, grade, subject, type,termfreq_1,termfreq_0, freq.diff,
               prop.docs_1, prop.docs_0, diff)
sout1$subject=factor(sout1$subject, labels=c("Science","Social"))
sout1$type=factor(sout1$type, labels=c("Taught","Untaught"))
sout1[,4:6]=apply(sout1[,4:6],2,as.integer)
sout1$p.adj=stars
names(sout1)=c("","","",
               "MORE","Control","Diff.",
               "MORE","Control","Diff.",
               "")

tab.cwords = xtable::xtable(as.data.frame(sout1), auto=F,
                    caption="Use of taught and untaught concept words in treatment and control,
                    grouped by grade and subject. Columns show cumulative frequency (total number of occurrences) and prevalence
                    (number of unique occurrences) across essays in each group.", label="tab:cwords",
                    align="llllcccrrrl")


print(tab.cwords,type="latex",align="lllcccrrrl",caption.placement="top",include.rownames=F,
      add.to.row=list(pos=list(-1),command="\\hline\n \\multirow{2}{*}{Grade} & \\multirow{2}{*}{Subject} & \\multirow{2}{*}{Word type} & \\multicolumn{3}{c}{Frequency}
                      & \\multicolumn{3}{c}{Prevalence}\\\\ \\cline{4-6}\\cline{7-9}\n"),
      hline.after=c(0,nrow(sout1)),
      file="tables/cwords.tex")






### Table 2: Human versus machine predicted quality scores
load("results/overall_impact_ests.RData")

all.out = list(out.Yobs[[1]], out.Yobs[[2]],
                out.simil[[1]], out.simil[[2]],
                out.ML[[1]], out.ML[[2]])

texreg::screenreg(all.out,
               caption.above=T,
               custom.header=list("Human-coded"=1:2, "Machine similarity"=3:4, "ML predicted"=5:6),
               custom.model.names=rep(c("Science", "Social studies"),3),
               custom.coef.map = list("grade1"="Grade 1 Baseline",
                                      "grade2"="Grade 2 Baseline",
                                      "maprit_imp_std"="Pretest Score",
                                      "grade1:more"="MORE (Grade 1)",
                                      "grade2:more" = "MORE (Grade 2)"))

texreg::texreg(all.out, file="tables/compare_quality.tex",
               label="tab:MLquality",
               caption="Estimated effects (in effect size units) of grade level, pretest scores (MAP/RIT),
               and receipt of MORE intervention compared to typical instruction on  average human-coded quality scores (left),
               average descriptive similarity scores (center) and average machine learning (ML) predicted quality scores (right)
               for each grade level and subject.",
               caption.above=T,
               custom.header=list("Human-coded"=1:2, "Machine similarity"=3:4, "ML predicted"=5:6),
               custom.model.names=rep(c("Science", "Social studies"),3),
               custom.coef.map = list("grade1"="Grade 1 Baseline",
                                      "grade2"="Grade 2 Baseline",
                                      "maprit_imp_std"="Pretest Score",
                                      "grade1:more"="MORE (Grade 1)",
                                      "grade2:more" = "MORE (Grade 2)"))








# Supplement Table 1: Treatment effect estimates from MLM ####
load("results/tx_models.RData")
texreg::texreg(list(est.sci, est.soc), file="tables/impact_est.tex",
               label="tab:impact_est",
               caption="Estimated effects (in effect size units) of grade level, pretest scores (MAP/RIT),
               and receipt of MORE intervention compared to typical instruction on average (human-coded)
               writing quality scores in science and social studies.",
               caption.above=T,
               custom.model.names=c("Science", "Social Studies"),
               custom.coef.map = list("(Intercept)"=NA, "grade2"="Grade 2",
                                      "maprit_imp"="Pretest Score", "more"="MORE"))




