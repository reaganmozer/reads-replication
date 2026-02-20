
source( here::here( "scripts/00_setup.R" ) )



## Theme for all ggplot objects
my_theme = theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title= element_text(hjust=0.5,size=13),
        axis.title = element_text(size=13),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=12),
        axis.line = element_line(colour = "black"),
        strip.text.x = element_text(size = 12),
        strip.text.y=element_text(size=11))

theme_set( my_theme )


# Load results for all figures
load("data-generated/all.info.RData")
all.info$grade=as.factor(all.info$grade)
levels(all.info$grade)=c("Grade 1", "Grade 2")
all.info$subject=as.factor(all.info$subject)
levels(all.info$subject)=c("Science", "Social Studies")

load("results/LIWC_diffs_results.RData")


dd$feature = stringi::stri_replace_all_regex(dd$feature,
                                          pattern=c('lex_','liwc_','taaco_'),
                                          replacement=c('','',''),
                                          vectorize=FALSE)


dd$type = ifelse( dd$feature %in% c("WC","WPS","TTR","R", "Flesch", "xxx",
                                         "Sixltr",'Flesch.Kincaid','Flesch',
                                         "Analytic","Authentic","Clout","Tone","ARI"), 
                       "Planned", "Unplanned" )


dd.sci = dd %>% 
  filter(subject=="science") %>%
  select(-subject)
dd.soc = dd %>% 
  filter(subject!="science") %>%
  select(-subject)


#### MAP THE FUNCTION BELOW TO PLOT_TEXTFX IN TADA!

## Figure 2: Planned comparisons of LIWC features
pdf(file="figures/Fig2.pdf", width=9, height=8)
par(mfrow=c(2,2),
    mar=c(4.1,8.9,2.5,1.1),
    mgp=c(2.5,0.5,0))
rcttext::plot_textfx(dd.sci[dd.sci$grade==1 & dd.sci$type=="Planned",], 
                     main="Grade 1 Science",
                     xlim=c(-0.8,0.8))
rcttext::plot_textfx(dd.soc[dd.soc$grade==1 & dd.sci$type=="Planned",],
                     main="Grade 1 Social Studies",
                     xlim=c(-0.8,0.8))
rcttext::plot_textfx(dd.sci[dd.sci$grade==2 & dd.sci$type=="Planned",],
                     main="Grade 2 Science",
                     xlim=c(-0.8,0.8))
rcttext::plot_textfx(dd.soc[dd.soc$grade==2 & dd.sci$type=="Planned",],
                     main="Grade 2 Social Studies",
                     xlim=c(-0.8,0.8))
dev.off()
par(mfrow=c(1,1))

## Figure 3: Unplanned comparisons of LIWC features
par(mfrow=c(2,2),mar=c(4.1,6.9,2.5,1.1),mgp=c(2.5,0.5,0))
pdf(file="figures/Fig3.pdf", width=9, height=8)
par(mfrow=c(2,2),mar=c(4.1,6.9,2.5,1.1),mgp=c(2.5,0.5,0))
rcttext::plot_textfx(dd.sci[dd.sci$grade==1 & dd.sci$type!="Planned",], main="Grade 1 Science",xlim=c(-0.8,0.8))
rcttext::plot_textfx(dd.soc[dd.soc$grade==1 & dd.sci$type!="Planned",], main="Grade 1 Social Studies",xlim=c(-0.8,0.8))
rcttext::plot_textfx(dd.sci[dd.sci$grade==2 & dd.sci$type!="Planned",], main="Grade 2 Science",xlim=c(-0.8,0.8))
rcttext::plot_textfx(dd.soc[dd.soc$grade==2 & dd.sci$type!="Planned",], main="Grade 2 Social Studies",xlim=c(-0.8,0.8))
dev.off()
par(mfrow=c(1,1))



# CCS plots ----

load("results/CCS_results.RData")

g1.sci=ccs_out[ccs_out$subject=="science" & ccs_out$grade==1,-c(1:2)]
g2.sci=ccs_out[ccs_out$subject=="science" & ccs_out$grade==2,-c(1:2)]

g1.soc=ccs_out[ccs_out$subject!="science" & ccs_out$grade==1,-c(1:2)]
g2.soc=ccs_out[ccs_out$subject!="science" & ccs_out$grade==2,-c(1:2)]


pdf(file="figures/Fig4a.pdf", width=9, height=5)
par(mfrow=c(1,2))
rcttext::plot_ccs(g1.sci, main="G1 Science",xlim=c(-0.2,0.40))
rcttext::plot_ccs(g2.sci, main="G2 Science",xlim=c(-0.2,0.40))
dev.off()


pdf(file="figures/Fig4b.pdf", width=9, height=5)
par(mfrow=c(1,2))
plot_ccs(g1.soc, main="G1 Social",xlim=c(-0.2,0.40))
plot_ccs(g2.soc, main="G2 Social",xlim=c(-0.2,0.40))
dev.off()
par(mfrow=c(1,1))



# Figure: Distribution of similarity scores ----

levels(all.info$subject)=c("Science","Social Studies")
levels(all.info$grade)=c("Grade 1", "Grade 2")

ggplot(all.info, aes(x=1-tdm.raw.cosine, 
                     col=as.factor(more),
                     fill=as.factor(more))) +
  geom_hline(yintercept=0, col="black")+
  facet_grid(grade~subject,scales="free_y")+
  geom_density(linewidth=1.05, position="identity", alpha=0.2)+
  labs(x="Cosine similarity",y="Density")+
  scale_colour_discrete(name = "", 
                        labels = c("Control","Treatment"),
                        aesthetics=c("colour","fill"))+
  scale_x_continuous(limits=c(-0.1,1.0))+
  theme(axis.text.x = element_text(size=11), 
        legend.text = element_text(size=11),
        legend.position="bottom")

ggsave("figures/Fig5.pdf", width=8, height=6)





