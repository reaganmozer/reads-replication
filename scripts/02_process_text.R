# Process and clean the essay texts
#
# Loads the g1 & g2 public file and makes cleaned datasets for later
# analysis.
#
# Actions:
#
# Correct spelling for a list of commonly misspelled words in corpus
#
# Also write individual text files for each document to be processed
# by third party software.
#


dat=read.csv( here::here("data-raw/write_machinelearning_replication_main.csv"),
              encoding='WINDOWS-1252')



reads.dict=read.table( here::here("data/reads_dict.txt") )
head( reads.dict )


# include only those who have all demographics and pre-test scores
check.admin = dplyr::select(dat, s_id, s_white_num:s_ses_high, s_dib_score_1819w, s_maprit_1819w, s_itt_consented)
table(apply(check.admin, 1, function(x)sum(is.na(x))))

check.admin$anyNA = sapply(1:nrow(check.admin), function(x) sum(is.na(check.admin[x,-c(1)])))

rm.ids = check.admin$s_id[check.admin$anyNA>0]
dat = dat %>% filter(!s_id %in% rm.ids)

rm(check.admin, rm.ids)

dat = select(dat, s_id, t_moreresearchid, sch_researchid, s_itt_consented,
             s_grade, s_maprit_1819w,
             s_sci_write, s_ss_write,
             s_sci_response, s_ss_response)
dat = dat %>% rename(t_id=t_moreresearchid,
                     sch_id=sch_researchid,
                     more=s_itt_consented,
                     grade=s_grade)
dat$grade=as.factor(dat$grade)
levels(dat$grade)=c(1,2)

dat %>% group_by(grade,more) %>% summarise(n.students=length(unique(s_id)))


dat2 = dat %>% tidyr::pivot_longer(cols=s_sci_write:s_ss_response, names_to=c("subject",".value"),
                            names_pattern="s_?(.*)_(.*)")  %>% rename(score=write, text=response) %>%
  filter(!is.na(score)) %>% mutate(subject=recode(subject, sci="science", ss="social")) %>%
  select(s_id:grade, subject, everything()) %>% arrange(s_id, grade, subject)

dat2 %>% group_by(grade,subject) %>%
  summarise(n.students=length(unique(s_id)))


# Something weird left over from Windows-1252 encoding; convert to UTF-8
dat2$text<- iconv(dat2$text, from='WINDOWS-1252', to='ASCII', sub=" ")

#dat2$text <- iconv(dat2$text, from='WINDOWS-1252', to='UTF-8')

dat2 %>% group_by(subject) %>%
  summarise(mean.nchar = mean(nchar(text)),
            sd.nchar=sd(nchar(text)))


apply(dat2,2,anyNA) # check for no missing



# Clean up some punctuation stuff
dat2$text.sc = iconv(tolower(dat2$text), from="UTF-8", to="ASCII", sub="")
dat2$text.sc = rcttext::repair_spelling( dat2$text.sc,
                                      c("s's","s'","."),
                                      c("s'","s'",".") )
dat1 = dat2



# Some data cleaning/autocorrecting of the misspelled words
words = tribble( ~from, ~to,
                 "aateroid","asteroid",
                 "advanture","adventure",
                 "accidently","accidentally",
                 "apiniter","painter",
                 "beause","because",
                 "leonard","leonardo",
                 "dinos","dinosaurs",
                 "dino's","dinosaurs",
                 "dino","dinosaur",
                 "shoud","should",
                 "wiht","with",
                 "earhar","earhart",
                 "earhard","earhart",
                 "leonarod","leonardo",
                 "thinked","think",
                 "da vinci","davinci",
                 "da v inci","davinci",
                 "nad","and",
                 "dont","do not",
                 "theat","that",
                 "rees","trees",
                 "studing","studying",
                 "striked","strike",
                 "opinon","opinion",
                 "oone","one",
                 "cuting","cutting",
                 "sthe","the",
                 "sould ","should",
                 "tryed ","tried",
                 "writed ","write",
                 "rianforest ","rainforest",
                 "i've","i have",
                 "i'll","i will",
                 "they're","they are",
                 "flyed","fly",
                 "drawed","drew",
                 "alantic","atlantic",
                 "airplain","airplane",
                 "airplanee","airplane",
                 "airplanees","airplanes",
                 "beause","because",
                 "oceann","ocean",
                 "airplanee","airplane",
                 "did't","did not",
                 "wouldd","would",
                 "wantd","wanted",
                 "coildn't","could not",
                 "coild","could",
                 "probems","problems",
                 "probem","problem",
                 "studf","stuff",
                 "machinee","machine",
                 "anotherr","another" )



dat2$text.sc = rcttext::repair_spelling( dat2$text.sc, words )

# Expand some contractions
dat2$text.sc = rcttext::repair_spelling( dat2$text.sc,
                                c("they're","i'll"),
                                c("they are","i will") )



additional_words = c("mona","striked","dinos","venus","wolly","xbox","youtube","ipads")
additional_words = unique( c( additional_words, reads.dict$V1 ) )
skip_prefix = c("#","1","2","3","8")

dat2$text.sc=rcttext::apply_hunspell( dat2$text.sc,
                             additional_words = additional_words,
                             skip_prefix = skip_prefix )






out=quanteda.textstats::textstat_frequency(quanteda::dfm(quanteda::tokens(dat2$text.sc)))
out$check=hunspell::hunspell_check(out$feature)
head(out[out$check==F,],n=20)


dat2$spellcheck = 1*(dat1$text.sc!=dat2$text.sc)

dat2 = dat2 %>% filter(!text %in% c("","can't decipher")) %>% arrange(s_id, subject) %>% mutate(ID=row_number())
rownames(dat2)=NULL


text = select(dat2, ID, s_id, grade, subject, more, text, text.sc)
meta = select(dat2, -text, -text.sc)


text$text.sc = tm::stripWhitespace(text$text.sc)


#### Save processed data to intermediate files ####

write.csv(text,
          file="data-generated/text_g1g2_consented.csv",
          row.names=F)

save(meta, text,
     file="data-generated/meta.RData" )


# Write cleaned essays to text files for analysis via TAACO
if (FALSE){ # only need to run once to generate intermediate files

rcttext::prep_external(text$text.sc,
                    dir=here::here("data-external/main-texts/"),
                    docnames=text$ID)
}
