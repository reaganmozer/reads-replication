
# Subset the raw data down to a dataset with each essay being a row,
# and only consenting students in the file

source( here::here( "scripts/00_setup.R" ) )

dat=read.csv( here::here("data-raw/write_machinelearning_replication_main.csv"),
              encoding='WINDOWS-1252')


reads.dict=read.table( here::here("data/reads_dict.txt") )
head( reads.dict )


table( dat$s_itt_consented )
# include only those who have all demographics and pre-test scores
check.admin = select(dat, stu_id, s_dib_score_1819w, s_maprit_1819w, s_maprit_1819w_std, s_itt_consented)
table(apply(check.admin, 1, function(x)sum(is.na(x))))

check.admin$anyNA = sapply(1:nrow(check.admin), function(x) sum(is.na(check.admin[x,-c(1)])))

rm.ids = check.admin$stu_id[check.admin$anyNA>0]
dat = dat %>% filter(!stu_id %in% rm.ids)

rm(check.admin, rm.ids)

dat = select(dat, stu_id, tch_id, sch_id, s_itt_consented,
             s_grade, s_maprit_1819w,
             s_sci_write, s_ss_write,
             s_sci_response, s_ss_response)
dat = dat %>% rename(s_id = stu_id,
                     t_id=tch_id,
                     more=s_itt_consented,
                     grade=s_grade)
dat$grade=as.factor(dat$grade)
levels(dat$grade)=c(1,2)

dat %>% group_by(grade,more) %>% summarise(n.students=length(unique(s_id)))

names(dat)

dat2 = dat %>% tidyr::pivot_longer(cols=s_sci_write:s_ss_response, names_to=c("subject",".value"),
                                   names_pattern="s_?(.*)_(.*)")  %>%
  rename(score=write, text=response) %>%
  filter(!is.na(score)) %>% 
  mutate(subject=recode(subject, sci="science", ss="social")) %>%
  select(s_id:grade, subject, everything()) %>% 
  arrange(s_id, grade, subject)

dat2 %>% group_by(grade,subject) %>%
  summarise(n.students=length(unique(s_id)))


# Something weird left over from Windows-1252 encoding; convert to UTF-8
dat2$text<- iconv(dat2$text, from='WINDOWS-1252', to='ASCII', sub=" ")

#dat2$text <- iconv(dat2$text, from='WINDOWS-1252', to='UTF-8')

dat2 %>% group_by(subject) %>%
  summarise(mean.nchar = mean(nchar(text)),
            sd.nchar=sd(nchar(text)))


apply(dat2,2,anyNA) # check for no missing

write_rds(dat2, file="data-generated/student_essays.RData")

