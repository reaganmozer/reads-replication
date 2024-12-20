# Process and clean the essay texts
#
# Make cleaned datasets for later analysis from the student essays
#
# Actions:
#
# Correct spelling for a list of commonly misspelled words in corpus
#
# Also write individual text files for each document to be processed
# by third party software.
#

source( here::here( "scripts/00_setup.R" ) )

dat1=read_rds( "data-generated/student_essays.RData" )


reads.dict=read.table( here::here("data/reads_dict.txt") ) 
head( reads.dict )

# Convert to lowercase and ASCII
dat1$text.lc = iconv(tolower(dat1$text), from="UTF-8", to="ASCII", sub="")

# Clean up some punctuation stuff
dat1$text.lc = rcttext::repair_spelling( dat1$text.lc,
                                         c("s's", "s'", "."),
                                         c("s'", "s'", ".") )

# Expand some contractions
dat1$text.lc = rcttext::repair_spelling( dat1$text.lc,
                                         c("they're","i'll", "that s"),
                                         c("they are","i will", "that is") )


dat1$text.lc = tm::stripWhitespace(dat1$text.lc)



# Autocorrect some common misspelled words
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

dat1$text.sc = rcttext::repair_spelling( dat1$text.lc, words )


# Apply automatic spelling
additional_words = c("mona","striked","dinos","venus","wolly","xbox","youtube","ipads")
additional_words = unique( c( additional_words, reads.dict$V1 ) )
skip_prefix = c("#","1","2","3","8")

dat1$text.sc=rcttext::apply_hunspell( dat1$text.sc,
                                      additional_words = additional_words,
                                      skip_prefix = skip_prefix )



dat1$spellcheck = 1*(dat1$text.lc!=dat1$text.sc)
table(dat1$spellcheck)


#### Drop empty or undecipherable essays ####

nrow(dat1)

dat1 = dat1 %>% 
  filter(!text %in% c("","can't decipher")) %>% 
  arrange(s_id, subject) %>% 
  mutate(ID=row_number()) 

nrow( dat1)



#### Save processed data to intermediate files ####


text = select(dat1, ID, s_id, grade, subject, more, text, text.sc) 
meta = select(dat1, -text, -text.lc, -text.sc) 


write.csv(text,
          file="data-generated/text_g1g2_consented.csv",
          row.names=FALSE)

save(meta, text, file="data-generated/meta.RData" )


# Write cleaned essays to text files for analysis via TAACO
if (FALSE){ 
  # only need to run once to generate intermediate files
  
  rcttext::prep_external(text$text.sc,
                         dir=here::here("data-external/main-texts/"),
                         docnames=text$ID)
}
