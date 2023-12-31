## Process pilot data (a subset of G1 science essays from a different
## evaluation) that we will use to train the ML model

source( here::here( "scripts/00_setup.R" ) )


pilot = read.csv( here::here( "data-raw/write_machinelearning_pilot_replication.csv" ),
                 encoding='WINDOWS-1252')


pilot <- pilot %>% mutate(ID=row_number())

# Drop empty responses
pilot <- pilot |> filter(response != "")

# Grab key columns of metadata
all.feats = select(pilot, ID, 
                   writing_quality_score_2, more)

# Convert text to UTF-8
pilot$response <- iconv(pilot$response, from='WINDOWS-1252', to='ASCII', sub=" ")

# Get text (and repair one piece of spelling)
essay.text = pilot$response %>%
  repair_spelling( "shoud", "should" )


# Save intermediate file for processing in LIWC
pilot.out = data.frame(ID=pilot$ID, text.sc=essay.text)
write.csv(pilot.out,file="data-generated/text_pilot.csv",row.names=F)

# Generate set of general features
all.feats = rcttext::generate_features(essay.text,
                              meta = all.feats,
                               sent = TRUE,
                              clean_features = FALSE,
                              terms = "xxx",
                              read = c("Flesch","Flesch.Kincaid", "ARI", "ELF",
                                       "meanWordSyllables"),
                              ld=c("TTR","R","K"),
                              verbose = TRUE )

# Note: term of 'xxx' are illegible words/phrases
table( all.feats$xxx )

# Add Word2Vec projections for each essay on 50 dimensions
options(timeout=240) # may need to run this if you get an error downloading the file below
glove.50d = textdata::embedding_glove6b(dimensions = 50)

all.feats = rcttext::extract_w2v( clean_text(essay.text),
                         meta = all.feats,
                         model = glove.50d )



# Add externally computed LIWC-generated features
all.feats <- rcttext::extract_liwc("data-generated/LIWC_pilot.csv",
              meta = all.feats, ID.liwc = "ID", ID.meta = "ID",
              clean = FALSE )


# And externally computed TAACO features
if (FALSE){ # only need to run once to generate intermediate files
  rcttext::prep_external(essay.text, dir="data-external/pilot-texts/", docnames=pilot$ID)
}

all.feats <- rcttext::extract_taaco("data-generated/taaco_pilot.csv",
                            meta = all.feats,
                            ID.meta = "ID" )


# Drop features we don't need/that are redundant
dim(all.feats)
all.feats = rcttext::clean_features( all.feats,
                            ignore = c( "ID",
                                        "writing_quality_score_2",
                                        "more" ) )

dim(all.feats)


all.pilot = all.feats %>%  rename(Yobs=writing_quality_score_2, Z=more)

names(all.pilot) = gsub("lex_f.ent[, -c(1)]","lex_f.ent",names(all.pilot),fixed=T)
names(all.pilot) = gsub(" ","_",names(all.pilot),fixed=T)


save(all.pilot, file="data-generated/all.pilot.RData")
