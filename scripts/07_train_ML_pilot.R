
## Train an ML model to predict human-coded quality scores for the
## pilot data (G1 science essays from a different evaluation)

source( here::here( "scripts/00_setup.R" ) )

options(stringsAsFactors = FALSE)




load( here::here( "data-generated/all.pilot.RData") )
dat = select(all.pilot,-ID)

dim(dat)
# Preprocess the feature space to remove collinear features and features with near-zero variance

X0 = select(dat, -Yobs, -Z, -starts_with("sent_")) # exclude sentiment features from prediction to avoid redundancies

if ( FALSE ) {
  # TODO: What is the following? Should it be in clean_features()?  
  
  X = predict(caret::preProcess(X0, method=c("nzv","corr"), uniqueCut=2, cutoff=0.95), X0)
  caret::findLinearCombos(X) # sanity check to make sure no redundant features
  
}
X = clean_features(X0)
dim(X)

# Cut way down for demo purposes
X = X[,1:30]

# Fit a model trained on the pilot data
# Warning! this takes a few minutes to run
table(dat$Yobs) # check bounds
set.seed(123)

fit = train_models(x=X, y=dat$Yobs, n.tune=3, preProc=NULL, bounds=c(0,11),
                     include_BART = FALSE,
                     verbose = TRUE )
save(fit, file="data-generated/pilotML_model.RData")


# Use the trained model to predict for the current data set ----

load("data-generated/all.info.RData")
all = select(all.info, score, more, everything()) %>% 
  dplyr::rename(Yobs=score, Z=more)
newX = all %>%
  dplyr::select( all_of( colnames(X) ) ) %>%
  as.matrix()

# Generate predictions for case study sample

sub = select(all, s_id, t_id, sch_id, subject, grade)
all.ML.scores = add_predictions( fit, newX, sub, prefix="" )

# How well do they predict?
round(cor(all$Yobs,all.ML.scores[ , -c(1:5) ]), 3)


save(all.ML.scores, file="data-generated/all.ML.scores.RData")
