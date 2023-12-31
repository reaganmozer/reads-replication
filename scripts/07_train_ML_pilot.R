## Train an ML model to predict human-coded quality scores for the pilot data (G1 science essays from a different evaluation)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../")
options(stringsAsFactors = F)




train_ensemble = function( x, y, n.tune=3, preProc=NULL, bounds=NULL) {

  doParallel::registerDoParallel(parallel::detectCores()-1)
  foreach::getDoParWorkers()


  ind = caret::createResample(y, times=5)
  control = caret::trainControl(method="cv", number=5, index=ind,
                                savePredictions="final",allowParallel=T,
                                predictionBounds = bounds)

  # Fit our "ML" models


  methods = c("bstTree", "cforest","cubist",
              "glmnet", "knn", "pcr",
              "rf", "rpart1SE", "RRFglobal",
              "svmPoly","svmRadial", "treebag")

  methods.tl = list(gbm = caretEnsemble::caretModelSpec(method="gbm",verbose=F))
  mods = caretEnsemble::caretList(x=as.matrix(x), y=y,trControl=control, preProcess=preProc,
                                  methodList=methods,tuneLength=n.tune, tuneList=methods.tl)


  c0 = caret::trainControl(method="cv", number=5, index=ind, predictionBounds=bounds,
                           savePredictions="final",allowParallel=F)

  bart = caretEnsemble::caretList(x=as.matrix(x), y=y,  preProcess=preProc,
                                  methodList="bartMachine", trControl=c0, verbose=F, serialize=T,
                                  tuneGrid=data.frame(num_trees=c(50,100), k=2, alpha=0.95, beta=2, nu=3))



  all.mods = c(mods, bart)

  tc.new=caret::trainControl(predictionBounds=bounds,
                             method="cv",number=5)

  stack = caretEnsemble::caretStack(all.mods,trControl=tc.new, tuneLength=n.tune*2)

  doParallel::stopImplicitCluster()


  fit = list(all.mods,  stack)
  names(fit)=c("all.mods","stack")
  return(fit)
}

load("data-generated/all.pilot.RData")
dat = select(all.pilot,-ID)

# Preprocess the feature space to remove collinear features and features with near-zero variance

X0 = select(dat, -Yobs, -Z, -starts_with("sent_")) # exclude sentiment features from prediction to avoid redundancies
X = predict(caret::preProcess(X0, method=c("nzv","corr"), uniqueCut=2, cutoff=0.95), X0)
caret::findLinearCombos(X) # sanity check to make sure no redundant features



# Fit a model trained on the pilot data
# Warning! this takes a few minutes to run
table(dat$Yobs) # check bounds
set.seed(123)
fit = train_ensemble(x=X, y=dat$Yobs, n.tune=3, preProc=NULL, bounds=c(0,11))
save(fit, file="data-generated/pilotML_model.RData")

# Use the trained model to predict for the current data set
load("data-generated/all.info.RData")
all = select(all.info, score, more, everything()) %>% dplyr::rename(Yobs=score, Z=more)
newX= as.matrix( select(all, names(X)) )

# Generate predictions for case study sample
yhat = predict(fit, newX)
yhat.all = do.call(cbind, yhat)
colnames(yhat.all) = tm::removeNumbers(colnames(yhat.all))

sub = select(all, s_id, t_id, sch_id, subject, grade)
out = as.data.frame(cbind(sub, yhat.all))

all.ML.scores = out
round(cor(all$Yobs,yhat.all),3)


save(all.ML.scores, file="data-generated/all.ML.scores.RData")
