##
## Simple CCS analysis of the essays within each grade and subject
##

options(stringsAsFactors = FALSE)

source( here::here( "scripts/cluster_threshold_C.R" ) )

library( tidyverse )
library( tm )


#### Load the data #####

load( here::here( "data-generated/meta.RData" ) )

text <- filter( text, subject == "science" )
meta <- filter( meta, subject == "science" )
text$sch_id = meta$sch_id
rm( meta )

text$more_pm = 2 * text$more - 1


# Make corpus object

clean.txt = function(x){
  out = stripWhitespace(removeNumbers(removePunctuation(tolower(x))))
  out
}

corpus = tm::VCorpus( VectorSource( clean.txt(text$text.sc ) ))

#corpus = clean.text( corpus )
corpus

raw_corpus = tm::VCorpus( VectorSource(text$text ) )
raw_corpus = tm::tm_map(raw_corpus, content_transformer(tolower))


###### Function to fit a collection of standard models to look at ######


scat = function( str, ... ) {
  cat( sprintf( str, ... ) )
}


# Find threshold C and run textreg with that tuning parameter.
# @return Textreg result object with the passed tuning parameter.
tuned_textreg <- function( corpus, Z, cluster_id, ... ) {
  require(textreg)
  C = cluster.threshold.C( corpus, Z, cluster_id=cluster_id, R = 20)
  C_L2 = quantile(C,0.80)
  
  scat( "L2 C: %.2f / %.2f\n", C[[1]], C_L2 )
  res1 = textreg( corpus = corpus,
                  Z,
                  C = C_L2,
                  ... )
  res1
}


make.result.table = function( sc, more_sc, cluster_id ) {
  
  res1 = tuned_textreg( corpus = sc,
                        more_sc, cluster_id,
                        verbosity = 0)
  
  res2 = tuned_textreg( corpus = sc,
                        more_sc, cluster_id,
                        gap=1,
                        verbosity = 0)
  
  res3 = tuned_textreg( corpus = sc,
                        more_sc, cluster_id,
                        binary.features = TRUE,
                        verbosity = 0)
  
  res4 = tuned_textreg( corpus = sc,
                        more_sc, cluster_id,
                        Lq = 3,
                        verbosity = 0)
  res4
  
  
  res5 = tuned_textreg( corpus = sc,
                        more_sc, cluster_id,
                        Lq = 1.5,
                        verbosity = 0)
  res5
  
  results = list( res1, res2, res3, res4, res5 )
  
  # Hack to drop trailing * due to poor implementation, sadly, of
  # textreg.
  for ( i in 1:length(results) ) {
    results[[i]]$model$ngram = gsub( " \\*$", "", results[[i]]$model$ngram )
  }
  
  Cs = map_dbl( results, \(.x) .x$notes$C )
  
  tbl = textreg::make.list.table( results,
                                  model.names = paste( c("L2","L2 (gap)","L2 (bin)","L3","L1.5"),
                                                       round( Cs, digits=1 ),
                                                       sep="-" ),
                                  method = "rank" )
  
  tbl
}


results.tab = function(result, corp.sub, Z) {
  result$n.mods = sapply(1:nrow(result),function(x) sum(!is.na(result[x,c(2:6)])))
  tmp = subset(result,select=c(phrase, n.mods, num.reports, num.tag))
  tmp$count.neg = tmp$num.reports-tmp$num.tag
  names(tmp)=c("phrase","n.mods","docs.total", "docs1","docs0")
  tmp2 = tmp %>% mutate(prop.docs1= docs1/sum(Z),
                        prop.docs0 = docs0/sum(1-Z),
                        prop.diff = prop.docs1-prop.docs0)
  
  
  phrases = unique(tmp2$phrase)
  m = make.phrase.matrix(phrases, corp.sub)
  tmp2$phrase.tot = colSums(m)
  tmp2$tot1 = colSums(m[Z==1,])
  tmp2$tot0 = colSums(m[Z==0,])
  
  out = select(tmp2, phrase, n.mods, tot1, tot0,prop.diff)
  out$docs1 = paste0(tmp2$docs1, " (",round(tmp2$prop.docs1,2)*100, "%)")
  out$docs0 = paste0(tmp2$docs0, " (",round(tmp2$prop.docs0,2)*100, "%)")
  out$diff.val=tmp2$prop.diff
  out$diff = paste0(round(tmp2$prop.diff,2)*100,"%")
  out$diff[tmp2$prop.diff>0]=paste0("+",out$diff[tmp2$prop.diff>0])
  #out$docs.diff = paste0(round(tmp2$prop.diff*100,1),"%")
  out = out[with(out,order(desc(n.mods),desc(prop.diff))),]
  out = select(out, phrase, n.mods, tot1, tot0,docs1, docs0, diff, diff.val)
  rownames(out)=NULL
  out
}




set.seed(1234)
r_Sci = make.result.table( corpus, text$more_pm, cluster_id = text$sch_id )
r_Sci


out_sci = results.tab(r_Sci, corp.sub=corpus,
                         Z=text$more)
out_sci



