##
## Simple CCS analysis of the essays within each grade and subject
##

options(stringsAsFactors = FALSE)

source( here::here( "scripts/cluster_threshold_C.R" ) )

library( tidyverse )
library( rcttext )


#### Load the data #####

load( here::here( "data-generated/meta.RData" ) )

text$sch_id = meta$sch_id
text$sch_gr_block = paste0( meta$sch_id, "-", meta$grade )

table( paste0( text$subject, "-", text$grade  ), Tx=text$more )

# Number essays in each of our 4 groups
table( text$grade, text$subject )

text$more_pm = 2 * text$more - 1


###### Function to fit a collection of standard CCS models ######


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


make.result.list = function( sc, more_sc, cluster_id ) {
  
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
  
  results = list( L2 = res1, `L2 (gap)` = res2, 
                  `L2 (bin)` = res3, L3 = res4, L1.5 = res5 )
  
  results
}


# Do the analysis ----

# Initial pass: find threshold regularization
ind1 = which(text$subject=="science" & text$grade==1)
ind2 = which(text$subject=="science" & text$grade==2)


# Grade 1 science
set.seed(1234)
sci_g1 = make.result.list( text$text[ind1], text$more_pm[ind1], 
                           cluster_id = text$sch_id[ind1] )
r_Sci_g1 = ccs_list_table( sci_g1 )
r_Sci_g1

out_sci_g1 = ccs_result_table(r_Sci_g1, corpus=text$text[ind1],
                              Z=text$more[ind1])
out_sci_g1


# Grade 2
set.seed(1234)
sci_g2 = make.result.list( text$text[ind2], text$more_pm[ind2], cluster_id = text$sch_id[ind2] )
r_Sci_g2 = ccs_list_table( sci_g2 )
r_Sci_g2

out_sci_g2= ccs_result_table(r_Sci_g2, corpus=text$text[ind2],
                             Z=text$more[ind2])

out_sci = data.frame(subject="science", 
                     grade=c(rep(1,nrow(out_sci_g1)), 
                             rep(2,nrow(out_sci_g2))),
                     rbind(out_sci_g1, out_sci_g2))

## Social science
ind3 = which(text$subject!="science" & text$grade==1)
ind4 = which(text$subject!="science" & text$grade==2)

# Grade 1
set.seed(1234)
soc_g1 = make.result.list( text$text[ind3], text$more_pm[ind3], cluster_id = text$sch_id[ind3] )
r_Soc_g1 = ccs_list_table( soc_g1 )
r_Soc_g1

out_soc_g1= ccs_result_table(r_Soc_g1, corpus=text$text[ind3],
                             Z=text$more[ind3])

# Grade 2
set.seed(1234)
soc_g2 = make.result.list( text$text[ind4], text$more_pm[ind4], cluster_id = text$sch_id[ind4] )
r_Soc_g2 = ccs_list_table( soc_g2 )
r_Soc_g2

out_soc_g2 = ccs_result_table(r_Soc_g2, corpus=text$text[ind4],
                             Z=text$more[ind4])

out_soc = data.frame(subject="social", 
                     grade=c(rep(1,nrow(out_soc_g1)),
                             rep(2,nrow(out_soc_g2))),
                     rbind(out_soc_g1, out_soc_g2))


ccs_out = rbind(out_sci, out_soc)
ccs_out$grade=as.factor(ccs_out$grade)
ccs_out$subject=as.factor(ccs_out$subject)

save( r_Sci_g1, r_Sci_g2,
      r_Soc_g1, r_Soc_g2,
      ccs_out,
      file="results/CCS_results.RData" )

