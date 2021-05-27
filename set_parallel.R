## Set up parallelization for MSstats summarization 
# depends on:

library(MSstats)
library(parallel)


msstats_process <- function(x){
          
          cl <- makeCluster(detectCores() - detectCores()/3 )
          
          msstasout <- dataProcess(x,
                                   logTrans=2,
                                   normalization="equalizeMedians",
                                   nameStandards=NULL,
                                   address="",
                                   fillIncompleteRows=TRUE,
                                   featureSubset="all",
                                   remove_uninformative_feature_outlier=FALSE,
                                   n_top_feature=3,
                                   summaryMethod="TMP",
                                   equalFeatureVar=TRUE,
                                   censoredInt="NA",
                                   cutoffCensored="minFeature",
                                   MBimpute=TRUE,
                                   remove50missing=FALSE,
                                   maxQuantileforCensored=0.999,
                                   clusters=cl)
          
          stopCluster(cl)
          
          return(msstasout)
}
