# Ceci Westbrook, 2022
# This script looks through the generated timing files and identifies any that are empty (i.e., 0 0 0).
# Contrasts including these regressors will zero out so need to be identified and excluded.

library(gdata)
library(tidyverse)
setwd('/ix/cladouceur/westbrook-data/Scripts')
allsubs <- read.csv('imaging_subjects.txt',header = FALSE)
setwd('/ix/cladouceur/westbrook-data/results')
write.table("ID_Num Regressor session run usability",'usable_regressors.txt',col.names=FALSE,row.names = FALSE,quote=FALSE)

# for debugging
#allsubs = data.frame(2003)

for(subject in allsubs[,]){
  subDir <- paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',subject,sep="")
  for(session in c('pretreatment','posttreatment')){
    for(run in c(1,2)){
      for(filename in c('AmbigApproachResponse2s','AmbigAvoidResponse2s','ApproachResponse2s',
                      'AvoidResponse2s','XResponse2s','Fixation','GainFeedback','LossFeedback','XRespFeedback')){
        possibleError <- tryCatch(assign(filename,read.csv(
          paste(subDir,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',filename,'_run-0',run,'.txt',
                sep=""),sep=" ",header=FALSE)),error = function(x) x)
        if(get(filename)[1,1] != 0){
          line = paste(subject,' ',filename,' ',session,' run',run,' usable',sep="")
        } else {
          line = paste(subject,' ',filename,' ',session,' run',run,' BLANK',sep="")
        }
        write.table(line,'usable_regressors.txt',col.names=FALSE,row.names = FALSE,quote=FALSE,append=TRUE)
        assign(filename,matrix(1,1))
        }
      }
  }
  }
