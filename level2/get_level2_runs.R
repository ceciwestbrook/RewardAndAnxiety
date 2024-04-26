# Ceci Westbrook, 2022
# This script looks through the files generated in level1/ to identify subjects missing a run from
# either pretreatment or posttreatment data so that they can be correctly entered into the level2 feats.

library(dplyr)
setwd('/ix/cladouceur/westbrook-data/Scripts')
allsubs <- read.table('imaging_subjects.txt')
setwd('/ix/cladouceur/westbrook-data/Scripts/level1')
prerun1 <- t(read.table('subjects_prerun1.txt'))
prerun2 <- t(read.table('subjects_prerun2.txt'))
postrun1 <- t(read.table('subjects_postrun1.txt'))
postrun2 <- t(read.table('subjects_postrun2.txt'))
setwd('/ix/cladouceur/westbrook-data/Scripts/level2')
regressors <- read.table('usable_regressors.txt',header=TRUE)
setwd('/ix/cladouceur/westbrook-data/Scripts/level2/usableruns/Feb2024_newcontrast/')

cope1 = data.frame("ID_Num"=numeric())
cope2 = data.frame("ID_Num"=numeric())
cope3 = data.frame("ID_Num"=numeric())
cope4 = data.frame("ID_Num"=numeric())
cope5 = data.frame("ID_Num"=numeric())
cope6 = data.frame("ID_Num"=numeric())
cope7 = data.frame("ID_Num"=numeric())
cope8 = data.frame("ID_Num"=numeric())
cope9 = data.frame("ID_Num"=numeric())
cope10 = data.frame("ID_Num"=numeric())
cope11 = data.frame("ID_Num"=numeric())
cope12 = data.frame("ID_Num"=numeric())
cope13 = data.frame("ID_Num"=numeric())
cope14 = data.frame("ID_Num"=numeric())
cope15 = data.frame("ID_Num"=numeric())

for(session in c("pre","post")){
  for(run in c("run1","run2")){
    assign("curfile",get(paste0(session,run)))
    for(subject in curfile){
  # cope1 - Approach (ApproachResponse2s) - Fixation
      if( regressors[regressors$ID_Num==subject &
                      regressors$session==paste0(session,"treatment") &
                      regressors$run==run &
                      regressors$Regressor=="ApproachResponse2s", "usability"]=="usable"){
        cope1 <- rbind(cope1,subject)
        write.table(cope1,paste0("cope1",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
      }
      assign(paste0("cope1",session,run),cope1)

  # cope2 - Approach - Control
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="ApproachResponse2s", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="XResponse2s", "usability"]=="usable"){
        cope2 <- rbind(cope2,subject)
      }
      assign(paste0("cope2",session,run),cope2)
      write.table(cope2,paste0("cope2",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope3 - Avoid - Fixation
        if( regressors[regressors$ID_Num==subject &
                       regressors$session==paste0(session,"treatment") &
                       regressors$run==run &
                       regressors$Regressor=="AvoidResponse2s", "usability"]=="usable"){
          cope3 <- rbind(cope3,subject)
        } 
      assign(paste0("cope3",session,run),cope3)
      write.table(cope3,paste0("cope3",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope4 - Avoid - Control
          if( regressors[regressors$ID_Num==subject &
                         regressors$session==paste0(session,"treatment") &
                         regressors$run==run &
                         regressors$Regressor=="AvoidResponse2s", "usability"]=="usable" &
              regressors[regressors$ID_Num==subject &
                         regressors$session==paste0(session,"treatment") &
                         regressors$run==run &
                         regressors$Regressor=="XResponse2s", "usability"]=="usable"){
            cope4 <- rbind(cope4,subject)
          }
      assign(paste0("cope4",session,run),cope4)
      write.table(cope4,paste0("cope4",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope5 - AmbigApproach - Fixation
            if( regressors[regressors$ID_Num==subject &
                           regressors$session==paste0(session,"treatment") &
                           regressors$run==run &
                           regressors$Regressor=="AmbigApproachResponse2s", "usability"]=="usable"){
              cope5 <- rbind(cope5,subject)
            }   
      assign(paste0("cope5",session,run),cope5)
      write.table(cope5,paste0("cope5",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope6 - AmbigApproach - Control
              if( regressors[regressors$ID_Num==subject &
                             regressors$session==paste0(session,"treatment") &
                             regressors$run==run &
                             regressors$Regressor=="AmbigApproachResponse2s", "usability"]=="usable" &
                  regressors[regressors$ID_Num==subject &
                             regressors$session==paste0(session,"treatment") &
                             regressors$run==run &
                             regressors$Regressor=="XResponse2s", "usability"]=="usable"){
                cope6 <- rbind(cope6,subject)
              }    
      assign(paste0("cope6",session,run),cope6)
      write.table(cope6,paste0("cope6",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope7 - AmbigApproach - Approach
                if( regressors[regressors$ID_Num==subject &
                               regressors$session==paste0(session,"treatment") &
                               regressors$run==run &
                               regressors$Regressor=="AmbigApproachResponse2s", "usability"]=="usable" &
                    regressors[regressors$ID_Num==subject &
                               regressors$session==paste0(session,"treatment") &
                               regressors$run==run &
                               regressors$Regressor=="ApproachResponse2s", "usability"]=="usable"){
                  cope7 <- rbind(cope7,subject)
                }      
      assign(paste0("cope7",session,run),cope7)
      write.table(cope7,paste0("cope7",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope8 - ApproachAll - Fixation
                  if( regressors[regressors$ID_Num==subject &
                                 regressors$session==paste0(session,"treatment") &
                                 regressors$run==run &
                                 regressors$Regressor=="ApproachResponse2s", "usability"]=="usable" &
                      regressors[regressors$ID_Num==subject &
                                 regressors$session==paste0(session,"treatment") &
                                 regressors$run==run &
                                 regressors$Regressor=="AmbigApproachResponse2s", "usability"]=="usable"){
                    cope8 <- rbind(cope8,subject)
                  }
      assign(paste0("cope8",session,run),cope8)
      write.table(cope8,paste0("cope8",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope9 - ApproachAll - Control
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="ApproachResponse2s", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="AmbigApproachResponse2s", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="XResponse2s", "usability"]=="usable"){
        cope9 <- rbind(cope9,subject)
      }
      assign(paste0("cope9",session,run),cope9)
      write.table(cope9,paste0("cope9",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope10 - Control - Fixation
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="XResponse2s", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="Fixation", "usability"]=="usable"){
        cope10 <- rbind(cope10,subject)
      }
      assign(paste0("cope10",session,run),cope10)
      write.table(cope10,paste0("cope10",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope11 - GainFeedback - Fixation
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="GainFeedback", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="Fixation", "usability"]=="usable"){
        cope11 <- rbind(cope11,subject)
      }
      assign(paste0("cope11",session,run),cope11)
      write.table(cope11,paste0("cope11",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

  # cope12 - LossFeedback - Fixation
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="LossFeedback", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="Fixation", "usability"]=="usable"){
        cope12 <- rbind(cope12,subject)
      }
      assign(paste0("cope12",session,run),cope12)
      write.table(cope12,paste0("cope12",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

 # cope13 - GainFeedback - LossFeedback
      if( regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="GainFeedback", "usability"]=="usable" &
          regressors[regressors$ID_Num==subject &
                     regressors$session==paste0(session,"treatment") &
                     regressors$run==run &
                     regressors$Regressor=="LossFeedback", "usability"]=="usable"){
        cope13 <- rbind(cope13,subject)
      }
      assign(paste0("cope13",session,run),cope13)
      write.table(cope13,paste0("cope13",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)

    # cope14 - GainFeedback - XRespFeedback
    if( regressors[regressors$ID_Num==subject &
                   regressors$session==paste0(session,"treatment") &
                   regressors$run==run &
                   regressors$Regressor=="GainFeedback", "usability"]=="usable" &
        regressors[regressors$ID_Num==subject &
                   regressors$session==paste0(session,"treatment") &
                   regressors$run==run &
                   regressors$Regressor=="XRespFeedback", "usability"]=="usable"){
      cope14 <- rbind(cope14,subject)
    }
    assign(paste0("cope14",session,run),cope14)
    write.table(cope15,paste0("cope14",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
    
  # cope15 - LossFeedback - XRespFeedback
  if( regressors[regressors$ID_Num==subject &
                 regressors$session==paste0(session,"treatment") &
                 regressors$run==run &
                 regressors$Regressor=="LossFeedback", "usability"]=="usable" &
      regressors[regressors$ID_Num==subject &
                 regressors$session==paste0(session,"treatment") &
                 regressors$run==run &
                 regressors$Regressor=="XRespFeedback", "usability"]=="usable"){
    cope15 <- rbind(cope15,subject)
  }
  assign(paste0("cope15",session,run),cope15)
  write.table(cope15,paste0("cope15",session,run,"_usableruns.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE)
}
    cope1 = data.frame("ID_Num"=numeric())
    cope2 = data.frame("ID_Num"=numeric())
    cope3 = data.frame("ID_Num"=numeric())
    cope4 = data.frame("ID_Num"=numeric())
    cope5 = data.frame("ID_Num"=numeric())
    cope6 = data.frame("ID_Num"=numeric())
    cope7 = data.frame("ID_Num"=numeric())
    cope8 = data.frame("ID_Num"=numeric())
    cope9 = data.frame("ID_Num"=numeric())
    cope10 = data.frame("ID_Num"=numeric())
    cope11 = data.frame("ID_Num"=numeric())
    cope12 = data.frame("ID_Num"=numeric())
    cope13 = data.frame("ID_Num"=numeric())
    cope14 = data.frame("ID_Num"=numeric())
    cope15 = data.frame("ID_Num"=numeric())
  }
}

run1run2 = data.frame("ID_Num"=numeric())
run1run1 = data.frame("ID_Num"=numeric())
run2run2 = data.frame("ID_Num"=numeric())

setwd('/ix/cladouceur/westbrook-data/Scripts/level2/make_fsfs/Feb2024_newcontrast/')

for(cope in c("cope1","cope2","cope3","cope4","cope5","cope6","cope7","cope8","cope9","cope10","cope11","cope12","cope13","cope14","cope15")){
  for(session in c("pre","post")){
    for(subject in allsubs[,]){
      if(subject %in% get(paste0(cope,session,"run1"))[,1] & subject %in% get(paste0(cope,session,"run2"))[,1]){
        run1run2 <- rbind(run1run2,subject)
        assign(paste0(cope,session,"run1run2"),run1run2)
      } else if(subject %in% get(paste0(cope,session,"run1"))[,1] & ! subject %in% get(paste0(cope,session,"run2"))[,1]) {
        run1run1 <- rbind(run1run1,subject)
        assign(paste0(cope,session,"run1run1"),run1run1)
        } else if(! subject %in% get(paste0(cope,session,"run1"))[,1] & subject %in% get(paste0(cope,session,"run2"))[,1]) {
        run2run2 <- rbind(run2run2,subject)
        assign(paste0(cope,session,"run2run2"),run2run2)
        }
    }

    possibleError <- tryCatch(write.table(get(paste0(cope,session,"run1run1")),paste0(cope,session,"run1run1.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE),error = function(x) x)
    possibleError <- tryCatch(write.table(get(paste0(cope,session,"run1run2")),paste0(cope,session,"run1run2.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE),error = function(x) x)
    possibleError <- tryCatch(write.table(get(paste0(cope,session,"run2run2")),paste0(cope,session,"run2run2.txt"),col.names=FALSE,row.names = FALSE,quote=FALSE),error = function(x) x)
    
    run1run2 = data.frame("ID_Num"=numeric())
    run1run1 = data.frame("ID_Num"=numeric())
    run2run2 = data.frame("ID_Num"=numeric())
    }
  }
