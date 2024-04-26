library(gdata)
library(tidyverse)
setwd('/ix/cladouceur/westbrook-data/Scripts/')
allsubs <- read.csv('imaging_subjects.txt',header = FALSE)
setwd('/ix/cladouceur/westbrook-data/Scripts/level1/behav_data/')

# for testing
#allsubs = data.frame(2003)

for(subject in allsubs[,]){
  
  # 12/2023 - previously I lumped a bunch of parts of the task in as no-interest.
  # now, taking some of those parts out (e.g., feedback)
  # I am going to overwrite the existing timing files because these have more data and this shouldn't
  # result in any data loss.
  subDir <- paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',subject,sep="")
  
  # making directories
  # don't think I need this anymore
  #if (!file.exists(subDir)) {
  #  dir.create(subDir)
  #}
  
  for(sesnum in c('02','22')) {
      for(run in c('a','b')) {
        possibleError <- tryCatch(read_csv2(paste(subject,'_',sesnum,run,'.dat',sep=""),skip=1,col_name=FALSE),
               error = function(x) x)
        
        session <- ifelse(sesnum=='02',"pretreatment",
                          ifelse(sesnum=='22',"posttreatment",""))

        runnum <- ifelse(run=='a',"01",
                          ifelse(run=='b',"02",""))
        
        # check to make sure the file exists, otherwise break the loop
        if(!inherits(possibleError, "error")) {
          
          # make directories for later
          # don't think I need this anymore
          # sesDir <- paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',subject,'/ses-',session,sep="")
          # if (!file.exists(sesDir)) {
          #  dir.create(sesDir)
          #  dir.create(paste(sesDir,'/func',sep=""))
          # }

          # read in the dat files and convert them into useable tibbles
          d <- read_csv2(paste(subject,'_',sesnum,run,'.dat',sep=""),skip=1,col_name=FALSE)
          d2 <- as_tibble(d[1:nrow(d)-1,])
          d2 <- mutate_if(d2,
                          is.character,
                          str_replace_all, "  "," ")
          curr_sub <- separate(d2,col=X1,into = c("Trial_type","Trial_begin","Num_avoid_resp","Num_approach_resp",
                                                  "Last_response_T","Outcome_T","GainLoss_Magnitude","Cumulative_points"),sep=" ")
          curr_sub <- as_tibble(sapply(curr_sub,as.numeric))
          curr_sub$next_trial_start <- lead(curr_sub$Trial_begin)
          curr_sub$last_trial_points <- lag(curr_sub$Cumulative_points)
          curr_sub$GainLoss <- ifelse(curr_sub$Cumulative_points > curr_sub$last_trial_points,"Gain",
                                      ifelse(curr_sub$Cumulative_points < curr_sub$last_trial_points,"Loss",
                                             ifelse(curr_sub$Cumulative_points == curr_sub$last_trial_points,"NoChange",NA)))
          curr_sub[1,"GainLoss"] <- ifelse(curr_sub[1,"Cumulative_points"] > 0, "Gain",
                                           ifelse(curr_sub[1,"Cumulative_points"] < 0, "Loss",
                                                  ifelse(curr_sub[1,"Cumulative_points"] == 0, "NoChange",NA)))
          curr_sub[nrow(curr_sub),"next_trial_start"] <- 384.10
          curr_sub$Response <- ifelse(curr_sub$Last_response_T > 0, (curr_sub$Last_response_T - curr_sub$Trial_begin),(curr_sub$Outcome_T - curr_sub$Trial_begin))
          curr_sub$PostResponse <- ifelse(curr_sub$Last_response_T > 0,(curr_sub$Outcome_T - curr_sub$Last_response_T),0)
          curr_sub$FixTime <- curr_sub$next_trial_start - 1.5 # fixation timestamps weren't recorded but it was always 1.5s long
          curr_sub$Fixation <- 1.5 # fixation timestamps weren't recorded but it was always 1.5s long
          curr_sub$Feedback <- curr_sub$FixTime - curr_sub$Outcome_T # feedback period minus fixation duration
          ResponseTrials <- curr_sub[curr_sub$Last_response_T>0,]
          NoResponseTrials <- curr_sub[curr_sub$Last_response_T==0,]
          
          # Here, we break things down into every possible event type, in case future analyses want to slice it
          # differently. This will get consolidated for current purposes down below.
          
          # NB:
          # Approach/Avoid is defined by which column gets to 6 responses.
          # Final trial type for ambiguous trials (approach/avoid) will be determined by what the subject's response was,
          # NOT what the correct response is. In other words, if they approach and had a loss, this is "AmbigApproachLoss."
          
          AvoidLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidLossFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="Loss",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AvoidNoLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoLossFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==1 & ResponseTrials$GainLoss=="NoChange",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          ApproachGainResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachGainPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachGainFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachGainFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="Gain",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          ApproachNoGainResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoGainPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoGainFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoGainFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==2 & ResponseTrials$GainLoss=="NoChange",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigApproachGainResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachGainPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachGainFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachGainFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Gain",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigApproachLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigApproachLossFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_approach_resp==6 & ResponseTrials$GainLoss=="Loss",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigAvoidNoLossResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigAvoidNoLossPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigAvoidNoLossFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigAvoidNoLossFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==4 & ResponseTrials$Num_avoid_resp==6 & ResponseTrials$GainLoss=="NoChange",c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          XRespResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          XRespPostResponse <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("Last_response_T","PostResponse")],1,.name_repair = ~ c("onset","duration","modulator"))
          XRespFeedback <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          XRespFixation <- tibble(ResponseTrials[ResponseTrials$Trial_type==3 ,c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AvoidNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==1 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==1 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AvoidNoRespFixation <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==1 ,c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          ApproachNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==2 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==2 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          ApproachNoRespFixation <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==2 ,c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          AmbigNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==4 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==4 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          AmbigNoRespFixation <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==4 ,c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          XNoRespResponse <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==3 ,c("Trial_begin","Response")],1,.name_repair = ~ c("onset","duration","modulator"))
          XNoRespFeedback <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==3 ,c("Outcome_T","Feedback")],1,.name_repair = ~ c("onset","duration","modulator"))
          XNoRespFixation <- tibble(NoResponseTrials[NoResponseTrials$Trial_type==3 ,c("FixTime","Fixation")],1,.name_repair = ~ c("onset","duration","modulator"))
          
          # Consolidate:
          # The only conditions we are interested in are AvoidNoLoss, Approach Reward, AmbigApproach, and 
          # AmbigAll(AmbigApproach + AmbigAvoid). Failure and no-response trials can be modeled out.
          # Analysis plan 12/2023: still splitting out the first 2s of the response period.
          # Individual regressors for the rest of the trial, the feedback, and the fixation periods.
          # For BIDS purposes, will create events for the entire response period; for our
          # analyses, will split out the first 2s and model out the rest of the response period.

          # Events of interest
          AvoidNoLossResponse$analysis_vars <- "AvoidResponse"
          AvoidNoLossFeedback$analysis_vars <- "AvoidNoLossFeedback"
          AvoidLossFeedback$analysis_vars <- "AvoidLossFeedback"
          ApproachGainResponse$analysis_vars <- "ApproachResponse"
          ApproachGainFeedback$analysis_vars <- "ApproachGainFeedback"
          AmbigApproachGainResponse$analysis_vars <- "AmbigApproachResponse"
          AmbigApproachLossResponse$analysis_vars <- "AmbigApproachResponse"
          AmbigApproachGainFeedback$analysis_vars <- "AmbigApproachGainFeedback"
          AmbigApproachLossFeedback$analysis_vars <- "AmbigApproachLossFeedback"
          AmbigAvoidNoLossResponse$analysis_vars <- "AmbigAvoidResponse"
          AmbigAvoidNoLossFeedback$analysis_vars <- "AmbigAvoidFeedback"
          XRespResponse$analysis_vars <- "XResponse"
          XRespFeedback$analysis_vars <- "XRespFeedback"
          
          # Going to lump all the fixations together as baseline.
          # However, could break them out in the future if we needed to for some reason.
          AvoidLossFixation$analysis_vars <- "Fixation"
          AvoidNoLossFixation$analysis_vars <- "Fixation"
          ApproachGainFixation$analysis_vars <- "Fixation"
          ApproachNoGainFixation$analysis_vars <- "Fixation"
          AmbigApproachGainFixation$analysis_vars <- "Fixation"
          AmbigApproachLossFixation$analysis_vars <- "Fixation"
          AmbigAvoidNoLossFixation$analysis_vars <- "Fixation"
          XRespFixation$analysis_vars <- "Fixation"
          AvoidNoRespFixation$analysis_vars <- "Fixation"
          ApproachNoRespFixation$analysis_vars <- "Fixation"
          AmbigNoRespFixation$analysis_vars <- "Fixation"
          XNoRespFixation$analysis_vars <- "Fixation"
          
          # For the 2s version:
          # Events of interest
          AvoidResponse2s <- AvoidNoLossResponse
          ApproachResponse2s <- ApproachGainResponse
          AmbigApproachResponse2s <- rbind(AmbigApproachGainResponse,AmbigApproachLossResponse)
          AmbigAvoidResponse2s <- AmbigAvoidNoLossResponse
          XResponse2s <- XRespResponse
        
          
          # Write
          for(onsetfile in c("AvoidLossResponse","AvoidLossPostResponse","AvoidLossFeedback","AvoidNoLossResponse","AvoidNoLossPostResponse",
          "AvoidNoLossFeedback","ApproachGainResponse","ApproachGainPostResponse","ApproachGainFeedback",
          "ApproachNoGainResponse","ApproachNoGainPostResponse","ApproachNoGainFeedback","AmbigApproachGainResponse",
          "AmbigApproachGainPostResponse","AmbigApproachGainFeedback","AmbigApproachLossResponse",
          "AmbigApproachLossPostResponse","AmbigApproachLossFeedback","AmbigAvoidNoLossResponse","AmbigAvoidNoLossPostResponse",
          "AmbigAvoidNoLossFeedback","XRespResponse","XRespPostResponse","XRespFeedback","AvoidNoRespResponse",
          "AvoidNoRespFeedback","ApproachNoRespResponse","ApproachNoRespFeedback","AmbigNoRespResponse","AmbigNoRespFeedback",
          "XNoRespResponse","XNoRespFeedback","AvoidNoLossFixation", "ApproachGainFixation","ApproachNoGainFixation",
          "AmbigApproachGainFixation", "AmbigApproachLossFixation", "AmbigAvoidNoLossFixation",
          "XRespFixation", "AvoidNoRespFixation", "ApproachNoRespFixation", "AmbigNoRespFixation",
          "XNoRespFixation")) {
            
            # Events of no interest
            if(onsetfile %in% c("AvoidLossResponse","AvoidLossPostResponse","AvoidNoLossPostResponse","AvoidNoLossFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "AvoidNoInterest"
              assign(onsetfile,cur_df)
            }
            
            if(onsetfile %in% c("ApproachGainPostResponse","ApproachNoGainResponse","ApproachNoGainPostResponse",
                                "ApproachNoGainFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "ApproachNoInterest"
              assign(onsetfile,cur_df)
            }
            
            if(onsetfile %in% c("AmbigApproachGainPostResponse","AmbigApproachLossPostResponse",
                                "AmbigAvoidNoLossPostResponse","AmbigAvoidNoLossFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "AmbigNoInterest"
              assign(onsetfile,cur_df)
            }
            
            if(onsetfile %in% c("XRespPostResponse")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "XNoInterest"
              assign(onsetfile,cur_df)
            }
            
            if(onsetfile %in% c("AvoidNoRespResponse","AvoidNoRespFeedback","ApproachNoRespResponse","ApproachNoRespFeedback",
                                "AmbigNoRespResponse","AmbigNoRespFeedback","XNoRespResponse","XNoRespFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "NoRespNoInterest"
              assign(onsetfile,cur_df)
            }
            
            # Lumping together gain and loss feedback
            if(onsetfile %in% c("AvoidLossFeedback","AmbigApproachLossFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "LossFeedback"
              assign(onsetfile,cur_df)
            }
            
            if(onsetfile %in% c("ApproachGainFeedback","AmbigApproachGainFeedback")){
              cur_df <- get(onsetfile)
              cur_df$analysis_vars <- "GainFeedback"
              assign(onsetfile,cur_df)
            }
            
            # add extra column for use with BIDS tsvs
            cur_df <- get(onsetfile)
            cur_df$event <- onsetfile
            assign(onsetfile,cur_df)
          }
          
          # make tsv file for BIDS format
          allevents <- Reduce(
            function(x, y) merge (x, y, all=TRUE),
            list(AvoidLossResponse,AvoidLossPostResponse,AvoidLossFeedback,AvoidNoLossResponse,AvoidNoLossPostResponse,
                 AvoidNoLossFeedback,ApproachGainResponse,ApproachGainPostResponse,ApproachGainFeedback,
                 ApproachNoGainResponse,ApproachNoGainPostResponse,ApproachNoGainFeedback,AmbigApproachGainResponse,
                 AmbigApproachGainPostResponse,AmbigApproachGainFeedback,AmbigApproachLossResponse,
                 AmbigApproachLossPostResponse,AmbigApproachLossFeedback,AmbigAvoidNoLossResponse,AmbigAvoidNoLossPostResponse,
                 AmbigAvoidNoLossFeedback,XRespResponse,XRespPostResponse,XRespFeedback,AvoidNoRespResponse,
                 AvoidNoRespFeedback,ApproachNoRespResponse,ApproachNoRespFeedback,AmbigNoRespResponse,AmbigNoRespFeedback,
                 XNoRespResponse,XNoRespFeedback,AvoidNoLossFixation, ApproachGainFixation,ApproachNoGainFixation,
                 AmbigApproachGainFixation, AmbigApproachLossFixation, AmbigAvoidNoLossFixation,
                 XRespFixation, AvoidNoRespFixation, ApproachNoRespFixation, AmbigNoRespFixation,
                 XNoRespFixation))
          
          allevents <- allevents[is.na(allevents$onset)==FALSE,c("onset","duration","event","analysis_vars","modulator")]
          allevents$onset <- as.numeric(allevents$onset)
          allevents <- allevents[order(allevents$onset),]
          
          for(avars in unique(allevents$analysis_vars)) {
            write.table(allevents[allevents$analysis_vars==avars,c("onset","duration","modulator")],
                        paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',
                              subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                              avars,'_run-',runnum,'.txt',sep=""),
                        sep=" ",row.names = FALSE,col.names = FALSE)
          }
          # For the 2s version:
          for(fullist in c("AmbigAvoidResponse","ApproachResponse","GainFeedback","AvoidResponse","LossFeedback",
                           "XResponse","XRespFeedback","AmbigApproachResponse","ApproachNoInterest","AvoidNoInterest",
                           "AmbigNoInterest","XNoInterest","NoRespNoInterest")){
            if(nrow(allevents[allevents$analysis_vars==fullist,]) == 0){
              towrite <- as.data.frame(cbind(0,0,0))
              write.table(towrite,paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',
                                           subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                        fullist,'_run-',runnum,'.txt',sep=""),
                          sep=" ",row.names = FALSE,col.names = FALSE)
              
              if(fullist %in% c("AmbigApproachResponse","AmbigAvoidResponse","ApproachResponse","AvoidResponse","XResponse")){
                write.table(towrite,paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',
                                          subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                          fullist,'2s_run-',runnum,'.txt',sep=""),
                            sep=" ",row.names = FALSE,col.names = FALSE)
                write.table(towrite,paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',
                                          subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                          fullist,'2s_RestOfTrial_run-',runnum,'.txt',sep=""),
                            sep=" ",row.names = FALSE,col.names = FALSE)
              }
              
            } else {
              if(fullist %in% c("AmbigApproachResponse","AmbigAvoidResponse","ApproachResponse","AvoidResponse","XResponse")){
                resp2s <- paste(fullist,"2s",sep="")
                cur_df2s <- get(resp2s)
                cur_dfRest <- get(resp2s)
                cur_df2s$duration <- 2
                cur_dfRest$onset <- cur_dfRest$onset + 2
                cur_dfRest$duration <- cur_dfRest$duration - 2
                write.table(cur_df2s[,c("onset","duration","modulator")],
                            paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',
                                  subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                  resp2s,'_run-',runnum,'.txt',sep=""),
                            sep=" ",row.names = FALSE,col.names = FALSE)
                write.table(cur_dfRest[,c("onset","duration","modulator")],
                            paste('/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-',
                                  subject,'/ses-',session,'/sub-',subject,'_ses-',session,'_FSLtiming-',
                                  resp2s,'_RestOfTrial_run-',runnum,'.txt',sep=""),
                            sep=" ",row.names = FALSE,col.names = FALSE)
              }
              
          }

          # BIDS events.tsv
          write.table(allevents[,c("onset","duration","event","analysis_vars")],
                      paste("/ix/cladouceur/westbrook-data/Scripts/level1/fsl_level1_timingfiles/sub-",subject,
                                             "/ses-",session,"/func/sub-",subject,"_ses-",session,"_task-avoid_run-",runnum,"_events.tsv", sep=""),sep = "\t",row.names=FALSE,quote = FALSE)
          }
          
        }
      }
    }
}

