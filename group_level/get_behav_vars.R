# This is a script designed to output the list of behavioral covariates for copying/pasting
# into FSL's FEAT.

library(haven)
library(tidyverse)
library(readxl)
library(reshape2)
setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/')
d <- read_xlsx('/ix/cladouceur/westbrook-data/Scripts/group_level/allQC.xlsx')
clinical_vars <- d[,c("ID_Num","dispo","Therapy","Gender","Race","AGE_Pre","AGE_Post","AGE_1yr",
                      "Completed_TX","Number_TX","Remission21","Relapse","Response","PARS_01_Total_Score")]

# to get the right subjects by timepoint (pretreatment, posttreatment, or prepost) and cope

setwd('/ix/cladouceur/westbrook-data/Scripts/level2/make_fsfs/Feb2024_newcontrast/')
for(cope in c("cope1","cope2","cope3","cope4","cope5","cope6","cope7","cope8","cope9","cope10","cope11","cope12","cope13","cope14","cope15")) {
  for(session in c("pre","post")){

    try1 <- try(read.table(paste0(cope,session,"run1run2.txt")))
    try2 <- try(read.table(paste0(cope,session,"run1run1.txt")))
    try3 <- try(read.table(paste0(cope,session,"run2run2.txt")))
    if (!inherits(try1, 'try-error')) c1 <- try1 else c1 <- NULL
    if (!inherits(try2, 'try-error')) c2 <- try2 else c2 <- NULL
    if (!inherits(try3, 'try-error')) c3 <- try3 else c3 <- NULL
    towrite <- rbind(c1,c2,c3)
    #write.table(towrite,paste0('/ix/cladouceur/westbrook-data/Scripts/group_level/subjects_by_cope/',cope,session,"treatment.txt"),row.names = FALSE, col.names = FALSE)
    towrite$sub <- 1
    colnames(towrite) <- c("ID_Num",paste0(cope,session,"_subs"))
    assign(paste0(cope,session),towrite)
  }
}

df_list <- list(clinical_vars,cope1pre,cope1post,cope2pre,cope2post,cope3pre,cope3post,cope4pre,cope4post,
                cope5pre,cope5post,cope6pre,cope6post,cope7pre,cope7post,cope8pre,cope8post,cope9pre,cope9post,
                cope10pre,cope10post,cope11pre,cope11post,cope12pre,cope12post,cope13pre,cope13post,cope14pre,
                cope14post,cope15pre,cope15post)

clinical_vars <- df_list %>% reduce(full_join, by='ID_Num')

### THERAPY GROUPS ###
clinical_vars$therapy_numeric <- ifelse(clinical_vars$Therapy=="CBT",1,
                                        ifelse(clinical_vars$Therapy=="CCT",2,
                                               ifelse(clinical_vars$Therapy=="Control",0,NA)))
clinical_vars$CBT <- ifelse(clinical_vars$Therapy=="CBT",1,0)
clinical_vars$CCT <- ifelse(clinical_vars$Therapy=="CCT",1,0)
clinical_vars$Control <- ifelse(clinical_vars$Therapy=="Control",1,0)
clinical_vars$Anx <- ifelse(clinical_vars$Therapy %in% c("CBT","CCT"),1,
                            ifelse(clinical_vars$Therapy=="Control",0,NA))

### Set up some variables that we will use later ###

###### Imputed data ######
# No longer using this, but I did use it to define the Response variable

setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/behav_analyses/')
imputed <- read_spss('Derived1_5.sav')
imputed <- imputed[order(imputed$ID_Num),c("ID_Num","Imputation_","Remission21_Imp","Remission36_Imp","PARS_21_35erImp","PARS_36_35erImp")]
imputed$PARS_21_35erImp <- as.numeric(imputed$PARS_21_35erImp)
imputed$PARS_36_35erImp <- as.numeric(imputed$PARS_36_35erImp)
imputed$Remission21_Imp <- as.numeric(imputed$Remission21_Imp)
imputed$Remission36_Imp <- as.numeric(imputed$Remission36_Imp)

for (varname in variable.names(imputed[,3:6])) {
  cur_var <- dcast(imputed,ID_Num~Imputation_,value.var=varname)
  colnames(cur_var) <- c("ID_Num",paste0(varname,"_0"),paste0(varname,"_1"),paste0(varname,"_2"),paste0(varname,"_3"),
                         paste0(varname,"_4"),paste0(varname,"_5"))
  assign(paste0("test_",varname),cur_var)
}

df_list <- list(test_PARS_21_35erImp,test_PARS_36_35erImp,test_Remission21_Imp,test_Remission36_Imp)
imputed_vars <- df_list %>% reduce(full_join, by='ID_Num')
clinical_vars <- left_join(clinical_vars,imputed_vars,by="ID_Num")

clinical_vars$Anx_Inv <- ifelse(clinical_vars$Anx==1,0,
                                ifelse(clinical_vars$Anx==0,1,NA))
clinical_vars$Remission21 <- as.numeric(clinical_vars$Remission21)
clinical_vars$Remis_Inv <- ifelse(clinical_vars$Remission21==1,0,
                                  ifelse(clinical_vars$Remission21==0,1,NA))

# For the interaction terms
clinical_vars$Anx_dummy <- ifelse(clinical_vars$Anx==1,1,
                                  ifelse(clinical_vars$Anx==0,-1,NA))
clinical_vars$Resp_dummy <- ifelse(clinical_vars$PARS_21_35erImp_0==1,1,
                                   ifelse(clinical_vars$PARS_21_35erImp_0==0,-1,NA))
clinical_vars$Therapy_dummy <- ifelse(clinical_vars$Therapy=="CBT",1,
                                      ifelse(clinical_vars$Therapy=="CCT",-1,NA))

clinical_vars$RespImp1_dummy <- ifelse(clinical_vars$PARS_21_35erImp_1==1,1,
                                       ifelse(clinical_vars$PARS_21_35erImp_1==0,-1,NA))
clinical_vars$forAnova_RespImp1 <- clinical_vars$PARS_21_35erImp_1
clinical_vars[which(is.na(clinical_vars$forAnova_RespImp1)),"forAnova_RespImp1"] <- 0
clinical_vars$forAnova_RespImp1_Inv <- ifelse(clinical_vars$PARS_21_35erImp_1==1,0,1)
clinical_vars[which(is.na(clinical_vars$forAnova_RespImp1_Inv)),"forAnova_RespImp1_Inv"] <- 0

clinical_vars$Gender <- as.numeric(clinical_vars$Gender)
clinical_vars$AGE_Pre <- as.numeric(clinical_vars$AGE_Pre)
clinical_vars$PARS_01_Total_Score <- as.numeric(clinical_vars$PARS_01_Total_Score)

# Money vs. points
clinical_vars$Money <- ifelse(clinical_vars$ID_Num<2221,"Points","Money")
clinical_vars$Money_Numeric <- ifelse(clinical_vars$Money == "Money",1,
                                      ifelse(clinical_vars$Money=="Points",0,NA))
clinical_vars$Money_Inv <- ifelse(clinical_vars$Money_Numeric==1,0,
                                  ifelse(clinical_vars$Money_Numeric==0,1,NA))

# depression data
setwd('/ix/cladouceur/westbrook-data/files/')
CATS <- read_spss("CATS_DataSet_All.sav")
clinical_vars <- merge(clinical_vars,CATS[,c("ID_Num","MFQ_C_1_Total_Score")],by="ID_Num",all.x=TRUE)

setwd('/ix/cladouceur/westbrook-data/Scripts/level2/make_fsfs/Feb2024_newcontrast/')

################################### GENERATE BEHAV VARS FOR FEAT BELOW ############################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################

# For ICCs - print out a list of only control subjects
#controls <- clinical_vars[clinical_vars$Therapy=="Control","ID_Num"]
#write.table(controls$ID_Num,"Controls.txt",row.names = FALSE, col.names = FALSE)

#setwd('/ix/cladouceur/westbrook-data/Scripts/group_level/subjects_by_cope/')

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
### PRETREATMENT ###

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 9: ApproachAll - Control
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Anx vs NPD adjusted for age & gender
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Interaction (CBT vs. CCT) X Response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender","Therapy_dummy","Resp_dummy")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- loop[which(loop$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- loop[which(loop$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 14: GainFeedback - XRespFeedback (Control feedback)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)

for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(1,"\t",loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\n"
  )
}

# Anx vs NPD adjusted for age & gender
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Interaction (CBT vs. CCT) X Response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender","Therapy_dummy","Resp_dummy")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- loop[which(loop$ID_Num==subject),"Therapy_dummy"]$Therapy_dummy
  var2 <- loop[which(loop$ID_Num==subject),"Resp_dummy"]$Resp_dummy
  var3 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var4 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",(var1*var2),"\t",1,"\t",var3,"\t",var4,"\n")
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
### PREPOST ###

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 9: ApproachAll - Control
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Main Effect
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(1,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Anx vs NPD adjusted for age & gender
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),
                      c("ID_Num","AGE_Pre","Gender", "PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Responders ONLY vs. controls (adj for age and gender)
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$Anx) & 
                        clinical_vars$PARS_21_35erImp_0 %in% c(1,NA),c("ID_Num","AGE_Pre","Gender","PARS_21_35erImp_0","Anx")]
loop$exclude <- ifelse(loop$Anx==1 & is.na(loop$PARS_21_35erImp_0),1,0)
loop <- loop[loop$exclude==0,]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs),
                      c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Control"]$Control
  var4 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var5 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\t",var5,"\n")
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 14: GainFeedback - XRespFeedback
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Main effect
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(1,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Anx vs NPD adjusted for age & gender
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),
                      c("ID_Num","AGE_Pre","Gender", "PARS_21_35erImp_0","PARS_01_Total_Score")]
nrow(loop)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n")
}

# Responders ONLY vs. controls (adj for age and gender)
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$Anx) & 
                        clinical_vars$PARS_21_35erImp_0 %in% c(1,NA),c("ID_Num","AGE_Pre","Gender","PARS_21_35erImp_0","Anx")]
loop$exclude <- ifelse(loop$Anx==1 & is.na(loop$PARS_21_35erImp_0),1,0)
loop <- loop[loop$exclude==0,]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"]$Anx,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"]$Anx_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Therapy type
# CBT vs CCT
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs),
                      c("ID_Num","AGE_Pre","Gender")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  var1 <- clinical_vars[which(clinical_vars$ID_Num==subject),"CBT"]$CBT
  var2 <- clinical_vars[which(clinical_vars$ID_Num==subject),"CCT"]$CCT
  var3 <- clinical_vars[which(clinical_vars$ID_Num==subject),"Control"]$Control
  var4 <- loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre
  var5 <- loop[which(loop$ID_Num==subject),"Gender"]$Gender
  cat(var1,"\t",var2,"\t",var3,"\t",var4,"\t",var5,"\n")
}
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

### SENSITIVITY ###
### MONEY VS POINTS ###

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 9: Approach All - Control
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Pretreatment - Money vs. points feedback
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender","Money_Numeric")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Numeric"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\n"
  )
}

# Prepost changes predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs),
                      c("ID_Num","AGE_Pre","Gender","Money_Numeric")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Numeric"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\n"
  )
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 14: Gain - Control Feedback
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Pretreatment - Money vs. points feedback
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender","Money_Numeric")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Numeric"]$Money_Numeric,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Inv"]$Money_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\n"
  )
}

# Prepost changes predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs),
                      c("ID_Num","AGE_Pre","Gender","Money_Numeric")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Numeric"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Money_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\n"
  )
}

### Adjusting for baseline PARS ###

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 9: Approach All - Control 
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

# Baseline predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"],"\n")
}

# Prepost changes predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) & 
                        !is.na(clinical_vars$PARS_01_Total_Score),
                      c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"],"\n")
}

# COPE 14
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 14: GainFeedback - XRespFeedback (Control feedback)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

# Baseline predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0),c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"]$PARS_21_35erImp_0,"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"]$forAnova_RespImp1_Inv,"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"]$AGE_Pre,"\t",
      loop[which(loop$ID_Num==subject),"Gender"]$Gender,"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"]$PARS_01_Total_Score,"\n")
}

# Prepost changes predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) & 
                        !is.na(clinical_vars$PARS_01_Total_Score),
                      c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$PARS_01_Total_Score <- loop$PARS_01_Total_Score - mean(loop$PARS_01_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"PARS_01_Total_Score"],"\n")
}

### Adjusting for baseline MFQ ###

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 9: Approach All - Control
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Anx vs NPD adjusted for age & gender
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$MFQ_C_1_Total_Score) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender","MFQ_C_1_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$MFQ_C_1_Total_Score <- loop$MFQ_C_1_Total_Score - mean(loop$MFQ_C_1_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"MFQ_C_1_Total_Score"],"\n"
  )
}

# Baseline predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) & 
                        !is.na(clinical_vars$MFQ_C_1_Total_Score),
                      c("ID_Num","AGE_Pre","Gender","MFQ_C_1_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$MFQ_C_1_Total_Score <- loop$MFQ_C_1_Total_Score - mean(loop$MFQ_C_1_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope9_nofixation.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"MFQ_C_1_Total_Score"],"\n")
}

# Prepost changes predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope9pre_subs) &
                        !is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) & 
                        !is.na(clinical_vars$MFQ_C_1_Total_Score),
                      c("ID_Num","AGE_Pre","Gender","MFQ_C_1_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$MFQ_C_1_Total_Score <- loop$MFQ_C_1_Total_Score - mean(loop$MFQ_C_1_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope9_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"MFQ_C_1_Total_Score"],"\n")
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 14: GainFeedback - XRespFeedback (Control feedback)
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# Anx vs NPD adjusted for age & gender
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$MFQ_C_1_Total_Score) &
                        !is.na(clinical_vars$Anx),c("ID_Num","AGE_Pre","Gender","MFQ_C_1_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$MFQ_C_1_Total_Score <- loop$MFQ_C_1_Total_Score - mean(loop$MFQ_C_1_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"MFQ_C_1_Total_Score"],"\n"
  )
}


# Baseline predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) &
                        !is.na(clinical_vars$MFQ_C_1_Total_Score),
                      c("ID_Num","AGE_Pre","Gender","PARS_01_Total_Score","MFQ_C_1_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$MFQ_C_1_Total_Score <- loop$MFQ_C_1_Total_Score - mean(loop$MFQ_C_1_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-pretreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"MFQ_C_1_Total_Score"],"\n")
}

# Prepost changes predicting treatment response
loop <- clinical_vars[!is.na(clinical_vars$cope14pre_subs) &
                        !is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$PARS_21_35erImp_0) & 
                        !is.na(clinical_vars$MFQ_C_1_Total_Score),
                      c("ID_Num","AGE_Pre","Gender","MFQ_C_1_Total_Score")]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
loop$MFQ_C_1_Total_Score <- loop$MFQ_C_1_Total_Score - mean(loop$MFQ_C_1_Total_Score)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/prepost/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}
for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"PARS_21_35erImp_0"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"forAnova_RespImp1_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\t",
      loop[which(loop$ID_Num==subject),"MFQ_C_1_Total_Score"],"\n")
}


#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
### POSTTREATMENT ###

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 9: ApproachAll - Control
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

# Responders ONLY vs. controls (adj for age and gender)
loop <- clinical_vars[!is.na(clinical_vars$cope9post_subs) &
                        !is.na(clinical_vars$Anx) & 
                        clinical_vars$PARS_21_35erImp_0 %in% c(1,NA),c("ID_Num","AGE_Pre","Gender","PARS_21_35erImp_0","Anx")]
loop$exclude <- ifelse(loop$Anx==1 & is.na(loop$PARS_21_35erImp_0),1,0)
loop <- loop[loop$exclude==0,]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope9_nofixation.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\n"
  )
}

#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
# COPE 14: Gain - XResp Feedback
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------

# Responders ONLY vs. controls (adj for age and gender)
loop <- clinical_vars[!is.na(clinical_vars$cope14post_subs) &
                        !is.na(clinical_vars$Anx) & 
                        clinical_vars$PARS_21_35erImp_0 %in% c(1,NA),c("ID_Num","AGE_Pre","Gender","PARS_21_35erImp_0","Anx")]
loop$exclude <- ifelse(loop$Anx==1 & is.na(loop$PARS_21_35erImp_0),1,0)
loop <- loop[loop$exclude==0,]
nrow(loop)
loop$AGE_Pre <- loop$AGE_Pre - mean(loop$AGE_Pre)
loop$Gender <- loop$Gender - mean(loop$Gender)
for(subject in loop$ID_Num){
  cat(paste0("/ix/cladouceur/westbrook-data/processed/sub-",subject,"/ses-posttreatment/level2/cope14_nofixation_newcontrast.gfeat/cope1.feat\n"))
}

for(subject in loop$ID_Num){
  cat(clinical_vars[which(clinical_vars$ID_Num==subject),"Anx"],"\t",
      clinical_vars[which(clinical_vars$ID_Num==subject),"Anx_Inv"],"\t",
      loop[which(loop$ID_Num==subject),"AGE_Pre"],"\t",
      loop[which(loop$ID_Num==subject),"Gender"],"\n"
  )
}
