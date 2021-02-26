#install all necessary packages
install.packages(c("compute.es","metafor","robumeta","psych", "MASS", "readxl","car","metaviz", "clubSandwich", "altmeta", "haven","rjags"))
library(rjags)
library(compute.es)
library(metafor)
library(psych)
library(MASS)
library(readxl)
library(car)
library(robumeta)
library(metaviz)
library(clubSandwich)
library(altmeta)
library(haven)

#Procedures below should be followed as it is. 
#Warning: Skipping procedures and/or disregarding the order may result in confusion and errors in the dataset and subsequent analyses.
#manually add time stamps helps.

#import raw dataset for computation of effect sizes. conducted on 5/25/2020####
setwd("C:\\Users\\huang\\Dropbox\\PHD2016to2020\\PHD2020SPRING\\Guilt_appeal_meta\\Analysis\\raw_data")#for windows
setwd("/Users/thalassa/Dropbox/PHD2016to2020/PHD2020SPRING/Guilt_appeal_meta/Analysis/raw_data")#for mac
list.files(getwd())
data0<-as.data.frame(read_xlsx("Guilt Meta_code_USE.xlsx"))
names(data0)
str(data0)
is.data.frame(data0)
data0$C_No<-as.numeric(data0$C_No)
data0$C_M<-as.numeric(data0$C_M)
data0$C_SD<-as.numeric(data0$C_SD)
data0$T_No<-as.numeric(data0$T_No)
data0$T_M<-as.numeric(data0$T_M)
data0$T_SD<-as.numeric(data0$T_SD)

#check the means and standard deviations see if there is anything odd or erroneous (e.g., negative SD or participant no, etc).####
describe(data0$C_M)
describe(data0$C_SD)
describe(data0$T_M)
describe(data0$T_SD)
describe(data0$C_No)
describe(data0$T_No)

#compute hedge's g based on cohen's d/mean/sd. ####
data0<-escalc(measure = "SMD", m1i=T_M, m2i= C_M,sd1i = T_SD ,sd2i = C_SD, n1i= T_No, n2i = C_No, data=data0, var.names = c("g","v_g"), append=TRUE, digits=4)

#save it as a separate document for next step####
write.csv(data0,file="data0_DO_NOT_USE.csv")#keep this one for the record in case you accidentally mess up the data. You can always go back to this clean one if that happens.
write.csv(data0, file="data_USE.csv")#this will be used in subsequent analyses.



#Then compute the rest effect sizes based on test statistics such as F, t, and others. This has to be manual. Enter the results into the data0. conducted on 5/26/2020.####

tes(t = 4.62, n.1 = 19, n.2 = 20, dig = 8)# t-test results
res(0.037,n=165, dig=8)#r results
fes(f = 141.74,n.1 = 1,n.2 = 474, dig=8)#f-test results
chies(3.98, n=1, dig=8)
omega

#all aforementioned procedures are are with data0.
#Then import data0 for further analyses.

#import data0. 5/26/2020.####
list.files(getwd())
data<-as.data.frame(read.csv("data_USE_controllability.csv"))
str(data)
#Should we do outlier analysis?  Yes: do outlier first then do descriptive analysis: No: proceed to descriptive analysis

#outlier analysis. conducted on 5/26/2020.####
#checking outliers####
describe(data$g)

#identify outliers with "metaoutliers" of "altmeta"
metaoutliers(data$g, data$v_g, model="RE")
#$outliers
#[1] 3  12 122 124 130 135 136 177

#locate effect sizes of outliers in the dataset
data$g[c( 3 , 12 ,122, 124, 130 ,135 ,136 ,177)]
#[1]11.899110  3.591900  5.592429  5.673932 -1.752937  5.117763  7.739827  6.581572

#identify studies with outliers
names(data)
data$Study.ID[c(3 , 12 ,122, 124, 130 ,135 ,136 ,177)]
#[1] "Agrawal_2010 Study 1"     "Allard_2015 Study 3"      "Coulter_1995"            
#[4] "Coulter_1995"             "Graton_2016 Experiment 2" "Higgs_2020"              
#[7] "Higgs_2020"               "Peloza_2013 Study 2"   

#try to drop outlier. NOTE: "data1" is WITHOUT outliers; "data" is WITH outliers.####
data1<-data[-c( 3 , 12 ,122, 124, 130 ,135 ,136 ,177),]
write.csv(data1, "data_no_outliers.csv")

+#publication bias####

#using metafor to compute under RE model

analysis<-rma(yi=g,vi=v_g,data=data1,measure="GEN",method="REML",level=95, digits=4)
summary(analysis)


#correlation test
ranktest(analysis)

#Kendall's tau = 0.0949, p = 0.0418
#There is a significant relationship between standard error and effect size, which indicated potential publication bias.

#regression test
regtest(analysis)

#test for funnel plot asymmetry: z = 3.4193, p = 0.0006
#Result from intercpt there is not enough evidence between standard error and effect size.

#moderator analysis( whether this study is published or not)
fail1<-fsn(yi=g,vi=v_g, data=data1, type="Rosenthal",alpha=.05, digits=4, target=XX)
fail1
#fail-safe number is  25522
#Observed Significance Level: <.0001
#Target Significance Level:   0.05


fail2<-fsn(yi=g,vi=v_g, data=data1, type="Orwin",alpha=.05,digits=4,target=.2)
fail2 
#fail 2 is 133
#Average Effect Size: -0.0359
#Target Effect Size:  0.2000

#trim and fill
tf<-trimfill(analysis)
tf
summary(tf)
#Estimated number of missing studies on the left side: 0 (SE = 8.1167)

#PEESE

table(data1$Study.ID)
all<-as.data.frame(table(data1$Study.ID))
all
small<-all[c(which(all$Freq==1)),]

small<-data[c(which(all$Freq==1)),]
large<-data[c(which(all$Freq>1)),]

#depedent variable
peese.rve <- robu(g ~ v_g, var.eff.size = v_g, modelweights= "CORR",studynum = Study.ID, rho = 0.8, data = large)
print(peese.rve)

#independent variable
peese.ind<-rma(yi = g, vi = v_g, data = small, method="DL", slab = Study.ID, digits = 7, mods = ~ v_g)
print(peese.ind)

#forest, funnel, etc. figures and plots####
names(data1)
#create a new dataframe for forest plot
data2<-data1
data2<-data1[order(-data1$g),]


#forest plot is OK.
viz_forest(x = data2[, c("g", "v_g")], study_labels = data1[, c("Study.ID")],
           summary_label = "Summary effect", xlab = "Hedge's g", annotate_CI = TRUE, text_size = 1)

forest.robu(intercept, es.lab = "A1_DV_Name",study.lab = "Study.ID", "Effect Size" = g, "Weights" = v_g)

#rain forest is not good.
viz_forest(x = data1[, c("g", "v_g")], study_labels = data1[, c("Study.ID")],
           summary_label = "Summary effect", xlab = "Hedge's g", variant = "rain")



names(data1)
#trim and fill funnel plot
viz_funnel(data1[, c("g" ,"v_g")])

viz_funnel(data1[, c("g", "v_g")], 
           contours_col = "Greys",
           trim_and_fill = TRUE, trim_and_fill_side = "right", xlab="ES",
           egger = TRUE)

viz_funnel(data1[, c("g", "v_g")], sig_contours = TRUE, addev_contours = TRUE)

#convert all theoretical moderators to factors and label the levels according to the codebook. use data1. conducted on 5/30/2020####
str(data1)
#funding
is.factor(data1$Funding)
data1$Funding<-as.factor(data1$Funding)
data1$Funding<-recode(data1$Funding, recodes= " 'other' = 'No Funding' ")
levels(data1$Funding)
levels(data1$Funding)<-c("No Funding","Federal/National government", "other")
data1$Funding
table(data1$Funding)

names(data1)
str(data1)
#stimuli modality
data1$Stimuli_modality
table(data1$Stimuli_modality)
data1$Stimuli_modality<-as.factor(data1$Stimuli_modality)
is.factor(data1$Stimuli_modality)
levels(data1$Stimuli_modality)<- c("Text only", "Text+Image", "Audio only", "Unspecified", "Other")
#remove other or unspecified
data1$Stimuli_modality<-recode(data1$Stimuli_modality, recodes= " NA=' unspecified' ")




#study location
data1$Study_location
data1$Study_location<-as.factor(data1$Study_location)
levels(data1$Study_location)<- c("US", "Outside US")
table(data1$Study_location)


#manipulation
data1$Manipulation
data1$Manipulation<-as.factor(data1$Manipulation)
levels(data1$Manipulation)<- c("message of any modality", "recall", "other")
table(data1$Manipulation)
table(sub_data1$Manipulation)
#remove other
data1$Manipulation<-recode(data1$Manipulation, recodes= "  NA = 'Other' ")

#narrative
data1$Narrative
data1$Narrative<-as.factor(data1$Narrative)
levels(data1$Narrative)<- c("Narrative", "Non-Narrative", "Unspecified")
table(data1$Narrative)
#remove all unspecified
data1$Narrative<-recode(data1$Narrative, recodes= "  NA = 'NA' ")

#event type
data1$Event_Type
data1$Event_Type<- as.factor(data1$Event_Type)
levels(data1$Event_Type)<- c("Education", "Advertising/Marketing","Environment","Medical/Health", "Safety Instructions", "Other")
table(data1$Event_Type)
#remove other
data1$Event_Type<-recode(data1$Event_Type, recodes= "'Other' = NA")

#reparation
data1$Reparation
table(data1$Reparation)
data1$Reparation<-as.factor(data1$Reparation)
levels(data1$Reparation)<- c("Yes", "No", "Other or NA")
#remove others
data1$Reparation<-recode(data1$Reparation, recodes= "NA = 'Unspecified'")


#arousal appraisal
data1$Arousal_Appraisal
data1$Arousal_Appraisal<-as.factor(data1$Arousal_Appraisal)
levels(data1$Arousal_Appraisal)<- c("Harm", "Responsibility", "Other")
table(data1$Arousal_Appraisal)
#remove others
data1$Arousal_Appraisal<-recode(data1$Arousal_Appraisal, recodes= "NA = 'Other'")


#locus of control. Note: locus0 is the duplicate of locus. There is only one level with this variable. Therefore this should NOT be regarded as a moderator
data1$Locus0
data1$Locus0<-as.factor(data1$Locus)
table(data1$Locus0)
is.factor(data1$Locus0)
data1$Locus0<-recode(data1$Locus0, recodes= " '3' = 'NA'; '99' = 'NA' ")
levels(data1$Locus0)
levels(data1$Locus0)<-c("Internal", "NA")

#stability. Note:
data1$Stability0<-as.factor(data1$Stability)
data1$Stability0
table(data1$Stability0)
data1$Stability0<-recode(data1$Stability0, recode = " '3' = 'NA'; NA= 'NA'")
levels(data1$Stability0)<- c("Yes", "No", "NA")
#remove other
data1$Stability0<-recode(data1$Stability0, recodes= "NA = 'NA'")

#controllability
data1$Controllability
data1$Controllability<-as.factor(data1$Controllability)
levels(data1$Controllability)<- c("Yes", "No", "NA")
#remove NA
data1$Controllability<-recode(data1$Controllability, recodes= "NA = 'NA'")
table(data1$Controllability)


#social distance
data1$Social_Distance
data1$Social_Distance<-as.factor(data1$Social_Distance)
levels(data1$Social_Distance)<-c("Personal relationship", 
                                 "Work/Professional",
                                 "Strangers",
                                 "Other")
levels(data1$Social_Distance)
table(data1$Social_Distance)
#remove other
data1$Social_Distance<-recode(data1$Social_Distance, recodes= " NA = 'Other' ")

#study design. CONTAINS ONLY ONE LEVEL: EXPERIMENT. CANNOT USE IN ANALYSIS.
data1$Study_design
data1$Study_design<-as.factor(data1$Study_design)

#measure time
data1$Measure_time
is.factor(data1$Measure_time)
data1$Measure_time<-as.factor(data1$Measure_time)
levels(data1$Measure_time)<- c("Immediately", "1-7 days", "More than 7 days")
table(data1$Measure_time)

#sample frame
data1$Sampling_frame
data1$Sampling_frame<-as.factor(data1$Sampling_frame)
levels(data1$Sampling_frame)<-c( "Online Panel", 
                                 "University Panel",
                                 "Community Sample")
table(data1$Sampling_frame)

#Type of DV
#import the column of revised dv type
list.files(getwd())
data_dvtype<-read.csv("data_DV_Type.csv")
names(data_dvtype)
str(data_dvtype)
data_dvtype[,c("A1_DV_Type", "A1_DV_Name")]

data_kk<-data_dvtype[,c("A1_DV_Type", "A1_DV_Name")]
write.csv(data_kk, "data_kk.csv")

data_dvtype<-data_dvtype[-c( 3 , 12 ,122, 124, 130 ,135 ,136 ,177),]
write.csv(data_dvtype, "dvtype.csv")
#add to the original dataset
data1$A1_DV_Type<-as.factor(data_dvtype$A1_DV_Type)
table(data1$A1_DV_Type)
levels(data1$A1_DV_Type)<- c("Guilt",
                             "Attitude", 
                             "Intention",
                             "Behavior", 
                             "Other emotion",
                             "Cognition"," Motivation","Efficacy")

#persuasion object####
table(data1$Persuasion_Object)
table(sub_data1$Persuasion_Object)
sub_data1$Study.ID




#descriptive analyses. conducted on 5/26/2020.####
describe(data1$g)
describe(data1$v_g)

#total number of study
unique(data1$Study.ID)#without outliers
unique(data$Study.ID)#with outliers

#a total of 38 studies
#convert characters to factors
data1$M_Age<-as.numeric(data1$M_Age)
data1$No_women<-as.numeric(data1$No_women)
data1$No_Black<-as.numeric(data1$No_Black)
data1$No_Hispanic<-as.numeric(data1$No_Hispanic)
data1$No_Asian<-as.numeric(data1$No_Asian)
data1$No_White<-as.numeric(data1$No_White)

#percentage of White in the sample
data1$white_percent<-(data1$No_White)/(data1$Sample_size)
data1$women_percent<-(data1$No_women)/(data1$Sample_size)

#percentage of Non-White
data1$nonwhite<- 1-(data1$white_percent)

#add year of publication into the dataset
names(data_dvtype)
data_dvtype$Pub_Year
data1$pub.year<-data_dvtype$Pub_Year
str(data1)

#convert publication year to categorical variable
data1$pub.year
data1$year_c<-recode(data1$pub.year, 
                     recodes="lo:1990='before 1990';1990.1:2000='1991-2000';
                            2000.1:2010='2001-2010';2001.1:hi = '2011- to date' ")
data1$year_c


#publication type
data1$Pub_Type<-as.factor(data1$Pub_Type)
data1$Pub_Type
levels(data1$Pub_Type)<-c ("Published", "Unpublished")


#select unique studies for descriptive analyses. 5/31/2020####
duplicated(data1$Study.ID)
which(duplicated(data1$Study.ID)== "FALSE")
length(which(duplicated(data1$Study.ID)== "FALSE"))


#extract unique studies and save as a separate dataset####
#dataset without outliers
sub_data1<-data1[c(which(duplicated(data1$Study.ID)== "FALSE")),]#sub_data1 is only used in descriptive analyses. DO NOT USE this datset in the other analyses.
#dataset with outliers
sub_data2<-data[c(which(duplicated(data$Study.ID)== "FALSE")),]#sub_data2 is only used in descriptive analysis. DO NOT USE it in the other analyses


names(sub_data1)
#number of papers left after two rounds of screening
unique(sub_data1$Article_ID)
#a total of 28 papers

#year of publication
table(sub_data1$year_c)
#   1991-2000     2001-2010 2011- to date 
#      4             9            25 



#total and average number of participants 
names(sub_data1)
str(sub_data1)

#average sample size of all 38 studies
describe(sub_data1$Sample_size)
#   vars  n   mean    sd   median trimmed   mad min  max range skew kurtosis    se
#        38 253.11 226.49    163  218.78 119.35  40 1054  1014 1.67   2.64    36.74

#sum of sample
sum(sub_data1$Sample_size)
# 9618

#average mean age of participants in all 38 studies
describe(sub_data1$M_Age)
#   vars  n  mean    sd median trimmed  mad   min max range skew kurtosis   se
#     1  30 28.74 10.01  22.81    27.3 4.89 19.34  54 34.66  0.9    -0.34 1.83

#average percentage of women in 38 studies
describe(sub_data1$women_percent)
#  vars  n mean   sd median trimmed  mad min max range  skew kurtosis   se
#       32 0.56 0.24   0.55    0.57 0.1   0   1     1 -0.6     0.87 0.04

#average percentage of White in 38 studies
describe(sub_data1$white_percent)
#vars  n mean   sd median trimmed  mad  min max range  skew kurtosis   se
#    1 12 0.71  0.1   0.73    0.73 0.1 0.48 0.8  0.32   -1      0.1 0.03
#average percentage of Non-White
describe(sub_data1$nonwhite)
#  vars  n mean  sd median trimmed mad min  max range skew kurtosis   se
#     1 12 0.29 0.1   0.27    0.27 0.1 0.2 0.52  0.32    1      0.1 0.03

#publication type
table(sub_data1$Pub_Type)
#  Published Unpublished 
#    31           7 

#study location
table(sub_data1$Study_location)
names(sub_data1)

sub_data1[c("Study_location","Study_location_3_TEXT")]

#sample frame
table(sub_data1$Sampling_frame)
#  Online Panel University Panel Community Sample 
#      10               23                5 


#average number of messages across all studies
describe(sub_data1$No_Message_1Conditio)

#MAIN ANALYSES:meta-regression using RVE: without moderators####
intercept<-  robu(formula = g  ~ 1 , data = data1, studynum = Study.ID, modelweights = "HIER", var.eff.size = v_g,  rho = .8,  small = TRUE)
print(intercept)
#g = .28


#main analysis WITH outlier
intercept_with_outliers<-robu(formula = g  ~ 1 , data = data, studynum = Study.ID, modelweights = "HIER", var.eff.size = v_g,  rho = .8,  small = TRUE)
print(intercept_with_outliers)
#g = .38. It inflated the overall effect size despite the significance.

#main analysis using metafor
rma<-rma(yi=data1$g, vi=data1$v_g, measur= "GEN",method= "REML", data=data1, level=95, digits = 4)
summary(rma)
#Q(df = 207) = 1616.6474, p-val < .0001


#meta-regression using RVE with all moderators####
names(data1)

#try
try<-robu(g ~   A1_DV_Type + Stability0 + Controllability +  Locus0 
          , data = data1, studynum = Study.ID, modelweights = "HIER", var.eff.size = v_g,  rho = .8,  small = TRUE)
print(try)

#try all moderators in one regression: too many. only use some of them
names(data1)

str(data1$Social_Distance)

#all coded moderaotrs: pub type, funding, location, modality, manipulation, narrative, event type, reparation, arousal, locus, stability, controllability, social distance, measure time, sample frame, age, women, white, dv type.
moderator_all<- robu(g ~ Funding + Stimuli_modality + Manipulation + Narrative + Event_Type
    + Stability0 + Locus0 + Social_Distance + A1_DV_Type              
, data = data1, studynum = Study.ID, modelweights = "HIER", var.eff.size = v_g,  rho = .8,  small = TRUE)
print(moderator_all)


##pairwise comparison
#sampling frame
try<- robu(g ~ Sampling_frame -1    
, data = data1, studynum = Study.ID, modelweights = "HIER", var.eff.size = v_g,  rho = .8,  small = TRUE)

sample<-rma(yi=data1$g, vi=data1$v_g, mods = ~ Sampling_frame -1, measur= "GEN",
            method= "REML", data=data1, level=95, digits = 4)
summary(sample)
anova(sample, btt=1:3)
anova(sample, L=rbind(c(0, 1, -1), c(1, 0, -1),c(1, -1, 0)))

install.packages("multcomp")
library(multcomp)


#TEST
Wald_test(moderator3, constraints = 1:3, vcov="CR2")
Wald_test(moderator3, constraints = 17:21, vcov = "CR2")
Wald_test(moderator3, constraints = 31:37, vcov = "CR2")

#dv type
dv_type<-robu(g ~ A1_DV_Type , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE, na.action = na.omit )
print(dv_type)



#percentage of women; non-white; mean age
names(data1)
str(data1)

#fulfill NAs with mean
describe(data1$M_Age)
data1$M_Age<-recode(data1$M_Age, recodes= " NA = '27.82'")
describe(data1$nonwhite)
data1$nonwhite<-recode(data1$nonwhite, recodes= " NA= 0.28")
describe(data1$women_percent)
data1$women_percent<-recode(data1$women_percent, recodes= " NA = .56")

#cont
names(data1)
cont<-robu(g ~ No_Message_1Conditio  +M_Age + women_percent + nonwhite , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(cont)

cont_rma<-rma(yi=data1$g, vi=data1$v_g,  mods = ~ M_Age + women_percent + nonwhite + No_Message_1Conditio -1, measure= "GEN", method= "REML", data=data1,  level=95, digits = 4)
summary(cont_rma)



women_rma<-rma(yi=data1$g, vi=data1$v_g, mods = ~ women_percent, measur= "GEN",
              method= "REML", data=data1, level=95, digits = 4)
summary(women_rma)

white_rma<-rma(yi=data1$g, vi=data1$v_g, mods = ~ white_percent, measure= "GEN",
                method= "REML", data=data1, level=95, digits = 4)
summary(white_rma)

age_rma<-rma(yi=data1$g, vi=data1$v_g, mods = ~ M_Age, measure= "GEN",
             method= "REML", data=data1, level=95, digits = 4)
summary(age_rma)

no<-rma(yi=data1$g, vi=data1$v_g, mods = ~ No_Message_1Conditio, measure= "GEN",
        method= "REML", data=data1, level=95, digits = 4)
summary(no)

table(data1$A1_DV_Type)
names(data1)

#measure time of dv
measure_t<-robu(g ~  Measure_time -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8, small= TRUE )
print(measure_t)
data1$Measure_time


measuret_rma<-rma(yi=data1$g, vi=data1$v_g, mods = ~ Measure_time -1, measure= "GEN",
                  method= "REML",data= data1, level=95, digits = 4)

summary(measuret_rma)
anova(measuret_rma, L=rbind(c(0, 1, -1), c(1, 0, -1),c(1, -1, 0)))

#dv type
dv<-robu(g ~ A1_DV_Type -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8, small= TRUE )
print(dv)

data1$A1_DV_Type
dvtype_rma<-rma(yi=data1$g, vi=data1$v_g, mods = ~ A1_DV_Type -1 , measure= "GEN",
                method= "REML",data= data1, level=95, digits = 4)
summary(dvtype_rma)
dvtype_rma
#only guilt, attitude, behavioral intention and behavior are significant
anova(dvtype_rma, L=rbind(c(1, -1, 0, 0, 0,0, 0, 0),c(1,0, -1, 0, 0, 0,0, 0),c(1, 0, 0, -1,0, 0,0, 0),c(0, 1, -1, 0, 0,0, 0, 0), c(0, 1, 0, -1,0, 0, 0, 0), c(0, 0, 1, -1,0, 0,0, 0), c(0,0, -1, 1,0, 0,0, 0), c(0, -1, 1, 0,0, 0,0, 0), c(0, -1, 0, 1,0, 0,0, 0)))

table(data1$A1_DV_Type)


#event type
is.factor(data1$Event_Type)
levels(data1$Event_Type)
table(sub_data1$Event_Type)
event<-robu(g ~ Event_Type -1, data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(event)
Wald_test(event, constraints = 1:5, vcov = "CR2")

eventtype<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Event_Type -1, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(eventtype)

anova(eventtype, L=rbind(c(1, 0,-1, 0, 0),c(1,0, 0, 0, -1),c(0, 0, 1,0, -1)))

#fund type
fund<-robu(g ~ Funding -1, data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(fund)


fund_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Funding, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(fund_rma)

table(sub_data1$Funding)

#study location

location<-robu(g ~ Study_location -1, data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(location)


location_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Study_location, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(location_rma)

table(sub_data1$Study_location)
sub_data1$Study_location

#message modality
names(data1)


modality<-robu(g ~ Stimuli_modality -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(modality)


modality_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Stimuli_modality, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(modality_rma)
anova(modality_rma, L=rbind(c(0, 1,-1, 0),c(0,1,0, -1),c(0, 0, 1, -1)))



table(sub_data1$Stimuli_modality)

#reparation
rep<-robu(g ~ Reparation -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(rep)


rep_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Reparation -1, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(rep_rma)
anova(rep_rma, L=c(-1, 0, 1))


table(sub_data1$Reparation)

#arousal appraisal
names(data1)
app<-robu(g ~ Arousal_Appraisal -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(app)
table(sub_data1$Arousal_Appraisal)

app_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Arousal_Appraisal -1, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(app_rma)
anova(app_rma, L=c(-1, 0, 1))

#locus of control: PENDING####

names(data1)
locus<-robu(g ~Locus0 -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(locus)
table(sub_data1$Locus0)

locus_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Locus0, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(locus_rma)
anova(app_rma, L=c(1, 0, -1))


#stability of the event
names(data1)
stab<-robu(g ~ Stability0 -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(stab)
table(sub_data1$Stability0)

stab_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Stability0, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(stab_rma)
anova(stab_rma, L=c(-1, 0, 1))

#controllability

names(data1)
control<-robu(g ~ Controllability -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(control)
table(sub_data1$Controllability)

control_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Controllability, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(control_rma)
anova(control_rma, L=c(0, -1, 1))


#social distance

names(data1)
sd<-robu(g ~ Social_Distance -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(sd)
table(sub_data1$Social_Distance)

sd_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Social_Distance, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(sd_rma)
anova(sd_rma, L=c(0, -1, 1))


#manipulation

names(data1)
mani<-robu(g ~ Manipulation -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(mani)
table(sub_data1$Manipulation)

mani_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Manipulation, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(mani_rma)
anova(mani_rma, L=c(-1, 0, 1))


#narrative 
names(data1)
nar<-robu(g ~ Narrative -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(nar)
table(sub_data1$Narrative)

nar_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ Narrative, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(nar_rma)
anova(nar_rma, L=rbind(c(0, -1, 1), c(1, 0, -1), c(1, -1, 0)))

#publication type
pub<-robu(g ~ Pub_Type -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8,small= TRUE )
print(pub)

year_pub_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ year_c, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(pub_rma)

#publication year
names(data1)
years<-robu(g ~ year_c -1 , data = data1, studynum =  Study.ID, modelweights = "HIER", var.eff.size = v_g, rho = .8, small= TRUE )

print(years)

year_rma<-rma(yi=data1$g, vi=data1$v_g, mods= ~ year_c, measure ="GEN", method="REML", data= data1, level=95, digits=4)
summary(year_rma)



#NOTES: 6/4/2020
#1. for some moderators, unspecified needs to be kept.



#sub-analysis by DV type: NOT NECESSARY. DO NOT USE####

#guilt
guilt<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Guilt"),], measure="GEN",method="REML",level=95, digits=4)
summary(guilt)

#attitude
levels(data1$A1_DV_Type)
att<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Attitude"),], measure="GEN",method="REML",level=95, digits=4)
summary(att)

#intent
intention<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Intention"),], measure="GEN",method="REML",level=95, digits=4)
summary(intention)

#actual behavior. marginally significant. negative relationship: fear appeal used, less behaviors.
behavior<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Behavior"),], measure="GEN",method="REML",level=95, digits=4)
summary(behavior)

#Other emotion except guilt
other_emo<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Other emotion"),], measure="GEN",method="REML",level=95, digits=4)
summary(other_emo)

#cognition
cog<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Cognition"),], measure="GEN",method="REML",level=95, digits=4)
summary(cog)

table(data1$A1_DV_Type)
#Motivation
mot<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Motivation"),], measure="GEN",method="REML",level=95, digits=4)
summary(mot)

#efficacy
eff<-rma(yi=g,vi=v_g,data=data1[c(data1$A1_DV_Type=="Efficacy"),], measure="GEN",method="REML",level=95, digits=4)
summary(eff)





#Findings and notes: To date: 5/31/2020####
#1. the overall effect size is significant, a small to medium effect. Next step is to identify significant moderators is critical as we will be able to tell under what condition guilt appeal is a little more effective than nothing;
#2 should we keep outliers or drop them? Either way does not change the significance of the overall effects. I'd say drop them as they are extremely big. In case where reviewers ask about how we handle the outliers, if they insist deleting them, we may have to rerun the whole analysis otherwise.
#3. Only few moderators vary the the effect sizes. One option is selecting only some dependent variables. SO we have to decide which moderators to choose for our meta-regression model. For now effective moderators are: event type, funding, measure type, dv type, 
#INeffective: reparation, apprasial, social distance, sampling frame,
#4. Overall effect size of the meta analysis conducted by Xu and Guo (2018) was incredibly large, r=.49, stems from the small sample, only 8 studies. This effect size might be inaccurate because it draws from a very limited number of studies. This should be included in the literature saying why we need to have a new meta-analysis.
#5. based on sub-analyses by DV type, guilt appeal was significant in the context of environment protection whatsoever.
#6. I have saved this .r file in the Google drive. Everyone can try out. Just pay attention to the protocols.
#7. In many moderators we have levels like "other" or "unspecified." I worry they will be criticized by reviewers later. We have to justify our choices on those levels. Remove those with "others" or "unspecified."
# mutually exclusive. It coule be a potential issue.
# message feature?
#report both datasets (with or without outliers).






