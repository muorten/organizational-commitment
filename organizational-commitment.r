#Script Name: Read and prepare survey data
#Author: Martin Wallebohr
#Description: Reads google form data out of a google spreadsheet within in R environemnt and performs statistical analysis on organizational commitment
#Script uses Jenny's googlesheets github code --> https://github.com/jennybc/googlesheets
#License: Do what ever you want license.
#-------------------------------------------------------------------------------------------

#install necessary packages manually
#install.packages("devtools")
#devtools::installgithub("jennybc/googlesheets)
#install.packages("Hmisc")
#install.packages("corrgram")

#for later output to LATEX use stargazer
#install.packages("stargazer")
#library(stargazer)
#stargazer(ac_model,cc_model,nc_model)
#stargazer(ac_model,cc_model,nc_model,  title="Multiple Regression Results", no.space=TRUE, model.numbers=FALSE, column.labels=c("Affective Commitment", "Continuance Commitment", "Normative Commitment"))
#stargazer(cor_model,  title="Intercorrelation Matrix")

#Load libraries
library("googlesheets") > suppressPackageStartupMessages(library("dplyr"))
library(Hmisc)
library(corrgram)

#clear workspace
rm(list=ls())

#read out and prompts your google drive files, on first connect url token request is initialized
glimpse(gs_ls())

#Register original Form and response sheet document (here: Survey) and assign it to variable org_survey
#org_survey <- gs_title("Survey Responses")

#Create copy of form and response sheet in order to protect data of the original form
#gs_copy(org_survey,to ="Copy of Survey Responses")

#Register "copy of Survey Data" document. The name of the document / title should of course be available in your google drive account.
copy_survey <- gs_title("Copy of Survey Responses")

#print some glimpse data of the survey document
#glimpse(gs_read(copy_survey))

#create new spreadsheet where processed form responses are put in and later on multi regression analyses can be performed in
#gs_new("Multi-Regression-Data")

#copy survey data into R object
temp = copy_survey %>% gs_read()

#RANK-----------------------------------------------------------------------------------------------------------
#create single data frame containing string rank values...employee...supervisor...department leader
rank = temp[,2]
names(rank)[1]="rank"

#replaces string hierachy values with scale integer values
rank = replace(rank, rank =="student-intern",1)
rank = replace(rank, rank =="ordinary employee",2)
rank = replace(rank, rank =="team leader",3)
rank = replace(rank, rank =="group leader",4)
rank = replace(rank, rank =="department leader",5)

#make rank vector numeric
rank = as.numeric(rank$rank)

#WORKPLACE-------------------------------------------------------------------------------------------------------
#create vector for workplace (external-internal)
wp = temp[,4]
names(wp)[1]="wp"

#replaces string for workplace (JA-NEIN) with scale integer values
wp = replace(wp, wp=="YES",1)
wp = replace(wp, wp=="NO",0)

wp = as.numeric(wp$wp)

#SPECIALIST ROLE-------------------------------------------------------------------------------------------------
#create vector for specialist role (external-internal)
sr= temp[,3]
names(sr)[1]="sr"

#replaces string for specialist role (JA-NEIN) with scale integer values
sr = replace(sr, sr=="YES",1)
sr = replace(sr, sr=="NO",0)

sr = as.numeric(sr$sr)

#Affective Commitment (AC)---------------------------------------------------------------------------------------
#Here the commitment values for AC are gathered and a mean value is computed
ac = temp[,5:8]
ac = rowMeans(ac,na.rm = FALSE, dims = 1)

#plot ac distribution - x-axis:level of commitment y-axis: count of answers for ac
#mean values are rounded
ac_round = round(ac)
#count number of rounded mean values
ac_round_vector = c(sum(ac_round==1),sum(ac_round==2),sum(ac_round==3),sum(ac_round==4), sum(ac_round==5))
#plot
png(filename = "AC count mean.png", width=1920, height=1080, units="px", pointsize=30)
#plot using http://www.harding.edu/fmccown/r/
barplot(ac_round_vector, col=rainbow(5), ylim=c(0,100), xpd = FALSE, main="Distribution for Affective Commitment", xlab="Level of Affective Commitment", ylab="Count", names.arg=c("1", "2", "3","4","5"))
text(c(0.7,1.9,3.1,4.3,5.5),2, round(ac_round_vector,2), offset = 1.0)
dev.off()

#Continuance Commitment (CC)--------------------------------------------------------------------------------------
#Here the commitment values for CC are gathered and a mean value is computed
cc = temp[,9:11]
cc = rowMeans(cc,na.rm = FALSE, dims = 1)

#plot cc distribution - x-axis:level of commitment y-axis: count of answers for ac
#mean values are rounded
cc_round = round(cc)
#count number of rounded mean values
cc_round_vector = c(sum(cc_round==1),sum(cc_round==2),sum(cc_round==3),sum(cc_round==4), sum(cc_round==5))
#plot
png(filename = "CC count mean.png", width=1920, height=1080, units="px", pointsize=30)
#plot using http://www.harding.edu/fmccown/r/
barplot(cc_round_vector, col=rainbow(5), ylim=c(0,100), xpd = FALSE, main="Distribution for Continuance Commitment", xlab="Level of Continuance Commitment", ylab="Count", names.arg=c("1", "2", "3","4","5"))
text(c(0.7,1.9,3.1,4.3,5.5),2, round(cc_round_vector,2), offset = 1.0)
dev.off()

#Normative Commitment (NC)---------------------------------------------------------------------------------------
#Here the commitment values for NC are gathered and a mean value is computed
nc = temp[,12:14]
nc = rowMeans(nc,na.rm = FALSE, dims = 1)

#plot cc distribution - x-axis:level of commitment y-axis: count of answers for ac
#mean values are rounded
nc_round = round(nc)
#count number of rounded mean values
nc_round_vector = c(sum(nc_round==1),sum(nc_round==2),sum(nc_round==3),sum(nc_round==4), sum(nc_round==5))
#plot
png(filename = "NC count mean.png", width=1920, height=1080, units="px", pointsize=30)
#plot using http://www.harding.edu/fmccown/r/
barplot(nc_round_vector, col=rainbow(5), ylim=c(0,100), xpd = FALSE, main="Distribution for Normative Commitment", xlab="Level of Normative Commitment", ylab="Count", names.arg=c("1", "2", "3","4","5"))
text(c(0.7,1.9,3.1,4.3,5.5),2, round(nc_round_vector,2), offset = 1.0)
dev.off()

#Supervisor Support (SS)-----------------------------------------------------------------------------------------
#Here the values for SS is gathered and mean value is computed
ss = temp[,15:18]
ss = rowMeans(ss,na.rm = FALSE, dims = 1)

#Colleague Support (CS)------------------------------------------------------------------------------------------
#Here the values for CS is gathered and mean value is computed
cs = temp[,19:20]
cs = rowMeans(cs,na.rm = FALSE, dims = 1)

#Job Characteristics (JC) - Skills and Authonomy ----------------------------------------------------------------
#Here the values for jc (Skills) is gathered and mean value is computed
jc = temp[,21:22]
jc = rowMeans(jc,na.rm = FALSE, dims = 1)

#Create data panel of all necessary variables
mr_data = data.frame(ac,cc,nc,rank,wp,sr,ss,cs,jc)
mr_data = data.matrix(mr_data, rownames.force = NA)
#Compute and plot mean variables for all constellations

# select all persons and compute mean values
ac_mean = mean(subset(mr_data, select = ac))
cc_mean = mean(subset(mr_data, select = cc))
nc_mean = mean(subset(mr_data, select = nc))
ss_mean = mean(ss)
cs_mean = mean(cs)
jc_mean = mean(jc)


# Plot all mean values of all variables
png(filename = "Mean values organizational commitment.png", width=1920, height=1080, units="px", pointsize=30)
#plot using http://www.harding.edu/fmccown/r/
barplot(c(ac_mean,cc_mean,nc_mean), col=rainbow(3), ylim=c(1,5), xpd = FALSE, main="Mean Values of organizational commitment", ylab="Commitment Level", names.arg=c("AC", "CC", "NC"))
text(c(0.7,1.9,3.1),2, round(c(ac_mean,cc_mean,nc_mean),2), offset = 1.0)
dev.off()

#select certain employees and compute single mean values from the commitment variables in order to compare them
# select persons who work external (OEM office) and compute mean values
ac_mean_ext = mean(subset(mr_data,wp==1,select = ac))
cc_mean_ext = mean(subset(mr_data,wp==1,select = cc))
nc_mean_ext = mean(subset(mr_data,wp==1,select = nc))

# select persons who work internal (xy office) and compute mean values
ac_mean_int = mean(subset(mr_data,wp==0,select = ac))
cc_mean_int = mean(subset(mr_data,wp==0,select = cc))
nc_mean_int = mean(subset(mr_data,wp==0,select = nc))

# select persons who have specialist role and compute mean values
ac_mean_sr = mean(subset(mr_data,sr==1,select = ac))
cc_mean_sr = mean(subset(mr_data,sr==1,select = cc))
nc_mean_sr = mean(subset(mr_data,sr==1,select = nc))

# select persons who have NO specialist role and compute mean values
ac_mean_nsr = mean(subset(mr_data,sr==0,select = ac))
cc_mean_nsr = mean(subset(mr_data,sr==0,select = cc))
nc_mean_nsr = mean(subset(mr_data,sr==0,select = nc))

#Plot distribution of organizational commitment with all constellations
png(filename = "OC Distribution.png", width=1920, height=1080, units="px", pointsize=30)
#plot using http://www.harding.edu/fmccown/r/
barplot(matrix(c(ac_mean, ac_mean_sr,ac_mean_nsr, ac_mean_ext, ac_mean_int, cc_mean, cc_mean_sr, cc_mean_nsr,cc_mean_ext, cc_mean_int, nc_mean, nc_mean_sr,nc_mean_nsr, nc_mean_ext, nc_mean_int), nrow=5,  ncol=3), main="Organizational Commitment Distribution", ylab= "Commitment Level",
        beside=TRUE, col=rainbow(5), ylim=c(1,5), xpd = FALSE, names.arg=c("AC", "CC", "NC"))
legend("topleft", c("All employees", "Specalist Role","Non-Specialist Role","OEM Office","xy Office"),  
       bty="n", fill=rainbow(5));
dev.off()

#Multiple Regression Analysis + Correlation Analysis------------------------------------------------------------------------------

#Compute Multi-Regression Model for Affective Commitment and possible explanatories of it (all employees)

ac_model = lm(ac ~ ss + cs + jc + rank)
cc_model = lm(cc ~ ss + cs + jc + rank)
nc_model = lm(nc ~ ss + cs + jc + rank)
cor_model = cor(mr_data, method="pearson",use="complete")

#show and print the summary of the model in a txt file
sink("Script Output.txt",append = FALSE, split = TRUE)

cat("Intercorrelation matrix of all variables:\n")
print(cor(mr_data, method="pearson"))
print(summary(ac_model))
print(summary(cc_model))
print(summary(nc_model))

#Konfident Interval
cat("With a confident level of 95% the input variables of the models are in a range of:\n")
print(confint(ac_model,conf.level=0.95))

#stop printing in txt file
sink()

#Plot error values of Multi Regression Model
png(filename = "Multi-Regression-Model.png", width=1920, height=1080, units="px", pointsize=30)
layout(matrix(1:4,2,2))
plot(ac_model)
dev.off()

#Plot correlation diagramms with color illustration
png(filename = "Intercorrelation Matrix-color-ellipse.png", width=1920, height=1080, units="px", pointsize=30)
corrgram(mr_data, order=NULL, lower.panel=panel.shade,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         main="Intercorrelation Matrix")
dev.off()

#Plot correlation diagramms with ellipse illustration
png(filename = "Intercorrelation Matrix-points.png", width=1920, height=1080, units="px", pointsize=30)
corrgram(mr_data, order=NULL, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Intercorrelation Matrix")
dev.off()
