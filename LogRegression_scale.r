options(java.parameters = "-Xmx8000m")
#library(rJava) 
#library(xlsx) 
#dat <- read.xlsx("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xlsx",sheetName = "HMDA_R_extract", header=TRUE)

#dat <- RxTextData("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.txt", stringsAsFactors = FALSE, delimiter = "\t" , firstRowIsColNames = TRUE)
#xdfDat<-rxImport(dat,outFile = "C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xdf",overwrite=TRUE)
#mortgage_Dat<-rxDataStepXdf("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xdf",
#	rowSelection = !is.na(applicant_income_000s) & !is.na(loan_amount_000s) & !is.na(tract_to_msamd_income) & action_taken != 6,
#	transforms = list(loan_income_ratio = loan_amount_000s/applicant_income_000s) ,
#	overwrite=TRUE)

#mortgage_Dat[,1]<-as.factor(mortgage_Dat[,1])
#mortgage_Dat[,2]<-as.factor(mortgage_Dat[,2])
#mortgage_Dat[,6]<-as.factor(mortgage_Dat[,6])
#mortgage_Dat[,7]<-as.factor(mortgage_Dat[,7])
#mortgage_Dat[,8]<-as.factor(mortgage_Dat[,8])
#mortgage_Dat[,9]<-as.factor(mortgage_Dat[,9])
#mortgage_Dat[,13]<-as.factor(mortgage_Dat[,13])
#mortgage_Dat[,14]<-as.factor(mortgage_Dat[,14])
#mortgage_Dat[,15]<-as.factor(mortgage_Dat[,15])
#mortgage_Dat[,16]<-as.factor(mortgage_Dat[,16])
#mortgage_Dat[,17]<-as.factor(mortgage_Dat[,17])
#mortgage_Dat[,18]<-as.factor(mortgage_Dat[,18])
#mortgage_Dat[,19]<-as.factor(mortgage_Dat[,19])
#names(mortgage_Dat)[length(mortgage_Dat)-2]<-"Respondent_Name"

#xdfDat<-rxImport(mortgage_Dat,outFile = "C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract_final.xdf",overwrite=TRUE)
mortgage_Dat<-rxDataStepXdf("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract_final.xdf" , rowSelection = denial_reason_1 != 6 & denial_reason_1 != 7 | is.na(denial_reason_1) )

approved_mortgages <-rxDataStepXdf("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xdf",rowSelection = action_taken != 6 & action_taken_binary ==1 )
top_Firms<-aggregate(approved_mortgages$loan_amount_000s, by=list(Respondent_Name=approved_mortgages$"Respondent name"), FUN=sum)
top_Firms <- top_Firms[order(-top_Firms$x),] 
top_Firms<-top_Firms[1:10,]

dat<-merge(mortgage_Dat,top_Firms,by = "Respondent_Name")
dat$Respondent_Name<-as.factor(dat$Respondent_Name)

provider_List<-levels(dat$Respondent_Name)

i<-length(provider_List)
library(MASS)
library(relaimpo)
for (j in 1:i){

currentProvider<-provider_List[j]
workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
workingDat <- na.omit(workingDat_dirty[c(
		"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
		,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
		"owner_occupancy","preapproval","property_type","number_of_1_to_4_family_units","population"
		)])

print("------------------------------------------------")
print(paste("--------------",currentProvider,"--------------"))
print("------------------------------------------------")


form <- action_taken_binary ~ 1
scope <- list(
    lower = ~ 1,
    upper = ~ loan_amount_000s + applicant_income_000s + tract_to_msamd_income + loan_income_ratio + minority_population 
		+ applicant_ethnicity + applicant_race_1 + applicant_sex + hoepa_status + lien_status + loan_purpose 
		+ owner_occupancy + preapproval + property_type +
		number_of_1_to_4_family_units/population + population)

varsel <- rxStepControl(method = "stepwise", scope = scope)	
	
model<- rxLogit(form, data = workingDat, variableSelection = varsel, coefLabelStyle = "Revo")
	
print(summary(model))
#print(coef(model))
save(model,file = paste("C:/Users/gersonda/Dropbox/CINBITools/",currentProvider,".rdata",sep=""))
}


for (j in 1:i){
currentProvider<-provider_List[j]
load(paste("C:/Users/gersonda/Dropbox/CINBITools/",currentProvider,".rdata",sep=""))
print("------------------------------------------------")
print(paste("--------------",currentProvider,"--------------"))
print("------------------------------------------------")
print(coef(model))
}

library(pROC)
for (j in 1:i){
	currentProvider<-provider_List[j]
	workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
	workingDat <- na.omit(workingDat_dirty[c(
			"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
			,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
			"owner_occupancy","preapproval","property_type","purchaser_type","number_of_1_to_4_family_units","population"
			)])
	load(paste("C:/Users/gersonda/Dropbox/CINBITools/",currentProvider,".rdata",sep=""))
	
	prob_vec <- rxPredict(modelObject = model, data = workingDat, predVarNames = "prob")

	workingDat$prob <- unlist(prob_vec)
	g <- roc(action_taken_binary ~ prob, data = workingDat)
	
	plot(g, print.auc=TRUE , main=currentProvider) 
	print(paste("--------------",currentProvider,"--------------"))
	print(auc(g)  )
} 

for (j in 1:i){

currentProvider<-provider_List[j]
workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
workingDat <- na.omit(workingDat_dirty[c(
		"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
		,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
		"owner_occupancy","preapproval","property_type","purchaser_type","number_of_1_to_4_family_units","population"
		)])

print("------------------------------------------------")
print(paste("--------------",currentProvider,"--------------"))
print("------------------------------------------------")

workingDat$home_percentage<-workingDat$number_of_1_to_4_family_units/workingDat$population

model<-rxDForest(action_taken_binary~
		loan_amount_000s + applicant_income_000s + tract_to_msamd_income + loan_income_ratio + minority_population 
		+ applicant_ethnicity + applicant_race_1 + applicant_sex + hoepa_status + lien_status + loan_purpose 
		+ owner_occupancy + preapproval + property_type + population + home_percentage
		, workingDat)

print(summary(model))
#print(coef(model))
save(model,file = paste("C:/Users/gersonda/Dropbox/CINBITools/DF_",currentProvider,".rdata",sep=""))
}



for (j in 1:i){
	currentProvider<-provider_List[j]
	workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
	workingDat <- na.omit(workingDat_dirty[c(
			"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
			,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
			"owner_occupancy","preapproval","property_type","purchaser_type","number_of_1_to_4_family_units","population"
			)])
	workingDat$home_percentage<-workingDat$number_of_1_to_4_family_units/workingDat$population
	load(paste("C:/Users/gersonda/Dropbox/CINBITools/DF_",currentProvider,".rdata",sep=""))

	prob_vec <- rxPredict(modelObject = model, data = workingDat, type = "prob")
	
	workingDat$prob <- unlist(prob_vec[,2])

	g <- roc(action_taken_binary ~ prob, data = workingDat)
	
	plot(g) 
	print(paste("--------------",currentProvider,"--------------"))
	print(auc(g)  )
} 


















library(sjPlot)

currentProvider<-provider_List[2] #Fifth Third
workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
workingDat <- na.omit(workingDat_dirty[c(
		"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
		,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
		"owner_occupancy","preapproval","property_type","number_of_1_to_4_family_units","population"
		)])
		
workingDat$applicant_ethnicity1 <- ifelse(workingDat$applicant_ethnicity == 1,1,0)
workingDat$applicant_ethnicity2 <- ifelse(workingDat$applicant_ethnicity == 2,1,0)
workingDat$applicant_ethnicity3 <- ifelse(workingDat$applicant_ethnicity == 3,1,0)
workingDat$applicant_ethnicity4 <- ifelse(workingDat$applicant_ethnicity == 4,1,0)		
workingDat$applicant_race_11 <- ifelse(workingDat$applicant_race_1 == 1,1,0)
workingDat$applicant_race_12 <- ifelse(workingDat$applicant_race_1 == 2,1,0)
workingDat$applicant_race_13 <- ifelse(workingDat$applicant_race_1 == 3,1,0)
workingDat$applicant_race_14 <- ifelse(workingDat$applicant_race_1 == 4,1,0)
workingDat$applicant_race_15 <- ifelse(workingDat$applicant_race_1 == 5,1,0)
workingDat$applicant_race_16 <- ifelse(workingDat$applicant_race_1 == 6,1,0)
workingDat$applicant_race_17 <- ifelse(workingDat$applicant_race_1 == 7,1,0)
workingDat$applicant_sex1 <- ifelse(workingDat$applicant_sex == 1,1,0)
workingDat$applicant_sex2 <- ifelse(workingDat$applicant_sex == 2,1,0)
workingDat$applicant_sex3 <- ifelse(workingDat$applicant_sex == 3,1,0)
workingDat$applicant_sex4 <- ifelse(workingDat$applicant_sex == 4,1,0)
workingDat$hoepa_status1 <- ifelse(workingDat$hoepa_status == 1,1,0)
workingDat$hoepa_status2 <- ifelse(workingDat$hoepa_status == 2,1,0)	
workingDat$lien_status1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$lien_status2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$owner_occupancy1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$owner_occupancy2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$owner_occupancy3 <- ifelse(workingDat$lien_status == 3,1,0)	
workingDat$preapproval1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$preapproval2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$preapproval3 <- ifelse(workingDat$lien_status == 3,1,0)	
workingDat$property_type1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$property_type2 <- ifelse(workingDat$lien_status == 2,1,0)	
		

print("------------------------------------------------")
print(paste("--------------",currentProvider,"--------------"))
print("------------------------------------------------")

	
model<- glm(action_taken_binary ~ loan_amount_000s + applicant_income_000s + tract_to_msamd_income + loan_income_ratio + minority_population 
		+ applicant_ethnicity1 +  applicant_ethnicity2
		+ applicant_race_11 + applicant_race_12 + applicant_race_13 + applicant_race_14 + applicant_race_15
		+ applicant_sex1 + applicant_sex2 
		+ property_type 
		+
		number_of_1_to_4_family_units/population + population, data = workingDat, family=binomial(logit))
		
print(summary(model))


currentProvider<-provider_List[7] #PNC Bank
workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
workingDat <- na.omit(workingDat_dirty[c(
		"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
		,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
		"owner_occupancy","preapproval","property_type","number_of_1_to_4_family_units","population"
		)])
		
workingDat$applicant_ethnicity1 <- ifelse(workingDat$applicant_ethnicity == 1,1,0)
workingDat$applicant_ethnicity2 <- ifelse(workingDat$applicant_ethnicity == 2,1,0)
workingDat$applicant_ethnicity3 <- ifelse(workingDat$applicant_ethnicity == 3,1,0)
workingDat$applicant_ethnicity4 <- ifelse(workingDat$applicant_ethnicity == 4,1,0)		
workingDat$applicant_race_11 <- ifelse(workingDat$applicant_race_1 == 1,1,0)
workingDat$applicant_race_12 <- ifelse(workingDat$applicant_race_1 == 2,1,0)
workingDat$applicant_race_13 <- ifelse(workingDat$applicant_race_1 == 3,1,0)
workingDat$applicant_race_14 <- ifelse(workingDat$applicant_race_1 == 4,1,0)
workingDat$applicant_race_15 <- ifelse(workingDat$applicant_race_1 == 5,1,0)
workingDat$applicant_race_16 <- ifelse(workingDat$applicant_race_1 == 6,1,0)
workingDat$applicant_race_17 <- ifelse(workingDat$applicant_race_1 == 7,1,0)
workingDat$applicant_sex1 <- ifelse(workingDat$applicant_sex == 1,1,0)
workingDat$applicant_sex2 <- ifelse(workingDat$applicant_sex == 2,1,0)
workingDat$applicant_sex3 <- ifelse(workingDat$applicant_sex == 3,1,0)
workingDat$applicant_sex4 <- ifelse(workingDat$applicant_sex == 4,1,0)
workingDat$hoepa_status1 <- ifelse(workingDat$hoepa_status == 1,1,0)
workingDat$hoepa_status2 <- ifelse(workingDat$hoepa_status == 2,1,0)	
workingDat$lien_status1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$lien_status2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$owner_occupancy1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$owner_occupancy2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$owner_occupancy3 <- ifelse(workingDat$lien_status == 3,1,0)	
workingDat$preapproval1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$preapproval2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$preapproval3 <- ifelse(workingDat$lien_status == 3,1,0)	
workingDat$property_type1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$property_type2 <- ifelse(workingDat$lien_status == 2,1,0)	
		

print("------------------------------------------------")
print(paste("--------------",currentProvider,"--------------"))
print("------------------------------------------------")

	
model2<- glm(action_taken_binary ~ loan_amount_000s + applicant_income_000s + tract_to_msamd_income + loan_income_ratio + minority_population 
		+ applicant_ethnicity1 +  applicant_ethnicity2
		+ applicant_race_11 + applicant_race_12 + applicant_race_13 + applicant_race_14 + applicant_race_15
		+ applicant_sex1 + applicant_sex2 
		+ property_type 
		+
		number_of_1_to_4_family_units/population + population, data = workingDat, family=binomial(logit))
		
		
print(summary(model2))


currentProvider<-provider_List[10] #PNC Bank
workingDat_dirty<-dat[dat$Respondent_Name == currentProvider,]
workingDat <- na.omit(workingDat_dirty[c(
		"action_taken_binary","loan_amount_000s","applicant_income_000s","tract_to_msamd_income","loan_income_ratio","minority_population"
		,"applicant_ethnicity","applicant_race_1","applicant_sex","hoepa_status","lien_status","loan_purpose",
		"owner_occupancy","preapproval","property_type","number_of_1_to_4_family_units","population"
		)])
		
workingDat$applicant_ethnicity1 <- ifelse(workingDat$applicant_ethnicity == 1,1,0)
workingDat$applicant_ethnicity2 <- ifelse(workingDat$applicant_ethnicity == 2,1,0)
workingDat$applicant_ethnicity3 <- ifelse(workingDat$applicant_ethnicity == 3,1,0)
workingDat$applicant_ethnicity4 <- ifelse(workingDat$applicant_ethnicity == 4,1,0)		
workingDat$applicant_race_11 <- ifelse(workingDat$applicant_race_1 == 1,1,0)
workingDat$applicant_race_12 <- ifelse(workingDat$applicant_race_1 == 2,1,0)
workingDat$applicant_race_13 <- ifelse(workingDat$applicant_race_1 == 3,1,0)
workingDat$applicant_race_14 <- ifelse(workingDat$applicant_race_1 == 4,1,0)
workingDat$applicant_race_15 <- ifelse(workingDat$applicant_race_1 == 5,1,0)
workingDat$applicant_race_16 <- ifelse(workingDat$applicant_race_1 == 6,1,0)
workingDat$applicant_race_17 <- ifelse(workingDat$applicant_race_1 == 7,1,0)
workingDat$applicant_sex1 <- ifelse(workingDat$applicant_sex == 1,1,0)
workingDat$applicant_sex2 <- ifelse(workingDat$applicant_sex == 2,1,0)
workingDat$applicant_sex3 <- ifelse(workingDat$applicant_sex == 3,1,0)
workingDat$applicant_sex4 <- ifelse(workingDat$applicant_sex == 4,1,0)
workingDat$hoepa_status1 <- ifelse(workingDat$hoepa_status == 1,1,0)
workingDat$hoepa_status2 <- ifelse(workingDat$hoepa_status == 2,1,0)	
workingDat$lien_status1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$lien_status2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$owner_occupancy1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$owner_occupancy2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$owner_occupancy3 <- ifelse(workingDat$lien_status == 3,1,0)	
workingDat$preapproval1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$preapproval2 <- ifelse(workingDat$lien_status == 2,1,0)	
workingDat$preapproval3 <- ifelse(workingDat$lien_status == 3,1,0)	
workingDat$property_type1 <- ifelse(workingDat$lien_status == 1,1,0)	
workingDat$property_type2 <- ifelse(workingDat$lien_status == 2,1,0)	
		

print("------------------------------------------------")
print(paste("--------------",currentProvider,"--------------"))
print("------------------------------------------------")

	
model3<- glm(action_taken_binary ~ loan_amount_000s + applicant_income_000s + tract_to_msamd_income + loan_income_ratio + minority_population 
		+ applicant_ethnicity1 +  applicant_ethnicity2
		+ applicant_race_11 + applicant_race_12 + applicant_race_13 + applicant_race_14 + applicant_race_15
		+ applicant_sex1 + applicant_sex2 
		+ property_type 
		+
		number_of_1_to_4_family_units/population + population, data = workingDat, family=binomial(logit))
	
print(summary(model3))


sjp.glmm(model, model2,model3)


#print(coef(model))
save(model,file = paste("C:/Users/gersonda/Dropbox/CINBITools/",currentProvider,".rdata",sep=""))

