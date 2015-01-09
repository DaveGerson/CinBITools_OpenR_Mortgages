options(java.parameters = "-Xmx8000m")
library(rJava) 
library(xlsx) 
dat <- read.xlsx("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xlsx",sheetName = "HMDA_R_extract", header=TRUE)

#dat <- RxTextData("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.txt", stringsAsFactors = FALSE, delimiter = "\t" , firstRowIsColNames = TRUE)
#xdfDat<-rxImport(dat,outFile = "C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xdf")
mortgage_Dat<-rxDataStepXdf("C:/Users/gersonda/Dropbox/Datasets/HMDA/HMDA_R_extract.xdf",rowSelection = !is.na(applicant_income_000s) & !is.na(loan_amount_000s) & !is.na(tract_to_msamd_income) & action_taken == 1, transforms = list(loan_income_ratio = loan_amount_000s/applicant_income_000s) )

mortgage_Dat_sub<-rxDataStep(mortgage_Dat,removeMissings = TRUE,varsToKeep = c("applicant_income_000s","loan_amount_000s","tract_to_msamd_income","loan_income_ratio"))
library("psych")
#Winsorizing data to remove "heavy outliers" from analysis
mortgage_Dat_winsored<-data.frame(winsor(mortgage_Dat_sub,trim=.05))
##Scale data so the clustering doesn't favor the "larger" numerical datapoints.
mortgage_Dat_scaled<-data.frame(scale(mortgage_Dat_winsored))



#Computing total sum of squares to demo Calinski Harabasz Index
gmean<-apply(mortgage_Dat_scaled,2,FUN=mean)
sqr_edist <- function(x, y) { sum((x-y)^2) } #Euclidean Distance Calculator
totss<-sum(apply(mortgage_Dat_scaled,1,FUN=function(row){sqr_edist(row,gmean)}))
mortcount<-nrow(mortgage_Dat_scaled)

#prep for kmeans loop
cluster_WSS_diag<- c()
cluster_BSS_diag<-c()
chnumerator<-c()
chdenominator<- c()

mortClusters<-mortgage_Dat_sub
k<-1
for (k in 1:10){
##Actual Generation of the clusters
mortCluster<-rxKmeans(~applicant_income_000s + loan_amount_000s + tract_to_msamd_income + loan_income_ratio,mortgage_Dat_scaled,numClusters = k)


##Actual Generation of the clusters
aggregate(mortgage_Dat_sub,by=list(mortCluster$cluster),FUN=mean)
aggregate(mortgage_Dat_sub,by=list(mortCluster$cluster),FUN=median)
#Cluster Daiagnostics based on within cluster variance. End Goal is to implement Calinski Harabasz
cluster_WSS_diag<-union(cluster_WSS_diag,mortCluster$tot.withinss)
cluster_BSS_diag<-union(cluster_BSS_diag,totss-cluster_WSS_diag[k])
chnumerator<-union(chnumerator,cluster_BSS_diag[k]/k-1)
chdenominator<-union(chdenominator,(cluster_WSS_diag[k]/(mortcount-k)))
mortClusters <- data.frame(mortClusters, mortCluster$cluster)
names(mortClusters)[length(mortClusters)]<-paste("Cluster",k,sep="")
}



scoringList<-list(CH = chnumerator/chdenominator, wss = cluster_WSS_diag, tss = totss )
#Plotting Calinski Harabasz Index
plot(scoringList$wss,type="l",col="blue")
par(new=TRUE)
plot(scoringList$CH,type="l",col="red")


#The "Elbow" of my data shows a peak at 4 and 6
names(mortClusters)[1:4]<-c("applicant_income_000s_scaled","loan_amount_000s_scaled","tract_to_msamd_income_scaled","loan_income_ratio_scaled")
mortgage_Dat_out<-data.frame(mortgage_Dat,mortClusters)
cluster_outfile<-"C:/Users/gersonda/Dropbox/CINBITools/HMDA_clusterbook.csv"
write.table(mortgage_Dat_out, file = cluster_outfile , row.names = FALSE, sep = "~")


