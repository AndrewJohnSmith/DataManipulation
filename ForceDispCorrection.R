#This script allows you to import multiple force displacement data from an entrie folder (i.e. typically output from an Instron materials testing machine) and calculate maximum reaction force and energy (work-done).
 #Removeleft and right refer to the amount of force you wish to start and end at for the calulation; loadadsust refers to the load you wish to cancel (i.e. if you did not zero the load before testing). The remaining arguments are for saving the trails and plotting the results.
  #Feel free to e-mail me comments about possible corrections or imporvements.

InstronCorrection<-function(RemoveLeft=1.5, RemoveRight=1.5, LoadAdjust=0.53, PlotColours="black", Add=F, SaveMaxSum=F, SaveCorrected=F){

GetFiles=function(){
  print(list.files())
  as.numeric(readline("What index value is the folder you want to correct? \n"))->FilesIndex
  setwd(paste(getwd(), "/",  list.files()[FilesIndex], sep=""))
}

GetFiles()

data_list<-lapply(list.files(), read.csv, header=F, skip=1)

for(i in 1:length(data_list)){
data_list[[i]]<-data_list[[i]][,c(3,2,1)]
}

for(i in 1:length(data_list)){
data_list[[i]][,1]<-data_list[[i]][,1]-LoadAdjust
}


for(i in 1:length(data_list)){
data_list[[i]][,2]<-max(data_list[[i]][,2])-data_list[[i]][,2]
}


for(i in 1:length(data_list)){
	data_list[[i]][,1:2]<-abs(data_list[[i]][,1:2])
}



for(j in 1:length(data_list)){
	for(i in nrow(data_list[[j]])){
		data_list[[j]][,4]<-seq(from=1, to=i, by=1)
	}
}


Row_Lists <-matrix(data=NA, nrow = length(data_list), ncol = ncol(data_list[[1]]))
for(i in 1:length(data_list)){
	Row_Lists[i,]<-as.vector(data_list[[i]][(data_list[[i]][,1] == data_list[[i]][,1][data_list[[i]][,1]>RemoveLeft][1]),][1,], mode="numeric")
}


for(i in 1:length(data_list)){
data_list[[i]][,2]<-data_list[[i]][,2]-Row_Lists[,2][i]
}


for(i in 1:length(data_list)){
data_list[[i]]<-data_list[[i]][-1:-(Row_Lists[i,4]-1),]
}


for(i in 1:length(data_list)){
colnames(data_list[[i]])<-c("Load (KN)", "XHd Pos. (mm)", "Time (sec.)", "Rows")
}




for(i in 1:length(data_list)){
data_list[[i]][,"Rows"]<-seq(1, nrow(data_list[[i]]), by=1)
}

data_list_rev<-list()
for(i in 1:length(data_list)){
data_list_rev[[i]]<-apply(data_list[[i]], 2, rev)
}

####Reversal####

for(i in 1:length(data_list)){
data_list_rev[[i]]<-data_list_rev[[i]][(data_list_rev[[i]][,1] == data_list_rev[[i]][,1][data_list_rev[[i]][,1]>RemoveRight][1]),]
}

Final<-list()
for(i in 1:length(data_list)){
Final[[i]]<-data_list[[i]][-(matrix(data_list_rev[[i]], ncol=4)[1,4]+1):-max(data_list[[i]][,4]),-4]
}

for(i in 1:length(Final)){
Final[[i]][,2]<-Final[[i]][,2]*-1
}

####Maximum and Sums####
RHMethod<- NULL
RHMethod<-vector("list", length(Final))

for(j in 1:length(Final)){
	for(i in 1:nrow(Final[[j]])){
	RHMethod[[j]][i]<-Final[[j]][i,1]*(Final[[j]][i+1,2]-Final[[j]][i,2])
	RHMethod[[j]][is.na(RHMethod[[j]])] <- 0
	}
}


Sums<-as.vector(lapply(RHMethod, sum), mode="numeric")
#Maximum<-as.vector(lapply(RHMethod, max), mode="numeric")


Max<-NULL
for(i in 1:length(Final)){
Max[i]<-max(Final[[i]][,1])
}


SumsSTDEV<-sd(Sums)
SumsMean<-mean(Sums)
MaxSTDEV<-sd(Max)
MaxMean<-mean(Max)



####Saving####
#Outputs all your trail values, then the mean, then the standard deviation.
if(SaveCorrected==T){
	for(i in 1:length(Final)) {       
      write.csv(Final[[i]], paste("CorrectedRun", i, ".csv", sep = ""), row.names=F)
	}
}


if(SaveMaxSum==T){
	cat(Sums, SumsMean, SumsSTDEV, "\n", file="OurFile.txt", append=T)
	cat(Max, MaxMean, MaxSTDEV, "\n", file="OurFile.txt", append=T)
}

####Plotting####
if(Add==F){
	plot(Final[[1]][,2],Final[[1]][,1], type="l", xlim=c(-13000,0), ylim=c(0,5000))
		for(i in 2:length(Final)){
			lines(Final[[i]][,2],Final[[i]][,1], type="l", col=PlotColours)
		}
}

if(Add==T){
	for(i in 1:length(Final)){
		lines(Final[[i]][,2],Final[[i]][,1], type="l", col=PlotColours)
	}
}


}