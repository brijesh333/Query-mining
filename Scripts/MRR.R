temp<-0
MRR<-0
MRR1<-0
for(l in 1:100)
{
  MRR1[l]<-0
}

Ndocs<- read.csv("/media/brijesh/study/DAIICT/Study/SEM 2/IR Project/Dataset/Testing dataset/3000Query.csv")
Ndocs <- 2000Query

Ndocs_unique<-Ndocs

##Ndocs_unique <- Ndocs1[-grep("AnonID",colnames(Ndocs1))]
Ndocs_unique <- Ndocs_unique[-grep("QueryTime",colnames(Ndocs_unique))]
Ndocs_unique <- Ndocs_unique[-grep("Query",colnames(Ndocs_unique))]
Ndocs_unique <- Ndocs_unique[-grep("ClickURL",colnames(Ndocs_unique))]

nd<-unique(Ndocs_unique$AnonID)
for(i in 1:length(nd))
{
  MRR<-0
  New_User_ID<-nd[i]
  Ndocs1<-Ndocs
  Ndocs1 <- subset(Ndocs,AnonID==New_User_ID)
  Ndocs1 <- Ndocs1[-grep("AnonID",colnames(Ndocs1))]
  Ndocs1 <- Ndocs1[-grep("QueryTime",colnames(Ndocs1))]
  Ndocs1 <- Ndocs1[-grep("Query",colnames(Ndocs1))]
  Ndocs1 <- Ndocs1[-grep("ClickURL",colnames(Ndocs1))]
  Ndocs1 <- Ndocs1[-grep("X",colnames(Ndocs1))]
  
  
  aa<-nrow(Ndocs1)
  temp=0
  
  for(j in 1:aa)
  {
    temp<-temp+(1/Ndocs1[j,])
  }
  
  MRR<-temp/aa
 ## MRR1[i]<-0
  MRR1[i]<-MRR
  ##MRR1<-(MRR1+MRR)
}

Final_MRR<-0

for(k in 1:length(nd))
{
  Final_MRR <- Final_MRR + MRR1[k]  
}
Final_MRR<-(Final_MRR/length(nd))
Final_MRR