# Cleans everything
rm(list = ls())

# Working directory
setwd("")

TimeDeb=Sys.time()

# Opening the table containing all the data (export of TADARIDA)
library(data.table)
DataTot=fread("")
head(DataTot)
colnames(DataTot)[10]="temps_fin"

# Opening the table containing informations about of each acoustic recorder in stereo recordings
ListDir=read.csv2("ListPoint.csv")

# Select only bats
SpeciesList=fread("SpeciesList.csv")
BatSpecies=subset(SpeciesList,SpeciesList$Group=="bat")
head(BatSpecies)
TC=merge(DataTot,BatSpecies,by.x="espece",by.y="Esp")
TC=droplevels(TC)

TC$temps_debut=as.numeric(as.character(TC$temps_debut))
TC$temps_fin=as.numeric(as.character(TC$temps_fin))
TC$frequence=as.numeric(as.character(TC$frequence))

library(Hmisc) 

micro=vector(length=nrow(TC))
Date=vector(length=nrow(TC))
Heure=vector(length=nrow(TC))
Minute=vector(length=nrow(TC))
Seconde=vector(length=nrow(TC))
MiliSec=vector(length=nrow(TC))

Fich=as.character(TC$donnee)

for (i in 1:length(Fich)){
  micro[i]=substr(Fich[i],nchar(Fich[i])-20,nchar(Fich[i])-20)
  Date[i]=substr(Fich[i],nchar(Fich[i])-18,nchar(Fich[i])-11)
  Heure[i]=substr(Fich[i],nchar(Fich[i])-9,nchar(Fich[i])-8)
  Minute[i]=substr(Fich[i],nchar(Fich[i])-7,nchar(Fich[i])-6)
  Seconde[i]=substr(Fich[i],nchar(Fich[i])-5,nchar(Fich[i])-4)
  MiliSec[i]=substr(Fich[i],nchar(Fich[i])-2,nchar(Fich[i]))
}

Datenum=as.numeric(Date)
Heure=as.numeric(Heure)
Minute=as.numeric(Minute)
Seconde=as.numeric(Seconde)
MiliSec=as.numeric(MiliSec)
Temps=Heure*3600+Minute*60+Seconde+MiliSec/1000

TCext=cbind(TC,micro,Date,Datenum,Heure,Minute,Seconde,MiliSec,Temps)

rm(FichiersManquantsG)
rm(FichiersManquantsD)
rm(PartManquantes)
rm(DirCriMauvais)

FichiersManquantsG=vector()
FichiersManquantsD=vector()
PartManquantes=vector()
DirCriMauvais=vector()

for (q in 1:nrow(ListDir))
{
  DirCri=ListDir$chemin_total_formule[q]
  if (dir.exists(paste0("",ListDir$chemin_total_formule[q],""))==FALSE){
    DirCriMauvais=c(DirCriMauvais,paste(ListDir$participation[q]))} else{
      TCsub=subset(TCext,TCext$participation==ListDir$participation[q])
      if(nrow(TCsub)==0){PartManquantes=c(PartManquantes,ListDir$participation[q])}
      else{
        TC0=subset(TCsub,micro=="0")
        TC1=subset(TCsub,micro=="1")
        TC01=find.matches(cbind(as.factor(TC0$participation),TC0$Datenum,TC0$Temps)
                          ,cbind(as.factor(TC1$participation),TC1$Datenum,TC1$Temps)
                          ,tol=c(0,0,5))
        for(i in 1:nrow(TC0))
        {
          rm(TempsCriDtot)
          rm(TempsCriG)
          TempsCriDtot=vector()
          TempsCriG=vector()
          if (file.exists(paste(DirCri,"/",TC0$donnee[i],".ta",sep="")))
          {
            CriFichier=read.table(paste(DirCri,"/",TC0$donnee[i],".ta",sep=""),h=T,sep="\t")
            CriFilTemps=subset(CriFichier
                               ,(CriFichier$StTime>(TC0$temps_debut[i]*1000))
                               &(CriFichier$StTime<(TC0$temps_fin[i]*1000)))
            CriFilFreq=subset(CriFilTemps
                              ,(CriFilTemps$Fmin<(TC0$frequence[i]))
                              &((CriFilTemps$Fmin+CriFilTemps$BW)>(TC0$frequence[i])))
            CriFilEcho=subset(CriFilFreq,
                              (CriFilFreq$PrevMP2>35))
            TempsCriG=TC0$Heure[i]*3600+TC0$Minute[i]*60+TC0$Seconde[i]+TC0$MiliSec[i]/1000+CriFilEcho$StTime/1000
            TC01$matches=as.matrix(TC01$matches)
            if(TC01$matches[i,1]!=0)
            {
              for (j in 1:ncol(TC01$matches))
              {
                if(TC01$matches[i,j]!=0){
                  if (TC0$espece[i]==TC1$espece[TC01$matches[i,j]])
                  {
                    if (file.exists(paste(DirCri,"/",TC1$donnee[TC01$matches[i,j]],".ta",sep="")))
                    {
                      CriFichierD=read.table(paste(DirCri,"/",TC1$donnee[TC01$matches[i,j]],".ta",sep=""),h=T,sep="\t")
                      CriFilTempsD=subset(CriFichierD
                                          ,(CriFichierD$StTime>(TC1$temps_debut[TC01$matches[i,j]]*1000))
                                          &(CriFichierD$StTime<(TC1$temps_fin[TC01$matches[i,j]]*1000)))
                      CriFilFreqD=subset(CriFilTempsD
                                         ,(CriFilTempsD$Fmin<(TC1$frequence[TC01$matches[i,j]]))
                                         &((CriFilTempsD$Fmin+CriFilTempsD$BW)>(TC1$frequence[TC01$matches[i,j]])))
                      CriFilEchoD=subset(CriFilFreqD,
                                         (CriFilFreqD$PrevMP2>35))
                      TempsCriD=TC1$Heure[TC01$matches[i,j]]*3600+TC1$Minute[TC01$matches[i,j]]*60+TC1$Seconde[TC01$matches[i,j]]+TC1$MiliSec[TC01$matches[i,j]]/1000+CriFilEchoD$StTime/1000         #temps macro du début du cri côté droit
                      if(j==1){TempsCriDtot=TempsCriD}else{TempsCriDtot=c(TempsCriDtot,TempsCriD)}
                    }
                    else{FichiersManquantsD=c(FichiersManquantsD,paste(DirCri,"/",TC1$donnee[TC01$matches[i,j]],".ta",sep=""))}
                  }
                }
              }
              if((length(TempsCriDtot)>0)&(length(TempsCriG)>0)){
                CriMatch=find.matches(TempsCriG,TempsCriDtot,tol=(ListDir$dist_micro[q]/340),maxmatch=1)
                DecTemps=vector()
                TempsG2=vector()
                Int=vector()
                for (k in 1:length(TempsCriG))
                {
                  if (CriMatch$matches[k]!=0)
                  {
                    DecTemps=c(DecTemps,TempsCriG[k]-TempsCriDtot[CriMatch$matches[k]])
                    TempsG2=c(TempsG2,CriFilEcho$StTime[k])
                    if (k>1){Int=c(Int,(CriFilEcho$StTime[k]-CriFilEcho$StTime[k-1]))}
                  }
                }
                if(length(DecTemps)>1){
                  Dec10=quantile(DecTemps,0.1)
                  Dec90=quantile(DecTemps,0.9)
                  DecMin=min(DecTemps)
                  DecMax=max(DecTemps)
                  DecDeb=mean(DecTemps[1:(ceiling(length(DecTemps)/10))])
                  DecFin=mean(DecTemps[(floor(length(DecTemps)*9/10)):(length(DecTemps))])
                  Int50=quantile(Int,0.5)
                  Int25=quantile(Int,0.25)
                  Trajtemp=c(as.character(TC0$participation[i])
                             ,as.character(TC0$donnee[i]),as.character(TC0$espece[i])
                             ,as.numeric(as.character(TC0$probabilite[i]))
                             ,as.character(TC0$temps_debut[i]),as.character(TC0$temps_fin[i])
                             ,DecDeb,DecFin,Dec10,Dec90,DecMin,DecMax,Int50,Int25)
                  if(exists("TrajTot")==T){TrajTot=rbind(TrajTot,Trajtemp)}else{TrajTot=Trajtemp}
                  print(paste(q,"/",nrow(ListDir),"  ",i,"/",nrow(TC0),"  ",Sys.time(),sep=""))
                }
              }
            }
          }else
          {FichiersManquantsG=c(FichiersManquantsG,paste(DirCri,"/",TC0$donnee[i],".ta",sep=""))}
        }
      }
    } 
} 

colnames(TrajTot)=c("Participation","File","Species","Prob","tdeb","tfin","DecDeb","DecFin","Dec10","Dec90","DecMin","DecMax","Int50","Int25")
TimeFin=Sys.time()
TimeTot=TimeDeb-TimeFin

# Files missing (*.TA files)
write.csv2(FichiersManquantsG,"Fich_Manq_G.csv")
write.csv2(FichiersManquantsD,"Fich_Manq_D.csv")

# Participation missing
write.csv2(PartManquantes,"Part_Manq.csv")

# Bad working directory of *.TA file
write.csv2(DirCriMauvais,"DirCriMauvais.csv")
write.csv2(TrajTot,"TrajTot.csv")
