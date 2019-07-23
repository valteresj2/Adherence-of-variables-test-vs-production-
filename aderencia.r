bivariada<-function(dados1,dados2,direc)
{
variaveis<-names(dados1)
library(xlsx)
library("Hmisc")
#dir.create("C:/Resultados_bivariadas")
cont<-c()
alvo<-dados1[,ncol(dados1)]
for(i in 2:(ncol(dados1)-1))
 {
   if(is.numeric(dados1[,i])=="TRUE")
    {
     a<-quantile(dados1[,i],probs = seq(0,1,by=0.2),na.rm=TRUE)
     a<-a[!duplicated(a)]
     z <- cut(dados1[,i],breaks=a)
     b<-as.matrix(table(z,alvo,useNA = "ifany"))
   cont[i-1]<-length(rownames(b))
     }else{
  cont[i-1]<-length(levels(dados1[,i]))
}
}
lista<-matrix(nrow=ncol(dados1),ncol=5)

for(j in 1:ncol(dados1))
 {
 #j<-1
 if(is.numeric(dados1[,j])=="TRUE")
 {



#j=4
a<-quantile(dados1[,j],probs = seq(0,1,by=0.05),na.rm=TRUE)
a<-a[!duplicated(a)]


z <- cut(dados1[,j],breaks=a,include.lowest = TRUE)
z1<-cut(dados2[,j],breaks=a,include.lowest = TRUE)

b<-as.matrix(table(z,useNA = "ifany")) ###mudar o 2 por j
d<-rownames(b)
b1<-c()

ef1<-sum(is.na(z1)==TRUE)
for(i in 1:length(d))
{

b1[i]<-sum(z1==d[i],na.rm=TRUE)
}
if(ef1>0)
{
 b1<-c(b1,ef1)
 b<-rbind(b,0)
 d<-c(d,"outros_valores")
 b1<-matrix(b1)
 rownames(b1)<-d
 rownames(b)<-d
}


b1<-matrix(b1)
rownames(b1)<-d

total<-c(sum(b[,1]))
b<-rbind(b,total)

total<-c(sum(b1[,1]))
b1<-rbind(b1,total)

freq<-c()
for(i in 1:nrow(b1))
{
freq[i]<-b1[i,1]/b1[nrow(b),1]
}
b1<-cbind(b1,freq)


freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,1]/b[nrow(b),1]
}
b<-cbind(b,freq)


freq_ac<-c()
for(i in 1:nrow(b1))
{
if(i==1)
{
freq_ac[i]<-b1[1,2]
}else{
freq_ac[i]<-freq_ac[i-1]+b1[i,2]
}
}
b1<-cbind(b1,freq_ac)


freq_ac<-c()
for(i in 1:nrow(b))
{
if(i==1)
{
freq_ac[i]<-b[1,2]
}else{
freq_ac[i]<-freq_ac[i-1]+b[i,2]
}
}
b<-cbind(b,freq_ac)

diff<-round(abs(b[,3]-b1[,3]),3)

ks1<-max(diff)

e<-data.frame(b,b1,diff,ks1,variaveis[j])
flag<-ifelse(ks1>=0.06,"MAU","BOM")
ad<-c(variaveis[j],flag,ks1,"NULL","NULL")
lista[j,]<-ad

coln<-c("Freq_teste","perc_teste","perc_ac_teste","Freq_online","perc_online","perc_ac_online","diff","KS1")

colnames(e)<-coln


write.xlsx(e,paste(direc,variaveis[j],".xlsx"),col.names=TRUE, row.names=TRUE)

}else{############variaveis categoricas##########

b<-as.matrix(table(dados1[,j],useNA = "ifany"))
d<-rownames(b)
b1<-c()

for(i in 1:length(d))
{
 b1[i]<-sum(dados2[,j]==d[i])

}
b1<-matrix(b1)
rownames(b1)<-d


total<-c(sum(b[,1]))
b<-rbind(b,total)

total<-c(sum(b1[,1]))
b1<-rbind(b1,total)

freq<-c()
for(i in 1:nrow(b1))
{
freq[i]<-b1[i,1]/b1[nrow(b),1]
}
b1<-cbind(b1,freq)


freq<-c()
for(i in 1:nrow(b))
{
freq[i]<-b[i,1]/b[nrow(b),1]
}
b<-cbind(b,freq)

diff<-abs(b1[,2]-b[,2])


alerta<-c()
for(i in 1:nrow(b))
{
if(diff[i]>0.05)
{
 alerta[i]<-"RUIM"
}else{
alerta[i]<-"BOM"
}
}


e<-data.frame(b,b1,diff,alerta,variaveis[j])
count<-sum(e[,5]>0.05)
flag1<-ifelse(count>0,"MAU","BOM")
tot<-nrow(e)-1
ad1<-c(variaveis[j],flag1,"NULL",count,tot)


lista[j,]<-ad1

coln<-c("Freq_teste","perc_teste","Freq_online","perc_online","diff","Alerta","Variavel")
colnames(e)<-coln
write.xlsx(e,paste(direc,variaveis[j],".xlsx"),col.names=TRUE, row.names=TRUE)

}

}
colnames(lista)<-c("Variaveis","SITUACAO","KS1","DIFF_NIVEIS_>5%","TOTAL_DE_NIVEIS")
write.xlsx(lista,paste(direc,"relatorios",".xlsx"),col.names=TRUE, row.names=TRUE)

}

