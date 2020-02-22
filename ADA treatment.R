

##############ABBV428####################
DayT1=rep(c("01","08","15","22","29"),21)
T1Dose=c(rep("1",40),rep("10",40),rep("30",25))
ADA1=c(248,0,193,230,789,
       57.7,0,0,61.3,144,
       0,0,7170,37700,60700,
       2040,886,21800,137000,160000,
       0,0,4990,6780,12000,
       0,0,6070,6560,13100,
       0,0,2190,6020,64300,
       0,0,0,81.3,221,
       
       0,0,802,2130,19800,
       0,0,43100,125000,42300,
       53,0,6730,1960,34600,
       0,0,7080,62900,398000,
       0,0,2130,6300,20500,
       186,0,15100,286000,467000,
       697,0,3370,52500,187000,
       0,0,6540,4730,10500,
       
       206,0,5000,21800,39800,
       0,0,237,415,4690,
       0,0,1420,21300,17000,
       0,0,4320,6190,52200,
       0,0,256,1910,4050
)
T1=rep("ABV428",105)

########################ABBV621#############
DayT2=rep(c("01","15","22","29"),28)
T2Dose=c(rep("4",32),rep("20",32),rep("100",48))
ADA2=c(0,10.7,0,81.5,
0,0,28.6,29,
0,45.6,953,1590,
0,0,20.4,70.4,
0,16,15.7,59.3,
0,0,19.7,180,
0,0,31.4,87.6,
0,29.8,86,57.3,

0,19.8,318,3820,
0,0,0,0,
0,0,364,618,
0,0,0,0,
0,0,15.6,28.2,
0,0,0,19.4,
0,158,106,24.3,
0,0,0,20.5,

0,0,0,0,
0,0,29.2,188,
0,0,149,640,
0,0,18.7,165,
0,0,0,0,
0,0,0,12.8,
0,0,0,0,
0,0,229,740,
0,0,88.9,632,
0,0,0,0,
0,0,0,0,
0,0,85,2030
)
T2=rep("ABV621",112)

###########ABBV794####################

DayT3=rep(c("01","08","15","22","29"),24)
T3Dose=c(rep("10",30),rep("30",30),rep("100",60))
ADA3=as.numeric(c(0,188,10700,173000,891000,
20.4,795,164000,463000,2970000,
17550,559000,1510000,147000,145000,
27.1,2280,156000,827000,505000,
613,46400, 3220000,503000,43200000,
19.5,7880,31700000,1220000,11700000,

0,11800,13700000,1400000,530000,
0,2070,4310000, 2550000,36200000,
0,672,11700000,320000,NA,
0,763,25600000,1980000,2640000,
0,69.5,636,51900,263000,
25.1,192000,5060000,1210000,10400000,

0,1920,140000,440000,711000,
48.9,228,420000,1240000,NA,
0,325,178000,45300000,NA,
0,81.9,6760,182000,120000,
28.2,262,137000,479000,587000,
24.8,269,NA,NA,NA,
148,604,NA,NA,NA,
247,26.8,NA,NA,NA,
0,397,65400,195000,417000,
0,712,152000,207000,414000,
81.8,4940,6230000,4800000,524000,
0,714,1500000,3480000,130000)
)
T3=rep("ABV794",120)





 




Dmat= data.frame(   c(DayT1,DayT2,DayT3)  ,c(T1Dose,T2Dose,T3Dose),c(T1,T2,T3), (c(ADA1,ADA2,ADA3)) )  
names(Dmat)=c("Day","level", "Treatment","ADA") 
Dmat1=data.frame(Dmat[which(Dmat$Treatment=="ABV428"),])
Dmat2=data.frame(Dmat[which(Dmat$Treatment=="ABV621"),])
Dmat3=data.frame(Dmat[which(Dmat$Treatment=="ABV794"),])

Dmat13=data.frame(   c(DayT1, DayT3)  ,c(T1Dose, T3Dose),c(T1, T3), (c(ADA1, ADA3)) )  
names(Dmat13)=c("Day","level", "Treatment","ADA") 
Dlevel10=data.frame(Dmat13[which(Dmat13$level==10),])

D1M1=tapply( (Dmat1$ADA[Dmat1$level=="1"]), Dmat1$Day[Dmat1$level=="1"] ,mean,na.rm=TRUE)
D1M2=tapply( (Dmat1$ADA[Dmat1$level=="10"]) , (Dmat1$Day[Dmat1$level=="10"]),mean,na.rm=TRUE)
D1M3=tapply( (Dmat1$ADA[Dmat1$level=="30"]), (Dmat1$Day[Dmat1$level=="30"]),mean,na.rm=TRUE)

D2M1=tapply( (Dmat2$ADA[Dmat2$level=="4"]), (Dmat2$Day[Dmat2$level=="4"]),mean,na.rm=TRUE)
D2M2=tapply( (Dmat2$ADA[Dmat2$level=="20"]), (Dmat2$Day[Dmat2$level=="20"]),mean,na.rm=TRUE)
D2M3=tapply( (Dmat2$ADA[Dmat2$level=="100"]), (Dmat2$Day[Dmat2$level=="100"]),mean,na.rm=TRUE)


D3M1=tapply( (Dmat3$ADA[Dmat3$level=="10"]), Dmat3$Day[Dmat3$level=="10"] ,mean,na.rm=TRUE)
D3M2=tapply( (Dmat3$ADA[Dmat3$level=="30"]), (Dmat3$Day[Dmat3$level=="30"]),mean,na.rm=TRUE)
D3M3=tapply( (Dmat3$ADA[Dmat3$level=="100"]), (Dmat3$Day[Dmat3$level=="100"]),mean,na.rm=TRUE)


#plot( DayT1[1:4],D1M1[1:4],type="b",lty=3)

#####
#D1matmean=data.frame(cbind(c(D1M1,D1M2,D1M3),
#c(rep("4",4),rep("20",4),rep("100",4))))
####
#xyplot(ADA~DayT1,       type="b",       group=T1Dose,       data=D1matmean,
  #     auto.key =list(
  #       points = FALSE, 
  #       columns=2,
   #      lines = TRUE)
#)

plot(D1M1,type="b",pch=23,xlab='Day',ylab='mean ADA',xaxt="n",xlim=c(1,6),ylim=c(-3,150000))
axis(1, at=1:5, labels=c("01","08","15","22","29"))
lines(D1M2,type="b",pch=9,col="blue")
lines(D1M3,type="b",pch=4,col="red")

lines(D2M1,type="b",pch=23,col=1,lwd=4)
lines(D2M2,type="b",pch=9,col="blue", lwd=4)
lines(D2M3,type="b",pch=4,col="red", lwd=4)
 
lines(D3M1,type="b",pch=23,col=1,lwd=8)
lines(D3M2,type="b",pch=9,col="blue", lwd=8)
lines(D3M3,type="b",pch=4,col="red", lwd=8)
 
 

legend(5.2,1500,c('low 428','Med 428','high 428',
               'low 621','Med 621','high 621',
               'low 974','Med 974','high 974'),lwd=c(1,1,1,4,4,4,8,8,8),col=c(1,"blue","red",
                                            1,"blue","red",1,"blue","red") )

################ANOVA#################
model=lm(ADA~Treatment+Day, data=Dlevel10)
summary(model)
fit <- aov(ADA~Treatment+Day, data=Dlevel10)
summary(fit)
 
par(mfrow=c(1,1))
boxplot((ADA)~Day+Treatment,data=Dlevel10,ylab="ADA titer",xlab="treatment levels",col="orange",border="brown",
        xaxt="n")
axis(1, at=1:10, labels=c("428D1","428D8","428D15","428D22","428D29",
                          "794D1","794D8","794D15","794D22","794D29"))
par(mfrow=c(2,1))
boxplot((ADA)~Day,data=Dlevel10[which(Dlevel10$Treatment=="ABV428"),],ylab="ADA titer",xlab="treatment Abv428 at 10mg/kg",col="orange",border="brown")
boxplot((ADA)~Day,data=Dlevel10[which(Dlevel10$Treatment=="ABV794"),],ylab="ADA titer",xlab="treatment Abv794 at 10mg/kg",col="orange",border="brown")


par(mfrow=c(4,1))
boxplot(ADA~Treatment, data=Dlevel10[which(Dlevel10$Day=="08"),],ylab="ADA titer",main="Day08 Comparison")
boxplot(ADA~Treatment, data=Dlevel10[which(Dlevel10$Day=="15"),],ylab="ADA titer",main="Day15 Comparison")
boxplot(ADA~Treatment, data=Dlevel10[which(Dlevel10$Day=="22"),],ylab="ADA titer",main="Day22 Comparison")
boxplot(ADA~Treatment, data=Dlevel10[which(Dlevel10$Day=="29"),],ylab="ADA titer",main="Day29 Comparison")

###at Day 29 only##
Day29ADA =c(Dlevel10[which(Dlevel10$Treatment=="ABV428" & Dlevel10$Day=="29"),]$ADA,
    Dlevel10[which(Dlevel10$Treatment=="ABV794" & Dlevel10$Day=="29"),]$ADA) 
TREAT=c(rep("ABV428",8),rep("ABV794",6))
Dmat29=data.frame(Day29ADA,TREAT)
names(Dmat29)=c("ADA","Trt")
 
 wil.fit29=wilcox.test(Dlevel10[which(Dlevel10$Treatment=="ABV428" & Dlevel10$Day=="29"),]$ADA, 
             Dlevel10[which(Dlevel10$Treatment=="ABV794" & Dlevel10$Day=="29"),]$ADA) 
wil.fit29

####at Day 22

wil.fit22=wilcox.test(Dlevel10[which(Dlevel10$Treatment=="ABV428" & Dlevel10$Day=="22"),]$ADA, 
                    Dlevel10[which(Dlevel10$Treatment=="ABV794" & Dlevel10$Day=="22"),]$ADA, paired=FALSE) 
wil.fit22

####at Day 15

wil.fit15=wilcox.test(Dlevel10[which(Dlevel10$Treatment=="ABV428" & Dlevel10$Day=="15"),]$ADA, 
                    Dlevel10[which(Dlevel10$Treatment=="ABV794" & Dlevel10$Day=="15"),]$ADA, paired=FALSE) 
wil.fit15

####at Day 8

wil.fit8=wilcox.test(Dlevel10[which(Dlevel10$Treatment=="ABV428" & Dlevel10$Day=="08"),]$ADA, 
                    Dlevel10[which(Dlevel10$Treatment=="ABV794" & Dlevel10$Day=="08"),]$ADA, paired=FALSE) 
wil.fit8
##########boxplots
par(mfrow=c(1,1))
boxplot((ADA)~Day+Treatment,data=Dlevel10,ylab="ADA titer",xlab="treatment levels",col="orange",border="brown")
par(mfrow=c(2,1))
boxplot(boxplot((ADA)~Day+Treatment,data=Dlevel10,ylab="ADA titer",xlab="treatment levels",col="orange",border="brown"))
boxplot(as.numeric(ADA)~level,data=Dmat1, ylab="ADA titer",xlab="ABV428 levels")
boxplot(as.numeric(ADA)~level,data=Dmat2, ylab="ADA titer",xlab="ABV621 levels")
boxplot(as.numeric(ADA)~level,data=Dmat3,ylab="ADA titer",xlab="ABV794 levels")





