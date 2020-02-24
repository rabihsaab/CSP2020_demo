###TPPmin is not used in program: it is just a reference to the minimal historical rate upon which 
#TPPgrey is estimated (which is the min requirement needed to be considered for the drug to be worthwhile
# for next stage
#	 
#	 
#	 
#	Go if posterior prob of exceeding base TPPgo>=goprob
#	No-Go if posterior prob of falling short of TPPgrey>=nogoprob
#	Prior of theta in binomial(n,theta)= Beta(alpha, beta)

library(Hmisc)
gonogo=function(n, TPPmin, TPPgrey,TPPgo,alpha=0.04,beta=0.06,
goprob=0.8,nogoprob=0.8)
{
 y=0:n
posta=c()
nogopost=c()
for (i in 1:n){
 posta[i]  = 1-pbeta(TPPgo, alpha+y[i]  ,beta+n -y[i]  )
nogopost[i]=pbeta(TPPgrey, alpha+y[i]  ,beta+n -y[i]  )
}
 
go=y[which(posta>=goprob)[1]]
nogo=y[which(nogopost==max(nogopost[nogopost<nogoprob]))]
grey=y[which(posta>=TPPgrey)[1]]
ptypeII=pbinom((nogo-1),n,TPPgo)   #Prob of False NO-GO when TPPgo is true
power=1-pbinom((go-1),n,TPPgo)  ###probability of Go if TPPgo is TRUE
        
go.grey.tppgrey=1-pbinom((nogo-1),n,TPPgrey)  #probability of being in grey zone or
                                                #go if TPPgrey was true
 
 
return(c(go=go, nogo=  nogo-1  ,
confgo=posta[(posta>=goprob)][1],confnogo=min(nogopost[nogopost>=nogoprob]),
ptypeII=ptypeII,power=power,go.grey.tppgrey=go.grey.tppgrey))
}
##outcome: >=go /n , to consider going to next phase
#### if <= nogo/n, then halt trial at current enrolment (it is not worhtwhile to go)
##### grey zone between go and nogo responders out of n
  #gonogo(16,0.3,0.1 ,0.2,goprob=0.7,nogoprob=0.8,alpha=0.04,beta=0.06)

temp=function(min, max){
  OP=c()
  for (i in min:max){
    
    tempOP =c(i,gonogo(i, 0.19 ,0.3,0.4))
    OP=rbind(OP,tempOP)
  }
  return(OP)
}
temp(12,24)