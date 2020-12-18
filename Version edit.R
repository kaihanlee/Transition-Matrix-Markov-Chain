###########################################################################################
q=0.8			#parameter q

quality="low"	#input quality class

nl=3			#n for low
pl=5*10^-4		#p for low
replacel=410	#price to replace low quality phones

nm=4			#n for medium
pm=2*10^-4		#p for medium
replacem=850	#price to replace medium quality phones

nh=4			#n for high
ph=1*10^-4		#p for high
replaceh=950	#price to replace high quality phones

###########################################################################################

#declare matrices
lowmatrix=matrix(0,nrow=nl,ncol=nl,dimnames=list(c(0:(nl-1)),c(0:(nl-1))),byrow=TRUE)
medmatrix=matrix(0,nrow=nm,ncol=nm,dimnames=list(c(0:(nm-1)),c(0:(nm-1))),byrow=TRUE)
highmatrix=matrix(0,nrow=nh,ncol=nh,dimnames=list(c(0:(nh-1)),c(0:(nh-1))),byrow=TRUE)

#loop for elements in low quality matrix
if(quality=="low"){
for(i in 1:nl){
for(j in 1:nl){

if(i==j) {
if(i==1 && j==1) {
lowmatrix[i,j]=(1-pl)+pl*(1-q^i)
} else {
lowmatrix[i,j]=1-pl
}
} 

else if(j-i==1){
lowmatrix[i,j]=pl*q^i
} 

else if(i-j>0) {
if(j==1){
if(i==nl){
lowmatrix[i,j]=pl
} else {
lowmatrix[i,j]=pl*(1-q^i)
}
} else {
lowmatrix[i,j]=0
}
}

else {
next
}

}
}

}

#loop for elements in medium quality matrix
if(quality=="med"){
for(i in 1:nm){
for(j in 1:nm){

if(i==j) {
if(i==1 && j==1) {
medmatrix[i,j]=(1-pm)+pm*(1-q^i)
} else {
medmatrix[i,j]=1-pm
}
} 

else if(j-i==1){
medmatrix[i,j]=pm*q^i
} 

else if(i-j>0) {
if(j==1){
if(i==nm){
medmatrix[i,j]=pm
} else {
medmatrix[i,j]=pm*(1-q^i)
}
} else {
medmatrix[i,j]=0
}
}

else {
next
}

}
}

}

#loop for elements in high quality matrix
if(quality=="high"){
for(i in 1:nh){
for(j in 1:nh){

if(i==j) {
if(i==1 && j==1) {
highmatrix[i,j]=(1-ph)+ph*(1-q^i)
} else {
highmatrix[i,j]=1-ph
}
} 

else if(j-i==1){
highmatrix[i,j]=ph*q^i
} 

else if(i-j>0) {
if(j==1){
if(i==nh){
highmatrix[i,j]=ph
} else {
highmatrix[i,j]=ph*(1-q^i)
}
} else {
highmatrix[i,j]=0
}
}

else {
next
}

}
}

}

###########################################################################################

k<-30000 # number of steps to simulate
trials<-1000

#declare vectors for states
level1<-rep(0,(k+1))
level2<-rep(0,(k+1))
level3<-rep(0,(k+1))
level4<-rep(0,(k+1))

#declare vector for low quality stationary distribution
lowstndist<-rep(0,3)

for(j in 1:trials){

X<-1		#initial state of the Markov Chain
level1[1]<-level1[1]+1		#Record the initial state

for(i in 1:k){
Y<-runif(1)
r<-lowmatrix[X[i],]
r<-cumsum(r)

if(Y<=r[1]){
X[i+1]= 1
#Update observed number of simulation at state 1 (0 faults)
level1[i+1]=level1[i+1]+1
}

else if(Y<=r[2]){
X[i+1]=2
#Update observed number of simulation at state 2 (1 faults)
level2[i+1]=level2[i+1]+1
}

else {
X[i+1] = 3
#Update observed number of simulation at state 3 (2 faults)
level3[i+1]=level3[i+1]+1
}

}
#Record last state
lowstndist[X[k+1]]<-lowstndist[X[k+1]]+1
}

lowstndist=lowstndist/trials
lowstndist		#low quality stationary distribution

#to get probability of each state
level1=level1/trials
level2=level2/trials
level3=level3/trials

#to prove that it converges to the stationary distribution that we have calculated
plot(level1, type="l", xlim=c(0,k+1), ylim=c(0,1), xlab="Steps",ylab="Proportion", col="red",lwd=2,main="Low Quality Stationary Distribution")
lines(level2, col="blue", lwd=2)
lines(level3, col="yellow",lwd=2)
legend(15,1, c("No faults", "1 fault", "2 faults"),col=c("red","blue","yellow"), lwd=c(2,2,2))
lines(125/289*rep(1,k+1))
lines(100/289*rep(1,k+1))
lines(64/289*rep(1,k+1))

###############################################################################################

#declare vector for medium quality stationary distribution
medstndist<-rep(0,4)

for(j in 1:trials){

X<-1		#initial state of the Markov Chain
level1[1]<-level1[1]+1		#Record the initial state

for(i in 1:k){
Y<-runif(1)
r<-medmatrix[X[i],] 
r<-cumsum(r)

if(Y<=r[1]){
X[i+1]= 1
#Update observed number of simulation at state 1 (0 faults)
level1[i+1]=level1[i+1]+1
}

else if(Y<=r[2]){
X[i+1]=2
#Update observed number of simulation at state 2 (1 faults)
level2[i+1]=level2[i+1]+1
}

else if(Y<=r[3]){
X[i+1] = 3
#Update observed number of simulation at state 3 (2 faults)
level3[i+1]=level3[i+1]+1
}

else {
X[i+1] = 4
#Update observed number of simulation at state 4 (3 faults)
level4[i+1]=level4[i+1]+1
}

}
#Record last state
medstndist[X[k+1]]<-medstndist[X[k+1]]+1
}

medstndist=medstndist/trials
medstndist		#medium quality stationary distribution

#to get probability of each state
level1=level1/trials
level2=level2/trials
level3=level3/trials
level4=level4/trials

#to prove that it converges to the stationary distribution that we have calculated
plot(level1, type="l", xlim=c(0,k+1), ylim=c(0,1), xlab="Steps",ylab="Proportion", col="red",lwd=2,main="Medium Quality Stationary Distribution")
lines(level2, col="blue", lwd=2)
lines(level3, col="yellow",lwd=2)
lines(level4, col="green",lwd=2)
legend(15,1, c("No faults", "1 fault", "2 faults","3 faults"),col=c("red","blue","yellow","green"), lwd=c(2,2,2,2))
lines(0.38848*rep(1,k+1))
lines(0.310783*rep(1,k+1))
lines(0.198901*rep(1,k+1))
lines(0.101836*rep(1,k+1))

###############################################################################################

#declare vector for high quality stationary distribution
highstndist<-rep(0,4)

for(j in 1:trials){

X<-1		#initial state of the Markov Chain
level1[1]<-level1[1]+1		#Record the initial state

for(i in 1:k){
Y<-runif(1)
r<-highmatrix[X[i],] 
r<-cumsum(r)

if(Y<=r[1]){
X[i+1]= 1
#Update observed number of simulation at state 1 (0 faults)
level1[i+1]=level1[i+1]+1
}

else if(Y<=r[2]){
X[i+1]=2
#Update observed number of simulation at state 2 (1 faults)
level2[i+1]=level2[i+1]+1
}

else if(Y<=r[3]){
X[i+1] = 3
#Update observed number of simulation at state 3 (2 faults)
level3[i+1]=level3[i+1]+1
}

else {
X[i+1] = 4
#Update observed number of simulation at state 4 (3 faults)
level4[i+1]=level4[i+1]+1
}

}
#Record last state
highstndist[X[k+1]]<-highstndist[X[k+1]]+1
}

highstndist=highstndist/trials
highstndist		#high quality stationary distribution

#to get probability of each state
level1=level1/trials
level2=level2/trials
level3=level3/trials
level4=level4/trials

#to prove that it converges to the stationary distribution that we have calculated
plot(level1, type="l", xlim=c(0,k+1), ylim=c(0,1), xlab="Steps",ylab="Proportion", col="red",lwd=2,main="High Quality Stationary Distribution")
lines(level2, col="blue", lwd=2)
lines(level3, col="yellow",lwd=2)
lines(level4, col="green",lwd=2)
legend(15,1, c("No faults", "1 fault", "2 faults","3 faults"),col=c("red","blue","yellow","green"), lwd=c(2,2,2,2))
lines(0.388479*rep(1,k+1))
lines(0.310780*rep(1,k+1))
lines(0.198899*rep(1,k+1))
lines(0.101842*rep(1,k+1))

###########################################################################################

#for q=0.8

#loop forlow, medium, and high quality classes
for(i in 1:nl){
if(i!=nl){
expectedcostlow[i]=(pl*(1-q^i)*replacel+pl*q^i*q*100*2^i)*lowstndist[i]
} else if(i==nl) {
expectedcostlow[i]=(pl*replacel)*lowstndist[i]
}
}
expectedcostlow=sum(expectedcostlow)	#sum of costs for each state
expectedcostlow

for(i in 1:nm){
if(i!=nm){
expectedcostmed[i]=(pm*(1-q^i)*replacem+pm*q^i*q*100*2^i)*medstndist[i]
} else if(i==nm) {
expectedcostmed[i]=(pm*replacem)*medstndist[i]
}
}
expectedcostmed=sum(expectedcostmed)	#sum of costs for each state
expectedcostmed

for(i in 1:nh){
if(i!=nh){
expectedcosthigh[i]=(ph*(1-q^i)*replaceh+ph*q^i*q*100*2^i)*highstndist[i]
} else if(i==nh) {
expectedcosthigh[i]=(ph*replaceh)*highstndist[i]
}
}
expectedcosthigh=sum(expectedcosthigh)	#sum of costs for each state
expectedcosthigh

totalexpectedcost=sum(expectedcostlow,expectedcostmed,expectedcosthigh)

###########################################################################################

m=0.01		#intervals between each points on the plane
q = seq(0,1,m)	#declare sequence with equal intervals
z = (1/m) + 1	#range

#declare vectors for expected costs
expectedcostlow=rep(0,nl)
expectedcostmed=rep(0,nm)
expectedcosthigh=rep(0,nh)
expectedcostlowq=rep(0,z)
expectedcostmedq=rep(0,z)
expectedcosthighq=rep(0,z)
totalexpectedcostq=rep(0,z)

#declare vector for new temporary stationary distribution
pil=rep(0,nl)
pim=rep(0,nm)
pih=rep(0,nh)

#get different low quality expected cost for sequence q
for(f in 1:z){
for(i in 1:nl){

#refer to report for derivation
pil[1]=1/(1+q[f]+q[f]^3)
pil[2]=1/((1/q[f])+1+q[f]^2)
pil[3]=(q[f]^2)/((1/q[f])+1+q[f]^2)

if(i!=nl){
expectedcostlow[i]=(pl*(1-q[f]^i)*replacel+pl*q[f]^i*q[f]*100*2^i)*pil[i]
}

else {
expectedcostlow[i]=(pl*replacel)*pil[i]
}
expectedcostlowq[f]=sum(expectedcostlow)
}
}

#plot graph for expectedcostlowq against q
plot(q,expectedcostlowq,type="l",main="Optimal Cost (Low)")
#get minimum low quality expected cost
min(expectedcostlowq)
#get q value for minimum low quality expected cost
q[which.min(expectedcostlowq)]

#get different medium quality expected cost for sequence q
for(g in 1:z){
for(i in 1:nm){

#refer to report for derivation
pim[1]=1/(1+q[g]+q[g]^3+q[g]^6)
pim[2]=1/((1/q[g])+1+q[g]^2+q[g]^5)
pim[3]=(q[g]^2)/((1/q[g])+1+q[g]^2+q[g]^5)
pim[4]=(q[g]^5)/((1/q[g])+1+q[g]^2+q[g]^5)

if(i!=nm){
expectedcostmed[i]=(pm*(1-q[g]^i)*replacem+pm*q[g]^i*q[g]*100*2^i)*pim[i]
}
else if(i==nm){
expectedcostmed[i]=(pm*replacem)*pim[i]
}
expectedcostmedq[g]=sum(expectedcostmed)
}
}

#plot graph for expectedcostmedq against q
plot(q,expectedcostmedq,type="l",main="Optimal Cost (Medium)")
#get minimum medium quality expected cost
min(expectedcostmedq)
#get q value for minimum medium quality expected cost
q[which.min(expectedcostmedq)]

#get different high quality expected cost for sequence q
for(h in 1:z){
for(i in 1:nh){

#refer to report for derivation
pih[1]=1/(1+q[h]+q[h]^3+q[h]^6)
pih[2]=1/((1/q[h])+1+q[h]^2+q[h]^5)
pih[3]=(q[h]^2)/((1/q[h])+1+q[h]^2+q[h]^5)
pih[4]=(q[h]^5)/((1/q[h])+1+q[h]^2+q[h]^5)

if(i!=nh){
expectedcosthigh[i]=(ph*(1-q[h]^i)*replaceh+ph*q[h]^i*q[h]*100*2^i)*pih[i]
}
else if(i==nh){
expectedcosthigh[i]=(ph*replaceh)*pih[i]
}
expectedcosthighq[h]=sum(expectedcosthigh)
}
}

#plot graph for expectedcosthighq against q
plot(q,expectedcosthighq,type="l",main="Optimal Cost (High)")
#get minimum high quality expected cost
min(expectedcosthighq)
#get q value for minimum high quality expected cost
q[which.min(expectedcosthighq)]

#sum of optimal cost of each class
for(i in 1:z){
totalexpectedcostq[i]=0.25*expectedcostlowq[i]+0.52*expectedcostmedq[i]+0.23*expectedcosthighq[i]
}
#plot graph for total optimum cost
plot(q,totalexpectedcostq,type="l",main="Total Optimal Cost")

#get total optimum cost
min(totalexpectedcostq)
#get q value for minimum total expected cost
q[which.min(totalexpectedcostq)]

totalexpectedcost=0.25*min(expectedcostlowq)+0.52*min(expectedcostmedq)+0.23*min(expectedcosthighq)
totalexpectedcost


