setwd("/Users/brittneyshaull/Documents/UCDavis/Stat250/Delays1987_2013/")
time1=proc.time()
cname1=list.files(pattern="198.\\.csv$")
cname2=list.files(pattern="199.\\.csv$")
cname3=c("2000.csv", "2003.csv", "2004.csv", "2005.csv", "2006.csv", "2007.csv")
cname=c(cname1, cname2, cname3)


name1=list.files(pattern="^2008_")
name2=list.files(pattern="^2009_")
name3=list.files(pattern="^2010_")
name4=list.files(pattern="^2011_")
name5=list.files(pattern="^2012_")
name=c(name1, name2, name3, name4, name5)

library(parallel)
c=detectCores()

freq15=function(fnames){
	cmd="grep -v ArrDelay %s | cut -f 15 -d , | sort -n | uniq -c"
	cmd=sprintf(cmd, paste(fnames, collapse=" "))
	system(cmd, intern=TRUE)
}

freq44=function(fnames){
	cmd="grep -v ARR_DELAY_NEW %s | cut -f 44 -d , | sort -n | uniq -c"
	cmd=sprintf(cmd, paste(fnames, collapse=" "))
	system(cmd, intern=TRUE)
}
#makes cluster for however many cores that exist in the machine
cl=makeCluster(c, type="FORK")

##Getting frequency table for up until 2008
clusp=clusterSplit(cl,cname)
freqtab=clusterApply(cl,clusp,freq15)
x1=textConnection(freqtab[[1]])
x2=textConnection(freqtab[[2]])
tb1=read.table(x1)
tb2=read.table(x2)
x=tb1$V1
y=tb1$V2
tb1=as.table(x)
names(tb1)=y
x=tb2$V1
y=tb2$V2
tb2=as.table(x)
names(tb2)=y
i=setdiff(names(tb2),names(tb1))
tb1[i]=0
tb1[names(tb2)]=tb1[names(tb2)]+tb2



#Getting Frequency table for 2008 and after
clusp=clusterSplit(cl,name)
freqtab=clusterApply(cl,clusp,freq44)
x1=textConnection(freqtab[[1]])
x2=textConnection(freqtab[[2]])
b1=read.table(x1)
b2=read.table(x2)
x=b1$V1
y=b1$V2
b1=as.table(x)
names(b1)=y
x=b2$V1
y=b2$V2
b2=as.table(x)
names(b2)=y
i=setdiff(names(b2),names(b1))
b1[i]=0
b1[names(b2)]=b1[names(b2)]+b2


stopCluster(cl)

#merging tables together
i=setdiff(names(b1), names(tb1))
tb1[i]=0
tb1[names(b1)]=tb1[names(b1)]+b1
V1=as.vector(tb1)
V2=as.integer(names(tb1))
freq=cbind(V1,V2)
freq[is.na(freq)]=0 #gets rid of the NAs
freq=freq[order(freq[ ,2]), ]

#calculating mean
numfreq=nrow(freq)
sum_track=0
i=1
while (i<=numfreq) {
	mult=freq[i,1]*freq[i,2] #multiplying row elements together
	sum_track=sum_track+mult #summing the multiplied elements across all rows
	i=i+1
}
total_obs=sum(freq[ ,1])
mean=sum_track/total_obs  #mean is the sum of the multiplied elements divided by the total number of observations
#Calculating Median
#Median is where 50% of observations lie above and below that number. This differs depending on whether or not the number of observations is even or odd.  In our case the only time this matters is when the number of observations is even and on the boundary because then the two numbers need to be averaged. I use an if statement to account for this case. Otherwise the median can be calculated by summing the frequency points in the first column as long as the sum is less than half. Then the next arrival delay in the following row will be the median.
half=total_obs/2
sum_v1=0
j=1
while (sum_v1<half) {  
        sum_v1=sum_v1+freq[j,1]
        j=j+1
        }
median=freq[j+1,2]
if (is.integer(half)==TRUE & sum_v1+freq[j+1,1]==half) {
        median=(freq[j+1,2]+freq[j+2,2])/2 }

#calculating Standard deviation
k=1
sum_sq=0
while (k<=numfreq) {
        sum_sq=sum_sq+freq[k,1]*((freq[k,2]-mean)^2)
        k=k+1}
st_dev=(sum_sq/(total_obs-1))^(1/2)

time2=proc.time()-time1

bshaull_assign2_method1=list(time = time2, results = c(mean = mean, median = median, sd = st_dev),
     system = Sys.info(),  session = sessionInfo())
save(bshaull_assign2_method1, file="bshaull_assign2_method1.rda")
