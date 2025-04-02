# main.R
# R to analyze spec/custom choice
# Tom Davidoff

library(data.table)
library(readstata13)
library(fixest)
library(ggplot2)

do16 <- 0
if (do16==1)  {
    (source("scripts/get16.R"))
}
d16 <- fread("data/derived/baseline16.csv")
print(summary(d16))

do24 <- 0
if (do24==1)  {
    (source("scripts/get24.R"))
}
d24 <- fread("data/derived/use24.csv")
print(summary(d24))


# merge on roll number fragment

d24[,rollStart:=as.numeric(rollStart)]
d24[,rollEnd24:=as.numeric(rollEnd24)]

d16[,rollStart:=floor(roll_number/10000)]
d16[,rollEnd16:=roll_number %% 10000]

d24 <- merge(d24,d16,by.x=c("rollStart"),by.y=c("rollStart")) 
print(summary(d24))

dLaneway <- read.dta13("data/raw/Laneway list 2015-23.dta")
print(names(dLaneway))
dLaneway <- data.table(dLaneway)[,.(roll_num,year_built,jur)]
print(table(dLaneway[,jur]))
dLaneway <- dLaneway[jur==200]
dLaneway[,jur==NULL]
print(dLaneway[,roll_num])
dLaneway[,roll_num:=as.character(as.numeric(roll_num))]
dLaneway[,hasLaneway:=1]
d24[,roll_num:=as.character(roll_number)]

d24 <- merge(d24,dLaneway,by="roll_num",all.x=TRUE,all.y=TRUE)
print(table(d24[,.(is.na(hasLaneway),is.na(rollEnd24))])) # about 50% of laneways -- not bad as all 30 50
d24[,hasLaneway:=ifelse(is.na(hasLaneway),0,hasLaneway)]

# get max transaction date by roll rumber
dMaxSale <- fread("data/derived/maxSale.csv")
d24 <- merge(d24,dMaxSale,by.x="ROLL_NUMBER24",by.y="ROLL_NUMBER",all.x=TRUE)
d24[,duplex:=grepl("Duplex,",ACTUAL_USE_DESCRIPTION)]
d24[,type:=ifelse(duplex==1,"duplex",ifelse(hasLaneway==1,"laneway","single"))]
print(summary(d24[,MB_Year_Built]))
d24[,spec:=0]
d24[,sale2024:=0]
FIRST_BUILT <- 2016 # results weirdly stronger with 2016 than prior years -- dearth of laneway early messing with fixed effect?
for (y in seq(FIRST_BUILT,2022)) {
    #d24[MB_Year_Built==y & (get(paste0("sale",y))==1 | get(paste0("sale",y+1))==1 | get(paste0("sale",y+2))==1  ),spec:=1]
    d24[MB_Year_Built==y & (get(paste0("sale",y))==1 | get(paste0("sale",y+1))==1  ),spec:=1]
    #d24[MB_Year_Built==y & ( get(paste0("sale",y+1))==1  ),spec:=1]
}
for (y in seq(FIRST_BUILT,2023)) {
    print(y)
    print(table(d24[MB_Year_Built==y,.(type,spec)]))
}

print(table(d24[MB_Year_Built>2016 & MB_Year_Built<2023,.(type,spec,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==1 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==0 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==1 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,NEIGHBOURHOOD)]))
print(table(d24[thirty==0 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,NEIGHBOURHOOD)]))

# Main regression
# Dummy for spec vs custom on property type dummies and share 
shares <- data.table(nbhd=character(),year=numeric(),thirty=integer(),type=character(),shareType=numeric(),typeSpec=numeric(),specShare=numeric())
for (y in seq(FIRST_BUILT,2023)) {
	for (n in unique(d24$NEIGHBOURHOOD)) {
		#for (s in c(0,1)) {
		for (s in c(1)) {
			dsub <- d24[NEIGHBOURHOOD==n & MB_Year_Built==y & thirty==s  ]
			for (t in c("single","duplex","laneway")) {
			    shares <- rbind(shares,data.table(nbhd=n,year=y,thirty=s,type=t,shareType=dsub[,mean(type==t)],typeSpec=dsub[type==t,mean(spec==1)],specShare=dsub[spec==1,mean(type==t)]))
			}
		    }
        }
}
print(summary(shares))


dnew <- d24[MB_Year_Built>FIRST_BUILT & MB_Year_Built<2023]
print(dnew[MB_Year_Built>2016,mean(spec),by=type])
print(dnew[MB_Year_Built>2016,mean(spec),by=MB_Year_Built])
print(dnew[MB_Year_Built>2016,quantile(improvementValue,na.rm=TRUE),by=.(spec)])
print(dnew[MB_Year_Built>2016,mean(improvementValue,na.rm=TRUE),by=.(spec)])
print(summary(feols(improvementValue~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
print(summary(feols(log(improvementValue)~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
dnew[,laneShare:=mean(type=="laneway"),by=.(NEIGHBOURHOOD,MB_Year_Built,thirty)]
dnew[,singleShare:=mean(type=="single"),by=.(NEIGHBOURHOOD,MB_Year_Built,thirty)]
dnew[,duplexShare:=mean(type=="duplex"),by=.(NEIGHBOURHOOD,MB_Year_Built,thirty)]
dnew[,ownShare:=ifelse(type=="laneway",laneShare,ifelse(type=="single",singleShare,duplexShare))]


# note individual reg actually lower t-stat than group level
print(summary(feols(spec ~ ownShare + log(improvementValue)| MB_Year_Built ^ NEIGHBOURHOOD ^ thirty+type,data=dnew)))

print(summary(feols(typeSpec~shareType |year ^  type+nbhd+thirty,data=shares)))
print(summary(feols(typeSpec~shareType +i(year) + i(type)|nbhd,data=shares,cluster="nbhd")))

ggplot(shares[type=="laneway",.(type,shareType,typeSpec,year),],aes(x=shareType,y=typeSpec,color=year)) +
    geom_point() 
ggsave("text/shareCustom.png")
