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

dLaneway <- openxlsx::read.xlsx("data/raw/20250401_UBC_CustomLanewayReport.xlsx")
#dLaneway <- read.dta13("data/raw/Laneway list 2015-23.dta")
print(names(dLaneway))
dLaneway <- data.table(dLaneway)[,.(Roll,Jur)]
print(table(dLaneway[,Jur]))
dLaneway[,Jur==NULL]
dLaneway[,Roll:=as.character(as.numeric(Roll))]
dLaneway[,hasLaneway:=1]
d24[,Roll:=as.character(roll_number)]

d24 <- merge(d24,dLaneway,by="Roll",all.x=TRUE,all.y=TRUE)
print(table(d24[,.(is.na(hasLaneway),is.na(rollEnd24))])) # about 50% of laneways -- not bad as all 30 50
d24[,hasLaneway:=ifelse(is.na(hasLaneway),0,hasLaneway)]

# get max transaction date by roll rumber
dMaxSale <- fread("data/derived/maxSale.csv")
d24 <- merge(d24,dMaxSale,by.x="ROLL_NUMBER24",by.y="ROLL_NUMBER",all.x=TRUE)
d24[,duplex:=grepl("Duplex,",ACTUAL_USE_DESCRIPTION)]
d24[,basement:=grepl("Residential Dwelling with Suite",ACTUAL_USE_DESCRIPTION)]
d24[,single:=grepl("Single Family Dwelling",ACTUAL_USE_DESCRIPTION)]
d24[,type:=ifelse(duplex==1,"duplex",ifelse(hasLaneway==1,"laneway",ifelse(basement==1,"basement",ifelse(single==1,"single","other"))))]
print(table(d24[,type]))
print(summary(d24[,MB_Year_Built]))
d24[,spec:=0]
d24[,sale2024:=0]
FIRST_BUILT <- 2017 # results weirdly stronger with 2016 than prior years -- dearth of laneway early messing with fixed effect?
for (y in seq(FIRST_BUILT,2023)) {
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
shares <- data.table(nbhd=character(),year=numeric(),thirty=integer(),type=character(),shareType=numeric(),typeSpec=numeric(),specShare=numeric(),priceLag=numeric())
dsales <- fread("data/derived/sales.csv")
dsales <- merge(dsales,d24[,.(ROLL_NUMBER24,MB_Year_Built,type,NEIGHBOURHOOD,thirty)],by.x="ROLL_NUMBER",by.y="ROLL_NUMBER24")
dsales <- dsales[CONVEYANCE_DATE==MB_Year_Built | (CONVEYANCE_DATE==MB_Year_Built+1) ]
KEEPLAG <- 2
for (y in seq(FIRST_BUILT,2023)) {
	for (n in unique(d24$NEIGHBOURHOOD)) {
		#for (s in c(0,1)) {
		for (s in c(1)) {
			dsub <- d24[NEIGHBOURHOOD==n & MB_Year_Built==y & thirty==s  ]
			for (t in c("single","duplex","laneway","basement")) {
				dss <- dsales[NEIGHBOURHOOD==n & CONVEYANCE_DATE<y & CONVEYANCE_DATE>=(y-KEEPLAG) | CONVEYANCE_DATE==(y-3)  & thirty==s & type==t]
				shares <- rbind(shares,data.table(nbhd=n,year=y,thirty=s,type=t,shareType=dsub[,mean(type==t)],typeSpec=dsub[type==t,mean(spec==1)],specShare=dsub[spec==1,mean(type==t)],priceLag=dss[,median((CONVEYANCE_PRICE),na.rm=TRUE)]))
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
print(summary(feols(landValue~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
print(summary(feols(log(landValue)~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
dnew[,laneShare:=mean(type=="laneway"),by=.(NEIGHBOURHOOD,MB_Year_Built,thirty)]
dnew[,singleShare:=mean(type=="single"),by=.(NEIGHBOURHOOD,MB_Year_Built,thirty)]
dnew[,duplexShare:=mean(type=="duplex"),by=.(NEIGHBOURHOOD,MB_Year_Built,thirty)]
dnew[,ownShare:=ifelse(type=="laneway",laneShare,ifelse(type=="single",singleShare,duplexShare))]


# note individual reg actually lower t-stat than group level
print(summary(feols(spec ~ ownShare + log(improvementValue),data=dnew)))
print(summary(feols(spec ~ ownShare + log(improvementValue)| MB_Year_Built^NEIGHBOURHOOD +type^MB_Year_Built,data=dnew)))
print("fraction spec among new by rounded 2016 improvement value")
STEP <- 10000
ggplot(dnew[,.(n=.N,spec=mean(spec)),by=.(improvement=round(improvementValue/STEP)*STEP)],aes(x=improvement,y=spec,size=n)) + geom_point()
ggsave("text/specImprovement.png")
# do it with year_built
ggplot(dnew[MB_year_built<2016,.(n=.N,spec=mean(spec)),by=.(yearBuilt=MB_year_built)],aes(x=yearBuilt,y=spec,size=n)) + geom_point()
ggsave("text/specYearBuilt.png")
# now effective year
ggplot(dnew[MB_effective_year<2016,.(n=.N,spec=mean(spec)),by=.(effectiveYear=MB_effective_year)],aes(x=effectiveYear,y=spec,size=n)) + geom_point()
ggsave("text/specEffectiveYear.png")
print(dnew[order(MB_year_built),.(.N,mean(spec)),by=.(MB_year_built)])
print(dnew[order(MB_effective_year),.(.N,mean(spec)),by=.(MB_effective_year)])
print(dnew[improvementValue>400000])

print(summary(feols(typeSpec~shareType |year ^  type+nbhd,data=shares)))
print(summary(feols(typeSpec~shareType |nbhd^type+year,data=shares,cluster="nbhd")))
print(summary(feols(typeSpec~shareType |year^nbhd + type^year,data=shares,cluster="nbhd")))
shares[,nbhdType:=paste0(nbhd,"_",type)]
print(summary(feols(typeSpec~shareType |year^type + nbhd^year ,data=shares,cluster="nbhdType")))
print(summary(feols(typeSpec~shareType|year^type + nbhd^year + nbhd^type ,data=shares,cluster="nbhd")))
print(summary(feols(specShare~ priceLag |year^type + nbhd^year ,data=shares,cluster="nbhd")))
# Funny sales result
print(shares[,mean(priceLag,na.rm=TRUE),by=.(type,year)])
print(shares[,mean(priceLag,na.rm=TRUE),by=.(type,nbhd)])
# is it desirable to have neighbourhood-type dummies?

shares[,rr:=runif(nrow(shares))]
x <- feols(shareType ~ rr|type+year+nbhd,data=shares)
print(c(var(x$residuals),var(shares$shareType,na.rm=TRUE)))
x <- feols(shareType ~ rr|type^year + nbhd^year,data=shares)
print(c(var(x$residuals),var(shares$shareType,na.rm=TRUE)))
x <- feols(shareType ~ rr|type^year+type^nbhd+nbhd^year,data=shares)
print(c(var(x$residuals),var(shares$shareType,na.rm=TRUE)))
x <- feols(shareType ~ rr|nbhd^type,data=shares)
print(c(var(x$residuals),var(shares$shareType,na.rm=TRUE)))


ggplot(shares[type=="laneway",.(type,shareType,typeSpec,year),],aes(x=shareType,y=typeSpec,color=year)) +
    geom_point() 
ggsave("text/shareCustom.png")

ggplot(shares[,.(shareType,typeSpec,nbhd)],aes(x=shareType,y=typeSpec,color=nbhd)) + geom_point() 
ggsave("text/shareSpecType.png")


