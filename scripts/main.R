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
d24 <- d24[laneok==1]

dLaneway <- openxlsx::read.xlsx("data/raw/20250401_UBC_CustomLanewayReport.xlsx")
#dLaneway <- read.dta13("data/raw/Laneway list 2015-23.dta")
print(names(dLaneway))
dLaneway <- data.table(dLaneway)[,.(Roll,Jur,YearBuilt)]
print(table(dLaneway[,Jur]))
dLaneway[,Jur==NULL]
dLaneway[,Roll:=as.character(as.numeric(Roll))]
dLaneway[,hasLaneway:=1]
d24[,Roll:=as.character(roll_number)]

d24 <- merge(d24,dLaneway,by="Roll",all.x=TRUE,all.y=TRUE)
print(table(d24[,.(is.na(hasLaneway),is.na(rollEnd24))])) # about 50% of laneways -- not bad as all 30 50
print(summary(d24[hasLaneway==1]))
print(summary(d24[,YearBuilt==MB_Year_Built]))
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
FIRST_BUILT <- 2010 # Note can do early for the non-duplex stuff, and no duplex really pre-18
LAST_BUILT <- 2018 # pre-duplex era?
for (y in seq(FIRST_BUILT,LAST_BUILT)) {
    d24[MB_Year_Built==y & (get(paste0("sale",y))==1 | get(paste0("sale",y+1))==1  ),spec:=1]
}

d24[,laneway:=ifelse(type=="laneway",1,0)]
print("mean spec by year")
for (y in seq(FIRST_BUILT,LAST_BUILT)) {
	print(y)
	print(d24[MB_Year_Built==y , mean(spec)])
	print(d24[MB_Year_Built==y  & thirty==1, mean(spec)])
	print(table(d24[MB_Year_Built==y,.(type,spec)]))
	print(d24[MB_Year_Built==y,mean(laneway),by=spec])
	print(d24[MB_Year_Built==y & thirty==1,mean(laneway),by=spec])
}

d24 <- d24[MB_Year_Built>=FIRST_BUILT & MB_Year_Built<=LAST_BUILT]
print("were spec late to laneway?")
print(summary(feols(type=="laneway" ~ spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=d24[MB_Year_Built>2008])))
print(summary(feols(type=="laneway" ~ spec*MB_Year_Built|NEIGHBOURHOOD ,data=d24[MB_Year_Built>2008])))
print(summary(d24[,.(laneway,spec)]))

q("no")

print(table(d24[MB_Year_Built>2016 & MB_Year_Built<2023,.(type,spec,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==1 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==0 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==1 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,NEIGHBOURHOOD)]))
print(table(d24[thirty==0 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,spec,NEIGHBOURHOOD)]))

# Main regression
# Dummy for spec vs custom on property type dummies and share 
shares <- data.table(nbhd=character(),year=numeric(),thirty=integer(),type=character(),shareType=numeric(),typeSpec=numeric(),specShare=numeric(),customShare=numeric(),priceLag=numeric())
dsales <- fread("data/derived/sales.csv")
dsales <- merge(dsales,d24[,.(ROLL_NUMBER24,MB_Year_Built,type,NEIGHBOURHOOD,thirty,spec)],by.x="ROLL_NUMBER",by.y="ROLL_NUMBER24")
sales <- dsales[CONVEYANCE_DATE==MB_Year_Built | (CONVEYANCE_DATE==MB_Year_Built+1) ]
for  (n in unique(dsales[,NEIGHBOURHOOD])) {
	print(n)
	print(dsales[NEIGHBOURHOOD==n & type=="duplex" & CONVEYANCE_DATE>2019 & thirty==1,CONVEYANCE_PRICE])
	print(quantile(dsales[NEIGHBOURHOOD==n & type=="duplex" & CONVEYANCE_DATE>2019 & thirty==1,CONVEYANCE_PRICE],na.rm=TRUE))
}
KEEPLAG <- 1
for (y in seq(FIRST_BUILT,LAST_BUILT)) {
	for (n in unique(d24$NEIGHBOURHOOD)) {
		for (s in c(0,1)) { #consider 50', too
		#for (s in c(1)) {
			dsub <- d24[NEIGHBOURHOOD==n & MB_Year_Built==y & thirty==s  ]
			for (t in c("single","duplex","laneway","basement")) {
			#for (t in c("single","laneway","basement")) {
				dss <- dsales[CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction" & NEIGHBOURHOOD==n & CONVEYANCE_DATE<y & CONVEYANCE_DATE>=(y-KEEPLAG) & thirty==s & type==t]
				shares <- rbind(shares,data.table(nbhd=n,year=y,thirty=s,type=t,shareType=dsub[,mean(type==t)],typeSpec=dsub[type==t,mean(spec==1)],specShare=dsub[spec==1,mean(type==t)],customShare=dsub[spec==0,mean(type==t)],priceLag=dss[,log(median(CONVEYANCE_PRICE,na.rm=TRUE))]))
			}
		    }
        }
}
print(summary(shares))

print("Herf")
print(summary(shares[,sum(shareType^2),by=.(year,nbhd,thirty)]))
print(summary(shares[,sum(customShare^2),by=.(year,nbhd,thirty)]))
print(summary(shares[,sum(specShare^2),by=.(year,nbhd,thirty)]))
print("pre duplex")
print(summary(shares[year<2019,sum(shareType^2),by=.(year,nbhd,thirty)]))
print(summary(shares[year<2019,sum(customShare^2),by=.(year,nbhd,thirty)]))
print(summary(shares[year<2019,sum(specShare^2),by=.(year,nbhd,thirty)]))
print(summary(shares[thirty==1 &year<2019,sum(shareType^2),by=.(year,nbhd,thirty)]))
print(summary(shares[thirty==1 &year<2019,sum(customShare^2),by=.(year,nbhd,thirty)]))
print(summary(shares[thirty==1 &year<2019,sum(specShare^2),by=.(year,nbhd,thirty)]))


dnew <- d24[MB_Year_Built>FIRST_BUILT & MB_Year_Built<2023]
print(dnew[MB_Year_Built>2016,mean(spec),by=.(thirty,fifty)])
print(dnew[MB_Year_Built>2016,mean(spec),by=type])
print(dnew[MB_Year_Built>2016,mean(spec),by=MB_Year_Built])
print(dnew[MB_Year_Built>2016,quantile(improvementValue,na.rm=TRUE),by=.(spec)])
print(dnew[MB_Year_Built>2016,mean(improvementValue,na.rm=TRUE),by=.(spec)])
print(summary(feols(improvementValue~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
print(summary(feols(log(improvementValue)~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
print(summary(feols(landValue~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))
print(summary(feols(log(landValue)~spec | MB_Year_Built ^ NEIGHBOURHOOD ^ thirty,data=dnew[MB_Year_Built>2016])))

# neighbourhood and type variation in spec?
print(dnew[,mean(spec),by=type])
print(dnew[,.(mean(spec),mean(type=="duplex")),by=.(NEIGHBOURHOOD)])
print(dnew[MB_Year_Built<2019,.(mean(spec),mean(type=="duplex")),by=.(NEIGHBOURHOOD)])

# note individual reg actually lower t-stat than group level
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

print(summary(feols(typeSpec~shareType |year^nbhd^thirty + type^year ,data=shares,cluster="nbhd")))
shares[,nbhdType:=paste0(nbhd,"_",type)]
print(summary(feols(typeSpec~shareType |year^type + nbhd^year^thirty + nbhdType^thirty,data=shares,cluster="nbhdType")))
# anything funny with laneways?
for (y in 2009:2019) {
	print(y)
	print(shares[year==y & type=="laneway",.(mean(typeSpec,na.rm=TRUE),mean(specShare,na.rm=TRUE),mean(shareType,na.rm=TRUE),mean(priceLag,na.rm=TRUE)),by=.(thirty)])
}


# mean laneway premium by year given nbhd
shares[,laneway:=ifelse(type=="laneway",1,0)]
print(summary(feols(priceLag ~ i(year)*laneway | nbhd,data=shares[thirty==1 & year>2010])))

print(summary(feols(typeSpec~shareType |year+ nbhd ,data=shares[year>2012 & year<2019 & type=="laneway" & thirty==1],cluster="nbhdType")))
print(summary(feols(typeSpec~shareType |year+ nbhd ,data=shares[year>2015 & type=="laneway" & thirty==1],cluster="nbhdType")))
print(summary(feols(typeSpec~shareType |year+ nbhd ,data=shares[year>2016 & year < 2020 & type=="laneway" & thirty==1],cluster="nbhdType")))

print(summary(feols(typeSpec~shareType |year^type + nbhd^year^thirty + nbhdType^thirty,data=shares[year<2020 & type!="duplex"],cluster="nbhdType")))
print(summary(feols(specShare~ priceLag |year^type + nbhd^year^thirty + nbhd^type^thirty,data=shares[type!="duplex"],cluster="nbhd")))
print(summary(feols(specShare~ priceLag |year^type + nbhd^year^thirty ,data=shares[type!="duplex"],cluster="nbhd")))
print(summary(feols(specShare~ priceLag |year^type+ nbhd^year^thirty + nbhd^type^thirty,data=shares[year<2020 & type!="duplex"],cluster="nbhd")))
# Funny sales result
print(shares[,mean(priceLag,na.rm=TRUE),by=.(type,year)])
print(shares[,mean(priceLag,na.rm=TRUE),by=.(type,nbhd)])
# convergance?
print(shares[year==2017,mean(priceLag,na.rm=TRUE),by=.(type,thirty,nbhd)])
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

for (y in 2010:2018) {
	ggplot(shares[year==y & type=="laneway",.(type,shareType,typeSpec,year),],aes(x=shareType,y=typeSpec,color=year)) +
	    geom_point() 
	ggsave(paste0("text/shareSpec",y,".png"))
}

ggplot(shares[,.(shareType,typeSpec,nbhd)],aes(x=shareType,y=typeSpec,color=nbhd)) + geom_point() 
ggsave("text/shareSpecType.png")

