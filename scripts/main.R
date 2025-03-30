# main.R
# R to analyze spec/custom choice
# Tom Davidoff

library(data.table)
library(readstata13)

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
d24[,maxSale:=ifelse(is.na(maxSale),0,maxSale)]
d24[,maxSale:=round(maxSale/10000)]
d24[,duplex:=grepl("Duplex,",ACTUAL_USE_DESCRIPTION)]
d24[,type:=ifelse(duplex==1,"duplex",ifelse(hasLaneway==1,"laneway","single"))]
print(summary(d24[,MB_Year_Built]))
for (y in seq(2016,2023)) {
    print(y)
    print(table(d24[MB_Year_Built==y,.(type,maxSale>=MB_Year_Built)]))
}
print(table(d24[MB_Year_Built>2016 & MB_Year_Built<2023,.(type,maxSale>=MB_Year_Built,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==1 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,maxSale>=MB_Year_Built,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==0 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,maxSale>=MB_Year_Built,STREET_DIRECTION_SUFFIX)]))
print(table(d24[thirty==1 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,maxSale>=MB_Year_Built,NEIGHBOURHOOD)]))
print(table(d24[thirty==0 & MB_Year_Built>2016 & MB_Year_Built<2022,.(type,maxSale>=MB_Year_Built,NEIGHBOURHOOD)]))
# Main regression
# Dummy for spec vs custom on property type dummies and share 
