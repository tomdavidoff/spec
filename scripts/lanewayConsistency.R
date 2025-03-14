# lanewayConsistency.R
# How consistent are different measures of laneway?
# Tom Davidoff
# March 11, 2025

library(data.table)
library(readstata13)

# first, geography of having a lane
dP <- fread("data/derived/lanewaySpatial.csv")
print(summary(dP))

# next, permits with laneways
dL <- fread("data/derived/lanewayPermitAddress.csv")
print(summary(dL))

# next, Tsur list of laneways
dT1 <- read.dta13("data/raw/Laneway list 2015-23.dta")
dT1 <- data.table(dT1)[,.(roll_num)]
print(summary(dT1))
dT2 <- read.dta13("data/raw/Vancouver Roll 2018.Roll+Laneway detailed.dta")
dT2 <- data.table(dT2)[,.(roll_num,haslaneway,postalcode,str_no,str_name,laneok1,laneok2,single_all)]
print(summary(dT2))

dT <- merge(dT2[single_all==1,.(roll_num,haslaneway,str_no,str_name)],dT1[,.(roll_num,haslaneway=1)],by="roll_num",all=TRUE)
dT[,haslaneway:=(haslaneway.x==1 | haslaneway.y==1)]
dT[is.na(haslaneway.x),haslaneway:=haslaneway.y]
dT[is.na(haslaneway.y),haslaneway:=haslaneway.x]
print(summary(dT))
# function to fullify street type
fullify <- function(x) {
    y <- gsub(" AV"," AVENUE",x)
    y <- gsub(" AVE"," AVENUE",y)
    y <- gsub(" AVENUENUE"," AVENUE",y)
    y <- gsub(" ST"," STREET",y)
    y <- gsub(" BLVD","BOULEVARD",y)
    y <- gsub(" DR"," DRIVE",y)
    y <- gsub(" PL"," PLACE",y)
    y <- gsub(" RD"," ROAD",y)
    y <- gsub(" HWY"," HIGHWAY",y)
    y <- gsub(" CRES"," CRESCENT",y)
    y <- gsub(" CRT"," COURT",y)
    y <- gsub(" TER"," TERRACE",y)
    y <- gsub(" PLZ"," PLAZA",y)
    return(y)
}
dT[,civicAddress:=paste(str_no,fullify(str_name))]

dLT <- merge(dL,dT,by="civicAddress",all=TRUE)
# lots of no laneway reported when yes permit
summary(dLT[haslaneway==1 ,(IssueDate>as.Date("2023-01-01"))])
# But almost all permit when laneway reported
summary(dLT[haslaneway==1 ,(IssueDate<as.Date("2023-01-01"))])