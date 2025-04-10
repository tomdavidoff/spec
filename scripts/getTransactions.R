# getTransactions.R
# Get transactions from the sales file, merge with property info
# Tom Davidoff
# March 5, 2025

library(data.table)
library(fixest)

dSales <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_sales_20240331_REVD24.csv",select=c("ROLL_NUMBER","FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION","JURISDICTION"))
dSales <- dSales[JURISDICTION=="City of Vancouver"]
print(summary(dSales))
print(table(dSales[,JURISDICTION])) # but keep all here
dSales[,CONVEYANCE_DATE:=floor(CONVEYANCE_DATE/10000000000)]
# note do not delete sales after 2015, as some non-Specs!
print(table(dSales[,CONVEYANCE_DATE]))
# vectorize the following dMaxSale <- dSales[,.(sale2016=max(CONVEYANCE_DATE==2016),sale2017=max(CONVEYANCE_DATE==2017),sale2018=max(CONVEYANCE_DATE==2018),sale2019=max(CONVEYANCE_DATE==2019),sale2020=max(CONVEYANCE_DATE==2020),sale2021=max(CONVEYANCE_DATE==2021),sale2022=max(CONVEYANCE_DATE==2022),sale2023=max(CONVEYANCE_DATE==2023)),by="ROLL_NUMBER"]
for (y in 2010:2023) {
   dSales[,paste0("sale",y):=max(CONVEYANCE_DATE==y),by="ROLL_NUMBER"]
}
print(summary(dSales))
dMaxSale <- dSales[,.(ROLL_NUMBER,sale2010,sale2011,sale2012,sale2013,sale2014,sale2015,sale2016,sale2017,sale2018,sale2019,sale2020,sale2021,sale2022,sale2023)]
dMaxSale <- unique(dMaxSale)
print(summary(dMaxSale))
print(summary(dMaxSale[,.N,by=ROLL_NUMBER]))

fwrite(dMaxSale,"data/derived/maxSale.csv")
q("no")

dDesc <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_descriptions_20240331_REVD24.csv",select=c("ROLL_NUMBER","FOLIO_ID","LAND_DEPTH","LAND_WIDTH","ACTUAL_USE_DESCRIPTION","MANUAL_CLASS_DESCRIPTION"))
print(table(dDesc[,ACTUAL_USE_DESCRIPTION]))
dDesc[,duplex:=grepl("Duplex,",ACTUAL_USE_DESCRIPTION)]
dDesc[,single:=(grepl("Residential Dwelling with Suite",ACTUAL_USE_DESCRIPTION)) | (grepl("Single Family Dwelling",ACTUAL_USE_DESCRIPTION))]
dDesc <- dDesc[,JURISDICTION:="City of Vancouver"]

print(summary(dDesc[duplex==1,.(LAND_WIDTH,LAND_DEPTH)]))
print(summary(dDesc[single==1,.(LAND_WIDTH,LAND_DEPTH)]))
dRoll[,thirtyThree:=abs(Land_Width_Width-33)<3]
print(table(dDesc[duplex==1,MANUAL_CLASS_DESCRIPTION]))
# Laneway not great here
print(table(dDesc[single==1,MANUAL_CLASS_DESCRIPTION]))
dDesc[,ROLL_NUMBER:=NULL]

# Get COV roll -- will have all duplexes,verify with laneway data
dRoll <- fread("~/docs/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",select=c("Roll_Number","MB_Year_Built","Zoning"))
dRoll <- dRoll[Zoning=="R1-1"] 
print(dRoll[,.N])
dRoll[,Roll_Number:=as.character(Roll_Number)]

dAddress <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_addresses_20240331_REVD24.csv",select=c("ROLL_NUMBER","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","POSTAL_CODE"))
dSales <- merge(dSales,dAddress,by="ROLL_NUMBER",all.x=TRUE)

dSales[,ROLL_NUMBER:=as.character(as.numeric(ROLL_NUMBER))] # gets rid of leading zeros

print("PRESALES")
print(dRoll[,.N])
dRoll <- merge(dSales,dRoll,by.x="ROLL_NUMBER",by.y="Roll_Number",all.y=TRUE)
print("POSTSALES")
print(dRoll[,.N])
summary(dRoll[,is.na(CONVEYANCE_PRICE)])


dLane <- fread("data/derived/lanewayConsistency.csv")
dRoll[,roll_num:=as.character(as.numeric(ROLL_NUMBER))]
dLane[,roll_num:=as.character(as.numeric(roll_num))]
dRoll <- merge(dRoll,dLane,by="roll_num")
print("POSTLANES")
print(dRoll[,.N])
dRoll[,laneway:=(lanewayNoPermit | lanewayPermit)]

dRoll[,fsa:=substr(POSTAL_CODE,1,3)]
dRoll[,year:=substr(as.character(CONVEYANCE_DATE),1,4)]
dRoll[,yearMonth:=substr(as.character(CONVEYANCE_DATE),1,6)]
dRoll[,postSale:=(year==MB_Year_Built) | (year==MB_Year_Built+1)]
print(summary(feols(log(CONVEYANCE_PRICE) ~ laneway | MB_Year_Built+fsa,data=dRoll[ postSale==1 & thirtyThree==1])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ laneway | MB_Year_Built+fsa,data=dRoll[ postSale==1 & thirtyThree==0])))
print(summary(feols(log(CONVEYANCE_PRICE) ~ laneway | thirtyThree^MB_Year_Built^fsa,data=dRoll[ postSale==1 & fsa=="V6K"])))
print(mean(dRoll[,postSale],na.rm=TRUE ))
print(dRoll[,mean(laneway),by=MB_Year_Built])
dZ <- dRoll[,.(hasPost=max(postSale),laneway=max(laneway)),by="roll_num"]
table(dZ[,.(hasPost,laneway)])

## NOT DONE
if (1>2) {
dLane <- fread("data/derived/lanewayPermitAddress.csv")
print(summary(dLane))


# replace ST with STREET, AVE with AVENUE, etc. in STREET_TYPE
# make a data.table with the fullified street types
dFull <- data.table(shortStreet=character(),fullStreet=character())
dFull <- rbind(dFull,data.table(shortStreet=c("ST","AV","AVE","BLVD","DR","PL","RD","HWY","CRES","CRT","TER","PLZ"),fullStreet=c("STREET","AVENUE","AVENUE","BOULEVARD","DRIVE","PLACE","ROAD","HIGHWAY","CRESCENT","COURT","TERRACE","PLAZA")))


dRoll <- merge(dRoll,dFull,by.x="STREET_TYPE",by.y="shortStreet",all.x=TRUE)
dRoll[,civicAddress:=paste(STREET_NUMBER,STREET_DIRECTION_PREFIX,STREET_DIRECTION_SUFFIX,STREET_NAME,fullStreet)]
dRoll[,civicAddress:=gsub("   "," ",civicAddress)]
dRoll[,civicAddress:=gsub("  "," ",civicAddress)]
dRoll <- merge(dRoll,dLane,by="civicAddress",all.x=TRUE)
print(summary(feols(log(CONVEYANCE_PRICE) ~ laneway | fsa^MB_Year_Built,data=dRoll[single==1])))

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


# get laneway okay or not
dOkay <- fread("data/derived/lanewaySpatial.csv")
summary(dOkay)
dOkay[,civicAddress:=paste(streetNumber,fullify(streetName))]
print(dOkay[,civicAddress])

dRoll <- merge(dRoll,dOkay,by="civicAddress",all.x=TRUE)


print(dRoll[,mean(laneway),by=laneok])
}
