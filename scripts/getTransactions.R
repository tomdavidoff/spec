# getTransactions.R
# Get transactions from the sales file, merge with property info
# Tom Davidoff
# March 5, 2025

library(data.table)
library(fixest)

dSales <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_sales_20240331_REVD24.csv",select=c("ROLL_NUMBER","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION","JURISDICTION"))
dSales <- dSales[JURISDICTION=="City of Vancouver"]
print(summary(dSales))
print(table(dSales[,JURISDICTION])) # but keep all here
dSales[,CONVEYANCE_DATE:=floor(CONVEYANCE_DATE/1000000)]
dSales <- dSales[CONVEYANCE_DATE>20150101]

# Get COV roll -- will have all duplexes,verify with laneway data
dRoll <- fread("~/docs/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",select=c("Roll_Number","MB_Year_Built","Land_Width_Width","Land_Depth_Depth","Land_Area_Total","Zoning"))
print(summary(dRoll))
print(table(dRoll[,Zoning]))
dRoll <- dRoll[Zoning=="R1-1" & abs(Land_Depth_Depth-120)<3 & (abs(Land_Depth_Depth-50)<3 | abs(Land_Width_Width-33)<3) & MB_Year_Built>2014] 
dRoll[,thirtyThree:=abs(Land_Width_Width-33)<3]
print(dRoll[,.N])
dRoll[,Roll_Number:=as.character(Roll_Number)]

dAddress <- fread("~/docs/data/bca/data_advice_REVD24_20240331/bca_folio_addresses_20240331_REVD24.csv",select=c("ROLL_NUMBER","STREET_NUMBER","STREET_DIRECTION_PREFIX","STREET_NAME","STREET_TYPE","STREET_DIRECTION_SUFFIX","POSTAL_CODE"))
dSales <- merge(dSales,dAddress,by="ROLL_NUMBER",all.x=TRUE)

dSales[,ROLL_NUMBER:=as.character(as.numeric(ROLL_NUMBER))] # gets rid of leading zeros

dRoll <- merge(dSales,dRoll,by.x="ROLL_NUMBER",by.y="Roll_Number",all.y=TRUE)
summary(dRoll[,is.na(CONVEYANCE_PRICE)])

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
dRoll[,fsa:=substr(POSTAL_CODE,1,3)]
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