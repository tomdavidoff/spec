# specHold.R 
# how long to spec builders hold properties?
# Also, when do they buy?
# Tom Davidoff
# 06/04/25

# strategy: find last sale before year built for single family homes
# consider doing for all teardowns class of 2016, but see how long hold is first
# do this for each metro area

library(data.table)
library(fixest)	
library(RSQLite)

# get dates of construction for single family homes 
# see if roll number ends in zero and no other by lot start works

DOCOMPILE <- TRUE

if (DOCOMPILE==TRUE) {
	dr <- unique(fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("JURISDICTION_CODE","REGIONAL_DISTRICT"),colClasses=c(ROLL_NUMBER="character")))

	dd25 <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("JURISDICTION_CODE","ROLL_NUMBER","FOLIO_ID","ACTUAL_USE_DESCRIPTION"),colClasses=c(ROLL_NUMBER="character"))
	print(summary(dd25))
	print(summary(dd25))
	dj25 <- unique(dd25[,.(JURISDICTION_CODE,ROLL_NUMBER,FOLIO_ID)]) # for sales data uniformity
	print(dj25[1:20])
	names(dj25) <- c("JURISDICTION_CODE","ROLL_NUMBER","FOLIO_ID")
	print(dj25[1:20])

	diri <- "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/"
	dirf <- list.files(diri)[1:2]

	di25 <- data.table(Jurisdiction=numeric(),Roll_Number=character(),MB_Year_Built=numeric(),MB_Effective_Year=numeric())
	for (f in paste0(diri,dirf)) {
		d <- fread(f,select=c("Jurisdiction","Roll_Number","MB_Year_Built","MB_Effective_Year"),colClasses=c(Roll_Number="character",Jurisdiction="numeric") )
		di25 <- rbind(di25,d)
	}
	di25 <- di25[!is.na(MB_Year_Built)]
	# to date 20250312000000

	d25 <- merge(dd25,di25,by.x=c("JURISDICTION_CODE","ROLL_NUMBER"),by.y=c("Jurisdiction","Roll_Number"))
	print(summary(d25))

	ds25 <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"),colClasses=c(CONVEYANCE_DATE="character"))
	# convert folioID to jurisdiction/roll_number as pre-redevelopment sales will have often same jurisd/roll_number, different FOLIO_ID. Certainly same rollStart
	ds25[,CONVEYANCE_DATE:=as.Date(CONVEYANCE_DATE,format="%Y%m%d%H%M%S")]
	print("probnlem 25")
	print(summary(ds25))
	print(summary(dj25))
	print(ds25[1:10])
	print(dj25[1:10])
	ds25 <- merge(ds25,dj25,by="FOLIO_ID") # add jurisdiction code and roll number

	# get sales data from 2016 to maximize chain of sales
	# open an sqlite connection to ~/docs/data/bca/REVD16_and_inventory_extracts.sqlite3
	print("trying16")
	db16 <- dbConnect(RSQLite::SQLite(), "~/OneDrive - UBC/Documents/data/bca/REVD16_and_inventory_extracts.sqlite3")
	df16 <- dbGetQuery(db16, "SELECT folioID, jurisdictionCode, rollNumber FROM folio") 
	ds16 <- dbGetQuery(db16, "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription FROM sales")
	print(summary(df16))
	print(summary(ds16))
	print("DD?")
	ds16 <- data.table(merge(df16,ds16,by="folioID"))
	setnames(ds16,c("jurisdictionCode","rollNumber","conveyanceDate","conveyancePrice","conveyanceTypeDescription","folioID"),c("JURISDICTION_CODE","ROLL_NUMBER","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION","FOLIO_ID"))
	print(ds16[1:10,CONVEYANCE_DATE])
	ds16[,CONVEYANCE_DATE:=as.Date(CONVEYANCE_DATE,format="%Y-%m-%d")]
	print("ds16then25")
	print(ds16[1:20,])
	print(summary(ds25))
	print(ds25[1:20])
	ds25 <- unique(rbind(ds25,ds16))
	print("winner!")
	print(d25[1:20,])
	print(ds25[1:20,])
	ds25[,JURISDICTION_CODE:=as.numeric(JURISDICTION_CODE)]
	d25 <- merge(d25,ds25,by=c("JURISDICTION_CODE","ROLL_NUMBER")) # add sales data
	# why the merge fail?
	d25 <- merge(d25,dr,by="JURISDICTION_CODE") # add regional district
	d25[,rollStart:=substring(ROLL_NUMBER,1,nchar(ROLL_NUMBER)-1)]
	d25[,nRoll:=.N,by=rollStart]

	fwrite(d25,"data/derived/specHold25.csv")

}

d25 <- fread("data/derived/specHold25.csv")

# how to observe prior sales?

PUNISHMENT <- 10^9
print(PUNISHMENT)
# reshape to horizontal with dcast to see all three sales
d25[,lastSale:=max(CONVEYANCE_DATE),by=c("JURISDICTION_CODE","ROLL_NUMBER")]
d25[,firstPost:=min(CONVEYANCE_DATE+PUNISHMENT*(year(CONVEYANCE_DATE) < MB_Year_Built)),by=c("JURISDICTION_CODE","ROLL_NUMBER")]
d25[,preDate:=as.Date("1600-01-01",format="%Y-%m-%d")]
d25[CONVEYANCE_DATE < firstPost, preDate:=CONVEYANCE_DATE]
d25[,lastPre:=max(preDate),by=FOLIO_ID]
d25[,yfpost:=year(firstPost)]
print(summary(d25))
d25[,spec:=yfpost < (MB_Year_Built+2)]
print(table(d25[spec==1,MB_Year_Built-year(lastPre)])) # approximates how long spec builders held
print(table(d25[(spec==1) & (JURISDICTION_CODE==200),MB_Year_Built-year(lastPre)])) # approximates how long spec builders held

# single flippers types
d25[,duplex:=grepl("Duplex,",ACTUAL_USE_DESCRIPTION)]
d25[,basement:=grepl("Residential Dwelling with Suite",ACTUAL_USE_DESCRIPTION)]
d25[,single:=grepl("Single Family Dwelling",ACTUAL_USE_DESCRIPTION)]
print(table(d25[(spec==1) & ((single+basement+duplex)>0),MB_Year_Built-year(lastPre)])) # approximates how long spec builders held



# which years did they buy?
print(table(d25[spec==1,year(lastPre)]))
print(table(d25[spec==1,yearmonth(lastPre)]))

# create a price index just metro Vancouver
dv <- d25[REGIONAL_DISTRICT=="Metro Vancouver",]
ri <- feols(log(CONVEYANCE_PRICE) ~ i(yearmon(CONVEYANCE_DATE))



