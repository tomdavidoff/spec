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
library(ggplot2)

# get dates of construction for single family homes 
# see if roll number ends in zero and no other by lot start works

DOCOMPILE <- FALSE

if (DOCOMPILE==TRUE) {
	dd25 <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("JURISDICTION_CODE","ROLL_NUMBER","FOLIO_ID","ACTUAL_USE_DESCRIPTION","REGIONAL_DISTRICT"),colClasses=c(ROLL_NUMBER="character"))
	dj25 <- unique(dd25[,.(JURISDICTION_CODE,ROLL_NUMBER,FOLIO_ID)]) # for sales data uniformity
	names(dj25) <- c("JURISDICTION_CODE","ROLL_NUMBER","FOLIO_ID")

	diri <- "~/OneDrive - UBC/Documents/data/bca/Residential_inventory_202501/"
	dirf <- list.files(diri)

	di25 <- data.table(Jurisdiction=numeric(),Roll_Number=character(),MB_Year_Built=numeric())
	for (f in paste0(diri,dirf)) {
		d <- fread(f,select=c("Jurisdiction","Roll_Number","MB_Year_Built"),colClasses=c(Roll_Number="character",Jurisdiction="numeric") )
		di25 <- rbind(di25,d)
	}
	di25 <- di25[!is.na(MB_Year_Built)]
	# to date 20250312000000

	d25 <- merge(dd25,di25,by.x=c("JURISDICTION_CODE","ROLL_NUMBER"),by.y=c("Jurisdiction","Roll_Number"))
	print(summary(d25))

	ds25 <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"),colClasses=c(CONVEYANCE_DATE="character"))
	# convert folioID to jurisdiction/roll_number as pre-redevelopment sales will have often same jurisd/roll_number, different FOLIO_ID. Certainly same rollStart
	ds25[,CONVEYANCE_DATE:=as.Date(CONVEYANCE_DATE,format="%Y%m%d%H%M%S")]
	ds25 <- merge(ds25,dj25,by="FOLIO_ID") # add jurisdiction code and roll number

	# get sales data from 2016 to maximize chain of sales
	# open an sqlite connection to ~/docs/data/bca/REVD16_and_inventory_extracts.sqlite3

	# get only single family lots as of 2016
	db16 <- dbConnect(RSQLite::SQLite(), "~/OneDrive - UBC/Documents/data/bca/REVD16_and_inventory_extracts.sqlite3")
	df16 <- dbGetQuery(db16, "SELECT folioID, jurisdictionCode, rollNumber FROM folio") 
	du16 <- dbGetQuery(db16, "SELECT folioID FROM folioDescription WHERE actualUseDescription IN ('Single Family Dwelling','Residential Dwelling with Suite')") #1.	“Single Family Dwelling” and “Residential Dwelling with Suite” are almost certainly SFHs. from ChatGPT, and is right_use
	du16 <- data.table(du16) # convert to data.table
	names(du16) <- "folioID" # rename to folioID
	df16 <- data.table(merge(df16,du16,by="folioID")) # keep only the valid single family
	# verify last digit of roll number is always zero, and only one observation per rollStart
	df16[,rollStart:=substring(rollNumber,1,nchar(rollNumber)-1)]
	print(summary(df16[,.N,by=rollStart]))
	# now the last digit thing
	df16[,lastDigit:=substring(rollNumber,nchar(rollNumber),nchar(rollNumber))]
	print(table(df16[,lastDigit])) # not true always 1
	df16[,lastDigit:=NULL]
	df16[,ner:=.N,by=c("rollStart","jurisdictionCode")] 
	print(df16[ner>1]) # these seem to be luxury
	print(table(df16[,ner]))
	df16 <- df16[ner==1]
	df16[,ner:=NULL] # drop count
	ds16 <- dbGetQuery(db16, "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription FROM sales")
	ds16 <- data.table(merge(df16,ds16,by="folioID"))
	ds16[,folioID:=NULL] # drop folioID
	setnames(ds16,c("jurisdictionCode","rollNumber","conveyanceDate","conveyancePrice","conveyanceTypeDescription"),c("JURISDICTION_CODE","ROLL_NUMBER","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"))
	print(ds16[1:10,CONVEYANCE_DATE])
	ds16[,CONVEYANCE_DATE:=as.Date(CONVEYANCE_DATE,format="%Y-%m-%d")]
	ds25[,FOLIO_ID:=NULL] # drop FOLIO_ID
	ds25[,rollStart:=substring(ROLL_NUMBER,1,nchar(ROLL_NUMBER)-1)] # create rollStart
	ds16[,in16:=1] # mark as in 2016
	ds25[,in16:=0] # mark as not in 2016
	print("namediff")
	# only want stuff that had roll start in 2016
	ds25 <- unique(rbind(ds25,ds16)) # note this procedure should allow duplexes as of 2025 -- but also note in 16 makes unique not happen
	ds25[,max16:=max(in16),by=c("JURISDICTION_CODE","rollStart")]
	ds25 <- ds25[max16==1] # keep only those with a 2016 data sale (know was single family use in 2016)
	ds25[,max16:=NULL] # drop max16 now redundant
	ds25[,in16:=NULL] # drop in16 now redundant
	IMPROVEDONLY <- 0
	if (IMPROVEDONLY==1) {
		ds25 <- ds25[CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction"] # only keep improved single property transactions
	} 
	ds25[,CONVEYANCE_TYPE_DESCRIPTION:=NULL] # drop conveyance type description now redundant
	ds25[,JURISDICTION_CODE:=as.numeric(JURISDICTION_CODE)]
	print(d25[1:20,])
	print(ds25[1:20,])
	print(table(d25[,JURISDICTION_CODE]))
	d25[,rollStart:=substring(ROLL_NUMBER,1,nchar(ROLL_NUMBER)-1)]
	d25[,maxBuilt:=max(MB_Year_Built),by=c("JURISDICTION_CODE","rollStart")] # in case of e.g. duplexes
	d25[,lastDigit:=substring(ROLL_NUMBER,nchar(ROLL_NUMBER),nchar(ROLL_NUMBER))]	
	d25[,minLastDigit:=min(lastDigit),by=c("JURISDICTION_CODE","rollStart")]
	d25 <- d25[lastDigit==minLastDigit] # keep only lowest digit of set in event multiple same year built
	print(summary(d25))
	d25 <- d25[MB_Year_Built == maxBuilt] # drop earlier of duplexes if any
	setnames(ds25,"ROLL_NUMBER","rollSales") # rename roll number to rollSales
	d25 <- merge(d25,ds25,by=c("JURISDICTION_CODE","rollStart")) # add sales data
	print(summary(d25))
	d25[,nRoll:=.N,by=rollStart]
	print(table(d25[,nRoll]))

	fwrite(d25,"data/derived/specHold25.csv")

}

d25 <- fread("data/derived/specHold25.csv")
d25 <- unique(d25) # shouldn't be, but are duplicates (overlaps in sales data?)
d25 <- d25[REGIONAL_DISTRICT=="Metro Vancouver",]
#d25 <- d25[JURISDICTION_CODE==200,]

# how to observe prior sales?

PUNISHMENT <- 10^9
print(PUNISHMENT)
# reshape to horizontal with dcast to see all three sales
print(summary(d25))
d25[,firstPost:=min(CONVEYANCE_DATE+PUNISHMENT*(year(CONVEYANCE_DATE) < MB_Year_Built)),by=c("JURISDICTION_CODE","rollStart")] # first sale post-construction but what if tie? see saleYearBuilt stuff
d25[,saleYearBuilt:=year(CONVEYANCE_DATE)==MB_Year_Built]
print(summary(d25[,length(unique(FOLIO_ID)),by=c("JURISDICTION_CODE","rollStart")])) # note this is correct, always 1, so equivalent
d25[,nSaleYearBuilt:=sum(saleYearBuilt),by="FOLIO_ID"]
print(table(d25[,nSaleYearBuilt])) # how many sales in year built typically
d25[,lastSaleYearBuilt:=0]
d25[nSaleYearBuilt>1,firstPost:=max(CONVEYANCE_DATE),by=c("FOLIO_ID")] # last sale in year built if multiple
d25[,preDate:=as.Date("1600-01-01",format="%Y-%m-%d")] # last sale before year built
d25[CONVEYANCE_DATE < firstPost, preDate:=CONVEYANCE_DATE] 
d25[,lastPre:=max(preDate),by=FOLIO_ID]
d25[,yfpost:=year(firstPost)]
d25[,lastSale:=max(CONVEYANCE_DATE),by="FOLIO_ID"] # last sale date
print(summary(d25))
d25[,spec:=yfpost < (MB_Year_Built+2)]
#d25[,firstObs:=min(CONVEYANCE_DATE),by=FOLIO_ID] # first observation date
d25[,firstObs:=min(CONVEYANCE_DATE),by=c("JURISDICTION_CODE","rollStart")] # first observation date
print(table(d25[MB_Year_Built>2000 & (CONVEYANCE_DATE==firstObs),year(firstObs)]))
print(table(d25[MB_Year_Built>2000 & (CONVEYANCE_DATE==firstObs),year(firstObs)>=MB_Year_Built]))
MAXPRE <- 6 # maximum number of years before year built to consider a spec builder, arbitraryish
d25[,specStrict:= spec*(year(lastPre)>(MB_Year_Built-MAXPRE))] # spec if last sale before year built is less than MAXPRE years before year built
d25[,specAlt:=spec*(yfpost==(MB_Year_Built+1) | month(firstPost)>6)] # alternative spec definition, if first sale after year built is one year after year built
d25[,customStrict:=yfpost>(MB_Year_Built+1)]
# strict definition -- pre-purchase not too long before built
print(summary(d25[spec==1,max(MB_Year_Built-year(lastPre)),by=FOLIO_ID])) # how long before year built did they buy? should be 1 or 2 years
print(table(d25[spec==1 & (CONVEYANCE_DATE==lastSale),MB_Year_Built-year(lastPre)])) # how long before year built did they buy? should be 1 or 2 years
print(table(d25[spec==1 & (CONVEYANCE_DATE==lastSale) & (yfpost==MB_Year_Built),month(firstPost)])) # are these custom builds bought early in year?
print(d25[MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=mean(spec)),by=MB_Year_Built][order(MB_Year_Built)])
print(d25[MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=mean(specStrict)),by=MB_Year_Built][order(MB_Year_Built)])
print(d25[MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=mean(specAlt)),by=MB_Year_Built][order(MB_Year_Built)])
print(d25[(JURISDICTION_CODE==200) & MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=mean(specAlt)),by=MB_Year_Built][order(MB_Year_Built)]) # Vancouver
print(d25[MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=1-mean(customStrict)),by=MB_Year_Built][order(MB_Year_Built)])
q("no")
d25[,specBuy:=(CONVEYANCE_DATE==lastPre)*spec] # spec buy is last sale before year built
d25[,specSale:=(CONVEYANCE_DATE==firstPost)*spec] # spec sale is first sale after year built
print(table(d25[specBuy==1,MB_Year_Built-year(CONVEYANCE_DATE)])) # approximates how long spec builders held ; last sale to one per obs
print(table(d25[specSale==1 & (JURISDICTION_CODE==200),MB_Year_Built-year(CONVEYANCE_DATE)])) # approximates how long spec builders held

# which years did they buy?
print(table(d25[specBuy==1,year(lastPre)]))
print(table(d25[specBuy==1,yearmon(lastPre)]))

# create a price index just metro Vancouver
# Deflate prices by CPI
dc <- fread("data/raw/statCanCPIwm.csv",select=c("REF_DATE","VALUE"))
setnames(dc,"VALUE","CPI") # rename value to CPI
dc[,year:=as.numeric(substring(REF_DATE,1,4))] # extract year from REF_DATE
dc[,month:=as.numeric(substring(REF_DATE,6,7))] # extract month from REF_DATE
print(dc)
d25[,year:=year(CONVEYANCE_DATE)] # add year to d25
d25[,month:=month(CONVEYANCE_DATE)] # add month to d25
d25 <- merge(d25,dc,by=c("year","month"),all.x=TRUE) # merge with CPI data
print(d25)
d25[,realPrice:= CONVEYANCE_PRICE/CPI] # create real value

dv <- d25[REGIONAL_DISTRICT=="Metro Vancouver",]
dv[,isOld:=(CONVEYANCE_DATE<firstPost) | year(CONVEYANCE_DATE)<MB_Year_Built] # indicates pre-reno sale
ri <- feols(log(realPrice) ~ i(yearmon(CONVEYANCE_DATE)) + isOld|FOLIO_ID, data=dv) # repeated sale with new home indicator
# extract monthly coefficients only
ri_coef <- coef(ri)[grepl("yearmon", names(coef(ri)))]
# make data table with yearmon as date and coefficient
# typical entry yearmon(CONVEYANCE_DATE)::2024.91666666667 
ri_dt <- data.table(numericDate=as.numeric(gsub("yearmon\\(CONVEYANCE_DATE\\)::","",names(ri_coef))), repeatedSaleIndex=ri_coef,origName=names(ri_coef))
print(ri_dt)

dv[,numericDate:=year(CONVEYANCE_DATE) + (month(CONVEYANCE_DATE)-1)/12] # convert to numeric date
dsb <- dv[,.(nSpecBuys=sum(spec*(CONVEYANCE_DATE==lastPre))/100,n=.N/100),by=numericDate] # scale to oneish
rib <- merge(ri_dt, dsb, by="numericDate") # merge with sales data
rib[,specShare:=100*nSpecBuys/n] # share of spec buys (always quite small)
rib[,n:=NULL] # drop n now redundant
rib <- melt(rib, id.vars="numericDate", measure.vars=c("repeatedSaleIndex","nSpecBuys","specShare"), variable.name="measure", value.name="value")
# plot the coefficients and number of spec buys
ggplot(rib[numericDate>2000], aes(x=numericDate, y=value, color=measure)) +
  geom_line() +
  labs(title="Monthly Repeated Sale Index and Number and share of Spec Buys",
       x="Numeric Date",
       y="Value") +
  scale_color_manual(values=c("blue", "red","green")) +
  theme_bw()
# save the plot
ggsave("text/specTiming.png", width=10, height=6)

# now sales
dss <- dv[,.(nSpecSales=sum(spec*(CONVEYANCE_DATE==firstPost))/100),by=numericDate] # scale to oneish
ris <- merge(ri_dt, dss, by="numericDate") # merge with sales data
ris <- melt(ris, id.vars="numericDate", measure.vars=c("repeatedSaleIndex","nSpecSales"), variable.name="measure", value.name="value")
# plot the coefficients and number of spec sales
ggplot(ris[numericDate>2000], aes(x=numericDate, y=value, color=measure)) +
  geom_line() +
  labs(title="Monthly Repeated Sale Index and Number of Spec Sales",
       x="Numeric Date",
       y="Value") +
  scale_color_manual(values=c("blue", "red")) +
  theme_bw()
# save the plot
ggsave("text/specSalesTiming.png", width=10, height=6)

# print share of all MB_Year_Built that are spec by year -- get this right
print(dv[MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=mean(spec)),by=MB_Year_Built][order(MB_Year_Built)])
print(dv[MB_Year_Built>2000 & (lastSale==CONVEYANCE_DATE),.(specShare=mean(specStrict)),by=MB_Year_Built][order(MB_Year_Built)])
x <- dv[ (MB_Year_Built>2000) & (lastSale==CONVEYANCE_DATE),.(specShare=sum(spec*(yfpost==MB_Year_Built))/.N,count=.N),by=MB_Year_Built]
print(x[order(MB_Year_Built)])
x <- dv[ (MB_Year_Built>2000) & (lastSale==CONVEYANCE_DATE),.(specShare=sum(specStrict*(yfpost==MB_Year_Built))/.N,count=.N),by=MB_Year_Built]
print(x[order(MB_Year_Built)])
print(dv[MB_Year_Built>2017,.N,by=c("spec","ACTUAL_USE_DESCRIPTION")])

