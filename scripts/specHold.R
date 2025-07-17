# specHold.R 
# how long to spec builders hold properties?
# Also, when do they buy?
# Tom Davidoff
# 06/04/25
# Modified as mergeRoll20052025.R does a lot

# Steps:
# 1. do mergeRoll20052025.R to get homes built after 2005 and their 2005 characterstics
# 2. Do this script and get sales for each property
# 3. Merge the sales data with the 2005 data and learn about spec vs custom

library(data.table)
library(fixest)	
library(RSQLite)
library(ggplot2)

# get dates of construction for single family homes 
# see if roll number ends in zero and no other by lot start works

# obtain static year built

DOCOMPILE <- FALSE

if (DOCOMPILE==TRUE) {
	dd25 <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("JURISDICTION_CODE","ROLL_NUMBER","FOLIO_ID","ACTUAL_USE_DESCRIPTION","REGIONAL_DISTRICT"),colClasses=c(ROLL_NUMBER="character"))
	dd25 <- dd25[REGIONAL_DISTRICT=="Metro Vancouver"] # drop empty regional districts
	dj25 <- unique(dd25[,.(JURISDICTION_CODE,ROLL_NUMBER,FOLIO_ID)]) # for sales data uniformity
	names(dj25) <- c("JURISDICTION_CODE","ROLL_NUMBER","FOLIO_ID")

	ds25 <- fread("~/OneDrive - UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"),colClasses=c(CONVEYANCE_DATE="character"))
	# convert folioID to jurisdiction/roll_number as pre-redevelopment sales will have often same jurisd/roll_number, different FOLIO_ID. Certainly same rollStart
	ds25[,CONVEYANCE_DATE:=as.Date(CONVEYANCE_DATE,format="%Y%m%d%H%M%S")]
	ds25 <- merge(ds25,dj25,by="FOLIO_ID") # add jurisdiction code and roll number

	# Now get 2016 data
	db16 <- dbConnect(RSQLite::SQLite(), "~/OneDrive - UBC/Documents/data/bca/REVD16_and_inventory_extracts.sqlite3")
	df16 <- dbGetQuery(db16, "SELECT folioID, jurisdictionCode, rollNumber FROM folio") 
		ds16 <- dbGetQuery(db16, "SELECT folioID, conveyanceDate, conveyancePrice, conveyanceTypeDescription FROM sales")
	ds16 <- data.table(merge(df16,ds16,by="folioID"))
	setnames(ds16,"folioID","FOLIO_ID") # rename folioID to FOLIO_ID
	setnames(ds16,c("jurisdictionCode","rollNumber","conveyanceDate","conveyancePrice","conveyanceTypeDescription"),c("JURISDICTION_CODE","ROLL_NUMBER","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"))
	ds16[,CONVEYANCE_DATE:=as.Date(CONVEYANCE_DATE,format="%Y-%m-%d")]

	ds1625 <- rbind(ds25,ds16) # note this procedure should allow duplexes as of 2025; note can't make unique here as "" and ,, same
	fwrite(ds1625,"data/derived/sales20162025.csv") # save sales data
}
ds <- fread("data/derived/sales20162025.csv")
ds <- unique(ds[,.(FOLIO_ID,CONVEYANCE_DATE,CONVEYANCE_PRICE,ROLL_NUMBER) ]) # remove duplicates with different conveyanceTypeDescription, see inside construction loop
db <- fread("data/derived/newBuilds.csv")
setkey(ds,FOLIO_ID)
setkey(db,FOLIO_ID)
# identify duplicates
print(ds[duplicated(ds),]) # should be none
print(db[duplicated(db) ,]) # should be none
print(head(ds))
print(head(db))
ds[,rollNumeric:=as.numeric(ROLL_NUMBER)] # convert roll number to numeric
db[,rollNumeric:=as.numeric(Roll_Number)] # convert roll number to numeric
print(db[duplicated(FOLIO_ID)& MB_Year_Built>2005,])  # these appear to be assemblies and rarely built post-2005
db <- db[duplicated(FOLIO_ID)==FALSE ] # drop duplicates, keep only those built before 2005
df <- merge(ds,db)
print(head(df))

# get maximal transaction date on or before year built, then minimal after that transaction
df[,conveyanceDateContinuous:=year(CONVEYANCE_DATE) + (month(CONVEYANCE_DATE)-1)/12 + mday(CONVEYANCE_DATE-1)/365.25] # convert to continuous date
df[,soldBuilt:= year(CONVEYANCE_DATE)==MB_Year_Built]
# create the maximal and minimal dates conditional on soldBuilt within FOLIO_ID, set to zero if none
df[,nIn:=sum(soldBuilt),by=FOLIO_ID] # number of sales in year built
df[,maxIn:=max(conveyanceDateContinuous*soldBuilt),by=FOLIO_ID] # max conveyance date in year built
df[,minIn:=min(conveyanceDateContinuous*soldBuilt+100000*(1-soldBuilt)),by=FOLIO_ID] # min conveyance date in year built
df[,maxPre:=max(conveyanceDateContinuous*(year(CONVEYANCE_DATE)<=MB_Year_Built)),by=FOLIO_ID] # max conveyance date before year built
df[nIn>1, maxPre:=minIn] # if multiple sales in year built, set maxPre to the minimal
df[,minPost:=min(conveyanceDateContinuous+3000*(year(CONVEYANCE_DATE)<MB_Year_Built )),by=FOLIO_ID] # can be same as maxPre
df[nIn>1, minPost:=maxIn] # if sales in year built, set minPost to maxIn
# handle minPost==maxPre.
print(df[nIn>1 & minPost==maxPre,.(FOLIO_ID,MB_Year_Built,CONVEYANCE_DATE,CONVEYANCE_PRICE,nIn,maxPre,minPost,minIn,maxIn,ROLL_NUMBER)])
df[,structureShare:= value_stru05/(value_stru05+value_land05)] # share of structure value in 2005
print(summary(df[MB_Year_Built>2005,])) 
dNew <- unique(df[MB_Year_Built>2005,.(maxPre=maxPre,minPost=minPost,MB_Year_Built=max(MB_Year_Built),structureShare,maxIn,minIn,nIn),by=FOLIO_ID])
print(head(dNew))
SPECDELTA <- 1 #1 vs 2 vs 3 appears little diff on structureShare results
dNew[,spec:=(floor(minPost)<=(MB_Year_Built+SPECDELTA))] # spec if first sale after year built is one year after year built
MAXHOLD <- 4 # maximum number of years before year built to consider a spec builder, arbitraryish, but see spec distro, 49-25 then flat
MAXHOLDEXTRA <- 1
dNew[,specStrict:=spec & (maxPre>=(MB_Year_Built-MAXHOLD)) & (maxPre<minPost)] # spec if last sale before year built is less than MAXHOLD years before year built also different sale dates
dNew[,specStrictExtra:=spec & (maxPre>=(MB_Year_Built-MAXHOLDEXTRA)) & (maxPre<minPost)] # spec if last sale before year built is less than MAXHOLD years before year built also different sale dates
print(table(dNew[,.(spec,nIn)]))
print(table(dNew[,.(spec,floor(maxPre)-MB_Year_Built)]))
print(table(dNew[,.(specStrict,floor(maxPre)-MB_Year_Built)]))
print(summary(dNew[spec==1 & specStrict==0,structureShare]))
print(summary(dNew[specStrict==1 & specStrictExtra==0,structureShare]))
print(summary(dNew[specStrictExtra==1,structureShare]))
print(summary(dNew[spec==0,structureShare]))

# problem diagnostic
df[, yr := year(CONVEYANCE_DATE)]
lateSpecs <- df[, .(
  nIn = sum(yr == MB_Year_Built),
  soldNextYr = any(yr == MB_Year_Built + 1)
), by = FOLIO_ID][nIn == 0 & soldNextYr == TRUE]

nrow(lateSpecs)  # how many are being missed?
table(is.na(dNew$minPost), dNew$nIn)

q("no")



	ds25[,max16:=max(in16),by=c("JURISDICTION_CODE","ROLL_NUMBER")]
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
# Write data.table code with the following outline: divide into (a) strictCustom: no sale in year built or after, unless sale in first 3 months so no way completed then; (b) strict spec: sale in year built or after, unless sale in first 3 months so no way completed then, and also a sale within crititcal value prior to that sale; (c) grey area, neither strict spec nor strict custom
MAXPRE <- 5 # maximum number of years before year built to consider a spec builder, arbitraryish
print(summary(d25[,length(unique(FOLIO_ID)),by=c("JURISDICTION_CODE","rollStart")])) # note this is correct, always 1, so FOLIO_ID and combo (jurisdiction rollStart) are equivalent
d25 <- d25[MB_Year_Built>1999 & MB_Year_Built<2024]
d25[,saleMonth:=month(CONVEYANCE_DATE)] # add sale month
d25[,saleYear:=year(CONVEYANCE_DATE)] # add sale year
MINBUILD <- 4 # presume can't build in less than this N months from start
d25[,specPrePurchase:= (saleYear<MB_Year_Built | (saleYear==MB_Year_Built & saleMonth<MINBUILD)) &   (saleYear >= (MB_Year_Built-MAXPRE))  ] # spec if sale before year built minus MAXPRE years
d25[,specSale:=(saleYear>=MB_Year_Built) & (saleYear<=(MB_Year_Built+1))]
d25[saleYear==MB_Year_Built & saleMonth<MINBUILD,specSale:=0] # if sale in year built, but before MINBUILD, not a spec sale
d25[,specPreDate:=as.Date("2500-01-01",format="%Y-%m-%d")] # pre purchase date
d25[,specPostDate:=as.Date("1600-01-01",format="%Y-%m-%d")] # post sale date
d25[specPrePurchase==1,specPreDate:=CONVEYANCE_DATE] # if spec pre purchase, set pre date to conveyance date
d25[specSale==1,specPostDate:=CONVEYANCE_DATE] # if spec sale, set post date to conveyance date
d25[,maxPost:=max(specPostDate),by=FOLIO_ID] # max post date for each folio ID
d25[,minPre:=min(specPreDate),by=FOLIO_ID] # min pre date for each folio ID
d25[,specStrict:=(maxPost>minPre) ]
d25[,customStrict:=year(maxPost)<MB_Year_Built]
d25[,grey:=(!specStrict & !customStrict)] # neither strict spec nor strict custom
d25[,lastSale:=max(CONVEYANCE_DATE),by=FOLIO_ID] # last sale date for each folio ID
print(summary(d25[CONVEYANCE_DATE==lastSale,.(specStrict,customStrict,grey)]))
print(d25[CONVEYANCE_DATE==lastSale,sum(specStrict & customStrict)]) # how many are both spec and custom?
d25 <- d25[CONVEYANCE_DATE==lastSale] # keep only last sale date
d25 <- d25[order(MB_Year_Built)]
# All
print(d25[order(MB_Year_Built),.(spec=mean(specStrict),custom=mean(customStrict),grey=mean(grey)),by=MB_Year_Built]) # share of each
# Vancouver
print(d25[JURISDICTION_CODE==200,.(spec=mean(specStrict),custom=mean(customStrict),grey=mean(grey)),by=MB_Year_Built]) # share of each
# West Vancouver
print(d25[JURISDICTION_CODE==328,.(spec=mean(specStrict),custom=mean(customStrict),grey=mean(grey)),by=MB_Year_Built]) # share of each
# Maple Ridge
print(d25[JURISDICTION_CODE==312,.(spec=mean(specStrict),custom=mean(customStrict),grey=mean(grey)),by=MB_Year_Built]) # share of each
# Richmond
print(d25[ JURISDICTION_CODE==320,.(spec=mean(specStrict),custom=mean(customStrict),grey=mean(grey)),by=MB_Year_Built]) # share of each
q("no")
d25[,firstPost:=min(CONVEYANCE_DATE+PUNISHMENT*(year(CONVEYANCE_DATE) < MB_Year_Built)),by=c("JURISDICTION_CODE","rollStart")] # first sale post-construction but what if tie? see saleYearBuilt stuff
d25[,saleYearBuilt:=year(CONVEYANCE_DATE)==MB_Year_Built]
d25[,nSaleYearBuilt:=sum(saleYearBuilt),by="FOLIO_ID"]
print(table(d25[,nSaleYearBuilt])) # how many sales in year built typically
d25[,lastSaleYearBuilt:=max(CONVEYANCE_DATE),by=c("FOLIO_ID","saleYearBuilt")] # last sale in year built
d25[,lastSaleYearBuilt:=max(lastSaleYearBuilt),by=c("FOLIO_ID")] # last sale in year built
d25[nSaleYearBuilt>1,firstPost:=max(CONVEYANCE_DATE*(year(CONVEYANCE_DATE)==MB_Year_Built)),by=c("FOLIO_ID")] # last sale in year built if multiple
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

