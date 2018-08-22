
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(googlesheets)
library(googledrive)
library(anytime)


txns <- gs_title("txns.csv")
txns <- as.data.frame(gs_read(txns,col_names = T, check.names= T))
txns[sapply(txns, function(x) all(is.na(x)))] <- NULL
txns$X13 <- NULL

cost <- gs_title("product_margins.csv")
cost <- as.data.frame(gs_read(cost,check.names= T))

all_orders <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/orders.csv", header=TRUE,sep=",")

# keep disbursements and subscription fees seperate
disburse_subs <- txns[(txns$"Transaction.Type" == "Disbursement" | txns$"Transaction.Type" == "Subscription Fee Charge"),]
disburse_subs <- as.data.frame.matrix(xtabs(Incl.VAT ~ Transaction.ID + Transaction.Type, data = disburse_subs))
disburse_subs <- disburse_subs[(disburse_subs$"Subscription Fee Charge" + disburse_subs$"Disbursement" != 0),]
disburse_subs$Transaction.ID <- rownames(disburse_subs) # convert row names to column order Id

# txn data processing #
txns <- txns[order(-txns$"Transaction.ID"),]

# strip out the order Id from Reference column
txns <- within(txns, OrderID <- str_extract(sub('.*\\:', '', txns$Reference), "[0-9]+"))

# strip out numbers from a string
str_x <- function(x) 
{
  # extract order ID and put into list. 1 = original order, 2 = return 
  x <- strsplit(x, "[^[:digit:]]")
  x <- as.numeric(unlist(x))
  x <- unique(x[!is.na(x)])
  return(x)  
}

# format date
date_x <- function(x)
{
   x <- as.Date(as.character(x, "Y%m/%d/%"), origin = "1970-01-01")
   return(x)	
} 

# strip out order ID from Transaction.Description column for manual txns 
txns[txns$Transaction.Type == 'Manual Charge' | txns$Transaction.Type == 'Manual Reversal',]$OrderID <- within(txns[(txns$Transaction.Type %in% c("Manual Charge", "Manual Reversal")), ], 
    OrderID <- str_extract(sub('.*\\:', '', Transaction.Description), "[0-9]+"))$OrderID

txns$OrderID <- ifelse(txns$Transaction.Type == "Disbursement", NA, txns$OrderID)
txns$OrderID <- ifelse(txns$Transaction.Type == "Subscription Fee Charge", NA, txns$OrderID)
txns$OrderID <- ifelse(txns$Transaction.Type == "Storage Fee Charge", NA, txns$OrderID)


# create unique order ID column to handle part orders (same order ID for different SKUs)
txns$dupli_ID <- paste(txns$OrderID, txns$Transaction.Type, sep='_')  

# keep orginal Order ID seperate untouched
txns$OrdID <- txns$OrderID

txns$OrderID <- ifelse(txns$Transaction.Type == "Subscription Fee Charge", "subsc", txns$OrderID)
txns$OrderID <- ifelse(txns$Transaction.Type == "Disbursement", "disburse", txns$OrderID)

# deal with part orders (txns), disbursements & subscription fees
txns <- transform(txns, 
    OrderID = ifelse(duplicated(dupli_ID) | duplicated(dupli_ID, fromLast=TRUE), 
              paste(OrderID, ave(dupli_ID, dupli_ID, FUN=seq_along),  sep='_'), OrderID))  

txns$Abs.Incl.VAT <- abs(txns$Incl.VAT) 
txns$Abs.OrdVal <- ave(txns$Abs.Incl.VAT,txns$OrderID, FUN = max)


# txns$ordIDVal <- paste(txns$OrdID, txns$Abs.OrdVal, sep='_')
# t <- transform(txns, 
#    OrderIDVals = ifelse(duplicated(ordIDVal) | duplicated(ordIDVal, fromLast=TRUE), 
#              paste(ordIDVal, ave(ordIDVal, ordIDVal, FUN=seq_along),  sep='_'), ordIDVal))


# negative for returns
txns$OrdVal <- ifelse(grepl("Reversal", txns$Transaction.Description), -1*txns$Abs.OrdVal, txns$Abs.OrdVal)
              
# match the correct % of fees (needed for part orders after split)
for (i in 1: nrow(txns)) {
  	txns$success_percent[i] <- ifelse(txns$Transaction.Type[i] == "Success Fee Charge", str_x(as.character(txns$Transaction.Description[i])), 0)
     
}

# fix date formats
txns$Transaction.Date <- anytime::anydate(txns$Transaction.Date)

txns$success_percent <- with(txns, ave(success_percent, OrderID, FUN = function(x) (max(x)) ))

# calculate the success fees in order to identify any mismatched fees with part orders
for (i in 1: nrow(txns)) {
  	txns$Incl.VAT_x[i] <- ifelse(txns$Transaction.Type[i] == "Success Fee Charge" | txns$Transaction.Type[i] == "Success Fee Reversal", 
    	ifelse(txns$Transaction.Date[i] < anytime::anydate('2018-04-01'), -1*txns$OrdVal[i] * txns$success_percent[i]/100 + (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.14, 
    	  -1*txns$OrdVal[i] * txns$success_percent[i]/100 + (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.15), txns$Incl.VAT[i])     
    
    txns$VAT_x[i] <- ifelse(txns$Transaction.Type[i] == "Success Fee Charge" | txns$Transaction.Type[i] == "Success Fee Reversal", 
    	ifelse(txns$Transaction.Date[i] < anytime::anydate('2018-04-01'), (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.14, 
    	  (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.15), txns$VAT[i])     
}

# round calc fees to 2 decimals
txns$Incl.VAT_x <- round(txns$Incl.VAT_x,digits = 2)
txns$VAT_x <- round(txns$VAT_x,digits = 2)
               
# txns[(txns$OrdID == "55857301"),]                        

# replaced Order Val & VAT for mismatched part orders

orders_fix <- txns[txns$VAT_x != txns$VAT & grepl("_", txns$OrderID), ]$OrderID # to fix later
txns[txns$VAT_x != txns$VAT & grepl("_", txns$OrderID), ][c("OrderID","Incl.VAT",  "VAT","Incl.VAT_x",  "VAT_x")]$Incl.VAT <- 
  txns[txns$VAT_x != txns$VAT & grepl("_", txns$OrderID), ][c("OrderID","Incl.VAT",  "VAT","Incl.VAT_x",  "VAT_x")]$Incl.VAT_x

txns[txns$VAT_x != txns$VAT & grepl("_", txns$OrderID), ][c("OrderID","Incl.VAT",  "VAT","Incl.VAT_x",  "VAT_x")]$VAT <- 
  txns[txns$VAT_x != txns$VAT & grepl("_", txns$OrderID), ][c("OrderID","Incl.VAT",  "VAT","Incl.VAT_x",  "VAT_x")]$VAT_x


# test
#txns[txns$VAT_x != txns$VAT & grepl("_", txns$OrderID), ][c("OrderID","Incl.VAT",  "VAT","Incl.VAT_x",  "VAT_x")]
#txns[txns$OrdID %in% c("56118290", "55857301"),]                       


# deal with returned orders
rtn_orders <- as.list(unique(txns[(txns$Reference.Type == "Return"),]$"OrderID"))
txns$OrderID <- ifelse(txns$OrderID %in% rtn_orders, paste(txns$OrderID, "rtn", sep="_"), txns$OrderID)
txns$OrderID <- ifelse(txns$Reference.Type == "Return", paste(txns$OrderID, "2", sep="_"), txns$OrderID)
txns$OrderID <- ifelse(txns$OrderID %in% as.list(unique(txns[grep("_rtn$", txns$OrderID), ]$OrderID)), paste(txns$OrderID, "1", sep="_"), txns$OrderID)

 
# order data by MaxTxnID - most recent first
order_x <- function(x)
{
   x <- x[order(-x$MaxTxnID),]
   return(x)	
} 

txns$OrdID <- as.numeric(txns$OrdID)
txns$OrdID <- replace(txns$OrdID, is.na(txns$OrdID), 0)

# deal with storage fee discount & rebate
txns$Transaction.Type<-ifelse(txns$Transaction.Type %in% c('Manual Reversal', 'Manual Payment') & grepl('Storage Fee',txns$Transaction.Description), 'Storage Fee Discount/Rebate', txns$Transaction.Type)

# summarize data by Order ID
txns_grp_ord <- group_by(txns, OrderID)
txns_ <- dplyr::summarise(txns_grp_ord, OrdVal = sum(Incl.VAT[Transaction.Type %in% c("Customer Order Payment")]),
           AbsOrdVal = max(as.numeric(Abs.Incl.VAT)),
           SuccessPercent = max(success_percent), 
           FulFil = sum(Incl.VAT[Transaction.Type %in% c("Fulfilment Fee Charge")]),
           SuccessFee = sum(Incl.VAT[Transaction.Type %in% c("Success Fee Charge")]),
           OrderReversal = sum(Incl.VAT[Transaction.Type %in% c("Customer Order Reversal")]),
           SFReversal = sum(Incl.VAT[Transaction.Type %in% c("Success Fee Reversal")]),
           Disburse = sum(Incl.VAT[Transaction.Type %in% c("Disbursement")]),
           SubscFee = sum(Incl.VAT[Transaction.Type %in% c("Subscription Fee Charge")]),
           ManualCharge = sum(Incl.VAT[Transaction.Type %in% c("Manual Charge")]), 
           ManualReversal = sum(Incl.VAT[Transaction.Type %in% c("Manual Reversal")]),
           StorageFee = sum(Incl.VAT[Transaction.Type %in% c("Storage Fee Charge") | Transaction.Type %in% "Storage Fee Discount/Rebate"]),           
           Vat = sum(VAT),
           MaxTxnID = max(Transaction.ID),
           Balance = Balance[which.max(Transaction.ID)],
           MaxTxnDte = max(Transaction.Date),
           NumTxns = length(Transaction.ID),
           OrdID = max(OrdID))


# order by txn ID desc           
txns_ <- order_x(txns_)

# keep only orders shipped orders, add returned orders later 
orders <- all_orders[(all_orders$Status == "Shipped"),]

# keep track of original order Id
orders$OrdIDs <- orders$OrderID

# deal with part orders (shipped)
 orders <- transform(orders, 
    OrderID = ifelse(duplicated(OrderID) | duplicated(OrderID, fromLast=TRUE), 
              paste(OrderID, ave(OrderID, OrderID, FUN=seq_along), sep='_'), OrderID))  
                                              
# merge txns with orders 
merge_txns_ord <- merge(x = txns_, y = orders, by = "OrderID", all.x = TRUE)
merge_txns_ord <- merge(x = merge_txns_ord, y = cost, by= "SKU", all.x = TRUE)

# fix sequence mistakes for part orders (shipped)
 mistakes <- na.omit(merge_txns_ord[merge_txns_ord$OrdVal != merge_txns_ord$Total & merge_txns_ord$OrdVal > 0,])  
fix_part_orders <- mistakes$OrderID

# orders and txns subsests (shipped)
ship_txns_ord <- na.omit(merge_txns_ord[merge_txns_ord$OrderReversal >= 0 & as.numeric(merge_txns_ord$OrdID) != 0,] [rev(order(merge_txns_ord[merge_txns_ord$OrderReversal >= 0 & as.numeric(merge_txns_ord$OrdID) != 0,]$OrderID)), ]) 
orders_ship <- orders[rev(order(orders$OrderID)), ] 
 
txns_ord_prt <- ship_txns_ord[ship_txns_ord$OrderID %in% fix_part_orders,] 
orders_prt <-  orders_ship[orders_ship$OrderID %in% fix_part_orders,] 

txns_ord_prt$old_OrderID <- txns_ord_prt$OrderID
txns_ord_prt$new_OrderID <- 0

for (i in 1: nrow(txns_ord_prt)) {
  for (j in 1: nrow(orders_prt)){
    txns_ord_prt$new_OrderID[i] <- ifelse(txns_ord_prt$OrdID[i] == orders_prt$OrdIDs[j] & txns_ord_prt$OrdVal[i] ==  orders_prt$Total[j], orders_prt$OrderID[j], txns_ord_prt$new_OrderID[i])  
  }
}  	 

txns_ord_prt <- txns_ord_prt[rev(order(txns_ord_prt$new_OrderID)),]

# cbind(txns_ord_prt[c("new_OrderID", "OrdVal")], orders_prt[c("Total",   "OrderID")])

# replace broken part orders with new order IDs
old <- txns_ord_prt$old_OrderID
new <- txns_ord_prt$new_OrderID

merge_txns_ord$OrderID <- mapvalues(merge_txns_ord$OrderID,
     from = old,
     to = new)

# merge again after correction of part order OrderIDs 
merge_txns_ord <- merge(x = merge_txns_ord, y = orders[c("OrderID", "SKU", "Qty", "Total", "Unit.Price")], by.x = "OrderID", by.y = "OrderID", all.x = TRUE)

#na.omit(merge_txns_ord[merge_txns_ord$OrdIDs == 55955252,])
#m <- m[rev(order(m$OrderID)),]
#merge_txns_ord <- merge_txns_ord[rev(order(merge_txns_ord $OrderID)),]
#cbind(merge_txns_ord$OrderID, m$OrderID)
#cbind(merge_txns_ord$OrderID, m$OrderID, ifelse(merge_txns_ord$OrderID==m$OrderID, 0,paste(merge_txns_ord$OrderID, m$OrderID, sep ='_')))
#ship_returned <- merge(returns, orders[!names(orders) %in% c("OrderID")], by.x = "OrderID", by.y = "OrdIDs")$OrderID

# remove unwanted columns
merge_txns_ord[,c("PO.Number","Customer", "Price", "Title", "Product", "SKU.x", "Qty.x", "Total.x", "Unit.Price.x", "AbsOrdVal")] <- list(NULL)

# returned orders
returns <- all_orders[(all_orders$Status == "Returned"),]
 
# TODO exclude 56118290 (weird unaccounted for return - no txn) *** CHECK STATUS LATER, TXN? - DONE ***
# returns <- returns[returns$OrderID != 56118290,]
# merge_txns_ord <- merge_txns_ord[merge_txns_ord$OrdID !=56118290,]

# join with cost 
returns <- merge(returns, cost)

# rename 
merge_txns_ord <- plyr::rename(merge_txns_ord, c("Cost" = "CP", "Date" = "OrdDate", "SKU.y" = "SKU", "Qty.y"= "Qty", "Total.y" = "Total", "Unit.Price.y" = "SP"))
returns <- plyr::rename(returns, c("Cost" = "CP", "Date" = "OrdDate"))

# get order date of returned orders and preserve
returns$OrdID = as.numeric(returns$OrderID)

# deal with part order (returns)
 returns <- transform(returns, 
    OrderID = ifelse(duplicated(OrderID) | duplicated(OrderID, fromLast=TRUE), 
              paste(OrderID, ave(OrderID, OrderID, FUN=seq_along), sep='_'), OrderID)) 

returns[, c(1,7)] <- sapply(returns[, c(1,7)], as.character)
merge_txns_ord[, c(1)] <- sapply(merge_txns_ord[, c(1)], as.character)

merge_txns_ord <- order_x(merge_txns_ord)


# function to deal with part order returns
str_x_part <- function(x) 
{
  y <- "_"
  n <- 1
  loc <- gregexpr("_", x)[[1]][n]
  return(substr(x, 1, loc+1))
}

merge_txns_ord$"OrdDate" <- anytime::anydate(merge_txns_ord$"OrdDate")
merge_txns_ord$SKU <- as.character(merge_txns_ord$SKU)
# merge_txns_ord$oid <- str_x_part(merge_txns_ord$OrderID)

# row by row add SKU info for returned orders
for (i in 1: nrow(merge_txns_ord)) {
	for (j in 1: nrow(returns)){
	
	  # if (grepl('[^[:alnum:]]', returns$OrderID[j]) == T) {
	  
	    if(length(strsplit(merge_txns_ord$OrderID[i], '_')[[1]]) > 3){
	  
	    merge_txns_ord$SKU[i] <- ifelse(str_x_part(merge_txns_ord$OrderID[i]) == returns$OrderID[j], returns$SKU[j], merge_txns_ord$SKU[i])
        merge_txns_ord$SP[i] <- ifelse(str_x_part(merge_txns_ord$OrderID[i]) == returns$OrderID[j], returns$Unit.Price[j], merge_txns_ord$SP[i])
        merge_txns_ord$Total[i] <- ifelse(str_x_part(merge_txns_ord$OrderID[i]) == returns$OrderID[j], returns$Total[j], merge_txns_ord$Total[i])
        merge_txns_ord$Qty[i] <- ifelse(str_x_part(merge_txns_ord$OrderID[i]) == returns$OrderID[j], returns$Qty[j], merge_txns_ord$Qty[i])
        merge_txns_ord$CP[i] <- ifelse(str_x_part(merge_txns_ord$OrderID[i]) == returns$OrderID[j], returns$CP[j], merge_txns_ord$CP[i])
        	  
	  }
	  
	  else {
	  	# str_x[1] is the orginal order, str_x[2] is the return	
        merge_txns_ord$SKU[i] <- ifelse(str_x(merge_txns_ord$OrderID[i])[1] == returns$OrderID[j], returns$SKU[j], merge_txns_ord$SKU[i])
        merge_txns_ord$SP[i] <- ifelse(str_x(merge_txns_ord$OrderID[i])[1] == returns$OrderID[j], returns$Unit.Price[j], merge_txns_ord$SP[i])
        merge_txns_ord$Total[i] <- ifelse(str_x(merge_txns_ord$OrderID[i])[1] == returns$OrderID[j], returns$Total[j], merge_txns_ord$Total[i])
        merge_txns_ord$Qty[i] <- ifelse(str_x(merge_txns_ord$OrderID[i])[1] == returns$OrderID[j], returns$Qty[j], merge_txns_ord$Qty[i])
        merge_txns_ord$CP[i] <- ifelse(str_x(merge_txns_ord$OrderID[i])[1] == returns$OrderID[j], returns$CP[j], merge_txns_ord$CP[i])
   
	  }     
   
    }
}

for (i in 1: nrow(merge_txns_ord)) { 
	
	merge_txns_ord$"OrdPartRet"[i] <- ifelse(length(strsplit(merge_txns_ord$OrderID[i], '_')[[1]]) > 3, str_x_part(merge_txns_ord$OrderID[i]), merge_txns_ord$OrdID[i])
}	  

returns$"OrdDate" <- anytime::anydate(returns$"OrdDate")

# add order date to returned orders
merge_txns_ord <- merge(x = merge_txns_ord, y = returns[c("OrderID", "OrdDate")], by.x = "OrdPartRet", by.y = "OrderID",  all.x = T )
merge_txns_ord$OrdDate <- with(merge_txns_ord, as.Date(ifelse(is.na(OrdDate.x), OrdDate.y, OrdDate.x), origin = "1970-01-01"))


merge_txns_ord$"OrdDate" <- anytime::anydate(merge_txns_ord$"OrdDate")

# remove unwanted columns
merge_txns_ord[,c( "X","X.1","OrdDate.y","OrdDate.x", "OrdIDs")] <- list(NULL)

# set status of returned orders to "Returned"
merge_txns_ord$Status <- as.character(merge_txns_ord$Status)
merge_txns_ord$Status <- ifelse(merge_txns_ord$OrderReversal < 0, "Returned", merge_txns_ord$Status)


# to fix later (status): shipped & returned status: 55013960, 56029056
# part_orders_ship_return_fix <- txns[txns$VAT_x != txns$VAT, ]$OrderID
# (merge_txns_ord[merge_txns_ord$OrdVal != merge_txns_ord$Total & merge_txns_ord$OrdVal != 0,])$OrdID
ship_returned <- merge(returns, orders[!names(orders) %in% c("OrderID")], by.x = "OrderID", by.y = "OrdIDs")$OrderID

# fix orders with shipped & returned status
merge_txns_ord$Status <- ifelse(merge_txns_ord$OrdID %in% ship_returned & merge_txns_ord$OrdVal > 0, "Shipped", merge_txns_ord$Status)


# format & rename & reorder columns 
cols <- c("GP", "Fees", "KRRetail", "KRNew", "FCG_New", "FCG", "FCGTotal", "CalcBal", "KR GP_0.4 (excl. fees)", "KR Fees_0.5", "FCG GP_0.6 (excl. fees)", "FCG Fees_0.5")
vals <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
sapply(1:length(cols), function(i)
   merge_txns_ord[, cols[i]] <<- as.numeric(vals[i])
)

# fix mismatched CP
for(i in 1:nrow(merge_txns_ord))
{
  for (j in 1: nrow(cost)){	
   merge_txns_ord$CP[i] <- ifelse(merge_txns_ord$SKU[i] == cost$SKU[j] & merge_txns_ord$CP[i] != cost$Cost[j], cost$Cost[j], merge_txns_ord$CP[i])
   }
} 

# test
# orders_ship[orders_ship$OrdIDs == 56401484,]
# merge_txns_ord[merge_txns_ord$OrdID == 56401484,]


# updating cost price (new stock prices)

#update_cp <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/updated_costs.csv", header=TRUE, sep=",")
update_cp <- gs_title("updated_costs")
update_cp <- as.data.frame(gs_read(update_cp,check.names= T))
update_cp$Cost_Start <- anytime::anydate(update_cp$Cost_Start)
update_cp$Cost_End   <- anytime::anydate(update_cp$Cost_End) 


merge_txns <- merge_txns_ord
update_cp_ <- update_cp

v <- merge(merge_txns, update_cp_[c("SKU","Cost","Cost_Start","Cost_End", "Split")],by=c("SKU"),all.x = T)

# update CP
v$CP <- ifelse(is.na(v$Cost), v$CP, 
          ifelse(v$OrdDate>=v$Cost_Start & v$OrdDate<=v$Cost_End,v$Cost,v$CP))

# remove rows outside of update CP bounds and keep rows that don't have a cost to be updated (i.e. keep rows with Cost = NA)
v <- v[-which(!(v$OrdDate>=v$Cost_Start & v$OrdDate<=v$Cost_End)),]

merge_txns_ord <- NULL
merge_txns_ord <- v

# calculate fees, GP & split
merge_txns_ord$"Fees" <- merge_txns_ord$"FulFil" + merge_txns_ord$"SuccessFee" + merge_txns_ord$"SFReversal"+ merge_txns_ord$"ManualCharge" + merge_txns_ord$"ManualReversal" 
merge_txns_ord$"GP" <- ifelse(merge_txns_ord$OrdVal > 0, 
                         merge_txns_ord$OrdVal - (merge_txns_ord$"CP" * merge_txns_ord$"Qty"),
                         merge_txns_ord$OrderReversal + (merge_txns_ord$"CP" * merge_txns_ord$"Qty"))

# merge_txns_ord$SKU %in% shared_profit &

# adjust profit according to capital contribution
# merge_txns_ord$"KR GP_0.4 (excl. fees)" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"GP" * 0.5, merge_txns_ord$"GP" * 0.4)     
# add half of the CP to KR Retail earnings for shared stock
#merge_txns_ord$"KRRetail" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"KR GP_0.4 (excl. fees)" + merge_txns_ord$"KR Fees_0.5" + ifelse(merge_txns_ord$OrdVal > 0, #0.5*merge_txns_ord$CP, -1*0.5*merge_txns_ord$CP), merge_txns_ord$"KR GP_0.4 (excl. fees)" + merge_txns_ord$"KR Fees_0.5")   
#merge_txns_ord$"FCG GP_0.6 (excl. fees)" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"GP" * 0.5, merge_txns_ord$"GP" * 0.6)
#merge_txns_ord$"FCG Fees_0.5" <- merge_txns_ord$"Fees" * 0.5                      
#merge_txns_ord$"FCG" <- merge_txns_ord$"FCG GP_0.6 (excl. fees)" + merge_txns_ord$"FCG Fees_0.5"

kr_percent <- 0.1
kr_percent_vat <- 0.05


merge_txns_ord$"KR Fees_0.5" <- merge_txns_ord$"Fees" * 0.5 


# add half of the CP to KR Retail earnings for shared stock

merge_txns_ord <- 
  mutate(merge_txns_ord, 
  KRRetail = ifelse(Split == 1, GP*0.5 + Fees*0.5 + ifelse(OrdVal > 0, 0.5*CP, -0.5*CP), OrdVal*kr_percent + OrderReversal*kr_percent),
  KR_Vat   = ifelse(Split == 1, GP*0.5 + Fees*0.5 + ifelse(OrdVal > 0, 0.5*CP-0.075*OrdVal, -0.5*CP+0.075*OrdVal), OrdVal*kr_percent_vat + OrderReversal* kr_percent_vat),
  FCG      = ifelse(Split == 1, GP*0.5 + Fees*0.5, GP+Fees-(merge_txns_ord$OrdVal*kr_percent+OrderReversal*kr_percent)),
  FCG_Vat  = ifelse(Split == 1, GP*0.5 + Fees*0.5 + ifelse(OrdVal > 0, -0.075*OrdVal, 0.075*OrdVal), GP+Fees-(merge_txns_ord$OrdVal*kr_percent_vat +OrderReversal*kr_percent_vat)),
  Shared   = ifelse(Split == 1, 1, 0))
                                                                                                          
# deductions / expenses to come off
#other_exp <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/other_expenses.csv", header=TRUE, sep=",")
other_exp <- gs_title("expenses")
other_exp <- as.data.frame(gs_read(other_exp,check.names= T))
other_exp$Date <- as.Date(other_exp$Date, origin = "1970-01-01")
other_exp$FCG.Expenses <- as.numeric(other_exp$FCG.Expenses)
other_exp$Amount <- as.numeric(other_exp$Amount)
other_exp_mnth <- aggregate(cbind(Amount, KRRetail.Expenses, FCG.Expenses) ~ month.abb[month(Date)] + year(Date),
             data= other_exp,FUN=sum)
names(other_exp_mnth)[1] = "mnth"
names(other_exp_mnth)[2] = "yr"
other_exp_mnth$dte <- paste(other_exp_mnth$mnth, other_exp_mnth $yr, sep = "") 

# stock contributions / to come off
#stock <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/stock.csv", header=TRUE, sep=",")
stock <- gs_title("stock_contributions")
stock <- as.data.frame(gs_read(stock,check.names= T))
stock$Date <- as.Date(stock$Date, origin = "1970-01-01")
stock$KR.Retail.Stock <- as.numeric(stock$KR.Retail.Stock)
stock$FCG.Stock <- as.numeric(stock$FCG.Stock)
stock$Total.Order.Value <- as.numeric(stock$Total.Order.Value)
stock_mnth <- aggregate(cbind(Total.Order.Value, KR.Retail.Stock, FCG.Stock) ~ month.abb[month(Date)] + year(Date),
             data= stock,FUN=sum)
names(stock_mnth)[1] = "mnth"
names(stock_mnth)[2] = "yr"
stock_mnth$dte <- paste(stock_mnth$mnth, stock_mnth$yr, sep = "") 

# KR Retail payouts
#kr.retail <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/KRRetail_payouts.csv", header=TRUE, sep=",")
kr.retail <- gs_title("KRRetail_payouts")
kr.retail <- as.data.frame(gs_read(kr.retail,check.names= T))
kr.retail$Date <- as.Date(kr.retail$Date, origin = "1970-01-01")
kr.retail$KR.Retail.Payout <- -1*as.numeric(kr.retail$Amount)
kr.retail_mnth <- aggregate(cbind(Date, KR.Retail.Payout) ~ month.abb[month(Date)] + year(Date),
             data= kr.retail,FUN=sum)
names(kr.retail_mnth)[1] = "mnth"
names(kr.retail_mnth)[2] = "yr"
kr.retail_mnth$dte <- paste(kr.retail_mnth$mnth, kr.retail_mnth$yr, sep = "")

# other incomes (retail: Dry Shower)
#dry_shower_retail <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/dry_shower_retail_sales.csv", header=TRUE, sep=",") 
dry_shower_retail <- gs_title("dry_shower_retail_sales")
dry_shower_retail <- as.data.frame(gs_read(dry_shower_retail,check.names= T))
dry_shower_retail$Date <- as.Date(dry_shower_retail$Date, origin = "1970-01-01")
dry_shower_retail_mnth <- aggregate(cbind(Date, KR.Retail.DryShower, FCG.DryShower) ~ month.abb[month(Date)] + year(Date),
             data= dry_shower_retail,FUN=sum) 
names(dry_shower_retail_mnth)[1] = "mnth"
names(dry_shower_retail_mnth)[2] = "yr"
dry_shower_retail_mnth$dte <- paste(dry_shower_retail_mnth$mnth, dry_shower_retail_mnth$yr, sep = "")

# storage fees and FCG Payouts
stats <- ddply(merge_txns_ord, .( paste(month.abb[month(MaxTxnDte)], year(MaxTxnDte), sep = "" )), summarise, 
           Storage.Fee = sum(StorageFee),
           Payout = sum(Disburse))
names(stats)[1] <- "dte"

# merge expenses, retail sales, storage & payouts (disbursements)
stats <- merge(x = other_exp_mnth, y = stats, by = "dte", all.y = TRUE)
stats <- merge(x = stats, y = stock_mnth, by = "dte", all.x = T)
stats <- merge(x = stats, y = kr.retail_mnth[c("KR.Retail.Payout", "dte")], by = "dte", all.x = T)
stats <- merge(x = stats, y = dry_shower_retail_mnth[c("KR.Retail.DryShower", "FCG.DryShower", "dte")], by = "dte", all.x = T)
stats$KR.Retail.Stock[is.na(stats$KR.Retail.Stock)] <- 0
stats$FCG.Stock[is.na(stats$FCG.Stock)] <- 0
stats$KR.Retail.Payout[is.na(stats$KR.Retail.Payout)] <- 0
stats$KRRetail.Expenses[is.na(stats$KRRetail.Expenses)] <- 0
stats$FCG.Expenses[is.na(stats$FCG.Expenses)] <- 0
stats$KR.Retail.DryShower[is.na(stats$KR.Retail.DryShower)] <- 0
stats$FCG.DryShower[is.na(stats$FCG.DryShower)] <- 0
rownames(stats) <- stats$dte
stats[,c("mnth", "yr", "Amount", "dte", "mnth.y", "yr.y",  "mnth.x", "yr.x", "Total.Order.Value")] <- list(NULL)

merge_txns_ord$OrdID <- as.numeric(merge_txns_ord$OrdID)
all_orders$OrderID <- as.numeric(all_orders$OrderID)
 
# add logical values for counting to get length of shipped / returned
# initiate first   
merge_txns_ord$ship <- FALSE 
merge_txns_ord$ret <- FALSE

# set logicals
merge_txns_ord$ship[merge_txns_ord$Status=="Shipped"] <- TRUE 
merge_txns_ord$ret[merge_txns_ord$Status=="Returned"] <- TRUE 

# clean up messy data (status = shipped & returned with only 3 txns)
x<- plyr::ddply(all_orders, .(OrderID, Status), nrow)

# returned as per order view but shipped as per txn view
return_no_txn <- as.list(merge_txns_ord %>% 
  filter(OrdID %in% x[x$Status=="Returned",]$OrderID) %>%
  group_by(OrdID) %>% 
  summarise(NumTxns = sum(NumTxns)) %>%
  filter(NumTxns == 3) %>%
  select(OrdID))[[1]]

y<- plyr::ddply(x, .(OrderID), nrow)
prob_ord <- y[y$V1>1,]$OrderID

# check number of txns in merge_txns_ord - if return not processed yet exlcude record
num_ord <- plyr::ddply(merge_txns_ord[merge_txns_ord$OrdID %in% prob_ord,], .(OrdID), nrow)
num_txns <- aggregate(merge_txns_ord[merge_txns_ord$OrdID %in% prob_ord,]$NumTxns, by=list(OrdID=merge_txns_ord[merge_txns_ord$OrdID %in% prob_ord,]$OrdID), FUN=sum)
excl_ord <- merge(x=num_ord, y=num_txns, by ="OrdID")
# 3 txns indicates that the return part order has not been accounted for yet, 6 indicates qty of 2 with no return accounted for yet (should be 8 or 10) 
excl_ord$OrdID <- excl_ord[excl_ord$x == 3 | excl_ord$x == 6 | excl_ord$x == 9,]$OrdID

# exlcude problem order from all orders
all_orders <- subset(all_orders, !(OrderID %in% unique(excl_ord$OrdID)) & !(OrderID %in% return_no_txn))

# convert order date to date class
all_orders$"Date" <- anydate(all_orders$"Date")

# order data by month, put each month's data in a list of Dfs
month_lst <- Map(function(x, y) seq(x,y, by = "months"), as.Date('2017-11-01'), as.Date(ceiling_date(today(), "month")-1))
lx <- month_lst[[1]]
orders_list <- lapply(lx, function(x) all_orders[all_orders$Date >= lubridate::floor_date(x, "month") & all_orders$Date <= lubridate::ceiling_date(x, "month")-1,])

# order data incl. txn by month #
orders_txns2017 <- list() # initiate lists
summary_sku <- list()
summary_day <- list()

for (i in 1: length(orders_list)) # populate list with as many entries as there are months
{
	ind1 <- which(merge_txns_ord$OrdID %in% orders_list[[i]]$OrderID)
	orders_txns2017[[i]] <- merge_txns_ord[ind1,]
	
	summary_day[[i]] <- orders_txns2017[[i]] %>% 
      group_by(OrdDate) %>% 
      summarise("Shipped" =sum(ship), "Returned" = sum(ret), "Shipped Sales" = sum(OrdVal), "Return Rate" = sum(ret) / (sum(ret) + sum(ship)), 
                "KR Retail_old" = sum(KRRetail),
                "KR Retail_new" = sum(KR_New),
                "FCG_new" = sum(FCG_New),
                "FCG_old" = sum(FCG)) 
}


for (i in 1: length(orders_list)) # populate list with as many entries as there are months
{
	
	ind2 <- which(merge_txns_ord$OrdID %in% orders_list[[i]]$OrderID)
	orders_txns2017[[i]] <- merge_txns_ord[ind2,]	

	# summary by SKU by month
    summary_sku[[i]] <- ddply(orders_txns2017[[i]], ~ SKU,
                   function(x) c("Shipped" = sum(x$ship),
                     "Returned" = sum(x$ret),
                     "Shipped Sales" = sum(x$OrdVal),
                     "Return Rate" = sum(x$ret) / (sum(x$ret) + sum(x$ship)), 
                     "KR Retail_old" = sum(x$KRRetail),
                     "KR Retail_new" = sum(x$KR_New),
                     "FCG_new" = sum(x$FCG_New),
                     "FCG_old" = sum(x$FCG),
                     "Avg KR Retail_old" = sum(x$KRRetail)/sum(x$ship),
                     "Avg KR Retail_new" = sum(x$KR_New)/sum(x$ship),
                     "Avg FCG_new" = sum(x$FCG_New)/sum(x$ship),
                     "Avg FCG_old" = sum(x$FCG)/sum(x$ship)))    
                     
    mnth <- months(as.Date(orders_txns2017[[i]]$OrdDate,origin = "1970-01-01"))[1]
	mnth <- substr(mnth, start = 1, stop = 3)
	yr <-  year(as.Date(orders_txns2017[[i]]$OrdDate, origin = "1970-01-01"))[1]
	names(orders_txns2017)[i] <- paste(mnth, yr, sep = "")         
	names(orders_list)[i] <- paste(mnth, yr, sep = "")         
	names(summary_sku)[i] <- paste(mnth, yr, sep = "")                  

	# arrange column order for presentation
    orders_txns2017[[i]] <- orders_txns2017[[i]][, c("OrdDate", "OrdID", "Status", "SKU", 
    "FulFil", "SuccessFee", "SFReversal", "ManualCharge", "ManualReversal", "Qty", "SP", "OrdVal", "Vat", "OrderReversal", "CP", "Fees", "KRRetail", "FCG", "Shared", "KR Shared Profit")]
    
    # order by order date desc
    orders_txns2017[[i]]$OrdDate <- as.Date(orders_txns2017[[i]]$OrdDate)
    orders_txns2017[[i]] <- orders_txns2017[[i]][rev(order(orders_txns2017[[i]]$OrdDate)), ] 
    
    # order by most profitable SKU
    summary_sku[[i]] <- summary_sku[[i]][rev(order(summary_sku[[i]][c("KR Retail")])), ]
                     
}






# summary VAT, sales, returns by month
stats_sales <- Map(function(x) ddply(x, .( dte = paste(month.abb[month(OrdDate)], year(OrdDate), sep = "" )), summarise,
                     Sales = sum(OrdVal), 
                     Returns = sum(OrderReversal),
                     Fees = sum(Fees), 
                     Vat = sum(Vat)), orders_txns2017
)

stats_sales <- as.data.frame(do.call(rbind, stats_sales))
stats_sales[,c("dte")] <- list(NULL)

# summary stats orders txns
ord_txns_status <- lapply(orders_txns2017, function(x) {setNames(aggregate(x$OrdID, by=list(Status= x$Status), FUN=length), c("Order Status", "Cnt")) })
ord_status <- lapply(orders_list, function(x) {setNames(aggregate(x$OrderID, by=list(Status= x$Status), FUN=length), c("Order Status", "Cnt")) })

# number orders per day
orders_day <- lapply(orders_txns2017, function(x) {setNames(aggregate(x$OrdID, by=list(Date = x$OrdDate), FUN=length), c("Date", "Orders")) })

# ave number orders per day
# orders_day <- lapply(orders_txns2017, function(x) {setNames(aggregate(x$OrdID, by=list(Date = x$OrdDate), FUN=avg), c("Date", "Orders")) })


# order by most profitable day of sales
for (i in 1: length(orders_day)) # populate list with as many entries as there are months
{
   #orders_day[[i]] <- orders_day[[i]][rev(order(orders_day[[i]]$Date)), ]
   orders_day[[i]] <- orders_day[[i]][rev(order(orders_day[[i]]$Orders)), ]
   orders_day[[i]]$Day <- weekdays(as.Date(orders_day[[i]]$Date, origin = "1970-01-01"))
}


# prepare name of CSV sink file
Year <- as.character(year(Sys.Date()))
Month <- month.abb[month(Sys.Date())]
Day <- as.character(day(Sys.Date()))


# =HyperLink("http://www.virtuousbicycle.com/", "Learn to Ride a Bike")

# start a sink file with a CSV extension
sink(paste0("/Users/justinmichell/Downloads/takealot_admin/takealot_order_details_clicker", Day, Month, Year, ".csv" ))

earnings_list <- list()
for (i in 1: length(orders_txns2017)) 
{
	earnings_list[[i]] <- orders_txns2017[[i]][, c("KRRetail", "FCG")]
    mnth <- months(as.Date(orders_txns2017[[i]]$OrdDate,origin = "1970-01-01"))[1]
	mnth <- substr(mnth, start = 1, stop = 3)
	yr <-  year(as.Date(orders_txns2017[[i]]$OrdDate, origin = "1970-01-01"))[1]    
    names(earnings_list)[i] <- paste(mnth, yr, sep = "")
}

earnings <- do.call(rbind,lapply(earnings_list, colSums))
earnings <- as.data.frame(earnings)
earnings$sort_date <- paste(substr(row.names(earnings), 4,7),substr(row.names(earnings), 1,3),'01',sep="-")
earnings$sort_date <- as.Date(strptime(earnings$sort_date ,format="%Y-%b-%d"))

earnings <- merge(earnings, stats, by = "row.names",all.x=TRUE)
rownames(earnings) <- earnings$Row.names
earnings$Row.names <- NULL
earnings <- merge(earnings, stats_sales, by = "row.names",all.x=TRUE)

earnings$KRRetail.Expenses[is.na(earnings$KRRetail.Expenses)] <- 0
earnings$FCG.Expenses[is.na(earnings$FCG.Expenses)] <- 0
earnings$"KRRetail.Total.Due" <- earnings$"KRRetail"+earnings$"KRRetail.Expenses"+ 0.5*earnings$"Storage.Fee" + earnings$"KR.Retail.Stock" + earnings$"KR.Retail.Payout"+earnings$"KR.Retail.DryShower"
earnings$"FCG.Total.Profit" <- earnings$"FCG" + earnings$"FCG.Expenses"  + 0.5*earnings$"Storage.Fee" + earnings$"FCG.Stock"+earnings$"FCG.DryShower"
earnings <- rename(earnings, c("KRRetail" = "KRRetail.Earnings", "FCG" = "FCG.Profit"))
earnings <- earnings[rev(order(earnings$sort_date)),]
rownames(earnings) <- earnings$Row.names
earnings[,c("sort_date", "Row.names")] <- list(NULL)

# KR Retail Summary
kr.retail <- earnings[c("KRRetail.Earnings", "KR.Retail.DryShower", "KRRetail.Expenses", "Storage.Fee", "KR.Retail.Stock", "KR.Retail.Payout","KRRetail.Total.Due")]
kr.retail$Storage.Fee <- 0.5*kr.retail$Storage.Fee
kr.retail <- rename(kr.retail, c("KRRetail.Earnings" = "Earnings", "KR.Retail.DryShower"= "DryShower Retail","KRRetail.Expenses" = "Expenses", "Storage.Fee" = "Storage Fee", "KR.Retail.Stock" = "Stock", "KR.Retail.Payout" = "Payout", "KRRetail.Total.Due" = "Total Due"))

# FCG Summary
fcg <- earnings[c("FCG.Profit", "FCG.DryShower", "FCG.Expenses", "Storage.Fee", "Payout", "FCG.Stock", "FCG.Total.Profit")]
fcg$Storage.Fee <- 0.5*fcg$Storage.Fee
fcg <- rename(fcg, c("FCG.Profit" = "Profit", "FCG.DryShower"= "DryShower Retail", "FCG.Expenses" = "Expenses", "Storage.Fee" = "Storage Fee", "FCG.Stock" = "Stock", "FCG.Total.Profit" = "Total Profit"))


cat('---------------------------------------------------------------Sales Summary---------------------------------------------------------------')
cat('\n')
write.csv(earnings[c("Sales", "Returns", "Fees", "Storage.Fee", "Vat")], quote = F, row.names = T)
cat('\n')
cat('\n')
cat('\n')

cat('---------------------------------------------------------------KR Retail Summary. Total Due: R',sum(kr.retail$"Total Due"),'---------------------------------------------------------------')
cat('\n')
write.csv(kr.retail, quote = F, row.names = T)

cat('\n')
cat('\n')
cat('\n')

cat('---------------------------------------------------------------FCG Summary---------------------------------------------------------------')
cat('\n')
write.csv(fcg, quote = F, row.names = T)

cat('\n')
cat('\n')
cat('\n')

cat('---------------------------------------------------------------Stock Order Summary---------------------------------------------------------------')
cat('\n')

stock <- stock[rev(order(stock$Date)),]
write.csv(stock, quote = F, row.names = F)

cat('\n')
cat('\n')
cat('\n')

for (i in length(orders_txns2017):1) # populate list with as many entries as there are months
{
   cat('---------------------------------------------------------------',names(orders_txns2017)[i], 'Orders---------------------------------------------------------------')
   cat('\n')
   cat('Summary')
   cat('\n')
   cat('KR Retail Earnings: R', paste(sum(orders_txns2017[[i]]$"KRRetail")), '     FCG Profit: R', paste(sum(orders_txns2017[[i]]$"FCG")))
   cat('\n')
   write.csv(ord_status[[i]], row.names = FALSE)  
   cat('\n')
   cat(' Order Summary by SKU ')
   cat('\n')
   write.csv(summary_sku[[i]], row.names = FALSE)
   cat('\n')
   cat('\n')
   cat('\n')   
   write.csv(orders_txns2017[[i]], row.names = FALSE)
   cat('\n')
   cat('\n')
   cat('\n')   
   cat('\n')
   cat('\n')
   cat('\n')
}   

# close the sink
sink()


#takealot_orders <- gs_upload(paste0("/Users/justinmichell/Downloads/takealot_admin/takealot_order_details_", Day, Month, Year, ".csv" ), overwrite = T)  

takealot_orders <- gs_upload(paste0("/Users/justinmichell/Downloads/takealot_admin/takealot_order_details_clicker", Day, Month, Year, ".csv" ), overwrite = T)  


# move file to correct folder in google drive
drive_mv(file = paste0("takealot_order_details_clicker", Day, Month, Year), path = paste0("Takealot - Clicker Way/")) 








