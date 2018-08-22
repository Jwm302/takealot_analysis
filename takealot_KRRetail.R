
library(plyr)
#library(tidyverse)
library(purrr)
library(dplyr)
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
# T0 DO only get order id not TSIN  
txns <- within(txns, OrderID <- ifelse(grepl("Order", txns$Reference), str_extract(sub('.*\\:', '', txns$Reference), "[0-9]+"), 0))

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

# strip out order ID from Transaction.Description column for manual txns (such as customer credits and manual order processing)       
txns <- mutate(txns, OrderID = case_when(
                                 grepl("Order", Transaction.Description) & (Transaction.Type == 'Manual Charge' | txns$Transaction.Type == 'Manual Reversal') ~ 
                                        str_extract(sub('.*\\:', '', Transaction.Description), "[0-9]+"),
                                 TRUE ~ as.character(OrderID)))   
                                        
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

# sequence with control over ranking order
# library(data.table)
#dt <- data.table(df)
#dt[, .( val
#   , num = rank(val))
#    , by = list(cat)][order(cat, num),]
# https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame


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
txns$Transaction.Date <- anydate(txns$Transaction.Date)

txns$success_percent <- with(txns, ave(success_percent, OrderID, FUN = function(x) (max(x)) ))

# calculate the success fees in order to identify any mismatched fees with part orders
for (i in 1: nrow(txns)) {
  	txns$Incl.VAT_x[i] <- ifelse(txns$Transaction.Type[i] == "Success Fee Charge" | txns$Transaction.Type[i] == "Success Fee Reversal", 
    	ifelse(txns$Transaction.Date[i] < anydate('2018-04-01'), -1*txns$OrdVal[i] * txns$success_percent[i]/100 + (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.14, 
    	  -1*txns$OrdVal[i] * txns$success_percent[i]/100 + (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.15), txns$Incl.VAT[i])     
    
    txns$VAT_x[i] <- ifelse(txns$Transaction.Type[i] == "Success Fee Charge" | txns$Transaction.Type[i] == "Success Fee Reversal", 
    	ifelse(txns$Transaction.Date[i] < anydate('2018-04-01'), (-1*txns$OrdVal[i] * txns$success_percent[i]/100)*0.14, 
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
txns$Transaction.Type<-ifelse(txns$Transaction.Type %in% c('Manual Reversal', 'Manual Payment', 'Storage Fee Rebate') & grepl('Storage Fee',txns$Transaction.Description), 'Storage Fee Discount/Rebate', txns$Transaction.Type)

# find the storage costs for each product
# find the indexes of all storage related txns
storage_ind <- which(txns$Transaction.Type %in% "Storage Fee Charge" | txns$Transaction.Type %in% "Storage Fee Discount/Rebate")
# extract only the 8 digit TSIN to match with each product
txns$TSIN <- 0
txns[storage_ind,]["TSIN"] <- ifelse(txns[storage_ind,]["Reference.Type"] == 'TSIN', c(str_match(txns[storage_ind,]$"Reference", "\\d\\d\\d\\d\\d\\d\\d\\d")), c(str_match(txns[storage_ind,]$"Transaction.Description", "\\d\\d\\d\\d\\d\\d\\d\\d")))


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


# replace broken part orders with new order IDs
old <- (txns_ord_prt$old_OrderID)
new <- (txns_ord_prt$new_OrderID)

merge_txns_ord$OrderID <- plyr::mapvalues(merge_txns_ord$OrderID,
     from = old,
     to = new)

# merge again after correction of part order OrderIDs 
merge_txns_ord <- merge(x = merge_txns_ord, y = orders[c("OrderID", "SKU", "Qty", "Total", "Unit.Price")], by.x = "OrderID", by.y = "OrderID", all.x = TRUE)

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
merge_txns_ord <- dplyr::rename(merge_txns_ord, CP = Cost, OrdDate = Date, SKU = SKU.y, Qty = Qty.y, Total = Total.y, SP = Unit.Price.y)
returns <- dplyr::rename(returns, CP = Cost, OrdDate = Date)

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

# only interested in orders (not disburse, subs, etc)
merge_txns_ord$OrderID <- ifelse(merge_txns_ord$OrdID == 0, 0, merge_txns_ord$OrderID)

# get length of order parts 
# 1 = normal shipped order
# 2 = part order
# 3 = return
# 4 = part order return
merge_txns_ord$ord_len <- unlist(map(strsplit(merge_txns_ord$OrderID, "_"), length))

# create new column for part returned orders: ord id concatenated with value after last '_', 1 = shipped order, 2 = returned
merge_txns_ord <- mutate(merge_txns_ord, ord_part_ret = case_when(
                              ord_len == 4 ~ paste(OrdID, map_chr(OrderID, function(s) strsplit(s, "_")[[1]][2]), sep='_'), 
                              TRUE ~ as.character(0)))   

# get SKU info for part order returns (ord len = 4)
part_ret_txns <- merge_txns_ord[merge_txns_ord$ord_len == 4,] 
part_ret_txns <- left_join(select(part_ret_txns, -c(CP, SKU, Qty, Total, OrdDate, OrdIDs, TSIN, Length)), select(returns, c(OrdDate, OrderID, SKU, Qty, CP, Unit.Price, Total, TSIN)) , by = c("ord_part_ret" = "OrderID"))

# get SKU info for returns (ord len = 3)
ret_txns <- merge_txns_ord[merge_txns_ord$ord_len == 3,] 
ret_txns <- left_join(select(ret_txns, -c(CP, SKU, Qty, Total, OrdDate, OrdIDs, TSIN, Length)), select(returns, c(OrdDate, OrdID, SKU, Qty, CP, Unit.Price, Total, TSIN)) , by = c("OrdID" = "OrdID"))

# combine / union return and part return orders 
m.new <- rbind(ret_txns, part_ret_txns)

# rename, remove and rearrange columns of both dfs before union
m.new <- dplyr::mutate(m.new, SP = Unit.Price)
m.new <- select(m.new, -c(ord_part_ret))
merge_txns_ord <- select(merge_txns_ord, -c(Length,ord_part_ret))
m.new <- select(m.new, c("OrdDate","OrdID","Status", "SKU","OrdVal","SuccessPercent","SuccessFee","FulFil","OrderReversal","SFReversal","Disburse","SubscFee", "ManualCharge",
                         "ManualReversal","StorageFee", "Vat", "Qty","CP","Total","SP","MaxTxnID","Balance","MaxTxnDte","NumTxns","TSIN","ord_len", "OrderID"))
merge_txns_ord <- select(merge_txns_ord, c("OrdDate","OrdID","Status", "SKU","OrdVal","SuccessPercent","SuccessFee","FulFil","OrderReversal","SFReversal","Disburse","SubscFee", "ManualCharge",
                         "ManualReversal","StorageFee", "Vat", "Qty","CP","Total","SP","MaxTxnID","Balance","MaxTxnDte","NumTxns","TSIN","ord_len", "OrderID"))


# combine all return orders with shipped orders
merge_txns_ord <- rbind(merge_txns_ord[merge_txns_ord$ord_len < 3,], m.new)

# set status of returned orders to "Returned"
merge_txns_ord$Status <- as.character(merge_txns_ord$Status)
merge_txns_ord$Status <- ifelse(merge_txns_ord$OrderReversal < 0, "Returned", merge_txns_ord$Status)

# get shipped & returned orders
ship_returned <- merge(returns, orders[!names(orders) %in% c("OrderID")], by.x = "OrderID", by.y = "OrdIDs")$OrderID

# fix orders with shipped & returned status, i.e. set status to shipped if ord val > 0
merge_txns_ord$Status <- ifelse(merge_txns_ord$OrdID %in% ship_returned & merge_txns_ord$OrdVal > 0, "Shipped", merge_txns_ord$Status)


# updating cost price (new stock prices)

#update_cp <- read.csv(file="/Users/justinmichell/Downloads/takealot_admin/updated_costs.csv", header=TRUE, sep=",")
update_cp <- gs_title("updated_costs")
update_cp <- as.data.frame(gs_read(update_cp,check.names= T))
update_cp$Cost_Start <- anydate(update_cp$Cost_Start)
update_cp$Cost_End   <- anydate(update_cp$Cost_End) 

v <- merge(merge_txns_ord, update_cp[c("SKU","Cost","Cost_Start","Cost_End", "Split")],by=c("SKU"),all.x = T)

# update CP
v$CP <- ifelse(is.na(v$Cost), v$CP, 
          ifelse(v$OrdDate>=v$Cost_Start & v$OrdDate<=v$Cost_End,v$Cost,v$CP))

# remove rows outside of update CP bounds and keep rows that don't have a cost to be updated (i.e. keep rows with Cost = NA)
v <- v[-which(!(v$OrdDate>=v$Cost_Start & v$OrdDate<=v$Cost_End)),] 

merge_txns_ord <- NULL
merge_txns_ord <- v


# testing #
# check if all SKUs of returned part orders match (by checking ord value and success fee)
# use first txn date when matching to get correct VAT rate (e.g. if orgiginal shipped txn was before VAT increase use 0.14)
#m.new <- m.adj
#m.new$MinOrdTxnDte <- ave(m.new$MaxTxnDte, m.new$OrdID, FUN = min)
#m.new <-dplyr::mutate(m.new, abs.SuccessFee = ifelse(OrdVal == 0, SFReversal, abs(SuccessFee)), abs.OrdVal = ifelse(OrdVal == 0, abs(OrderReversal), OrdVal))
#m.new <-dplyr::mutate(m.new, deriv_success_fee = round(Total * SuccessPercent/100 + Total * SuccessPercent/100*ifelse(as.Date(MinOrdTxnDte) >= '2018-04-01', 0.15, 0.14),2))
#m.new <-dplyr::mutate(m.new,diff = abs(abs.SuccessFee - deriv_success_fee))
# allow for small rounding differences
#m.new <- dplyr::mutate(m.new, check = ifelse(abs(abs.SuccessFee - deriv_success_fee) <= 0.011, 1, 0))
#m.new <- mutate(m.new, ord_part_ret = case_when(
#                              ord_len == 4 ~ paste(OrdID, map_chr(OrderID, function(s) strsplit(s, "_")[[1]][2]), sep='_'), 
#                              TRUE ~ as.character(0))) 
#m.old <- select(merge_txns_ord, c("OrdDate","OrdID","Status", "SKU","OrdVal","SuccessPercent","SuccessFee","FulFil","OrderReversal","SFReversal","Disburse","SubscFee", "ManualCharge",
#                         "ManualReversal","StorageFee", "Vat", "Qty","CP","Total","SP","MaxTxnID","Balance","MaxTxnDte","NumTxns","TSIN","ord_len", "OrderID"))
# see if only part of the order return cases are present
#select(m.new, ord_part_ret, OrdID, OrderID, SKU, Total, OrdVal, OrderReversal, check, abs.SuccessFee, SuccessFee, SFReversal) %>% filter(check==0) %>% arrange(OrdID)
#select(m.new, ord_part_ret, OrdID, OrderID, SKU, Total, OrdVal, OrderReversal, check, abs.SuccessFee, SuccessFee, SFReversal) %>% filter(check==0 & Total * 2 !=OrdVal) %>% arrange(OrdID)


# format & rename & reorder columns 
cols <- c("GP", "Fees", "KRRetail", "KR Shared Profit", "FCG", "FCGTotal", "CalcBal", "KR GP_0.4 (excl. fees)", "KR Fees_0.5", "FCG GP_0.6 (excl. fees)", "FCG Fees_0.5")
vals <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
sapply(1:length(cols), function(i)
   merge_txns_ord[, cols[i]] <<- as.numeric(vals[i])
)




# calculate fees, GP & split
merge_txns_ord$"Fees" <- merge_txns_ord$"FulFil" + merge_txns_ord$"SuccessFee" + merge_txns_ord$"SFReversal"+ merge_txns_ord$"ManualCharge" + merge_txns_ord$"ManualReversal" 
merge_txns_ord$"GP" <- ifelse(merge_txns_ord$OrdVal > 0, 
                         merge_txns_ord$OrdVal - (merge_txns_ord$"CP" * merge_txns_ord$"Qty"),
                         merge_txns_ord$OrderReversal + (merge_txns_ord$"CP" * merge_txns_ord$"Qty"))

# merge_txns_ord$SKU %in% shared_profit &

# adjust profit according to capital contribution
merge_txns_ord$"KR GP_0.4 (excl. fees)" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"GP" * 0.5, merge_txns_ord$"GP" * 0.4)
merge_txns_ord$"KR Fees_0.5" <- (merge_txns_ord$"Fees" * 0.5)                         
# add half of the CP to KR Retail earnings for shared stock
merge_txns_ord$"KRRetail" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"KR GP_0.4 (excl. fees)" + merge_txns_ord$"KR Fees_0.5" + ifelse(merge_txns_ord$OrdVal > 0, 0.5*merge_txns_ord$CP, -1*0.5*merge_txns_ord$CP), merge_txns_ord$"KR GP_0.4 (excl. fees)" + merge_txns_ord$"KR Fees_0.5")   

merge_txns_ord$"FCG GP_0.6 (excl. fees)" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"GP" * 0.5, merge_txns_ord$"GP" * 0.6)
merge_txns_ord$"FCG Fees_0.5" <- merge_txns_ord$"Fees" * 0.5                      
merge_txns_ord$"FCG" <- merge_txns_ord$"FCG GP_0.6 (excl. fees)" + merge_txns_ord$"FCG Fees_0.5"


# update KR Retail earnings as of 1 June 2018 (10% of net sales)
kr_percent <- 0.1
new_agree <- which(merge_txns_ord$OrdDate >= '2018-06-01')
merge_txns_ord[new_agree,]$"KRRetail" <- ifelse(merge_txns_ord[new_agree,]$Split == 1, merge_txns_ord[new_agree,]$"KRRetail",
merge_txns_ord[new_agree,]$OrdVal*kr_percent+merge_txns_ord[new_agree,]$OrderReversal*kr_percent)

merge_txns_ord$"KR Shared Profit" <- ifelse(merge_txns_ord$Split == 1, merge_txns_ord$"FCG", 0) 

#merge_txns_ord$"FCG_New" <- ifelse(merge_txns_ord$Split == 0, merge_txns_ord$"GP" + merge_txns_ord$"Fees"-(merge_txns_ord$OrdVal*kr_percent+ merge_txns_ord$OrderReversal*kr_percent), merge_txns_ord$"FCG")    

merge_txns_ord$"Shared" <- 0
merge_txns_ord$"Shared" <- ifelse(merge_txns_ord$Split == 1, 1, 0) 

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
names(other_exp_mnth)[3] = "Expenses"
other_exp_mnth$Expenses <- -1*other_exp_mnth$Expenses
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

storage_product <- ddply(txns[txns$TSIN != 0,][c("Transaction.Date", "TSIN", "Incl.VAT_x")], .(paste(month.abb[month(Transaction.Date)], year(Transaction.Date), sep = "" ), TSIN), numcolwise(sum))
storage_product <- merge(x = storage_product, y = cost[c("Product", "SKU", "TSIN")], by = "TSIN", all.x = TRUE)

names(storage_product)[2] <- "Date"
names(storage_product)[3] <- "Storage.Fee"

# merge storage fees with TSIN 
m <- merge(x=storage_product, y=update_cp[c("Cost_Start","Cost_End","Split","TSIN")],by = "TSIN", all.x = TRUE)

# remove products which are not shared 
m <- na.omit(m)

# https://stackoverflow.com/questions/47585254/check-whether-string-is-contained-within-a-list

# expand all dates per row by month intervals and put into a list 
lst <- Map(function(x, y) seq(x,y, by = "months"), update_cp$Cost_Start, ceiling_date(update_cp$Cost_End, "month")-1)

# make the TSIN the name of each list element
names(lst) <- update_cp$TSIN

#lst[names(lst2) == "52562803"]

# convert list to df adding NAs for empty cells
d <- ldply(lst, rbind)

# remove TSIN id column (used for verifying and matching dates with TSIN in previous step)
d[1] <- NULL

# convert each numeric cell back to date class (origin = '1970-01-01')
for (i in 1:ncol(d)){
  d[[i]] <- paste(month.abb[month(as.Date(d[[i]], origin ="1970-01-01"))], year(as.Date(d[[i]], origin ="1970-01-01")), sep="")	
}

storage_update_cp <- dplyr:::bind_cols(update_cp[c("TSIN", "Split")], d)

# matrix approach to matching months active storage months with TSIN 
#y<-data.frame(month=c('Nov2017','Dec2017','Jan2018','Feb2018','Mar2018','Apr2018','May2018','Jun2018', 'Jul2018'))
#y$month<-as.character(y$month)
#tsin_iterations = length(storage_update_cp$TSIN)
#month_iterations = nrow(y)
#output <- matrix(ncol= month_iterations, nrow=tsin_iterations)
#colnames(output) <- c('Nov2017','Dec2017','Jan2018','Feb2018','Mar2018','Apr2018','May2018','Jun2018', 'Jul2018')
#rownames(output) = storage_update_cp$TSIN[1:nrow(storage_update_cp)]
#for(i in 1: tsin_iterations){
#  for(j in 1: month_iterations){ 
  	# dates range from Nov2017 to Dec2019 - total of 24 possible months **will need to be updated after Dec2019
#    output[i,j] <- ifelse(any(grepl(y$month[j], storage_update_cp[i,3:26]) & storage_update_cp$Split[i] == 1), 1, 0)
# }
#}

# testing
#u$keep <- 0
#for(i in 1: nrow(u)){
	# dates range from Nov2017 to Dec2019 - total of 24 possible months **will need to be updated after Dec2019
#	u$keep[i] <- sum(ifelse(grepl(u$Date[i], paste(u[i,7:32])), 1, 0))
#}

# merge storage fees with storage month details for each TSIN  
storage_shared_final <- merge(x=storage_product, y=storage_update_cp, by = "TSIN", all.x=TRUE)

# loop through each row and check if the month is contained in the range of active storage months for each month's storage fee
storage_shared_final$keep <- 0
for(i in 1: nrow(storage_shared_final)){
	# dates range from Nov2017 to Dec2019 - total of 24 possible months **will need to be updated after Dec2019
	storage_shared_final$keep[i] <- sum(ifelse(grepl(storage_shared_final$Date[i], paste(storage_shared_final[i,7:32])), 1, 0))
}


# remove rows where storage month is not in range of active months and if product not shared and also storage fees before '2018-06-01'
storage_shared_final <- storage_shared_final[storage_shared_final$keep > 0 & storage_shared_final$Split == 1 & storage_shared_final$Date != 'May2018',]

# list all shared products by month as of '2018-06-01' (new_agree)
shared_stor_prod_month <- ddply(storage_shared_final[storage_shared_final$Split == 1,], .(Date), summarize, Storage.Shared.SKUs = toString(SKU))
names(shared_stor_prod_month)[names(shared_stor_prod_month) == 'Date'] <- 'dte'

# group shared storage by month
storage_shared_month <- aggregate(storage_shared_final$Storage.Fee, by=list(dte=storage_shared_final$Date), FUN=sum)
names(storage_shared_month)[names(storage_shared_month) == 'x'] <- 'KR.Storage.Shared'

# KR Retail Storage Fee based 50% of shared products as of '2018-06-01' (new_agree)
storage_shared_month$'KR.Storage.Shared' <- storage_shared_month$'KR.Storage.Shared'/2

# merge storage fees with SKUs by month
storage_shared_month <- merge(x= storage_shared_month, y=shared_stor_prod_month, by = "dte", all.x = TRUE)

# merge shared storage fees, expenses, retail sales, storage & payouts (disbursements)
stats <- merge(x = storage_shared_month, y = stats, by = "dte", all.y = TRUE)
stats <- merge(x = other_exp_mnth, y = stats, by = "dte", all.y = TRUE)
stats <- merge(x = stats, y = stock_mnth, by = "dte", all.x = T)
stats <- merge(x = stats, y = kr.retail_mnth[c("KR.Retail.Payout", "dte")], by = "dte", all.x = T)
stats <- merge(x = stats, y = dry_shower_retail_mnth[c("KR.Retail.DryShower", "FCG.DryShower", "dte")], by = "dte", all.x = T)
stats$KR.Storage.Shared[is.na(stats$KR.Storage.Shared)] <- 0
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
x<-ddply(all_orders, .(OrderID, Status), nrow)
y<- ddply(x, .(OrderID), nrow)
prob_ord <- y[y$V1>1,]$OrderID

# check number of txns in merge_txns_ord - if return not processed yet exlcude record
num_ord <- ddply(merge_txns_ord[merge_txns_ord$OrdID %in% prob_ord,], .(OrdID), nrow)
num_txns <- aggregate(merge_txns_ord[merge_txns_ord$OrdID %in% prob_ord,]$NumTxns, by=list(OrdID=merge_txns_ord[merge_txns_ord$OrdID %in% prob_ord,]$OrdID), FUN=sum)
excl_ord <- merge(x=num_ord, y=num_txns, by ="OrdID")
# 3 txns indicates that the return part order has not been accounted for yet, 6 indicates qty of 2 with no return accounted for yet (should be 8 or 10) 
excl_ord$OrdID <- excl_ord[excl_ord$x == 3 | excl_ord$x == 6 | excl_ord$x == 9,]$OrdID

# also exlcude transaction where return has not been processed (shipped as per txns but returned ord status)
excl <- merge_txns_ord %>% filter(is.na(OrdDate) & OrdVal >0) %>% select(OrdID)

# exlcude problem order from all orders
all_orders <- subset(all_orders, !(OrderID %in% unique(excl_ord$OrdID)) & !(OrderID %in% as.list(excl)[[1]]))

# convert order date to date class
all_orders$"Date" <- anydate(all_orders$"Date")

# order data by month, put each month's data in a list of Dfs
month_lst <- Map(function(x, y) seq(x,y, by = "months"), as.Date('2017-11-01'), as.Date(ceiling_date(today(), "month")-1))
lx <- month_lst[[1]]
orders_list <- lapply(lx, function(x) all_orders[all_orders$Date >= floor_date(x, "month") & all_orders$Date <= ceiling_date(x, "month")-1,])

# order data incl. txn by month #
orders_txns2017 <- list() # initiate lists
summary_sku <- list()
summary_day <- list()

for (i in 1: length(orders_list)) # populate list with as many entries as there are months
{
	
	ind1 <- which(merge_txns_ord$OrdID %in% orders_list[[i]]$OrderID)
	orders_txns2017[[i]] <- merge_txns_ord[ind1,]

    summary_day[[i]] <- ddply(orders_txns2017[[i]], ~ OrdDate,
                   function(x) c("Shipped" = sum(x$ship),
                     "Returned" = sum(x$ret),
                     "Shipped Sales" = sum(x$OrdVal),
                     "Return Rate" = sum(x$ret) / (sum(x$ret) + sum(x$ship)), 
                     "KR Retail" = sum(x$KRRetail)))

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
                     "KR Retail" = sum(x$KRRetail),
                     "Avg KR Retail" = sum(x$KRRetail)/sum(x$ship)))
                      #"Shared" = max(shared))) 

    # names list elements according to month year                 
    mnth <- months(as.Date(orders_txns2017[[i]]$OrdDate,origin = "1970-01-01"))[1]
	mnth <- substr(mnth, start = 1, stop = 3)
	yr <-  year(as.Date(orders_txns2017[[i]]$OrdDate, origin = "1970-01-01"))[1]
	names(orders_txns2017)[i] <- paste(mnth, yr, sep = "")         
	names(orders_list)[i] <- paste(mnth, yr, sep = "")         
	names(summary_sku)[i] <- paste(mnth, yr, sep = "")         

	# remove unwanted columns
	# orders_txns2017[[i]] <- orders_txns2017[[i]][ , -which(names(orders_txns2017[[i]]) %in% c("FCGTotal", "CalcBal", "OrdIDs", "OrderID", "Disburse", "SubscFee", "OrdPartRet", "ship", "ret", "GP"))]        
    
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
sink(paste0("/Users/justinmichell/Downloads/takealot_admin/takealot_order_details_krretail", Day, Month, Year, ".csv" ))

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
earnings <- dplyr::rename(earnings, KRRetail.Earnings = KRRetail, FCG.Profit = FCG)
earnings <- earnings[rev(order(earnings$sort_date)),]
rownames(earnings) <- earnings$Row.names

# adjust expenses & storage to 0 for orders after June - new arrangement
earnings$"KRRetail.Expenses.Adj" <- earnings$"KRRetail.Expenses"
earnings[earnings$sort_date >= '2018-06-01',]$"KRRetail.Expenses.Adj" <- 0
earnings$"Storage.Fee.Adj" <- earnings$"Storage.Fee"/2
earnings[earnings$sort_date >= '2018-06-01',]$"Storage.Fee.Adj" <- 0
earnings$KR.Storage.Shared <- earnings$"KR.Storage.Shared" +  earnings$"Storage.Fee.Adj"


# add up total owing
earnings$"KRRetail.Total.Due" <- earnings$"KRRetail.Earnings" + earnings$"KRRetail.Expenses.Adj" +  earnings$"KR.Storage.Shared" + earnings$"KR.Retail.Stock" + earnings$"KR.Retail.Payout" + earnings$"KR.Retail.DryShower"

earnings$"FCG.Total.Profit" <- earnings$"FCG.Profit" + earnings$"FCG.Expenses"  + 0.5*earnings$"Storage.Fee" + earnings$"FCG.Stock"+earnings$"FCG.DryShower"

# remove unwanted columns
earnings[,c("sort_date", "Row.names")] <- list(NULL)

# KR Retail Summary
kr.retail <- earnings[c("KRRetail.Earnings", "KR.Retail.DryShower", "KRRetail.Expenses.Adj", "KR.Storage.Shared", "KR.Retail.Stock", "KR.Retail.Payout","KRRetail.Total.Due")]
kr.retail <- plyr::rename(kr.retail, c("KRRetail.Earnings" = "Earnings", "KR.Retail.DryShower"= "DryShower Retail","KRRetail.Expenses.Adj" = "Expenses", "KR.Storage.Shared" = "Shared Storage Fee", "KR.Retail.Stock" = "Stock", "KR.Retail.Payout" = "Payout", "KRRetail.Total.Due" = "Total Due"))

# FCG Summary
#fcg <- earnings[c("FCG.Profit", "FCG.DryShower", "FCG.Expenses", "Storage.Fee", "Payout", "FCG.Stock", "FCG.Total.Profit")]
#fcg$Storage.Fee <- 0.5*fcg$Storage.Fee
#fcg <- rename(fcg, c("FCG.Profit" = "Profit", "FCG.DryShower"= "DryShower Retail", "FCG.Expenses" = "Expenses", "Storage.Fee" = "Storage Fee", "FCG.Stock" = "Stock", "FCG.Total.Profit" = "Total Profit"))

# shared stock stats

# matrix approach to matching active storage months with TSIN 
y<-data.frame(month=c('Nov2017','Dec2017','Jan2018','Feb2018','Mar2018','Apr2018','May2018','Jun2018', 'Jul2018', 'Aug2018'))
y$month<-as.character(y$month)
tsin_iterations = length(storage_update_cp$TSIN)
month_iterations = nrow(y)
output <- matrix(ncol= month_iterations, nrow=tsin_iterations)
colnames(output) <- c('Nov2017','Dec2017','Jan2018','Feb2018','Mar2018','Apr2018','May2018','Jun2018', 'Jul2018','Aug2018')
rownames(output) = storage_update_cp$TSIN[1:nrow(storage_update_cp)]
for(i in 1: tsin_iterations){
  for(j in 1: month_iterations){ 
  	 # dates range from Nov2017 to Dec2019 - total of 24 possible months **will need to be updated after Dec2019
   output[i,j] <- ifelse(any(grepl(y$month[j], storage_update_cp[i,3:26]) & storage_update_cp$Split[i] == 1), 1, 0)
 }
}

# flatten and convert to DF
shared_df <- as.data.frame(as.table(output))
shared_df <- plyr::rename(shared_df, c("Var1" = "TSIN", "Var2" = "Month", "Freq" = "Shared"))
shared_df <- shared_df[shared_df$Shared > 0,]

# get SKU by merging 
x <- merge(x=shared_df, y=update_cp[c("TSIN", "SKU")], by="TSIN", all.x=TRUE)
x <- subset(x,!duplicated(x[c("TSIN","Month","Shared","SKU")]))

# find the most recent (max) order date for each SKU by month: to see which shared SKUs are active in the month
all_orders$Month <- paste(month.abb[month(all_orders$Date)], year(all_orders$Date), sep = "")
most_recent_mnth <- aggregate(Date ~ SKU + Month, data = all_orders, max, na.rm = TRUE)
most_recent_mnth <- plyr::rename(most_recent_mnth, c("Date" = "Last.Sale.In.Month"))
# find the amount of sales per month / sku and merge to most recent order date
sale_value_month <- aggregate(Total ~ SKU + Month, data = all_orders[all_orders$Status=="Shipped",], sum, na.rm = TRUE)
sale_value_month <- plyr::rename(sale_value_month, c("Total" = "Total.Ship.Sales"))
shipped_sales <- aggregate(OrderID ~ SKU + Month, data = all_orders[all_orders$Status=="Shipped",], function(x) sum(!duplicated(x)))
most_recent <- merge(x=sale_value_month, y= most_recent_mnth, by=c("SKU", "Month"), all.x=TRUE) 
most_recent <- merge(x= most_recent, y= shipped_sales, by=c("SKU", "Month"), all.x=TRUE) 
most_recent <- plyr::rename(most_recent, c("OrderID" = "Shipped.Sales"))

# merge again to see if there was a sale in each shared month, i.e. if sharing is active for that month
shared_final <- merge(x=x, y= most_recent, by=c("SKU", "Month"), all.x=TRUE)

# remove non active records
shared_final <- na.omit(shared_final)

# get profit from shared stock for each month
shared_profit <- lapply(orders_txns2017, function(x) sum((x["KR Shared Profit"])))
shared_profit <- data.frame(Month=names(shared_profit), KR.Profit= unlist(shared_profit))
rownames(shared_profit) <- NULL

# number of shared products grouped by month
shared_summary <- plyr::ddply(shared_final, .(Month), summarize,
                        Total.SKUs = sum(Shared), 
                        SKUs = toString(SKU),
                        Max.Date = max(Last.Sale.In.Month),
                        Shipped.Orders = sum(Total.Ship.Sales),
                        Shipped.Sales.Amt = sum(Shipped.Sales))

# merge shared profirs with agrregated summary
shared_summary <- merge(shared_summary, shared_profit)

# order by date (descending)
shared_summary <- shared_summary[rev(order(shared_summary$Max.Date)),]

# convert the months into rows (for presentation)
rownames(shared_summary) <- shared_summary$Month

# remove unwanted columns 
shared_summary[,c("Max.Date", "Month")] <- list(NULL)  

# arrange columns for presentation
shared_summary <- shared_summary[, c("Total.SKUs", "Shipped.Orders", "Shipped.Sales.Amt", "KR.Profit", "SKUs")]
    

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

cat('---------------------------------------------------------------Shared Stock Stats---------------------------------------------------------------')
cat('\n')

write.csv(shared_summary, quote = F, row.names = T)

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
   cat('KR Retail Earnings: R', paste(sum(orders_txns2017[[i]]$"KRRetail")))
   cat('\n')
   write.csv(ord_status[[i]], row.names = FALSE)  
   cat('\n')
   cat(' Order Summary by SKU ')
   cat('\n')
   write.csv(summary_sku[[i]], row.names = FALSE)
   cat('\n')
   cat('\n')
   cat('\n')   
   write.csv(orders_txns2017[[i]][c("OrdDate", "OrdID", "Status", "SKU", "FulFil", "SuccessFee", "SFReversal", "ManualCharge", "ManualReversal", "Qty", "SP", "OrdVal", "Vat", "OrderReversal", "CP", "Fees", "KRRetail", "KR Shared Profit", "Shared")], row.names = FALSE)
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

takealot_orders <- gs_upload(paste0("/Users/justinmichell/Downloads/takealot_admin/takealot_order_details_krretail", Day, Month, Year, ".csv" ), overwrite = T)  


# move file to correct folder in google drive
drive_mv(file = paste0("takealot_order_details_krretail", Day, Month, Year), path = paste0("Takealot - Clicker Way/")) 


