print('Invoking Quote_PullData_script.R')

source('Quote_PullData_script.R')
print('Invoking SetUpScript.R')

source('Quote_SetUp.R')

print('Filter Bad Quote...')


#####
QuoteCombDetails$transaction_price <- 
	QuoteCombDetails$transaction_amount / QuoteCombDetails$transaction_quantity
QuoteCombDetails$ExplanationOfFlags <- NA


#####
# remainder rows index
i <- seq(nrow(QuoteCombDetails))

#####
#
# type 0 errors:
#
# 0. Part name contains dummy / not a valid part'
bad_rec_idx <- grepl(paste(BAD_PART_NAMES, collapse="|"), 
										 QuoteCombDetails$part_description)
QuoteCombDetails[i[bad_rec_idx], "ExplanationOfFlags"] <- "0. Part name contains dummy / not a valid part"
i <- i[!bad_rec_idx]
table(QuoteCombDetails$ExplanationOfFlags)

# 0. Quotes' amounts over 50M
bad_rec_idx <- (QuoteCombDetails[i, ]$transaction_description == "Quote"  
								& QuoteCombDetails[i, ]$transaction_amount > CUTOFF1)
QuoteCombDetails[i[bad_rec_idx], "ExplanationOfFlags"] <- paste0("0. Quotes' amounts over ", CUTOFF1)
i <- i[!bad_rec_idx]
table(QuoteCombDetails$ExplanationOfFlags)

# 0. Quotes or sales with <=0 amounts, <=0 quantities
bad_rec_idx <- (QuoteCombDetails[i, ]$transaction_amount <= 0
								| QuoteCombDetails[i, ]$transaction_quantity <= 0)
QuoteCombDetails[i[bad_rec_idx], "ExplanationOfFlags"] <- "0. Quotes or sales with <=0 amounts, <=0 quantities"
i <- i[!bad_rec_idx]
table(QuoteCombDetails$ExplanationOfFlags)

# 0. Quotes with per unit amounts over 100K
bad_rec_idx <- (QuoteCombDetails[i, ]$transaction_description == "Quote"  
								& QuoteCombDetails[i, ]$transaction_price > CUTOFF2)
QuoteCombDetails[i[bad_rec_idx], "ExplanationOfFlags"] <- paste0("0. Quotes with per unit amounts over ", CUTOFF2)
i <- i[!bad_rec_idx]
table(QuoteCombDetails$ExplanationOfFlags)

# 0. Quotes with per unit amounts over 1K AND quantities over 1K
bad_rec_idx <- (QuoteCombDetails[i, ]$transaction_description == "Quote"  
								& QuoteCombDetails[i, ]$transaction_price > CUTOFF3
								& QuoteCombDetails[i, ]$transaction_quantity > CUTOFF4)
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombDetails[i[bad_rec_idx], "ExplanationOfFlags"] <- paste0("0. Quotes with per unit amounts over ",
																																	 CUTOFF3," AND quantities over ", CUTOFF4)
}
i <- i[!bad_rec_idx]
table(QuoteCombDetails$ExplanationOfFlags)


#####
#
# Summary per part
#
QuoteCombPartSum <- QuoteCombDetails[i, ] %>%
	group_by(transaction_description, te_corporate_part_number) %>%
	summarise(unit_price_mean=mean(transaction_price, na.rm = TRUE),           # mean of unit price
						unit_price_sd=sd(transaction_price, na.rm = TRUE),               # sd of unit price
						unit_price_98_per=wtd.quantile(transaction_price,transaction_quantity,.98, na.rm = TRUE), # 98% sample percentile of unit price (value estimated from data)
						unit_price_wmedian=wtd.quantile(transaction_price,transaction_quantity,probs=0.5, na.rm = TRUE), # weighted median of unit price
						mean_quantity=mean(transaction_quantity, na.rm = TRUE),               # mean of quantity
						median_quantity=median(transaction_quantity, na.rm = TRUE),           # median of quantity
						sd_quantity=sd(transaction_quantity, na.rm = TRUE),                   # sd of quantity
						quantity_98_per=quantile(transaction_quantity,.98, na.rm = TRUE)    # 98% sample percentile of quantity (value estimated from data)                          
	)

QuoteCombPartSumQty <- 
	QuoteCombDetails %>%
	left_join(filter(QuoteCombPartSum, transaction_description == "Quote"), by = "te_corporate_part_number") %>%
	left_join(filter(QuoteCombPartSum, transaction_description == "Sales"), by = "te_corporate_part_number") 

# don't consider sales
j <- seq(nrow(QuoteCombPartSumQty))
kept_idx <- (QuoteCombPartSumQty$transaction_description.x == "Quote"
						 & is.na(QuoteCombPartSumQty$ExplanationOfFlags))
j <- j[kept_idx]

######
#
# continue filter based on summary
#
# 1. Quotes with per unit amount over 50x the median sales per unit amount
bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_price > CUTOFF5 * QuoteCombPartSumQty[j, ]$unit_price_wmedian.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("1. Quotes with per unit amount over ", CUTOFF5, "x the median sales per unit amount")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)


# 2. Quotes with per unit amount over 20x the median sales per unit amount where the latter is over 10 cents

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_price > CUTOFF6 * QuoteCombPartSumQty[j, ]$unit_price_wmedian.y
								& QuoteCombPartSumQty[j, ]$unit_price_wmedian.y > CUTOFF7)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("2. Quotes with per unit amount over ", CUTOFF6,"x the median sales per unit amount where the latter is over 10 cents")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)


# 3. Quotes with per unit amount over 10x the top sales percentiles (98%) where the latter is over 10 cents
bad_rec_idx <- ((QuoteCombPartSumQty[j, ]$transaction_price > CUTOFF8 * QuoteCombPartSumQty[j, ]$unit_price_98_per.y)
								& (QuoteCombPartSumQty[j, ]$unit_price_wmedian.y > CUTOFF7))
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("3. Quotes with per unit amount over ", CUTOFF8, 
					 "x the top sales percentiles (98%) except where the latter is over 10 cents")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)


# 4. Quotes with over 100x quantities of the median quantity sales

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_quantity > CUTOFF9 * QuoteCombPartSumQty[j, ]$median_quantity.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("4. Quotes with over ", CUTOFF9, "x quantities of the median quantity sales")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)

# 5. Quotes with over 20x quantities of the top sales percentiles (98%)

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_quantity > CUTOFF10 * QuoteCombPartSumQty[j, ]$quantity_98_per.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("5. Quotes with over ", CUTOFF10, "x quantities of the top sales percentiles (98%)")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)

# 6. Quotes with per unit amounts 3x larger than the top 98 percentile sales amount

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_price > CUTOFF11 * QuoteCombPartSumQty[j, ]$unit_price_98_per.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("6. Quotes with per unit amounts ", CUTOFF11, "x larger than the top 98 percentile sales amount")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)


# 7. Quotes with quantities over 10x the top percentiles (98%) quoted quantities

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_quantity > CUTOFF12 * QuoteCombPartSumQty[j, ]$quantity_98_per.x)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("7. Quotes with quantities over ", CUTOFF12, "x the top percentiles (98%) quoted quantities")
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)

######
# add a flag for all removal records
#
QuoteCombPartSumQty[!is.na(QuoteCombPartSumQty$ExplanationOfFlags), "RemoveExploreFlag"] <- "R"


# A. Quotes with per unit amount over 10x the median sales per unit amount

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_price > CUTOFF13 * QuoteCombPartSumQty[j, ]$unit_price_wmedian.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("A. Quotes with per unit amount over ", CUTOFF13, "x the median sales per unit amount")
	QuoteCombPartSumQty[j[bad_rec_idx], "RemoveExploreFlag"] <-
		"E"
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)

# B. Quotes with per unit amount over 2.5x the top sales percentiles (98%)

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_price > CUTOFF14 * QuoteCombPartSumQty[j, ]$unit_price_98_per.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("B. Quotes with per unit amount over ", CUTOFF14, "x the top sales percentiles (98%)")
	QuoteCombPartSumQty[j[bad_rec_idx], "RemoveExploreFlag"] <-
		"E"
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)

# C. Quotes with over 10x quantities of the top sales percentiles (98%)

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_quantity > CUTOFF15 * QuoteCombPartSumQty[j, ]$quantity_98_per.y)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("C. Quotes with over ", CUTOFF15, "x quantities of the top sales percentiles (98%)")
	QuoteCombPartSumQty[j[bad_rec_idx], "RemoveExploreFlag"] <-
		"E"
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)


# D. Quotes with quantities over 5x the top percentiles (98%) quoted quantities

bad_rec_idx <- (QuoteCombPartSumQty[j, ]$transaction_quantity > CUTOFF16 * QuoteCombPartSumQty[j, ]$quantity_98_per.x)
bad_rec_idx[is.na(bad_rec_idx)] <- FALSE # if na, don't remove
if(is.finite(sum(bad_rec_idx))) {
	QuoteCombPartSumQty[j[bad_rec_idx], "ExplanationOfFlags"] <-
		paste0("D. Quotes with quantities over ", CUTOFF16, "x the top percentiles (98%) quoted quantities")
	QuoteCombPartSumQty[j[bad_rec_idx], "RemoveExploreFlag"] <-
		"E"
}
j <- j[!bad_rec_idx]
table(QuoteCombPartSumQty$ExplanationOfFlags)

# E. Quotes with quantities over 25% of sum of quoted quantities above median


summary <- QuoteCombPartSumQty %>%
	group_by(ExplanationOfFlags, RemoveExploreFlag, transaction_description.x) %>%
	dplyr::summarize(num_of_rows = length(transaction_description))




COL_TO_KEEP <- c(names(QuoteCombDetails), c("transaction_description.x",  "RemoveExploreFlag"))
flagged_output_all <- QuoteCombPartSumQty[, COL_TO_KEEP]
flagged_output_all$transaction_description <- NULL
names(flagged_output_all)[which(names(flagged_output_all) == "transaction_description.x")] <- "transaction_description"

# only keep R or E quotes (remove sales or regular quotes)
flagged_output_keys <- QuoteCombPartSumQty[!is.na(QuoteCombPartSumQty$customer_quote_key_string) & 
																							!is.na(QuoteCombPartSumQty$RemoveExploreFlag),
																					 c("customer_quote_key_string", "RemoveExploreFlag", "ExplanationOfFlags")]
print('Writing File Flag Quotes Key')

write.table(flagged_output_keys, 
					file = file.path(dataPath, 
													 paste0("flagged_quotes_key", 
													 			 ".txt")), 
					row.names = F, sep = "\t")
print('File Successfully Written')
