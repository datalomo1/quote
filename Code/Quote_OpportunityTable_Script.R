# source("Quote_Setup.R")
print('Invoking Quote_FilterBadQuote_Script.R')
source("Quote_FilterBadQuote_Script.R")


# remove R flag quotes/sales
# create label for with/without pricing agreement
# create lable for LADD vs direct
QuoteDetailsLatest <- flagged_output_all %>%
	filter((is.na(RemoveExploreFlag) |  RemoveExploreFlag == "E")) %>%
	mutate(pricing_agreement_flag = ifelse(transaction_description == "Quote",
																				 "Quote",
																				 ifelse(is.na(pricing_agreement_id), 
																				 			 "No pricing agreement", "With pricing agreement")),
				 direct_LADD_flag = ifelse(customer_sales_organization_name == "LADD Dist. LLC", 
				 													"LADD Dist. LLC", "Direct"))
print('Creating Opportunity Table')

# create opportunity table, slice by year, pricing agreement, LADD, region
OpportunityTable <- 
	foreach(FiscalYear = c(2015, 2016, 2017), 
					.combine=rbind) %do% {
						foreach(PAflag = c("No pricing agreement", "With pricing agreement"), 
										.combine=rbind) %do% {
											foreach(LADDflag = c("LADD Dist. LLC", "Direct"), 
															.combine=rbind) %do% {
																foreach(region = levels(QuoteDetailsLatest$customer_region_name), 
																				.combine=rbind) %do% {
																					
																					subsetData <- filter(QuoteDetailsLatest,
																															 (fiscal_year == FiscalYear
																															  & (pricing_agreement_flag == "Quote" 
																															  	 | pricing_agreement_flag == PAflag)
																															  & direct_LADD_flag == LADDflag
																															  & customer_region_name == region))
																					message(nrow(subsetData), " rows for ", 
																									LADDflag, " ", PAflag, " in ", region, " ", FiscalYear)
																					if (nrow(subsetData) > 10) {
																						QuoteMedianByPart <- subsetData %>%
																							filter(is.finite(transaction_price)) %>%
																							group_by(te_corporate_part_number, transaction_description) %>%
																							arrange(transaction_price) %>%
																							mutate(cum_qty = cumsum(transaction_quantity),
																										 median_qty = last(cum_qty)/2,
																										 median_qty_cutoff = ifelse(median_qty >= cum_qty, 0, 1)) %>%
																							filter(median_qty_cutoff == 1) %>%
																							dplyr::summarize(median_transcation_price = first(transaction_price))
																						
																						# reshape to each part a row
																						priceMedianByPart <- dcast(QuoteMedianByPart, 
																																			 te_corporate_part_number ~ transaction_description, mean)
																						# sometimes only got quote/sales data, then don't do the rest...
																						if(length(names(priceMedianByPart)) == 3){
																							names(priceMedianByPart)[2:length(names(priceMedianByPart))] <- 
																								paste0("median_price_for_", names(priceMedianByPart)[2:length(names(priceMedianByPart))])
																							
																							QuoteDetailsLatestQty <- subsetData %>%
																								left_join(priceMedianByPart) %>%
																								dplyr::mutate(qty_above_median = 
																																ifelse(transaction_price > median_price_for_Sales, 
																																			 transaction_quantity, 0)) %>%
																								group_by(te_corporate_part_number, 
																												 part_description,
																												 product_code,
																												 product_line_code,
																												 transaction_description) %>%
																								arrange(transaction_price) %>%
																								dplyr::summarize(median_sales_price = mean(median_price_for_Sales, na.rm = TRUE),
																																 median_quote_price = mean(median_price_for_Quote, na.rm = TRUE),
																																 total_qty_above_median = sum(qty_above_median, na.rm = TRUE))
																							
																							# reshape to each part a row
																							priceMedianQtyByPart <- dcast(QuoteDetailsLatestQty, 
																																						te_corporate_part_number 
																																						+ part_description
																																						+ product_code
																																						+ product_line_code
																																						+ median_sales_price 
																																						+ median_quote_price
																																						~ transaction_description, mean)
																							names(priceMedianQtyByPart)[(length(names(priceMedianQtyByPart))-1):length(names(priceMedianQtyByPart))] <- 
																								paste0(names(priceMedianQtyByPart)[(length(names(priceMedianQtyByPart))-1):length(names(priceMedianQtyByPart))], 
																											 "_qty_above_median_sales_price")
																							
																							values <- priceMedianQtyByPart %>%
																								mutate(quote_sales_price_gap = (median_quote_price - median_sales_price),
																											 quote_sales_price_ratio = (median_quote_price / median_sales_price - 1),
																											 opportunity_dollar = ((Quote_qty_above_median_sales_price - Sales_qty_above_median_sales_price) 
																											 											* median_sales_price),
																											 fiscal_year = FiscalYear,
																											 pricing_agreement_flag = PAflag,
																											 direct_LADD_flag = LADDflag,
																											 customer_region_name = region)
																							message(LADDflag, " ", PAflag, " in ", region, " ", FiscalYear, " finished!")
																							return(values)
																							
																						}	else {
																							return(NULL)
																						}
																					} else {
																						return(NULL)
																					}
																				}}}}


# 1. For each part, sort the quotes by quote price
# 2. For each part, get ranking of price for each unit (use cumulative qty)
BU <- levels(flagged_output_all$product_structure_name_level_1)
OpportunityTable[is.na(OpportunityTable)] <- "" # remove "NA" when open csv
print('Writing Quote Median Unit Opportuinity File')

write.table(OpportunityTable, 
					file = file.path(dataPath, 
													 paste0("quote_median_by_unit_opportunity_table",".txt")),row.names = FALSE,sep = "\t")

print('Successful Writing File')
