# Verify that you are logged in as the correct user (yourself)
system("whoami")
# See if you have a kerberos ticket
system("klist")
# If you do not have a valid kerberos ticket, use kinit command to create one
# Created a key tab file to get kerberos authentication

system("kinit TE0S0446@TYCOELECTRONICS.COM -k -t ~/mykeytab")
# Call R ODBC library
library(RODBC)
# Impala
# Create Connection to Hive Database
# dbcon_impala <- odbcConnect("ImpalaDSN64")
dbcon_impala_PRD <- odbcConnect("ImpalaProd")
 print('Pull Data...')
 
# You can now run queries on Hive just like you would a SQL database. Here are some examples:
# sqlQuery(dbcon_impala_PRD,"SHOW TABLES")

system.time({
  QuoteCombDetails <- 
    sqlQuery(dbcon_impala_PRD, 
             "select 
								te_corporate_part_number, 
								part_description,
                product_line_code, 
                product_code, 
								pricing_agreement_id, 
								customer_sales_organization_id, 
								customer_sales_organization_name, 
								customer_region_name,  
								product_structure_name_level_1, 
								customer_quote_key_string, 
								fiscal_year, 
								fiscal_month, 
								transaction_description, 
								transaction_quantity,
								transaction_amount
							from customer_quotes.customer_quotes_direct_sales_combined_for_elimination_process_v
							where fiscal_year >= 2015")})
#where product_structure_name_level_1 = 'Industrial & Commercial Transportation' -- Remove so we process the entire dataset (all BUs)
#Application Tooling
#Industrial & Commercial Transportation
print('Successful Pull Data')
