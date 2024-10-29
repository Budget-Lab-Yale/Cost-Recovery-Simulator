hi! it's maddie!

i wrote these scripts with the expectation that no one other than
myself would use them... so they are... uh... messy...



== FOLDERS AND OTHER STUFF ==========================================

~/data         - raw soi tax stat files (see more below)
~/crosswalks   - matches industry names in soi tax stat files to
                 industry names in our depreciation model
~/inputs       - cleaned soi tax stat files (see more below)
historical.csv - historical investment data


data comes from
 - allcorp
    after 2014
      SOI Tax Stats - Corporation Income Tax Returns Complete Report
      (Publication 16)
        Table 1 (Part 1 of 2): Returns of Active Corporations,
        Selected Income Statement, Balance Sheet, and Tax Items 	
          https://www.irs.gov/statistics/soi-tax-stats-corporation-income-tax-returns-complete-report-publication-16
    before 2014
      SOI Tax Stats - Returns of active corporations - Table 1
          https://www.irs.gov/statistics/soi-tax-stats-returns-of-active-corporations-table-1
- ccorp
    after 2014
      SOI Tax Stats - Corporation Income Tax Returns Complete Report
      (Publication 16)
        Table 5.3: Returns of Active Corporations, other than Forms
        1120S, 1120-REIT, and 1120-RIC, Balance Sheet, Income
        Statement, Tax, and Selected Other Items	
          https://www.irs.gov/statistics/soi-tax-stats-corporation-income-tax-returns-complete-report-publication-16
    before 2014
      SOI Tax Stats - Table 12 - Returns of active corporations,
      other than Forms 1120-REIT, 1120-RIC and 1120S
          https://www.irs.gov/statistics/soi-tax-stats-table-12-returns-of-active-corporations-other-than-forms-1120-reit-1120-ric-and-1120s
 - sp
     all years
       SOI Tax Stats - Nonfarm sole proprietorship statistics
         Table 1.  Nonfarm Sole Proprietorships: Business Receipts,
         Selected Deductions, Payroll, and Net Income, by Industrial
         Sectors
           https://www.irs.gov/statistics/soi-tax-stats-nonfarm-sole-proprietorship-statistics
 - pa
     all years
       SOI Tax Stats - Partnership statistics by sector or industry
         Table 1:  All Partnerships:  Total Assets, Trade or Business
         Income and Deductions, Portfolio Income, Rental Income, and
         Total Net Income (Loss), by Industrial Group
           https://www.irs.gov/statistics/soi-tax-stats-partnership-statistics-by-sector-or-industry



== SCRIPTS ==========================================================

cleanv2.R
THIS SCRIPT IS SO SO MESSY PLEASE DO NOT LOOK AT IT :((
 - cleans raw soi tax stat data and matches it to the industries in 
   our depreciation model. this script isn't very well commented so
   here's a brief description of the process to clean each type of
   data file:
     - allcorp data : uses "minor industries" which are the most fine
                      grain
        => values for missing industries are imputed based on
           those industries historical investment shares
     - ccorp data   : uses "major industries" ... some observations
                      are "delted" in data after 2014
        => missing industries are imputed based on those industries
           shares of allcorp business receipts
        => delted values are imputed based on that industry's share
           of total receipts if available and are otherwise treated
           as missing industries
     - sp data      : uses a combination of mamjor + minor industries
        => missing industries are imputed based on allcorp shares
     - pa data      : uses industry groups (least fine grain) after
                      2014 and uses a combination of major + minor
                      industries before then
        => missing industries are imputed first based on 2014 shares
           if applicable and otherwise use the allcorp shares
 - inputs  : all the files in /data and /crosswalks + historical.csv
 - outputs : all the /inputs/legalformYEAR.csv files


builddataset.R
this script is a bit cleaner... i guess it's fine if you look at it
 - merges together all the cleaned soi tax stats data and then
   computes the ccorp share of business receipts by industry by year
 - inputs  : all the files in /inputs
 - outputs : ccorp_business_receipts.csv