# Codebook for USDC-DATASET-JOP.csv

Ryan Hübert and Ryan Copus  
Forthcoming in the _Journal of Politics_  
Version Date: January 2021

This codebook describes the variables contained in the main district court 
dataset we analyze in the article. To create this dataset, we merged data from
the FJC's Integrated Database (IDB) with original data we collected from docket
sheets available in each district court's electronic databases.

We will not describe the FJC IDB variables in detail here, and instead refer 
readers to the comprehensive codebooks available at 
[https://www.fjc.gov/research/idb](https://www.fjc.gov/research/idb). For the 
analysis in this article, we used data from three separate IDB datasets:

1. The **IDB Civil 1988-present** database contains all civil cases filed in 
   U.S. District Courts from 1988, and is available at 
   [https://www.fjc.gov/research/idb/interactive/IDB-civil-since-1988](https://www.fjc.gov/research/idb/interactive/IDB-civil-since-1988).  
2. The **IDB Appeals 1971-2007** database contains all appeals filed in 
   U.S. Courts of Appeals from 1971-2007, and is available at 
   [https://www.fjc.gov/research/idb/interactive/IDB-appeals-1971-2007](https://www.fjc.gov/research/idb/interactive/IDB-appeals-1971-2007).
3. The **IDB Appeals 2008-present** database contains all appeals filed in 
   U.S. Courts of Appeals from 2008, and is available at 
   [https://www.fjc.gov/research/idb/interactive/IDB-appeals-since-2008](https://www.fjc.gov/research/idb/interactive/IDB-appeals-since-2008).

Since our analysis spans from the 1990s to the early 2010s, we merged the two 
IDB Appeals databases to cover our analysis time period. We preserved PDF copies 
of the FJC's codebooks for each of these datasets in this repository.

For additional details about the data collection and cleaning process, please 
see the article and the online appendix.

## Variables Derived from FJC's IDB Civil database

- `OUTCOME` A categorical variable indicating the outcome of each case, coded by 
  combining `DISP` and `DIRECTION` variables in the IDB Civil database.   
- `APPEAL` A dummy variable indicating whether each case was appealed. Note that 
  this variable was coded by linking the IDB Civil database with the IDB Appeals 
  databases in order to see which cases were appealed.
- `YEAR` A categorical variable indicating the year that each case was filed in 
  a district court. Note that this variable Extracted from `FILEDATE` in the 
  IDB Civil database.
- `QUARTER` A categorical variable indicating the year and quarter that each 
  case was filed in a district court. Q1 spans January through March, Q2 spans 
  April through June, Q3 spans July through September, and Q4 spans October 
  through December. Note that this variable Extracted from `FILEDATE` in the 
  IDB Civil database.
- `SECTION` For a description, see the online appendix or the IDB Civil codebook.
- `ORIGIN` For a description, see the online appendix or the IDB Civil codebook.  
- `JURIS` For a description, see the online appendix or the IDB Civil codebook.  
- `PROSE` For a description, see the online appendix or the IDB Civil codebook.  
- `COUNTY` For a description, see the online appendix or the IDB Civil codebook.  
- `nature_of_suit` For a description, see the online appendix or the IDB Civil codebook. 
  Note that this variable is labeled `NOS` in the IDB Civil database.
- `jury_demand` For a description, see the online appendix or the IDB Civil codebook. 
  Note that this variable is labeled `JURY` in the IDB Civil database. 

## Variables Derived from Case Docket Sheets

### Case-related variables
- `file` A categorical variable with an (anonymized) unique ID for each case.
- `ifp_denial` A dummy variable indicating whether a case was a denial of _in 
  forma pauperis_ status. 
- `court` A categorical variable indicating the court in which the case was 
  filed. Values use the standard codes used in the courts' websites: cacd, cand, 
  caed, casd, ord, waed, and wawd.
- `division` A categorical variable indicating the division of the court in 
  which the case was filed. 

### Judge-related variables
- `jrepublican` A dummy variable indicating whether case was assigned to a 
  Republican appointee.
- `jpresident` A categorical variable indicating the assigned judge's appointing
  president. 
- `jid_anon` A categorgical variable with an (anonymized) unique ID for each 
  judge presiding over cases. 
- `jyears` An integer indicating the number of years the assigned judge had been 
  on the bench at the year of each case's filing in court. 
- `judge_replaced` A dummy variable indicating whether original judge was 
  replaced (e.g., through reassignment, see discussion in the online appendix). 
- `jsenior` A dummy variable indicating whether each case was assigned to a 
  senior judge. 

### Litigant-related variables
- `[pla/def/oth]_count` For a description, see the online appendix.
- `[pla/def/oth]_count_prose` For a description, see the online appendix.
- `[pla/def/oth]_count_anonymous` For a description, see the online appendix.
- `[pla/def/oth]_count_IND` For a description, see the online appendix.
- `[pla/def/oth]_count_BUS` For a description, see the online appendix.
- `[pla/def/oth]_count_GOV` For a description, see the online appendix.
- `[pla/def/oth]_count_LAW` For a description, see the online appendix.
- `[pla/def/oth]_count_OTH` For a description, see the online appendix.
- `[pla/def/oth]_count_repeat` For a description, see the online appendix.
- `[pla/def/oth]_acount` For a description, see the online appendix.
- `[pla/def/oth]_acount_repeat` For a description, see the online appendix.