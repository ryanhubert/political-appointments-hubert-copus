# Codebook for USDC-APPEALS-JOP.csv

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

## Variables Derived from FJC's IDB databases

### Variables from the IDB Appeals database

- `YEAR` A categorical variable indicating the year that each appeal was filed 
  in the Ninth Circuit. Note that this variable Extracted from `DKTDATE` in the 
  IDB Appeals database. 
- `APPTYPE` For a description, see the IDB Appeals codebook.
- `NOS` For a description, see the IDB Appeals codebook.
- `JURIS` For a description, see the IDB Appeals codebook.
- `USAPT` For a description, see the IDB Appeals codebook.
- `USAPE` For a description, see the IDB Appeals codebook.
- `DDIST` For a description, see the IDB Appeals codebook.
- `DOFFICE` For a description, see the IDB Appeals codebook.

### Variables directly from the IDB Civil database
_Note: we appended a "d" before these variables to indicate that they
correspond to originating district court cases and were drawn from the FJC's
FJC District Court Civil database._

- `dORIGIN` For a description, see the IDB Civil codebook.
- `dSECTION` For a description, see the IDB Civil codebook.
- `dJURY` For a description, see the IDB Civil codebook.
- `dCOUNTY` For a description, see the IDB Civil codebook.
- `dPROSE` For a description, see the IDB Civil codebook.
- `dTRCLACT` For a description, see the IDB Civil codebook.
- `dPROCPROG` For a description, see the IDB Civil codebook.
- `dNOJ` For a description, see the IDB Civil codebook.
- `dJUDGMENT` For a description, see the IDB Civil codebook.
- `dDISP` For a description, see the IDB Civil codebook.
- `dCLASSACT` For a description, see the IDB Civil codebook.

## Variables Derived from Case Docket Sheets

### Appeal case variables
- `case_id` A categorical variable with an (anonymized) unique ID for each 
  appeal.
- `pro_defendant` A dummy variable indicating whether the outcome of the appeal 
  favored the defendant in the originating district court case. 
- `CIRDIST` A categorical variable indicating which "unit" of of the Ninth 
  Circuit received the appeal. Values are north, middle or south. More details 
  on the units of the Ninth Circuit can be viewed here: 
  [http://cdn.ca9.uscourts.gov/datastore/uploads/rules/rules.htm](http://cdn.ca9.uscourts.gov/datastore/uploads/rules/rules.htm).

### District case variables
- `ftnoa` ???
- `dtrmfee` ???
- `dpltres` ???
- `ddefres` ???

### Appeal panel variables
- `panel_anon` A categorgical variable with an (anonymized) unique ID for each 
  distinct three-judge panel presiding over cases. 
- `maj_rep` A dummy variable indicating whether the three-judge panel assigned 
  to each appeal was comprised of a majority of Republican appointees.

### District judge variables
_Note: these variables correspond to the district judge assigned to the 
originating district court case._  
- `djudge_race` A categorical variable indicating the race of the district judge 
  in the originating district court case.
- `djudge_gender` A categorical variable indicating the gender of the district 
  judge in the originating district court case.
- `djudge_party` A categorical variable indicating the party of the appointing 
  president of the district judge in the originating district court case.
- `djudge_birth_year` A categorical variable indicating the birth year of the 
  district judge in the originating district court case.
- `djudge_senior` A dummy variable indicating whether the district judge in the 
  originating district court case was a senior judge.
- `djudge_president` A categorical variable indicating the appointing president 
  of the district judge in the originating district court case.

### Appealing litigant-related variables
- `aple_count` An integer variable indicating the number of appellees listed on 
  the appeal.
- `aplt_count` An integer variable indicating the number of appellants listed on 
  the appeal.
- `aple_att_count` An integer variable indicating the number of attorneys 
  representing the appellees listed on the appeal.
- `aplt_att_count` An integer variable indicating the number of attorneys 
  representing the appellants listed on the appeal.
- `aplt_deft` A dummy variable indicating whether appellant(s) was the defendant 
  in the originating district court case.
- `aplt_pltf` A dummy variable indicating whether appellant(s) was the plaintiff 
  in the originating district court case.
- `apltprose` A dummy variable indicating whether appellant(s) was a pro se 
  litigant.
- `apleprose` A dummy variable indicating whether appellee(s) was a pro se 
  litigant.
- `bothprose` A dummy variable indicating whether both were pro se litigants.