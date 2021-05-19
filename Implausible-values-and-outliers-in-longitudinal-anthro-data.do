/* (1) Set-up: 
Note that the data should be in long format and the following numerical
variables are used for this analysis:
- PartID: each child's unique identifier
- Age: age of child at time of measurement
- Length: length measurement of child 
- LAZ: length-for-age z-score of child 

It may be easier to rename the corresponding variables to match the variable
names used in the subsequent code. */

/* (2) Generating New Variables */
sort PartID Age
generate SqrtAge=sqrt(Age) // square root of age
generate LAZ_Complete=1 if LAZ!=. // variable to indicate whether or not a measurement was completed at a given time point
generate Length_Complete=1 if Length!=.
generate LAZ_Residual=. // creating variable to store jackknife residual values from LAZ model
generate Length_Residual=. 
by PartID: egen LAZ_Count=count(LAZ) // generating a count variable that indicates how many LAZ measurements are available per infant
by PartID: egen Length_Count=count(Length) 

/* (3) Running Model for LAZ, Storing Residuals and Flagging Outliers*/
levelsof PartID if LAZ_Count>3, local(levels) // storing all PartID values of children who have more than 3 measurements in local macro called 'levels'; note that this line of code needs to be run at the same time as the following loop
foreach id of local levels{ // looping through all PartIDs
	regress LAZ Age if PartID==`id' // regressing LAZ on age for just one ID
	predict Residual`id' if PartID==`id', rstudent // predicting jackknife residual
	replace LAZ_Residual=Residual`id' if PartID==`id' // storing predicted jacknknife residual into LAZ_Residual variable
	drop Residual`id'
}

/* (4) Running Model for Length, Storing Residuals and Flagging Outliers */
levelsof PartID if Length_Count>3, local(levels) 
foreach id of local levels{ 
	regress Length SqrtAge if PartID==`id' // regressing length on squart root of age for just one ID
	predict Residual`id' if PartID==`id', rstudent 
	replace Length_Residual=Residual`id' if PartID==`id' 
	drop Residual`id'
}
	
/* (5) Identifying Outliers */
	* a. LAZ Model, Using Cutoff of +/- 4
	generate LAZ_Outlier=0 if abs(LAZ_Residual)<4 
		replace LAZ_Outlier=1 if abs(LAZ_Residual)>4 & LAZ_Residual!=.
		replace LAZ_Outlier=9 if LAZ_Residual==.
	
	* b. Length Model, using Cutoff of +/- 4 
	generate Length_Outlier=0 if abs(Length_Residual)<4 // identifying outliers based on +/- cut-off
		replace Length_Outlier=1 if abs(Length_Residual)>4 & Length_Residual!=.
		replace Length_Outlier=9 if LAZ_Residual==.

/* Note that observations for which the jackknife residual could not be 
evaluated (i.e. participant had too few observations), the LAZ_Outlier variable
will be indicated as '9', and manual review of these observations should
be conducted */

/* (6) Identifying Implausible Values
Note: This assumes that any decrease in length from one time point
to the next represents a pair of biologically implausible values, for which
an error must have occurred in at least one of the two measurements */

	/* a. Calculating Change in Length Between Successive Measurements */
	sort PartID Length_Complete Age
	by PartID: generate Length_Change=Length-Length[_n-1] // calculating change between successive measurements in the same infant
	
	/* b. Identifying Pairs of Biologically Implausible Values */
	by PartID: generate Length_BIVPair=_n if Length_Change[_n+1]<0
	by PartID: replace Length_BIVPair=Length_BIVPair[_n-1] if Length_Change<0

	/* c. Determining Incorrect Value Based on Jackknife Residuals from LAZ Model */
	sort PartID Length_BIVPair
	by PartID Length_BIVPair: egen LAZ_BIV_Max=max(abs(LAZ_Residual)) if Length_BIVPair!=.
	generate LAZ_BIV=1 if abs(LAZ_BIV_Max)==abs(LAZ_Residual) & LAZ_BIV_Max!=.
	
	/* d. Determining Incorrect Value Based on Jackknife Residuals from Length Model */
	sort PartID Length_BIVPair
	by PartID Length_BIVPair: egen Length_BIV_Max=max(abs(Length_Residual)) if Length_BIVPair!=.
	generate Length_BIV=1 if abs(Length_BIV_Max)==abs(Length_Residual) & Length_BIV_Max!=.	
