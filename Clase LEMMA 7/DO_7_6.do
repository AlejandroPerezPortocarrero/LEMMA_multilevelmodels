******************************************************************************
* Module 7: Multilevel Models for Binary Responses Stata Practicals
*
*     P7.6: Adding Level 2 Explanatory Variables: Contextual Effects
*
* 	    George Leckie
*           Centre for Multilevel Modelling, 2010
******************************************************************************

use "7.6.dta", clear



* P7.6.1 Contextual effects

xtmelogit antemed magec magecsq meduc2 meduc3 ///
	wealth2 wealth3 wealth4 wealth5 ///
	urban ///
	|| comm: wealth4 wealth5, covariance(unstructured) ///
	mle variance intpoints(4)

tabulate wealth urban, row

bysort comm: egen wealth5mean = mean(wealth5)

xtmelogit antemed magec magecsq meduc2 meduc3 ///
	wealth2 wealth3 wealth4 wealth5 ///
	urban wealth5mean ///
	|| comm: wealth4 wealth5, covariance(unstructured) ///
	mle variance intpoints(4)
	


* P7.6.2 Cross-level interactions

generate wealth2Xwealth5mean = wealth2*wealth5mean

generate wealth3Xwealth5mean = wealth3*wealth5mean

generate wealth4Xwealth5mean = wealth4*wealth5mean

generate wealth5Xwealth5mean = wealth5*wealth5mean

xtmelogit antemed magec magecsq meduc2 meduc3 ///
	wealth2 wealth3 wealth4 wealth5 ///
	urban wealth5mean ///
	wealth2Xwealth5mean wealth3Xwealth5mean ///
	wealth4Xwealth5mean wealth5Xwealth5mean ///
	|| comm: wealth4 wealth5, covariance(unstructured) ///
	mle variance intpoints(4)

replace magec = 0

replace magecsq = 0

summarize meduc2

replace meduc2 = 0.307

summarize meduc3

replace meduc3 = 0.345

summarize urban

replace urban = 0.314

recode wealth5mean (0/0.1 = 0) (0.1/0.3 = 0.2) (0.3/1.0 = 0.4) 

recode wealth2Xwealth5mean wealth3Xwealth5mean ///
	wealth4Xwealth5mean wealth5Xwealth5mean ///
	(0/0.1 = 0) (0.1/0.3 = 0.2) (0.3/1.0 = 0.4) 

predict predprob, fixedonly

graph bar (mean) predprob, over(wealth5mean) over(wealth) saving(P762a, replace)

graph bar (mean) predprob, over(wealth5mean) over(wealth) asyvars ///
	ytitle(Predicted probability) ///
	b1title(Household wealth index (quntiles)) ///
	legend(rows(1) ///
	subtitle(Community proportion in the top wealth quintile)) ///
	saving(P762b, replace)
