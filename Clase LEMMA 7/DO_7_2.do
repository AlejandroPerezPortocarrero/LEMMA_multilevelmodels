******************************************************************************
* Module 7: Multilevel Models for Binary Responses Stata Practicals
*
*     P7.2: Latent Variable Representation of a Random Intercept Model
*
* 	    George Leckie
*           Centre for Multilevel Modelling, 2010
******************************************************************************

use "7.2.dta", clear



* P7.2.1 Comparison of a single-level and multilevel threshold model

summarize wealth

generate wealthc = wealth - 3.008

xtmelogit antemed magec magecsq meduc2 meduc3 wealthc ///
	|| comm:, variance

bysort comm: egen wealthmean = mean(wealth)

egen pickone = tag(comm)

histogram wealthmean if pickone==1, width(0.2) start(0) frequency ///
	saving(P721a, replace)

estimates store multilevel

logit antemed magec magecsq meduc2 meduc3 wealthc



* P7.2.2 Variance Partition Coefficient

estimates replay multilevel, variance
