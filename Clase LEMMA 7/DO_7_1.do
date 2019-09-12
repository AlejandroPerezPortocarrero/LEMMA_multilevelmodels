******************************************************************************
* Module 7: Multilevel Models for Binary Responses Stata Practicals
*
*     P7.1: Two-Level Random Intercept Model
*
* 	    George Leckie
*           Centre for Multilevel Modelling, 2010
******************************************************************************

use "7.1.dta", clear

describe



* P7.1.1 Specifying and estimating a two-level model

xtmelogit antemed || comm:, variance



* P7.1.2 Interpretation of the null two-level model

predict u0, reffects

predict u0se, reses

egen pickone = tag(comm)

sort u0

generate u0rank = sum(pickone)

serrbar u0 u0se u0rank if pickone==1, scale(1.96) yline(0) saving(P712a, replace)



* P7.1.3 Adding an explanatory variable

xtmelogit antemed magec || comm:, variance

predict predprob

generate predlogit = logit(predprob)

drop pickone

egen pickone = tag(comm mage)

twoway connected predlogit mage if pickone==1, connect(ascending) ///
	ytitle(Predicted log-odds) xtitle(Maternal age (years)) ///
	saving(P713a, replace)

xtmelogit antemed magec magecsq || comm:, variance

drop predprob predlogit

predict predprob

generate predlogit = logit(predprob)

twoway connected predlogit mage if pickone==1, connect(ascending) ///
	ytitle(Predicted log-odds) xtitle(Maternal age (years)) ///
	saving(P713b, replace)
