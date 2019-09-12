******************************************************************************
* Module 7: Multilevel Models for Binary Responses Stata Practicals
*
*     P7.4: Predicted Probabilities from a Multilevel Model
*
* 	    George Leckie
*           Centre for Multilevel Modelling, 2010
******************************************************************************

use "7.4.dta", clear

xtmelogit antemed magec magecsq meduc2 meduc3 wealthc ///
	|| comm:, variance

replace magec = 0

replace magecsq = 0
	
predict medianpredprob, fixedonly 

egen pickone = tag(wealth meduc)

sort wealth meduc

list wealth meduc medianpredprob if pickone==1, abbreviate(14) sepby(wealth)

generate medianpredlogit = logit(medianpredprob)

keep if pickone==1

keep wealth meduc medianpredprob medianpredlogit

list, abbreviate(15) sepby(wealth)

expand 1000

generate u = rnormal(0,sqrt(0.889))

generate meanpredprob = invlogit(medianpredlogit + u)

collapse (mean) meanpredprob, by(wealth meduc medianpredprob)

list wealth meduc medianpredprob meanpredprob, abbreviate(14) sepby(wealth)
