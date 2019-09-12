******************************************************************************
* Module 7: Multilevel Models for Binary Responses Stata Practicals
*
*     P7.5: Two-Level Random Slope Model
*
* 	    George Leckie
*           Centre for Multilevel Modelling, 2010
******************************************************************************


use "7.5.dta", clear



* P7.5.1 Allowing the effect of wealth to vary across communities

xtmelogit antemed magec magecsq meduc2 meduc3 wealthc ///
	|| comm: wealthc, covariance(unstructured) ///
	mle variance



* P7.5.2 Interpretation of a random slope model

estat recov, corr

predict u1 u0, reffects

egen pickone = tag(comm)

scatter u1 u0 if pickone==1, yline(0) xline(0) saving(P752a, replace)

preserve

replace magec = 0

replace magecsq = 0

replace meduc2 = 0

replace meduc3 = 0

predict predprob, fixedonly

generate predlogit = logit(predprob)

replace predlogit = predlogit + u0 + u1*wealthc

drop pickone

egen pickone = tag(comm wealthc)

line predlogit wealth if pickone==1, connect(ascending) saving(P752b, replace)

generate multiplewealth = pickone

bysort comm (wealth): replace multiplewealth = 0 if wealth[_N]==wealth[1]

line predlogit wealth if multiplewealth==1, connect(ascending) saving(P752c, replace)

restore

twoway function 0.865 + -0.232*x + 0.025*x^2, range(1 5) ///
	 saving(P752d, replace)



* P7.5.3 Fitting random coefficients to categorical wealth

xtmelogit antemed magec magecsq meduc2 meduc3 ///
	wealth2 wealth3 wealth4 wealth5 ///
	|| comm:, variance intpoints(4)

estimates store model1

preserve

replace magec = 0

replace magecsq = 0

summarize meduc2

replace meduc2 = 0.307

summarize meduc3

replace meduc3 = 0.345

predict predprob, fixedonly 

graph bar (mean) predprob, over(wealth) saving(P753a, replace)

restore

xtmelogit antemed magec magecsq meduc2 meduc3 ///
	wealth2 wealth3 wealth4 wealth5 ///
	|| comm: wealth5, covariance(unstructured) ///
	mle variance intpoints(4)

estimates store model2

lrtest model1 model2

xtmelogit antemed magec magecsq meduc2 meduc3 ///
	wealth2 wealth3 wealth4 wealth5 ///
	|| comm: wealth4 wealth5, covariance(unstructured) ///
	mle variance intpoints(4)

estimates store model3

lrtest model2 model3
