* Lutfi Sun | Nov 7, 2020 | BFI Labor Data Task *

** HOUSE KEEPING **

clear
cd "/Users/lutfisun/Desktop/BFI Labor Data Task"

ssc install estout
eststo clear

* numericcols option will replace NA entries with missing values that stata can understand
import delimited "nlsy79-prepared.csv", numericcols(5 6 7 8 9) 

* focus on those we have information about the move
drop if missing(region) & missing(urban) 
* Define employment as wage>0
gen employed = (wage > 0)  
gen emp_100 = employed * 100

********************************
** TASK 1: SUMMARY STATISTICS **
********************************

* changes in region or urban for the same individual would imply moving
* in this data, we cannot observe moves within the same region and to same type of area (eg rural to rural)
* using lag and logical test to see when and whether an individual moved
bysort i (year): gen moved = (region-region[_n-1] != 0) | (urban-urban[_n-1] != 0 & !missing(urban)) if _n!=1

* see if one moves to a more or less urban area
gen urb_move = moved * (urban-urban[_n-1])

* what urban area the individual left
gen urb0_x = moved * (urban[_n-1] == 0)
gen urb1_x = moved * (urban[_n-1] == 1)
gen urb2_x = moved * (urban[_n-1] == 2)

* if we want to look deeper into urban, we can generate a variable for each of the nine ways to move between rural and urban regions
gen urb0_0 = moved * (urban-urban[_n-1] ==  0) * (urban == 0)
gen urb0_1 = moved * (urban-urban[_n-1] ==  1) * (urban == 1)
gen urb0_2 = moved * (urban-urban[_n-1] ==  2) * (urban == 2)

gen urb1_0 = moved * (urban-urban[_n-1] == -1) * (urban == 0)
gen urb1_1 = moved * (urban-urban[_n-1] ==  0) * (urban == 1)
gen urb1_2 = moved * (urban-urban[_n-1] ==  1) * (urban == 2)

gen urb2_0 = moved * (urban-urban[_n-1] == -2) * (urban == 0)
gen urb2_1 = moved * (urban-urban[_n-1] == -1) * (urban == 1)
gen urb2_2 = moved * (urban-urban[_n-1] ==  0) * (urban == 2)

gen rur_rur = urb0_0
gen rur_urb = urb0_1 + urb0_2
gen urb_rur = urb1_0 + urb2_0
gen urb_urb = urb1_1 + urb1_2 + urb2_1 + urb2_2

* what region the individual left
gen frm_one  = moved * (region[_n-1] == 1)
gen frm_two  = moved * (region[_n-1] == 2)
gen frm_thre = moved * (region[_n-1] == 3)
gen frm_four = moved * (region[_n-1] == 4)

* Migration between regions: TABLE 1 *
estpost tabstat frm_one frm_two frm_thre frm_four moved, by(region) stat(sum)

graph bar (sum) frm_one frm_two frm_thre frm_four, over(region)

* Migration between area types: TABLE 2 *
estpost tabstat urb0_x urb1_x urb2_x moved, by(urban) stat(sum)

* Socioeconomic variables between regions: TABLE 3 *

* we can do this via regressions or tabstat
reg wage i.region, robust
estimates store regi_wage, title(regi_wage)
reg emp_100 i.region, robust
estimates store regi_emp, title(regi_emp)
reg educ i.region, robust
estimates store regi_educ, title(regi_educ)
esttab regi_wage regi_emp regi_educ using regi_soci.tex, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons constant)

estpost tabstat wage educ emp_100, by(region) statistics(mean sd) columns(statistics) listwise 
esttab, cell((mean(fmt(%9.1f)) sd(fmt(%9.2f)))) nonumber nomtitle
esttab using socioecon_regi.csv, modelwidth(10 20) cell((mean(fmt(%9.2f)) sd(par label(Standard Deviation)))) label nomtitle nonumber replace

estpost tabstat wage educ emp_100, by(urban) statistics(mean sd) columns(statistics) listwise 
esttab, cell((mean(fmt(%9.1f)) sd(fmt(%9.2f)))) nonumber nomtitle
esttab using socioecon_urbi.csv, modelwidth(10 20) cell((mean(fmt(%9.2f)) sd(par label(Standard Deviation)))) label nomtitle nonumber replace
eststo clear

*************************
** TASK 2: EVENT STUDY **
*************************

gen move_time = .

* if the move is x years later we are currently x years before the move
* some moves may overlap if someone moves more than once within the same five years

replace move_time = -2 if (moved[_n+2]==1 & _n!=1)
replace move_time = -1 if (moved[_n+1]==1 & _n!=1)
replace move_time = -0 if (moved[_n+0]==1 & _n!=1)
replace move_time =  1 if (moved[_n-1]==1 & _n!=1)
replace move_time =  2 if (moved[_n-2]==1 & _n!=1)

* take average of wage grouping by how many years before and after the move
egen move_w = mean(wage), by(move_time)
scatter move_w move_time

* now i do the same thing for four arbitrary moving scenarios between regions
* eg if there is a move from region 1 to region 2, then reg1_2 = 1
gen reg1_2  = frm_one  * (region == 2)
gen reg1_3  = frm_one  * (region == 3)
gen reg3_1  = frm_thre * (region == 1)
gen reg3_4  = frm_thre * (region == 4)

gen r12_time = .
gen r13_time = .
gen r31_time = .
gen r34_time = .

replace r12_time = -2 if (reg1_2[_n+2]==1 & _n!=1)
replace r12_time = -1 if (reg1_2[_n+1]==1 & _n!=1)
replace r12_time = -0 if (reg1_2[_n+0]==1 & _n!=1)
replace r12_time =  1 if (reg1_2[_n-1]==1 & _n!=1)
replace r12_time =  2 if (reg1_2[_n-2]==1 & _n!=1)

replace r13_time = -2 if (reg1_3[_n+2]==1 & _n!=1)
replace r13_time = -1 if (reg1_3[_n+1]==1 & _n!=1)
replace r13_time = -0 if (reg1_3[_n+0]==1 & _n!=1)
replace r13_time =  1 if (reg1_3[_n-1]==1 & _n!=1)
replace r13_time =  2 if (reg1_3[_n-2]==1 & _n!=1)

replace r31_time = -2 if (reg3_1[_n+2]==1 & _n!=1)
replace r31_time = -1 if (reg3_1[_n+1]==1 & _n!=1)
replace r31_time = -0 if (reg3_1[_n+0]==1 & _n!=1)
replace r31_time =  1 if (reg3_1[_n-1]==1 & _n!=1)
replace r31_time =  2 if (reg3_1[_n-2]==1 & _n!=1)

replace r34_time = -2 if (reg3_4[_n+2]==1 & _n!=1)
replace r34_time = -1 if (reg3_4[_n+1]==1 & _n!=1)
replace r34_time = -0 if (reg3_4[_n+0]==1 & _n!=1)
replace r34_time =  1 if (reg3_4[_n-1]==1 & _n!=1)
replace r34_time =  2 if (reg3_4[_n-2]==1 & _n!=1)

egen r12_w = mean(wage), by(r12_time)
egen r13_w = mean(wage), by(r13_time)
egen r31_w = mean(wage), by(r31_time)
egen r34_w = mean(wage), by(r34_time)

scatter r12_w r12_time
scatter r13_w r13_time
scatter r31_w r31_time
scatter r34_w r34_time

gr combine wage_move_12.gph wage_move_13.gph wage_move_31.gph wage_move_34.gph

* now i look at moves between urban and rural areas
* eg the dummy to indicate a move from rural to urban is named rur_urb
sum rur_rur rur_urb urb_rur urb_urb

gen rur_rur_t = .
gen rur_urb_t = .
gen urb_rur_t = .
gen urb_urb_t = .

replace rur_rur_t = -2 if (rur_rur[_n+2]==1 & _n!=1)
replace rur_rur_t = -1 if (rur_rur[_n+1]==1 & _n!=1)
replace rur_rur_t = -0 if (rur_rur[_n+0]==1 & _n!=1)
replace rur_rur_t =  1 if (rur_rur[_n-1]==1 & _n!=1)
replace rur_rur_t =  2 if (rur_rur[_n-2]==1 & _n!=1)

replace rur_urb_t = -2 if (rur_urb[_n+2]==1 & _n!=1)
replace rur_urb_t = -1 if (rur_urb[_n+1]==1 & _n!=1)
replace rur_urb_t = -0 if (rur_urb[_n+0]==1 & _n!=1)
replace rur_urb_t =  1 if (rur_urb[_n-1]==1 & _n!=1)
replace rur_urb_t =  2 if (rur_urb[_n-2]==1 & _n!=1)

replace urb_rur_t = -2 if (urb_rur[_n+2]==1 & _n!=1)
replace urb_rur_t = -1 if (urb_rur[_n+1]==1 & _n!=1)
replace urb_rur_t = -0 if (urb_rur[_n+0]==1 & _n!=1)
replace urb_rur_t =  1 if (urb_rur[_n-1]==1 & _n!=1)
replace urb_rur_t =  2 if (urb_rur[_n-2]==1 & _n!=1)

replace urb_urb_t = -2 if (urb_urb[_n+2]==1 & _n!=1)
replace urb_urb_t = -1 if (urb_urb[_n+1]==1 & _n!=1)
replace urb_urb_t = -0 if (urb_urb[_n+0]==1 & _n!=1)
replace urb_urb_t =  1 if (urb_urb[_n-1]==1 & _n!=1)
replace urb_urb_t =  2 if (urb_urb[_n-2]==1 & _n!=1)

egen rur_rur_w = mean(wage), by(rur_rur_t)
egen rur_urb_w = mean(wage), by(rur_urb_t)
egen urb_rur_w = mean(wage), by(urb_rur_t)
egen urb_urb_w = mean(wage), by(urb_urb_t)

scatter rur_rur_w rur_rur_t
scatter rur_urb_w rur_urb_t
scatter urb_rur_w urb_rur_t
scatter urb_urb_w urb_urb_t

gr combine wag_rur_rur.gph wag_rur_urb.gph wag_urb_rur.gph wag_urb_urb.gph

***************************************
** TASK 3: COMPARE MOVERS VS STAYERS **
***************************************

gen age = year - (1900 + birth)
sum age
reg wage age educ gender

* some variables seem to have nonlinear relationship
gen age2 = age * age
gen educ2 = educ * educ
gen ln_wage = ln(wage)

* after running wald-tests and looking at residuals and past studies, this model seems to be better
reg ln_wage age age2 educ educ2 gender
reg ln_wage age age2 educ2 gender

* TABLE 5: by region of origin *

* those who moved from region 1

gen one_leaver0 = 0
gen one_leaver1 = 0
gen one_leaver2 = 0
gen one_leaver3 = 0
gen one_leaver4 = 0

replace one_leaver0 = 1 if (move_time * frm_one   == 0 & move_time != .)
replace one_leaver1 = 1 if (move_time * frm_one[_n-1] == 1 & move_time != . & moved == 0)
replace one_leaver2 = 1 if (move_time * frm_one[_n-2] == 2 & move_time != . & moved == 0)
replace one_leaver3 = 1 if (move_time * frm_one[_n-3] == 3 & move_time != . & moved == 0)
replace one_leaver4 = 1 if (move_time * frm_one[_n-4] == 4 & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender one_leaver0 one_leaver1 one_leaver2 one_leaver3 one_leaver4 i.region, robust
estimates store m_left1, title(m_left1)

* those who moved from region 2

gen two_leaver0 = 0
gen two_leaver1 = 0
gen two_leaver2 = 0
gen two_leaver3 = 0
gen two_leaver4 = 0

replace two_leaver0 = 1 if (move_time * frm_two   == 0 & move_time != .)
replace two_leaver1 = 1 if (move_time * frm_two[_n-1] == 1 & move_time != . & moved == 0)
replace two_leaver2 = 1 if (move_time * frm_two[_n-2] == 2 & move_time != . & moved == 0)
replace two_leaver3 = 1 if (move_time * frm_two[_n-3] == 3 & move_time != . & moved == 0)
replace two_leaver4 = 1 if (move_time * frm_two[_n-4] == 4 & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender two_leaver0 two_leaver1 two_leaver2 two_leaver3 two_leaver4 i.region, robust
estimates store m_left1, title(m_left2)

* those who moved from region 3

gen thre_leaver0 = 0
gen thre_leaver1 = 0
gen thre_leaver2 = 0
gen thre_leaver3 = 0
gen thre_leaver4 = 0

replace thre_leaver0 = 1 if (move_time * frm_thre   == 0 & move_time != .)
replace thre_leaver1 = 1 if (move_time * frm_thre[_n-1] == 1 & move_time != . & moved == 0)
replace thre_leaver2 = 1 if (move_time * frm_thre[_n-2] == 2 & move_time != . & moved == 0)
replace thre_leaver3 = 1 if (move_time * frm_thre[_n-3] == 3 & move_time != . & moved == 0)
replace thre_leaver4 = 1 if (move_time * frm_thre[_n-4] == 4 & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender thre_leaver0 thre_leaver1 thre_leaver2 thre_leaver3 thre_leaver4 i.region, robust
estimates store m_left3, title(m_left3)

* those who moved from region 4

gen four_leaver0 = 0
gen four_leaver1 = 0
gen four_leaver2 = 0
gen four_leaver3 = 0
gen four_leaver4 = 0

replace four_leaver0 = 1 if (move_time * frm_four   == 0 & move_time != .)
replace four_leaver1 = 1 if (move_time * frm_four[_n-1] == 1 & move_time != . & moved == 0)
replace four_leaver2 = 1 if (move_time * frm_four[_n-2] == 2 & move_time != . & moved == 0)
replace four_leaver3 = 1 if (move_time * frm_four[_n-3] == 3 & move_time != . & moved == 0)
replace four_leaver4 = 1 if (move_time * frm_four[_n-4] == 4 & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender four_leaver0 four_leaver1 four_leaver2 four_leaver3 four_leaver4 i.region, robust
estimates store m_left4, title(m_left4)

esttab m_left1 m_left2 m_left3 m_left4 using m_left.tex, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons constant) stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

* TABLE 6: by region of arrival *

* those who moved to region 1

gen one_comer0 = 0
gen one_comer1 = 0
gen one_comer2 = 0
gen one_comer3 = 0
gen one_comer4 = 0

replace one_comer0 = 1 if (move_time * (region == 1)  & move_time != .)
replace one_comer1 = 1 if (move_time * (region[_n-1] == 1) & move_time != . & moved == 0)
replace one_comer2 = 1 if (move_time * (region[_n-2] == 1) & move_time != . & moved == 0)
replace one_comer3 = 1 if (move_time * (region[_n-3] == 1) & move_time != . & moved == 0)
replace one_comer4 = 1 if (move_time * (region[_n-4] == 1) & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender one_comer0 one_comer1 one_comer2 one_comer3 one_comer4 i.region, robust
estimates store m_comer1, title(m_comer1)

* those who moved to region 2

gen two_comer0 = 0
gen two_comer1 = 0
gen two_comer2 = 0
gen two_comer3 = 0
gen two_comer4 = 0

replace two_comer0 = 1 if (move_time * (region == 2)  & move_time != .)
replace two_comer1 = 1 if (move_time * (region[_n-1] == 2) & move_time != . & moved == 0)
replace two_comer2 = 1 if (move_time * (region[_n-2] == 2) & move_time != . & moved == 0)
replace two_comer3 = 1 if (move_time * (region[_n-3] == 2) & move_time != . & moved == 0)
replace two_comer4 = 1 if (move_time * (region[_n-4] == 2) & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender two_comer0 two_comer1 two_comer2 two_comer3 two_comer4 i.region, robust
estimates store m_comer2, title(m_comer2)

* those who moved to region 3

gen thre_comer0 = 0
gen thre_comer1 = 0
gen thre_comer2 = 0
gen thre_comer3 = 0
gen thre_comer4 = 0

replace thre_comer0 = 1 if (move_time * (region == 3)  & move_time != .)
replace thre_comer1 = 1 if (move_time * (region[_n-1] == 3) & move_time != . & moved == 0)
replace thre_comer2 = 1 if (move_time * (region[_n-2] == 3) & move_time != . & moved == 0)
replace thre_comer3 = 1 if (move_time * (region[_n-3] == 3) & move_time != . & moved == 0)
replace thre_comer4 = 1 if (move_time * (region[_n-4] == 3) & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender thre_comer0 thre_comer1 thre_comer2 thre_comer3 thre_comer4 i.region, robust
estimates store m_comer3, title(m_comer3)

* those who moved to region 4

gen four_comer0 = 0
gen four_comer1 = 0
gen four_comer2 = 0
gen four_comer3 = 0
gen four_comer4 = 0

replace four_comer0 = 1 if (move_time * (region == 4)  & move_time != .)
replace four_comer1 = 1 if (move_time * (region[_n-1] == 4) & move_time != . & moved == 0)
replace four_comer2 = 1 if (move_time * (region[_n-2] == 4) & move_time != . & moved == 0)
replace four_comer3 = 1 if (move_time * (region[_n-3] == 4) & move_time != . & moved == 0)
replace four_comer4 = 1 if (move_time * (region[_n-4] == 4) & move_time != . & moved == 0)

reg ln_wage age age2 educ2 gender four_comer0 four_comer1 four_comer2 four_comer3 four_comer4 i.region, robust
estimates store m_comer4, title(m_comer4)

esttab m_comer1 m_comer2 m_comer3 m_comer4 using m_comer.tex, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons constant) stats(r2 df_r bic, fmt(3 0 1) label(R-sqr dfres BIC))

***************************************
** Some Extra maybe for the APPENDIX **
***************************************

* would be interesting to check how moving behavior changes over time
gen decade = 10 * floor((year+1)/10)
gen year_5 =  5 * floor((year+1)/5)

estpost tabstat frm_one frm_two frm_thre frm_four moved, by(decade) stat(sum)
graph bar (sum) frm_one frm_two frm_thre frm_four, over(decade)

egen mean_w = mean(wage), by(year)
egen loq_w = pctile(wage), p(25) by(year)
egen upq_w = pctile(wage), p(75) by(year)
egen lo5_w = pctile(wage), p(5) by(year)
egen up5_w = pctile(wage), p(95) by(year)
egen up1_w = pctile(wage), p(99) by(year)
scatter mean_w loq_w upq_w lo5_w up5_w year

estpost tabstat rur_rur rur_urb urb_rur urb_urb moved, by(decade) stat(sum)
graph bar (sum) rur_rur rur_urb urb_rur urb_urb, over(decade)

graph bar (sum) rur_rur rur_urb urb_rur urb_urb, over(region)
graph bar (sum) rur_rur rur_urb urb_rur urb_urb

egen sum_r_r = sum(rur_rur), by(year)
egen sum_r_u = sum(rur_urb), by(year)
egen sum_u_r = sum(urb_rur), by(year)
egen sum_u_u = sum(urb_urb), by(year)

scatter sum_r_r sum_r_u sum_u_r sum_u_u year

egen sum5_r_r = sum(rur_rur), by(year_5)
egen sum5_r_u = sum(rur_urb), by(year_5)
egen sum5_u_r = sum(urb_rur), by(year_5)
egen sum5_u_u = sum(urb_urb), by(year_5)

scatter sum5_r_r sum5_r_u sum5_u_r sum5_u_u year_5

estpost tabstat urb0_0 urb0_1 urb0_2 urb1_0 urb1_1 urb1_2 urb2_0 urb2_1 urb2_2 moved, by(region) stat(sum)


* if want to chek third year (but may make things more difficult with possibly more overlapping moves)
replace move_time =  3 if ( moved[_n-3]==1 & _n!=1)
replace r12_time  =  3 if (reg1_2[_n-3]==1 & _n!=1)
replace r13_time  =  3 if (reg1_3[_n-3]==1 & _n!=1)
replace r31_time  =  3 if (reg3_1[_n-3]==1 & _n!=1)
replace r34_time  =  3 if (reg3_4[_n-3]==1 & _n!=1)

replace move_time =  4 if ( moved[_n-4]==1 & _n!=1)


