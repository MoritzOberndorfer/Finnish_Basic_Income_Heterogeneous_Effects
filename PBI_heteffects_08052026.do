/*
Partial basic income has positive and no evidence for heterogenous effects on mental health – 
An analysis of the Finnish basic income randomized experiment among people in unemployment 

Authors: Aapo Hiilamo, Moritz Oberndorfer
Code authors: Aapo Hiilamo, Moritz Oberndorfer


Data is openly available at:
https://services.fsd.tuni.fi/catalogue/FSD3488?study_language=en&lang=en

Code contains: 

1) Code for data preparation (Stata code)

2) Code for Table 1 (R code)

2) Code for analysis and visualisation of figure 1 (Stata code)

3) Code for analysis and visualisation of figure 2 (Stata code)

4) Code for analysis and visualisation of figure 3 (R code)


*/

/*
Setting paths
clear all
global username "`c(username)'"
dis "$username" // Displays your user name on your computer
if "$username" == "moritzob" {
global master = "C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero"
global code "C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero\scripts"
global log 		"C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero\out\log"
global dta 		"C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero/dta"
global docx 	"C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero\out\docx"
global graph  	"C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero\out\graphs"
global table 	"C:\Users\moritzob\OneDrive - University of Helsinki\postdoc life helsinki\Basic income Finland effect hetero\out\tables"
}

*/

**# 1) Data preparation
clear all
import delimited "$dta/daF3488_fin.csv",  clear
keep q17* t1 t2 t4 t6 t7 tyyppi suuralue kuntar q1 weight

gen weight_real = real(subinstr(weight, ",", ".", .))
destring suuralue kuntar t2, replace

//outcome coding
recode  q17* (7=.)
recode q17_3 q17_5 (1=6) (2=5) (3=4) (4=3) (5=2) (6=1)
egen anxiety_score=rowtotal(q17*)
egen missing_anxiety=rowmiss(q17*)
replace anxiety_score=. if missing_anxiety

sum anxiety_score, d
*gen anxiety_score_std=(anxiety_score-r(mean))/r(sd)
gen anxiety_score_std=((anxiety_score-5)/25)*100

gen clinically_significant=anxiety_score_std<=52 if !missing(anxiety_score)
tab clinically_significant tyyppi, col
pwcorr anxiety_score_std q17* //all good

//covariate coding
recode q1 (3 6 7=5) (4=3) (5=4), gen(c_past_employment)
label define past_employment_lbl 1 "Entrepreneur" 2 "Salaried" 3 "Student" 4 "Unemployed" 5 "Homemaker/Other/Retired"
label values c_past_employment past_employment_lbl

recode t4 (2=1) (3 4 5 =2) ( 6=3), gen(c_education) 
label define Education_lbl 1 "Low education" 2 "Medium education" 3 "High education"
label values c_education Education_lbl

recode t6 (5=.) (3=2), gen(family_type)

//renaming
rename (t1 t2  t7 tyyppi suuralue kuntar anxiety_score_std) (c_gender c_agegroup  c_householdsize treatment area c_urbanicity outcome)

label define gender_lbl ///
1 "Women" ///
2 "Men"
label values c_gender gender_lbl

label define AgeGroup_lbl ///
    1 "< 30 yrs" ///
    2 "30-34 yrs" ///
    3 "35-44 yrs" ///
    4 "45-54 yrs" ///
    5 "55 yrs +" 
label values c_agegroup AgeGroup_lbl



label define MunicipalGroup_lbl ///
    1 "Urban municipalities" ///
    2 "Densely populated municipalities" ///
    3 "Rural municipalities"
label values c_urbanicity MunicipalGroup_lbl

//creating dummies for variables without order (this is to prepare the data to causal forest)

gen c_area_north_east=area==4
gen c_area_west=area==3
drop area

gen c_single=family_type==1
gen c_fchildren=family_type==4

label define single_lbl ///
0 "Not single" ///
1 "Single"
label values c_single single_lbl

label define children_lbl ///
0 "No children" ///
1 "Children"
label values c_fchildren children_lbl

keep outcome treatment c_* weight_real clinically_significant  outcome q17* 

recode c_householdsize (5 6 7 = 5)
//missing values, 70 persons are lost, mainly due to missing outcome, little we can do about it. 
misstable summarize outcome treatment c_* 
regress outcome treatment i.(c_*) [pw=weight_real]
keep if e(sample) // 1527
drop c_householdsize

save "$dta/cleaned_data.dta",replace
**# Table 1
/* Table is created with R code

library(haven)
library(tidyverse)
library(tableone)
library(tidyverse)
library(kableExtra)
library(flextable)
library(patchwork)
library(reshape2)  

df <- read_dta("C:/Users/moritzob/OneDrive - University of Helsinki/postdoc life helsinki/Basic income Finland effect hetero/dta/cleaned_data.dta") |>
  select(treatment, q17_1,q17_2,q17_3,q17_4,q17_5,outcome,clinically_significant) |>
  rename(
    anxiety_item       = q17_1,
    depressed_item     = q17_2,
    peaceful_item      = q17_3,
    `feeling down_item`= q17_4,  
    happiness_item     = q17_5
  )
 
cor_mat <- cor(df, use = "complete.obs")
cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
melted_cor <- melt(cor_mat, na.rm = TRUE)

ggplot(data = melted_cor, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white", 
    midpoint = 0, limit = c(-1, 1), 
    name = "Pearson\nCorrelation"
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  theme(
    axis.title.x       = element_blank(),
    axis.title.y       = element_blank(),
    panel.grid.major   = element_blank(),
    panel.border       = element_blank(),
    panel.background   = element_blank(),
    axis.ticks         = element_blank(),
    legend.justification = c(1, 0),
    legend.position    = c(0.6, 0.7),
    legend.direction   = "horizontal"
  ) +
  guides(fill = guide_colorbar(
    barwidth = 7, 
    barheight = 1,
    title.position = "top", 
    title.hjust = 0.5
  ))

cleaned_data <- read_dta("00_data/02_processed/cleaned_data.dta") |>
  select(!outcome) |>
  rename(outcome = clinically_significant)

cleaned_data <- cleaned_data |>
  dplyr::mutate(
    c_agegroup = factor(c_agegroup,
                        levels = c(1, 2, 3, 4, 5),
                        labels = c("< 30 yrs", "30-34 yrs", "35-44 yrs", "45-54 yrs", "55 yrs +")
    ),
    c_urbanicity = factor(c_urbanicity,
                          levels = c(1, 2, 3),
                          labels = c("Urban municipalities", "Densely populated municipalities", "Rural municipalities")
    ),
    c_past_employment = factor(c_past_employment,
                               levels = c(1, 2, 3, 4, 5),
                               labels = c("Entrepreneur", "Salaried", "Student", "Unemployed", "Homemaker_Other_Retired")
    ),
    c_education = factor(c_education,
                         levels = c(1, 2, 3),
                         labels = c("Low", "Medium", "High")
    ),
    c_gender = factor(c_gender,
                      levels = c(1, 2),
                      labels = c("Women", "Male")
    ),
    c_single = factor(c_single,
                      levels = c(0, 1),
                      labels = c("Not single", "Single")
    ),
    c_fchildren = factor(c_fchildren,
                         levels = c(0, 1),
                         labels = c("No children", "Children")
    )
  )

categorical_vars <- c(
  "c_gender", "c_agegroup", "c_urbanicity", "c_past_employment",
  "c_education",  "c_single", "c_fchildren"
)

cleaned_data <- cleaned_data %>%
  mutate(across(all_of(categorical_vars), as.factor))
vars <- c("c_gender", "c_agegroup", "c_householdsize", "c_urbanicity", 
          "c_past_employment", "c_education", "c_area_north_east", 
          "c_area_west", "c_single", "c_fchildren", "outcome")

cat_vars <- vars[vars != "c_householdsize"]  

table1 <- CreateTableOne(
  vars = vars,
  strata = "treatment",         
  data = cleaned_data,
  factorVars = cat_vars
)
print(table1, quote = FALSE, noSpaces = TRUE)

table1_df <- print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE) %>%
  as.data.frame()
head(table1_df)

table1_df %>%
  kable(format = "html", booktabs = TRUE, align = "l") %>%
  kable_styling(full_width = FALSE, position = "center")

table1_flex <- flextable(as.data.frame(table1_df))
save_as_docx(table1_flex, path = "Table1.docx")


*/



**# Figure 1)
cd "$dta/"

*save ATE for figure 1
logit clinically_significant i.treatment i.(c_*) [pw=weight_real] , or
margins, dydx(treatment) post
global ATE=-.0737712*100
global ll_ATE=-.1190813*100
global ul_ATE=-.0284612*100
logit clinically_significant i.treatment i.(c_*) [pw=weight_real] , or

matrix list r(table)
di  r(table)[1,2] 
di  r(table)[5,2] 
di  r(table)[1,2] 
global OR_ATE=r(table)[1,2] 
global ll_OR_ATE=r(table)[5,2] 
global ul_OR_ATE=r(table)[6,2] 

tempfile prep_data
save `prep_data',replace

postfile myfile str20 moderator str20 value str35 labels str20 scale  float coef float se float testparm using interaction_coefficients, replace

foreach var of varlist c_* {
	levelsof `var', local(values) 
	foreach value of local values {
		capture  local vlname : value label `var'
		capture local labels: label `vlname' `value'
		logit clinically_significant i.treatment i.treatment##ib`value'.`var' i.(c_*) [pw=weight_real] , or
		local c = _b[1.treatment]
		local s = _se[1.treatment]
		testparm i.treatment#ib`value'.`var'
		local pval = r(p)
		post myfile  ("`var'") ("`value'") ("`labels'") ("log_or") (`c') (`s') (`pval') 

		margins, dydx(treatment) post at(`var' =`value')
		local c = _b[1.treatment]
		local s = _se[1.treatment]
		post myfile  ("`var'") ("`value'")  ("`labels'") ("rd") (`c') (`s') (`pval') 
	}
}

postclose myfile

use interaction_coefficients, clear

gen ll=coef-1.96*se if scale=="rd"
gen ul=coef+1.96*se if scale=="rd"
replace ll=exp((coef-1.96*se)) if scale=="log_or"
replace ul=exp(coef+1.96*se) if scale=="log_or"
replace coef=exp(coef) if scale =="log_or"

sort moderator scale value 
preserve
keep if scale=="rd"
gen ylab=_n

*ssc install labutil
labmask ylab, values(labels)

foreach var of varlist coef ll ul {
replace `var '=`var'*100 // estimates in percentage-points
}

tw ///
(rspike ll ul ylab, horizontal lcolor(black%80) lwidth(medium) ) ///
(scatter ylab coef, mcolor(black) msize(medium)) ///
(scatteri 5.5 -35 8.5 -35 8.5 15 5.5 15, base(5.5) recast(area) fcolor(gs8%30) fintensity(60) lwidth(none) ) ///
(scatteri 10.5 -35 12.5 -35 12.5 15 10.5 15, base(10.5) recast(area) fcolor(gs8%30) fintensity(60) lwidth(none) ) ///
(scatteri 17.5 -35 19.5 -35 19.5 15 17.5 15, base(17.5) recast(area) fcolor(gs8%30) fintensity(60) lwidth(none) ) ///
if scale=="rd", ///
ytitle(" ") ///
ylab(1(1)22, valuelabel  noticks) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-35(5)15, grid) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off) ///
title("Group-specific treatment effects; single-level logistic regression model", size(medium)) ///
xline($ATE, lcolor(black) lpattern(solid) ) ///
xline($ll_ATE $ul_ATE, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
plotregion(margin(small)) ///
fxsize(950) ///
name(fig1_rd, replace)

restore

preserve
sort moderator scale value 
keep if scale=="log_or"
gen ylab=_n

*ssc install labutil
labmask ylab, values(labels)


tw ///
(rspike ll ul ylab, horizontal lcolor(black%80) lwidth(medium) ) ///
(scatter ylab coef, mcolor(black) msize(medium)) ///
(scatteri 5.5 0.05 8.5 0.05 8.5 5 5.5 5, base(5.5) recast(area) fcolor(gs8%30) fintensity(60) lwidth(none) ) ///
(scatteri 10.5 0.05 12.5 0.05 12.5 5 10.5 5, base(10.5) recast(area) fcolor(gs8%30) fintensity(60) lwidth(none) ) ///
(scatteri 17.5 0.05 19.5 0.05 19.5 5 17.5 5, base(17.5) recast(area) fcolor(gs8%30) fintensity(60) lwidth(none) ) ///
if scale=="log_or", ///
ytitle("",) ///
ylab(1(1)22, nolab  noticks) ///
xtitle("Treatment effect in Odds ratio") ///
xlab(0.1 0.3 0.6 1 2 3 4 5, grid ) ///
xscale(r(0.1 5)log) ///
xline(1, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off) ///
title("Group-specific treatment effects; single-level logistic regression model", size(medium)) ///
xline($OR_ATE, lcolor(black) lpattern(solid) ) ///
xline($ll_OR_ATE $ul_OR_ATE, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
plotregion(margin(small)) ///
fxsize(600) ///
name(fig1_or, replace)

restore

grc1leg2 fig1_rd fig1_or, ///
loff maintotoptitle imargins(tiny)

graph export "$graph/figure1.svg",replace
graph export "$graph/figure1.pdf",replace

**# Figure 2

/*
Create social strata along axes of potential labour market disadvantage:
Gender, past employment status, age, and education
*/
use `prep_data', clear // open prepared data
*age
gen c_agegroup_mlm =c_agegroup // need to collapse medium and high education due to small cell sizes
recode c_agegroup_mlm (3=2) (4=2) (5=3)
tab c_agegroup_mlm,m
label define AgeGroup_lbl2 ///
    1 "< 30 yrs" ///
    2 "30-54 yrs" ///
    3"55 yrs +" 
label values c_agegroup_mlm AgeGroup_lbl2

gen c_education_mlm=c_education // need to collapse medium and high education due to small cell sizes
recode c_education_mlm (3=2)
label define Education_lbl2 1 "Low" 2 "Medium, High"
label values c_education_mlm Education_lbl2

*Also create the strata with education

gen stratum = 1000*c_gender + 100*c_agegroup_mlm + 10*c_past_employment + 1*c_education_mlm

egen strat_labelled = group(c_gender c_agegroup_mlm c_past_employment c_education_mlm), label missing

bysort stratum: gen n_stratum=_N
sum n_stratum, d
tab strat_labelled,m
codebook stratum

recode stratum (1121=1122) (1131=1132) (1142=1152) (1211=1212) (1311=1312) ///
(1331=1351) (1332=1352) (2121=2122) (2151=2141) (2312=2311) (2331=2351) (2332=2352)

codebook stratum
tab stratum treatment

decode strat_labelled, gen(mlab2)
replace mlab2="Women < 30 yrs Salaried" if stratum==1122
replace mlab2="Women < 30 yrs Student" if stratum==1132
replace mlab2="Women < 30 yrs Unemployed/Other Medium/High" if stratum==1152
replace mlab2="Women 30-54 yrs Entrepreneur" if stratum==1212
replace mlab2="Women 55+ yrs Entrepreneur" if stratum==1312
replace mlab2="Women 55+ yrs Retired/Student Low" if stratum==1351
replace mlab2="Women 55+ yrs Retired/Student Medium/High" if stratum==1352
replace mlab2="Men < 30 yrs Salaried" if stratum==2122
replace mlab2="Men < 30 yrs Unemployed/Other Low" if stratum==2141
replace mlab2="Men 55+ yrs Entrepreneur" if stratum==2311
replace mlab2="Men 55+ yrs Reitred/Student Low" if stratum==2351
replace mlab2="Men 55+ yrs Reitred/Student Medium/High" if stratum==2352

drop n_stratum
bysort stratum: gen n_stratum=_N
sum n_stratum, d
tab strat_labelled,m
codebook stratum
preserve // get table with strata size by intervention arm
collapse (mean) outcome (count) n_stratum ,by(treatment mlab2 stratum)
reshape wide n_stratum mlab2 outcome , i(stratum) j(treatment)
keep stratum mlab20  n_stratum0 n_stratum1
order stratum mlab20  n_stratum0 n_stratum1
gen n_total= n_stratum0 + n_stratum1
export delimited "$table/strata_size_$S_DATE.csv",replace
restore


sort stratum 
reg clinically_significant i.treatment##i.stratum weight_real // 86 parameters, equivalent to Model 1

*produce figure
margins i.treatment, pwcompare // get population average treatment effect
return list
matrix list r(table_vs)
di  r(table_vs)[1,1] *100
di  r(table_vs)[5,1]
global ATE = r(table_vs)[1,1] *100
global ll_ATE = r(table_vs)[5,1]*100
global ul_ATE = r(table_vs)[6,1]*100

margins r.treatment@stratum, saving("$dta/margins_m0.dta",replace)
tempfile data
save `data',replace

frame create margins
frame change margins
use "$dta/margins_m0.dta",clear

egen rank=rank(_margin), unique
rename _m2 stratum

merge 1:m stratum using `data', keepusing(mlab2) nogen
sort stratum
duplicates drop

gen pos=9

foreach var of varlist _margin _ci_lb _ci_ub {
replace `var'=`var'*100
}


tw ///
(rcap _ci_lb _ci_ub rank, horizontal lcolor(gs10) lwidth(vthin) ) ///
(scatter rank _margin, mcolor(black)) ///
(scatter rank _ci_ub if rank<21, msym(none) mlab(mlab2) mlabsize(vsmall) mlabcolor(black) ) ///
(scatter rank _ci_lb if rank>20, msym(none) mlab(mlab2) mlabv(pos) mlabsize(vsmall) mlabcolor(black) ) ///
, ///
ytitle("Strata Rank") ///
ylab(0(5)40 43, nogrid noticks) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-100(25)100, grid) ///
xscale(range(-190 170)) ///
xline($ATE, lcolor(black) lpattern(solid) ) ///
xline($ll_ATE $ul_ATE, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off) ///
title("{bf:Panel A}: Single-level linear probability model (LPM)", size(medium)) ///
name(LPM0, replace)

graph save "$graph/LPM0 $S_DATE.gph",replace

frame change default
save "$dta/data_for_R.dta",replace
export delimited "$dta/data_for_R.csv",replace
**# Estimation of bayesian multilevel model in R
*Stratum-specific treatment effects estimated with bayes multilevel models. 
*Do estimation in R and import draws from posterior distribution in stata
/*
R CODE for Bayesian multilevel modelling

library(Rcpp)
library(brms)
library(dplyr)

dta_path <- "C:/Users/moritzob/OneDrive - University of Helsinki/postdoc life helsinki/Basic income Finland effect hetero/dta"
data <- read.csv(file.path(dta_path, "data_for_R.csv"))


model1<- brmsformula(formula=
                       clinically_significant ~ treatment + weight_real +
                       (1 + treatment | stratum)) #M1

model1_prior <- get_prior(model1, data=data)
model1_prior # shows what the default brms priors are

parallel::detectCores() # shows the number cores

fit <- brm(formula=model1, 
           prior=c(set_prior("normal(0, 5)", class = "b"),
                   set_prior("cauchy(0, 2)", class = "sd")),
           data=data,
           family=gaussian(),
           seed=1, 
           chains=12,
           cores=12,
           iter=4000,
           warmup=2000,
           thin=1,) 

#pairs(fit)

# Print the summary of the model
summary(fit)

# Plot diagnostics
plot(fit)

# Posterior predictive checks
pp_check(fit)

#export draws, so I can work with them in stata
posterior_sample_M1<-posterior_samples(fit)
?as_draws
setwd(dta_path)
saveRDS(fit, file="fit_model.rds")
posterior_sample_M1<-as_draws_df(fit)
posterior::summarise_draws(posterior_sample_M1)
write.csv(posterior_sample_M1, 
          "posterior_sample_M1.csv", 
          row.names=FALSE)

*/

**# Use results from R brms for figure
import delimited "$dta/posterior_sample_M1.csv",clear


*get intercept-slope correlation
sum cor_stratum__intercept__treatmen,d
_pctile cor_stratum__intercept__treatmen, p(5 50 95)
return list

sum sd_stratum__treatment,d
_pctile sd_stratum__treatment, p(2.5 5 50 95 97.5)
return list


keep chain iteration draw *treatment *intercept
drop intercept b_intercept b_treatment sd_*

foreach var of varlist *treatment {
local var_og "`var'"
di "`var_og'"	
local new_name=substr("`var_og'", -14,5)
di "`new_name'"
rename `var' u1_`new_name'
}

foreach var of varlist *intercept {
local var_og "`var'"
di "`var_og'"	
local new_name=substr("`var_og'", -14,5)
di "`new_name'"
rename `var' u0_`new_name'
}

drop chain  iteration

reshape long u0_m u1_m , i(draw) j(stratum)

sort stratum draw

save "$dta/reffects_brms_LPM1.dta",replace


import delimited "$dta/posterior_sample_M1.csv",clear

*create long format for the coefficients 
keep b_intercept b_treatment b_weight_real draw

merge 1:m draw using "$dta/reffects_brms_LPM1"
drop _merge
sort  stratum draw

gen t1_t0=b_treatment + u1_m

*now do the aggregation
*stratum-specifc treatment effect
bysort stratum: egen mean_te=mean(t1_t0)
bysort stratum: egen ll_te=pctile(t1_t0), p(2.5)
bysort stratum: egen ul_te=pctile(t1_t0), p(97.5)

*stratum-average treatment effect
egen strat_average_te=mean(b_treatment)
egen ul_strat_average_te=pctile(b_treatment), p(97.5)
egen ll_strat_average_te=pctile(b_treatment), p(2.5)

*stratum-specific deviation
bysort stratum: egen ul_u1=pctile(u1_m), p(97.5)
bysort stratum: egen ll_u1=pctile(u1_m), p(2.5)

collapse (mean) b_treatment u0_m u1_m  t1_t0 mean_te ll_* ul_* strat_average_te, by(stratum )

*get mlab2
merge 1:m stratum using "$dta/data_for_R.dta", keepusing(mlab2) nogen
duplicates drop

*data for figure ready

*need to change scale
foreach var of varlist b_treatment u0_m u1_m t1_t0 mean_te ll_te ll_strat_average_te ll_u1 ul_te ul_strat_average_te ul_u1 strat_average_te {
replace `var'=`var'*100
}

egen rank=rank(mean_te), unique
egen rank_u1=rank(u1_m), unique

save "$dta/MLM_figure $S_DATE.dta",replace
use "$dta/MLM_figure $S_DATE.dta",clear
global CA=strat_average_te
di $CA
global ll_CA=ll_strat_average_te
global ul_CA=ul_strat_average_te
di $ll_CA ";"$ul_CA

graph use "$graph/LPM0 $S_DATE.gph"

tw ///
(rcap ll_te ul_te rank, horizontal lcolor(gs10) lwidth(vthin) ) ///
(scatter rank mean_te, mcolor(black)) ///
(scatter rank ul_te, msym(none) mlab(mlab2) mlabsize(vsmall) mlabcolor(black) ) ///
, ///
ytitle("Strata Rank") ///
ylab(0(5)40 43, nogrid noticks nolab) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-20(5)20, grid) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
xline($CA, lcolor(black) lpattern(solid) ) ///
xline($ll_CA $ul_CA, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
legend(off) ///
title("{bf:Panel B:} Bayesian Multilevel LPM", size(medium)) ///
fxsize(700) ///
name(LPM1_brms_effects, replace)


frame change margins

tw ///
(rcap _ci_lb _ci_ub rank, horizontal lcolor(gs10) lwidth(vthin) ) ///
(scatter rank _margin, mcolor(black)) ///
(scatter rank _ci_ub if rank<21, msym(none) mlab(mlab2) mlabsize(vsmall) mlabcolor(black) ) ///
(scatter rank _ci_lb if rank>20, msym(none) mlab(mlab2) mlabv(pos) mlabsize(vsmall) mlabcolor(black) ) ///
, ///
ytitle("", size(small)) ///
ylab(0(5)40 43, nogrid noticks nolab) ///
yscale(lstyle(none)) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-100(25)100, grid) ///
xscale(range(-190 180)) ///
xline($ATE, lcolor(black) lpattern(solid) ) ///
xline($ll_ATE $ul_ATE, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off) ///
title("{bf:Panel A}: Single-level linear probability model (LPM)", size(medium)) ///
fxsize(850) ///
name(LPM0, replace)

graph save "$graph/LPM0 $S_DATE.gph",replace


grc1leg2 LPM0 LPM1_brms_effects , ///
col(2) imargins(small) xtob1title ytol1title loff ///
name(CTE,replace)

graph save "$graph/CTE $S_DATE.gph",replace
graph export "$graph/CTE $S_DATE.tif",replace 

preserve // create table of group-specific effects
keep _margin _ci_lb _ci_ub mlab2 rank
order mlab2 _margin 
tostring  _margin _ci_lb _ci_ub , replace force format(%03.1f)
gen str cate_ci= _margin + " [" +_ci_lb + "; " + _ci_ub + "]"
keep mlab2 cate_ci rank
sort rank
export delimited "$table/LPM0_cates_$S_DATE.csv",replace
restore
frame change default
sort rank
keep mean_te ll_te ul_te mlab2 rank
global results_var = "mean_te ll_te ul_te"
tostring  $results_var , replace force format(%03.1f)
gen str cate_ci= mean_te + " [" +ll_te + "; " + ul_te + "]"
keep mlab2 rank cate_ci
export delimited "$table/BRMS_LPM1_cates_$S_DATE.csv",replace


**# Causal Forest estimation and Figure 3
/*
library(writexl)
library(grf)
library(rpart)
library(glmnet)
library(splines)
library(MASS)
library(sandwich)
library(stringr)
library(haven)
library(tidyverse)

num.rankings <- 4
seed_<-1111

cleaned_data <- read_dta("C:/Users/moritzob/OneDrive - University of Helsinki/postdoc life helsinki/Basic income Finland effect hetero/dta/cleaned_data.dta") |>
  dplyr::select(!outcome) |>
  rename(outcome = clinically_significant)

weights <-cleaned_data |>
  dplyr::select(weight_real)  %>%
  as.matrix()

Y <- cleaned_data$outcome %>%
  as.matrix()

W <- cleaned_data$treatment %>% as.numeric() |>
  as.matrix()

X <- cleaned_data |>
  dplyr::select(starts_with("c_")) |>
  dplyr::select(-starts_with("c_area")) |>
  dplyr::mutate(across(starts_with("c"), as.factor)) |>
  dplyr::mutate(across(starts_with("c_gender"), as.numeric)) |>
  stats::model.matrix(~ . - 1, data = _)

n<-nrow(cleaned_data)

n <- nrow(cleaned_data)
set.seed(seed_)
num.folds <- 5
folds <- sample(rep(seq_len(num.folds), length.out = n))

forest.W <- regression_forest(X, W, tune.parameters = "all",
                              seed =seed_)
W.hat <- predict(forest.W)$predictions

forest.Y <- regression_forest(X, Y, tune.parameters = "all",
                              seed =seed_)
Y.hat <- predict(forest.Y)$predictions

forest <- causal_forest(X = X,
                        Y= Y,
                        W = W,
                        clusters = folds, 
                        seed =seed_, 
                        tune.parameters = "all")

tau.hat <- predict(forest)$predictions
hist(tau.hat)

ranking <- rep(NA, n)
for (fold in seq(num.folds)) {
  tau.hat.quantiles <- quantile(tau.hat[folds == fold], probs = seq(0, 1, by=1/num.rankings))
  ranking[folds == fold] <- cut(tau.hat[folds == fold], tau.hat.quantiles, labels=seq(num.rankings))
}

cleaned_data$effects<- tau.hat
cleaned_data$effect_group<- ranking
outcome <- "outcome"
treatment <- "treatment"
fmla <- paste0(outcome, " ~ 0 + ranking*", treatment)
ols.ate <- lm(fmla, data=transform(cleaned_data, ranking=factor(ranking)))
summary(ols.ate)
fmla <- paste0(outcome, " ~ 0 + ranking + ranking:", treatment)
ols.ate <- lm(fmla, data=transform(cleaned_data, ranking=factor(ranking)))
summary(ols.ate)
ols.ate <- coeftest(ols.ate, vcov=vcovHC(ols.ate, type='HC2'))
interact <- which(grepl(":", rownames(ols.ate)))
ols.ate <- data.frame("ols", paste0("Q", seq(num.rankings)), ols.ate[interact, 1:2])
rownames(ols.ate) <- NULL # just for display
colnames(ols.ate) <- c("method", "ranking", "estimate", "std.err")
ols.ate

tau.hat <- predict(forest)$predictions
e.hat <- forest$W.hat # P[W=1|X]
m.hat <- forest$Y.hat # E[Y|X]

mu.hat.0 <- m.hat - e.hat * tau.hat        # E[Y|X,W=0] = E[Y|X] - e(X)*tau(X)
mu.hat.1 <- m.hat + (1 - e.hat) * tau.hat  # E[Y|X,W=1] = E[Y|X] + (1 - e(X))*tau(X)

# AIPW scores
aipw.scores <- tau.hat + W / e.hat * (Y -  mu.hat.1) - (1 - W) / (1 - e.hat) * (Y -  mu.hat.0)
ols <- lm(aipw.scores ~ 0 + factor(ranking))
summary(ols)
forest.ate <- data.frame("AIPW", paste0("Q", seq(num.rankings)), coeftest(ols, vcov=vcovHC(ols, "HC2"))[,1:2])
colnames(forest.ate) <- c("method", "ranking", "estimate", "std.err")
rownames(forest.ate) <- NULL # just for display
forest.ate

forest.ate %>%
  mutate(
    lower = estimate - 1.96 * std.err,
    upper = estimate + 1.96 * std.err,
    # Round to two decimals
    estimate = sprintf("%.2f", estimate),
    lower = sprintf("%.2f", lower),
    upper = sprintf("%.2f", upper),
    ci = paste0(estimate, " (", lower, ", ", upper, ")")
  ) %>%
  select(method, ranking, ci) %>%
  print(row.names = FALSE)

res <- rbind(forest.ate, ols.ate)

res_clean <- res %>%
  filter(method == "AIPW") %>%
  mutate(
    ranking = case_when(
      ranking == "Q1" ~ "Q1\nLow effect",
      ranking == "Q4" ~ "Q4\nHigh effect",
      TRUE ~ ranking
    )
  )
write_xlsx(res_clean, "estimates_cf.xlsx")

p<-ggplot(res_clean, aes(x = ranking, y = estimate)) +
  geom_point(size = 5) +
  geom_linerange(
    aes(ymin = estimate - 1.96 * std.err, ymax = estimate + 1.96 * std.err),
    size = 2.5  # thick lines for CI
  ) +
  ylab("") +
  xlab("") +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

ggsave("grf_groups.svg", plot = p, width = 4, height = 3, units = "in")


cleaned_data$group <- ranking
cleaned_data$effect <- predict(forest)$predictions

library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)

# Select variables starting with c_ and the grouping variable
selected_data <- cleaned_data %>%
  select(group, effect,starts_with("c_"))

# Create a Table 1 type summary including continuous variable
Table1_summary <- selected_data %>%
  mutate(across(-c(group, effect), as.factor)) %>%
  tbl_summary(by = group, percent = "column", missing = "no", statistic = list(all_categorical() ~ "{p}%", all_continuous() ~ "{mean}"),
              type = list(effect ~ "continuous")) %>%
  add_p()

# View the summary table
Table1_summary<- Table1_summary |> as.data.frame()
Table1_summary %>%
  kable(format = "html", booktabs = TRUE, align = "l") %>%
  kable_styling(full_width = FALSE, position = "center")

# Save as Word document
table1_flex <- flextable(Table1_summary)
save_as_docx(table1_flex, path = "Table_cf.docx")


*/


**# Figure 3
/*
using results from causal forest
*/

import excel "$master/estimates_cf.xlsx",clear firstrow

gen ll=estimate-1.96*stderr
gen ul=estimate+1.96*stderr
egen rank=rank(estimate)

foreach var of varlist estimate ll ul {
	replace `var'= `var'*100
}


tw ///
(rcap ll ul rank, horizontal lcolor(black) lwidth(medium)) ///
(scatter rank estimate, mcolor(black) msize(large)) ///
, ///
ylab(1 "Q4 strongest effect group" 2 "Q3" 3 "Q2" 4 "Q1 weakest effect group", nogrid noticks) ///
yt("") ///
xlab(-20(5)20) ///
xt("Treatment effect in percentage-points") ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off)

graph export "$graph/CF_figure.svg",replace

**# Sensitivity analysis
*Fewer strata in multilevel modelling approach
* Remove employment from strata which makes it
* age gender education
* And reproduce Figure 2


/*
Create social strata along axes of potential labour market disadvantage:
Gender, past employment status, age, and education
*/
use "$dta/cleaned_data.dta",replace
cd "$dta/"

*save ATE for figure 1
logit clinically_significant i.treatment i.(c_*) [pw=weight_real] , or
margins, dydx(treatment) post
global ATE=-.0737712*100
global ll_ATE=-.1190813*100
global ul_ATE=-.0284612*100
logit clinically_significant i.treatment i.(c_*) [pw=weight_real] , or

matrix list r(table)
di  r(table)[1,2] 
di  r(table)[5,2] 
di  r(table)[1,2] 
global OR_ATE=r(table)[1,2] 
global ll_OR_ATE=r(table)[5,2] 
global ul_OR_ATE=r(table)[6,2] 

*age
gen c_agegroup_mlm =c_agegroup // need to collapse medium and high education due to small cell sizes
recode c_agegroup_mlm (3=2) (4=2) (5=3)
tab c_agegroup_mlm,m
label define AgeGroup_lbl2 ///
    1 "< 30 yrs" ///
    2 "30-54 yrs" ///
    3"55 yrs +" 
label values c_agegroup_mlm AgeGroup_lbl2

gen c_education_mlm=c_education // need to collapse medium and high education due to small cell sizes
*recode c_education_mlm (3=2)
label define Education_lbl2 1 "Low" 2 "Medium" 3 "High"
label values c_education_mlm Education_lbl2
tab c_education_mlm, m
*Also create the strata with education

gen stratum = 100*c_gender + 10*c_agegroup_mlm + 1*c_education_mlm

egen strat_labelled = group(c_gender c_agegroup_mlm c_education_mlm), label missing

bysort stratum: gen n_stratum=_N
sum n_stratum, d
tab strat_labelled,m
tab strat_labelled treatment,m
tab stratum treatment,m

recode stratum ///
(112=113) /// Women <30 yrs Medium/High
(133=132) /// Women 55 yrs + Medium/High
(213=212) /// Men <30 yrs Medium/High
(233=232) // Men 55 yrs + Medium/High

codebook stratum
tab stratum treatment

decode strat_labelled, gen(mlab2)
replace mlab2="Women < 30 yrs Medium/High" if stratum==113
replace mlab2="Women 55 + yrs Medium/High" if stratum==132
replace mlab2="Men < 30 yrs Medium/High" if stratum==212
replace mlab2="Men 55 + yrs Medium/High" if stratum==232

drop n_stratum
bysort stratum: gen n_stratum=_N
sum n_stratum, d
tab strat_labelled,m
codebook stratum
preserve // get table with strata size by intervention arm
collapse (mean) outcome (count) n_stratum ,by(treatment mlab2 stratum)
reshape wide n_stratum mlab2 outcome , i(stratum) j(treatment)
keep stratum mlab20  n_stratum0 n_stratum1
order stratum mlab20  n_stratum0 n_stratum1
gen n_total= n_stratum0 + n_stratum1
export delimited "$table/S1_strata_size_$S_DATE.csv",replace
restore


sort stratum 
reg clinically_significant i.treatment##i.stratum weight_real // 86 parameters, equivalent to Model 1

*produce figure
margins i.treatment, pwcompare // get population average treatment effect
return list
matrix list r(table_vs)
di  r(table_vs)[1,1] *100
di  r(table_vs)[5,1]
global ATE = r(table_vs)[1,1] *100
global ll_ATE = r(table_vs)[5,1]*100
global ul_ATE = r(table_vs)[6,1]*100

margins r.treatment@stratum, saving("$dta/margins_m0.dta",replace)
tempfile data
save `data',replace

frame create margins
frame change margins
use "$dta/margins_m0.dta",clear

egen rank=rank(_margin), unique
rename _m2 stratum

merge 1:m stratum using `data', keepusing(mlab2) nogen
sort stratum
duplicates drop

gen pos=9

foreach var of varlist _margin _ci_lb _ci_ub {
replace `var'=`var'*100
}


tw ///
(rcap _ci_lb _ci_ub rank, horizontal lcolor(gs10) lwidth(vthin) ) ///
(scatter rank _margin, mcolor(black)) ///
(scatter rank _ci_ub if rank<21, msym(none) mlab(mlab2) mlabsize(vsmall) mlabcolor(black) ) ///
(scatter rank _ci_lb if rank>20, msym(none) mlab(mlab2) mlabv(pos) mlabsize(vsmall) mlabcolor(black) ) ///
, ///
ytitle("Strata Rank") ///
ylab(0(1)15 , nogrid noticks) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-100(25)100, grid) ///
xscale(range(-100 100)) ///
xline($ATE, lcolor(black) lpattern(solid) ) ///
xline($ll_ATE $ul_ATE, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off) ///
title("{bf:Panel A}: Single-level linear probability model (LPM)", size(medium)) ///
name(LPM0, replace)

graph save "$graph/LPM0 $S_DATE.gph",replace

frame change default
save "$dta/data_for_R.dta",replace
export delimited "$dta/data_for_R.csv",replace
**# Estimation of bayesian multilevel model in R
*Stratum-specific treatment effects estimated with bayes multilevel models. 
*Do estimation in R and import draws from posterior distribution in stata
/*
R CODE for Bayesian multilevel modelling

library(Rcpp)
library(brms)
library(dplyr)

dta_path <- "C:/Users/moritzob/OneDrive - University of Helsinki/postdoc life helsinki/Basic income Finland effect hetero/dta"
data <- read.csv(file.path(dta_path, "data_for_R.csv"))


model1<- brmsformula(formula=
                       clinically_significant ~ treatment + weight_real +
                       (1 + treatment | stratum)) #M1

model1_prior <- get_prior(model1, data=data)
model1_prior # shows what the default brms priors are

parallel::detectCores() # shows the number cores

fit <- brm(formula=model1, 
           prior=c(set_prior("normal(0, 5)", class = "b"),
                   set_prior("cauchy(0, 2)", class = "sd")),
           data=data,
           family=gaussian(),
           seed=1, 
           chains=12,
           cores=12,
           iter=12000,
           warmup=4000,
           thin=1,) 

#pairs(fit)

# Print the summary of the model
summary(fit)

# Plot diagnostics
plot(fit)

# Posterior predictive checks
pp_check(fit)

#export draws, so I can work with them in stata
#posterior_sample_M1<-posterior_samples(fit) # OLD apparently
#?as_draws
setwd(dta_path)
saveRDS(fit, file="fit_model.rds")
posterior_sample_M1<-as_draws_df(fit)
posterior::summarise_draws(posterior_sample_M2)
write.csv(posterior_sample_M2, 
          "posterior_sample_M2.csv", 
          row.names=FALSE)

*/

**# Use results from R brms for figure
import delimited "$dta/posterior_sample_M2.csv",clear


*get intercept-slope correlation
sum cor_stratum__intercept__treatmen,d
_pctile cor_stratum__intercept__treatmen, p(5 50 95)
return list

sum sd_stratum__treatment,d
_pctile sd_stratum__treatment, p(2.5 5 50 95 97.5)
return list


keep chain iteration draw *treatment *intercept
drop intercept b_intercept b_treatment sd_*

foreach var of varlist *treatment {
local var_og "`var'"
di "`var_og'"	
local new_name=substr("`var_og'", -13,4)
di "`new_name'"
rename `var' u1_`new_name'
}

foreach var of varlist *intercept {
local var_og "`var'"
di "`var_og'"	
local new_name=substr("`var_og'", -13,4)
di "`new_name'"
rename `var' u0_`new_name'
}

drop chain  iteration

reshape long u0_m u1_m , i(draw) j(stratum)

sort stratum draw

save "$dta/reffects_brms_LPM2.dta",replace


import delimited "$dta/posterior_sample_M2.csv",clear

*create long format for the coefficients 
keep b_intercept b_treatment b_weight_real draw

merge 1:m draw using "$dta/reffects_brms_LPM2"
drop _merge
sort  stratum draw

gen t1_t0=b_treatment + u1_m

*now do the aggregation
*stratum-specifc treatment effect
bysort stratum: egen mean_te=mean(t1_t0)
bysort stratum: egen ll_te=pctile(t1_t0), p(2.5)
bysort stratum: egen ul_te=pctile(t1_t0), p(97.5)

*stratum-average treatment effect
egen strat_average_te=mean(b_treatment)
egen ul_strat_average_te=pctile(b_treatment), p(97.5)
egen ll_strat_average_te=pctile(b_treatment), p(2.5)

*stratum-specific deviation
bysort stratum: egen ul_u1=pctile(u1_m), p(97.5)
bysort stratum: egen ll_u1=pctile(u1_m), p(2.5)

collapse (mean) b_treatment u0_m u1_m  t1_t0 mean_te ll_* ul_* strat_average_te, by(stratum )

*get mlab2
merge 1:m stratum using "$dta/data_for_R.dta", keepusing(mlab2) nogen
duplicates drop

*data for figure ready

*need to change scale
foreach var of varlist b_treatment u0_m u1_m t1_t0 mean_te ll_te ll_strat_average_te ll_u1 ul_te ul_strat_average_te ul_u1 strat_average_te {
replace `var'=`var'*100
}

egen rank=rank(mean_te), unique
egen rank_u1=rank(u1_m), unique

save "$dta/MLM_figure $S_DATE.dta",replace
use "$dta/MLM_figure $S_DATE.dta",clear
global CA=strat_average_te
di $CA
global ll_CA=ll_strat_average_te
global ul_CA=ul_strat_average_te
di $ll_CA ";"$ul_CA

graph use "$graph/LPM0 $S_DATE.gph"

tw ///
(rcap ll_te ul_te rank, horizontal lcolor(gs10) lwidth(vthin) ) ///
(scatter rank mean_te, mcolor(black)) ///
(scatter rank ul_te, msym(none) mlab(mlab2) mlabsize(small) mlabcolor(black) ) ///
, ///
ytitle("Strata Rank") ///
ylab(0(1)15 , nolab nogrid noticks) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-20(5)20, grid) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
xline($CA, lcolor(black) lpattern(solid) ) ///
xline($ll_CA $ul_CA, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
legend(off) ///
title("{bf:Panel B:} Bayesian Multilevel LPM", size(medium)) ///
fxsize(850) ///
name(LPM1_brms_effects, replace)


frame change margins

tw ///
(rcap _ci_lb _ci_ub rank, horizontal lcolor(gs10) lwidth(vthin) ) ///
(scatter rank _margin, mcolor(black)) ///
(scatter rank _ci_ub if rank<21, msym(none) mlab(mlab2) mlabsize(small) mlabcolor(black) ) ///
, ///
ytitle("", size(small)) ///
ylab(0(1)15, nogrid noticks nolab) ///
yscale(lstyle(none)) ///
xtitle("Treatment effect in percentage-points") ///
xlab(-50(25)100, grid) ///
xline($ATE, lcolor(black) lpattern(solid) ) ///
xline($ll_ATE $ul_ATE, lcolor(gs5) lpattern(dash) lwidth(thin)) ///
xline(0, lcolor(red) lpattern(solid) lwidth(vthin)) ///
legend(off) ///
title("{bf:Panel A}: Single-level linear probability model (LPM)", size(medium)) ///
fxsize(700) ///
name(LPM0, replace)

graph save "$graph/LPM0 $S_DATE.gph",replace


grc1leg2 LPM0 LPM1_brms_effects , ///
col(2) imargins(small) xtob1title ytol1title loff  ///
name(CTE,replace)

graph save "$graph/CTE S1 $S_DATE.gph",replace
graph export "$graph/CTE S1 $S_DATE.tif",replace 

preserve // create table of group-specific effects
keep _margin _ci_lb _ci_ub mlab2 rank
order mlab2 _margin 
tostring  _margin _ci_lb _ci_ub , replace force format(%03.1f)
gen str cate_ci= _margin + " [" +_ci_lb + "; " + _ci_ub + "]"
keep mlab2 cate_ci rank
sort rank
export delimited "$table/LPM0_S1_cates_$S_DATE.csv",replace
restore
frame change default
sort rank
keep mean_te ll_te ul_te mlab2 rank
global results_var = "mean_te ll_te ul_te"
tostring  $results_var , replace force format(%03.1f)
gen str cate_ci= mean_te + " [" +ll_te + "; " + ul_te + "]"
keep mlab2 rank cate_ci
export delimited "$table/BRMS_LPM1_S1_cates_$S_DATE.csv",replace




**# END
/*
For questions regarding Stata code, please contact moritz.oberndorfer@helsinki.fi
For questions regarding R code, please contact hiilamo@demogr.mpg.de

*/


