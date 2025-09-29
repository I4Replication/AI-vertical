
* Replication code for 
* Contactless Real-time Heart Rate Predicts the Performance of Elite Athletes:
* Evidence from Tokyo 2020 Olympic Archery Competition

* archery data
cd "your working directory"

import excel "archery_data.xlsx", sheet("Sheet1") firstrow clear


* generate the definition of HR

* this is the main HR definition used in the paper
* it is the average heart rate before the shooting
egen heart_12mean = rowmean(heart_before1 heart_before2 heart_before3 heart_before4 heart_before5 heart_before6 heart_before7 heart_before8 heart_before9 heart_before10 heart_before11 heart_before12)
sum heart_12mean


*********************************************************************************

* correlation coefficient
pwcorr score heart_12mean,sig obs

* Table 1: heart rate and performance

reg score heart_12mean, cluster(matchnumber)
est store level_results1

reg score heart_12mean age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store level_results2

areg score heart_12mean, absorb(match_name_id) cluster(matchnumber)
est store level_results3

areg score heart_12mean set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store level_results4

outreg2 [level_results*] using heartrate_score_table, dec(3) excel replace


* Table S2, HR just prior to shooting
reg score heart_b, cluster(matchnumber)
est store secondlevel_results1

reg score heart_b age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store secondlevel_results2

areg score heart_b, absorb(match_name_id) cluster(matchnumber)
est store secondlevel_results3

areg score heart_b set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store secondlevel_results4

outreg2 [secondlevel_results*] using heartrate_middle_score_table, dec(3) excel replace

* Table S3, control the standard deviation of HR

egen sd_hr_12 = rowsd(heart_before1 heart_before2 heart_before3 heart_before4 heart_before5 heart_before6 heart_before7 heart_before8 heart_before9 heart_before10 heart_before11 heart_before12)
sum sd_hr_12

reg score sd_hr_12 heart_12mean, cluster(matchnumber)
est store sd_hr_results1

reg score sd_hr_12 heart_12mean age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store sd_hr_results2

areg score sd_hr_12 heart_12mean, absorb(match_name_id) cluster(matchnumber)
est store sd_hr_results3

areg score sd_hr_12 heart_12mean set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store sd_hr_results4

outreg2 [sd_hr_results*] using sd_hr_results1_score_table, dec(4) excel replace


* Table 2 heterogeneity effect
* Here we consider the main elements one by one

* Table S4 Gender

gen heart_12mean_female = heart_12mean * female
areg score heart_12mean  heart_12mean_female set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store gender1

areg score heart_12mean set_number arrows countdown shoot_order if female == 0, absorb(match_name_id) cluster(matchnumber)
est store gender2

areg score heart_12mean set_number arrows countdown shoot_order if female == 1, absorb(match_name_id) cluster(matchnumber)
est store gender3

outreg2 [gender*] using gender_table, dec(3) excel replace


* Table S5 Age

sum age if stage == 1, detail
gen old_age = .
replace old_age = 0 if age <=26
replace old_age = 1 if age > 26 & age != .

gen heart_12mean_old_age = heart_12mean * old_age

areg score heart_12mean  heart_12mean_old_age set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store age1

areg score heart_12mean set_number arrows countdown shoot_order if old_age == 0, absorb(match_name_id) cluster(matchnumber)
est store age2

areg score heart_12mean set_number arrows countdown shoot_order if old_age == 1, absorb(match_name_id) cluster(matchnumber)
est store age3

outreg2 [age*] using age_table, dec(3) excel replace



* Table S6 world ranking
tab worldranking
sum worldranking, detail
sum worldranking if stage == 1, detail
gen worldranking_high = .
replace worldranking_high = 0 if worldranking > 56 & worldranking != .
replace worldranking_high = 1 if worldranking <=56

gen heart_12mean_worldranking_high = heart_12mean * worldranking_high

areg score heart_12mean  heart_12mean_worldranking_high set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store worldranking1

areg score heart_12mean set_number arrows countdown shoot_order if worldranking_high == 0, absorb(match_name_id) cluster(matchnumber)
est store worldranking2

areg score heart_12mean set_number arrows countdown shoot_order if worldranking_high == 1, absorb(match_name_id) cluster(matchnumber)
est store worldranking3

outreg2 [worldranking*] using worldranking_table, dec(3) excel replace



* Table S7 ranking round
sum rankinground_rank if stage == 1, detail
gen rankinground_high = .
replace rankinground_high = 0 if rankinground_rank > 32 & age != .
replace rankinground_high = 1 if rankinground_rank <=32

gen heart_12mean_rankinground_high = heart_12mean * rankinground_high

areg score heart_12mean  heart_12mean_rankinground_high set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store rankinground1

areg score heart_12mean set_number arrows countdown shoot_order if rankinground_high == 0, absorb(match_name_id) cluster(matchnumber)
est store rankinground2

areg score heart_12mean set_number arrows countdown shoot_order if rankinground_high == 1, absorb(match_name_id) cluster(matchnumber)
est store rankinground3

outreg2 [rankinground*] using rankinground_table, dec(3) excel replace


* Table S8 Stage of match
sum stage, detail
gen stage_early = .
replace stage_early = 0 if stage > 1 & stage != .
replace stage_early = 1 if stage <= 1

gen heart_12mean_stage_early = heart_12mean * stage_early
areg score heart_12mean  heart_12mean_stage_early set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store stage_early1

areg score heart_12mean set_number arrows countdown shoot_order if stage_early == 0, absorb(match_name_id) cluster(matchnumber)
est store stage_early2

areg score heart_12mean set_number arrows countdown shoot_order if stage_early == 1, absorb(match_name_id) cluster(matchnumber)
est store stage_early3

outreg2 [stage_early*] using stage_early_table, dec(3) excel replace


* Table S9 Set
sum set_number, detail
tab set_number
gen set_early = .
replace set_early = 0 if set_number > 2 & set_number != .
replace set_early = 1 if set_number <= 2

gen heart_12mean_set_early = heart_12mean * set_early
areg score heart_12mean  heart_12mean_set_early set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store set_early1

areg score heart_12mean arrows countdown shoot_order if set_early == 0, absorb(match_name_id) cluster(matchnumber)
est store set_early2


areg score heart_12mean arrows countdown shoot_order if set_early == 1, absorb(match_name_id) cluster(matchnumber)
est store set_early3

outreg2 [set_early*] using set_early_table, dec(3) excel replace


* Table 2

areg score heart_12mean  heart_12mean_female set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store table_two1

areg score heart_12mean  heart_12mean_old_age set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store table_two2

areg score heart_12mean  heart_12mean_worldranking_high set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store table_two3

areg score heart_12mean  heart_12mean_rankinground_high set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store table_two4

areg score heart_12mean  heart_12mean_stage_early set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store table_two5

areg score heart_12mean  heart_12mean_set_early set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store table_two6

outreg2 [table_two*] using table_two_table, dec(3) excel replace


* Table S10 Inverted-U

gen heart_12mean_square = heart_12mean ^ 2

reg score heart_12mean heart_12mean_square, cluster(matchnumber)
est store square_level_results1

reg score heart_12mean heart_12mean_square age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store square_level_results2


areg score heart_12mean heart_12mean_square, absorb(match_name_id) cluster(matchnumber)
est store square_level_results3

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store square_level_results4

outreg2 [square_level_results*] using square_heartrate_score_table, dec(5) excel replace



* Table S11 inverted U subsample analysis

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if female == 0, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero1

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if female == 1, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero2

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if old_age == 0, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero3

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if old_age == 1, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero4

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if worldranking_high == 0, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero5

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if worldranking_high == 1, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero6

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if rankinground_high == 0, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero7

areg score heart_12mean heart_12mean_square set_number arrows countdown shoot_order if rankinground_high == 1, absorb(match_name_id) cluster(matchnumber)
est store ucurve_hetero8

outreg2 [ucurve_hetero*] using ucurve_heterotable, dec(4) excel replace


* Table S12: arrow speed

pwcorr score speed, sig obs

reg speed heart_12mean  age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store speed1

areg speed heart_12mean  set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store speed2

outreg2 [speed*] using speed_table, dec(3) excel replace


* Table S13: countdown
pwcorr countdown heart_12mean, sig obs

reg countdown heart_12mean age female worldranking rankinground_rank stage set_number arrows shoot_order, cluster(matchnumber)
est store newcountdown1

areg countdown heart_12mean set_number arrows shoot_order, absorb(match_name_id) cluster(matchnumber)
est store newcountdown2

outreg2 [newcountdown*] using countdown_table, dec(3) excel replace


* Table S14: determintants of HR

reg heart_12mean female age worldranking rankinground_rank stage set_number arrows countdown shoot_order,cluster(matchnumber)
est store determinant1

areg heart_12mean set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store determinant2

outreg2 [determinant*] using determinants_table, dec(3) excel replace


* Table S15 heart rate and shooting timing

gen obs_heart_before = 0
replace obs_heart_before = 1 if heart_before1 !=. & heart_before2 == .
replace obs_heart_before = 2 if heart_before2 !=. & heart_before3 ==. & heart_before1 != .
replace obs_heart_before = 3 if heart_before3 !=. & heart_before4 ==.
replace obs_heart_before = 4 if heart_before4 !=. & heart_before5 ==.
replace obs_heart_before = 5 if heart_before5 !=. & heart_before6 ==.
replace obs_heart_before = 6 if heart_before6 !=. & heart_before7 ==.
replace obs_heart_before = 7 if heart_before7 !=. & heart_before8 ==.
replace obs_heart_before = 8 if heart_before8 !=. & heart_before9 ==.
replace obs_heart_before = 9 if heart_before9 !=. & heart_before10 ==.
replace obs_heart_before = 10 if heart_before10 !=. & heart_before11 ==.
replace obs_heart_before = 11 if heart_before11 !=. & heart_before12 ==.
replace obs_heart_before = 12 if heart_before12 !=.
tab obs_heart_before

egen strict_heart_12mean = rowmean(strict_heart_before1 strict_heart_before2 strict_heart_before3 strict_heart_before4 strict_heart_before5 strict_heart_before6 strict_heart_before7 strict_heart_before8 strict_heart_before9 strict_heart_before10 strict_heart_before11 strict_heart_before12)

gen heart_lower= .
replace heart_lower = 1 if heart_b <= strict_heart_12mean & heart_b !=. & strict_heart_12mean !=. & obs_heart_before != 1 & obs_heart_before != 0
replace heart_lower = 0 if heart_b > strict_heart_12mean & heart_b !=. & strict_heart_12mean !=. & obs_heart_before != 1 & obs_heart_before != 0
replace heart_lower = . if obs_heart_before == 1 & obs_heart_before == 0


reg score heart_12mean heart_lower, cluster(matchnumber)
est store heart_lower_results1

reg score heart_12mean heart_lower age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store heart_lower_results2

areg score heart_12mean heart_lower, absorb(match_name_id) cluster(matchnumber)
est store heart_lower_results3

areg score heart_12mean heart_lower set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store heart_lower_results4

outreg2 [heart_lower_results*] using heart_lower_results_table, dec(3) excel replace


* Table S16 lagged score
sort matchnumber name_id set_number arrow
by matchnumber name_id: gen score_lag = score[_n-1]

reg score heart_12mean age female worldranking rankinground_rank stage set_number arrows countdown shoot_order, cluster(matchnumber)
est store scorelag_results1

reg score heart_12mean score_lag age female worldranking rankinground_rank stage set_number arrows countdown shoot_order , cluster(matchnumber)
est store scorelag_results2

areg score heart_12mean set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store scorelag_results3

areg score heart_12mean score_lag set_number arrows countdown shoot_order, absorb(match_name_id) cluster(matchnumber)
est store scorelag_results4

outreg2 [scorelag_results*] using scorelag_score_table, dec(3) excel replace


* Table S17 Arellano-Bond model

tab set_number arrows
sort name_id matchnumber set_number arrows
by name_id matchnumber : gen arrow_id = _n
tab arrow_id

xtset match_name_id arrow_id

xtsum score heart_12mean set_number


xtabond score heart_12mean, vce(robust)
est store dynamicpanel1

xtabond score heart_12mean set_number arrows countdown shoot_order, vce(robust)
est store dynamicpanel2

xtabond score heart_12mean, lags(2) vce(robust)
est store dynamicpanel3

xtabond score heart_12mean set_number arrows countdown shoot_order, lags(2) vce(robust)
est store dynamicpanel4

outreg2 [dynamicpanel*] using dynamicpanel_table, dec(3) excel replace



* Table S18 leading and lagging

sort matchnumber name_id set_number arrows
gen total_points_before = setpoints - points if set_number !=1

gen rival_points = 2
replace rival_points = 0 if points == 2
replace rival_points = 1 if points == 1

sort matchnumber name_id set_number arrows
gen sum_previous_rival_points = .
replace sum_previous_rival_points = rival_points[_n-3] if set_number == 2
replace sum_previous_rival_points = rival_points[_n-3] + rival_points[_n-6] if set_number == 3 
replace sum_previous_rival_points = rival_points[_n-3] + rival_points[_n-6] + rival_points[_n-9] if set_number == 4
replace sum_previous_rival_points = rival_points[_n-3] + rival_points[_n-6] + rival_points[_n-9] + rival_points[_n-12] if set_number == 5
replace sum_previous_rival_points = rival_points[_n-3] + rival_points[_n-6] + rival_points[_n-9] + rival_points[_n-12] + rival_points[_n-15] if set_number == 6

gen total_previous_points_difference= total_points_before - sum_previous_rival_points


* generate the dummy to study leading and lagging
gen leading_dummy = .
replace leading_dummy = 0 if total_previous_points_difference <= 0
replace leading_dummy = 1 if total_previous_points_difference > 0 & total_previous_points_difference != .

gen lagging_dummy = .
replace lagging_dummy = 0 if total_previous_points_difference >= 0 & total_previous_points_difference != .
replace lagging_dummy = 1 if total_previous_points_difference < 0 


gen even_dummy = .
replace even_dummy = 0 if total_previous_points_difference != 0
replace even_dummy = 1 if total_previous_points_difference == 0

reg score total_previous_points_difference , cluster(matchnumber)
est store previous_point1

reg score total_previous_points_difference age female worldranking rankinground_rank stage set_number arrows countdown , cluster(matchnumber)
est store previous_point2

reg score leading_dummy lagging_dummy , cluster(matchnumber)
est store previous_point3

reg score leading_dummy lagging_dummy  age female worldranking rankinground_rank stage set_number arrows countdown, cluster(matchnumber)
est store previous_point4

outreg2 [previous_point*] using previous_point_table, dec(3) excel replace



* Table S19 Leading and Lagging in different stages

reg score total_previous_points_difference age female worldranking rankinground_rank set_number arrows countdown if stage_early == 1, cluster(matchnumber)
est store leading1

reg score total_previous_points_difference age female worldranking rankinground_rank set_number arrows countdown if stage_early == 0, cluster(matchnumber)
est store leading2


reg score leading_dummy lagging_dummy  age female worldranking rankinground_rank set_number arrows countdown if stage_early == 1, cluster(matchnumber)
est store leading3


reg score leading_dummy lagging_dummy  age female worldranking rankinground_rank set_number arrows countdown if stage_early == 0, cluster(matchnumber)
est store leading4

outreg2 [leading*] using leading_table, dec(3) excel replace




* Figures

* Figure 1

tab score
gen score_group = .
replace score_group = 1 if score <=8
replace score_group = 2 if score ==9
replace score_group = 3 if score ==10

nptrend heart_12mean, by(score_group)

* need to install cibar command first
* ssc install cibar
cibar heart_12mean, over(score_group) graphopts(ytitle("Heart rate") xtitle("Score") scheme(s1mono))

ttest heart_12mean if score_group <=2,by(score_group)
ttest heart_12mean if score_group >=2 & score_group != ., by(score_group)
ttest heart_12mean if score_group !=2,by(score_group)

* a more complete Figure 1 is produced by Figure 1.R with R language
* Figure 1.R uses the following exported excel data figure1.xls
export excel heart_12mean score score_group using "figure1", firstrow(variables) replace

* Figure S1
hist score, xtitle("Score") scheme(s1mono)
hist heart_12mean, xtitle("Heart rate") scheme(s1mono)


* Figure S2
gen hr_quantile = .
replace hr_quantile = 1 if heart_12mean >=61 & heart_12mean < 61+12.9
replace hr_quantile = 2 if heart_12mean >=61+12.9 & heart_12mean < 61+12.9*2
replace hr_quantile = 3 if heart_12mean >=61+12.9*2 & heart_12mean < 61+12.9*3
replace hr_quantile = 4 if heart_12mean >=61+12.9*3 & heart_12mean < 61+12.9*4
replace hr_quantile = 5 if heart_12mean >=61+12.9*4 & heart_12mean < 61+12.9*5
replace hr_quantile = 6 if heart_12mean >=61+12.9*5 & heart_12mean < 61+12.9*6
replace hr_quantile = 7 if heart_12mean >=61+12.9*6 & heart_12mean < 61+12.9*7
replace hr_quantile = 8 if heart_12mean >=61+12.9*7 & heart_12mean < 61+12.9*8
replace hr_quantile = 9 if heart_12mean >=61+12.9*8 & heart_12mean < 61+12.9*9
replace hr_quantile = 10 if heart_12mean >=61+12.9*9 & heart_12mean < 61+12.9*10
tab hr_quantile
cibar score,over1(hr_quantile) graphopts(ytitle("Score") xtitle("Heart Rate") scheme(s1mono))


* Figure S3
cibar score, over(total_previous_points_difference) graphopts(ytitle("Score") xtitle("The difference of athlete and rival's previous set points") scheme(s1mono))


* Table S1: Summary Statistics
keep name_id female matchnumber score speed heart_12mean heart_b total_points_before total_previous_points_difference setpoints stage set_number arrows countdown shoot_order age female worldranking rankinground_rank points

outreg2 using summary.doc, replace sum(log) title(Decriptive statistics)


duplicates drop name_id,force
keep female age worldranking rankinground_rank
outreg2 using summary2.doc, replace sum(log) title(Decriptive statistics 2)

