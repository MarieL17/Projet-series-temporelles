cd "C:\Users\Marie\Desktop\Projet Séries temp"

import excel bases_initiales\FUSION.xlsx, sheet("Revenu") clear

* création et mise en forme de la table de travail : 
rename A annee 
label variable annee "Année"
rename B trim
label variable trim "Trimestre"
rename C revenu
label variable revenu "Revenu disponible"
rename D conso
label variable conso "Consommation finale"
drop if _n==1
destring revenu, replace
destring conso, replace
save fusion.dta, replace

********************************************************************************
use fusion.dta, clear


* PRELIMINAIRES
* on déclare nos variables comme séries temporelles
generate date=annee+"q"+trim
generate time=quarterly(date,"YQ")
format time %tq
label variable time "temps"
tsset time, quarterly

* on génère les variables différenciée pour la suite
gen diff_conso= conso[_n]-conso[_n-1]
label variable diff_conso "Consommation finale différenciée"
gen diff_revenu = revenu[_n]-revenu[_n-1]
label variable diff_revenu "Revenu final différencié"
gen diff2_conso= diff_conso[_n]-diff_conso[_n-1]
label variable diff2_conso "Consommation différenciée d'ordre 2"
gen diff2_revenu= diff_revenu[_n]-diff_revenu[_n-1]
label variable diff2_revenu "Revenu différencié d'ordre 2"
gen temps=_n

**************
* QUESTION 2 *
**************
 * graphique
* twoway (tsline revenu) (tsline conso), xline(80)

* taux de croissance
gen txc_conso= (conso[_n]-conso[_n-1])/conso[_n]
gen txc_revenu = (revenu[_n] -revenu[_n-1])/revenu[_n]

* tsline txc_revenu, xline(100)
* tsline txc_conso, xline(100)
* choix de la borne inférieure : 1985

*******************
  drop if time<100
*******************
* twoway (tsline revenu) (tsline conso)

* transformation logarithmique 
gen log_revenu = ln(revenu)
label variable log_revenu "Logarithme du revenu"
gen log_conso = ln(conso)
label variable log_conso "Logarithme de la consommation"
* twoway (tsline log_revenu)(tsline log_conso)



**************
* QUESTION 4 * 
**************

* DICKEY-FULLER
* SERIE CONSO
* On cherche le lag adapté à la série conso
* lags(1) 
regress diff_conso temps l1.conso l1.(diff_conso)
estat ic 

* lags(2) 
regress diff_conso temps l1.conso l1.(diff_conso) l2.(diff_conso)
estat ic 

* lags(3) 
regress diff_conso temps l1.conso l1.(diff_conso) l2.(diff_conso) ///
l3.(diff_conso)
estat ic 

* lags(4) 
regress diff_conso temps l1.conso l1.(diff_conso) l2.(diff_conso) ///
l3.(diff_conso) l4.(diff_conso)
estat ic

* on retient un lag de 2, car au-delà les coeff ne sont plus significatifs
dfuller conso, lags(2) trend
* on ne peut pas rejeter H0 -> série conso différenciée

* SERIE CONSO DIFFERENCIEE
* on cherche le lag adapté à la série conso différenciée
* lags(1) 
regress diff2_conso temps l1.diff_conso l1.(diff2_conso)
estat ic 

*lags(2) 
regress diff2_conso temps l1.diff_conso l1.(diff2_conso) l2.(diff2_conso)
estat ic 

*lags(3) 
regress diff2_conso temps l1.diff_conso l1.(diff2_conso) l2.(diff2_conso) ///
l3.(diff2_conso)
estat ic

* On retient un lag de 1, car au-delà les coefficients ne sont plus significatifs 
dfuller diff_conso, lags(1) trend
* on peut rejeter H0 à 1%

* TEST PHILIPS-PERRON
pperron conso, lags(2) trend
pperron diff_conso, lags(1) trend
* Même conclusion

* TEST DFGLS
dfgls conso, maxlag(5)
dfgls diff_conso, maxlag(5)
* Mê conclusion 

* TEST ERS
dfgls conso, maxlag(5) ers 
dfgls diff_conso, maxlag(5) ers
* Mê conclusion

* TEST KPSS
kpss conso, maxlag(5)
kpss diff_conso, maxlag(5)


**************
* QUESTION 5 *
**************

* on travaille donc à présent sur diff_conso

* estimation ordre de grandeur partie AR
pac diff_conso, lag(20)
* on retient 2

* estimation ordre de grandeur partie MA
ac diff_conso, lag(20)
* on retient 2

**************
* QUESTION 6 *
**************

* on construit une matrice qui donne l'AIC et BIC des différents modèles à tester

matrix results_aic=I(3)
matrix results_bic=I(3)

forvalues p=0(1)2 {
	forvalues q=0(1)2 {
		quietly arima conso, arima(`p',1,`q')
		quietly estat ic
		matrix results_information=r(S)
		matrix results_aic[1+`p',1+`q'] = results_information[1,6] 
		matrix results_bic[1+`p',1+`q'] = results_information[1,5]
		* r(S) produit une matrice 1x6 de résultats
		* [1,6] : on récupère 1ère ligne 6ème colonne du résultat : BIC
		* [1,5] : on récupère 1ère ligne 6ème colonne du résultat : AIC
	}
}
matrix rownames results_aic = "p=0" "p=1" "p=2" 
matrix rownames results_bic = "p=0" "p=1" "p=2" 
matrix colnames results_aic = "q=0" "q=1" "q=2" 
matrix colnames results_bic = "q=0" "q=1" "q=2" 

matrix list results_bic
matrix list results_aic

* outtable using graph/BIC, mat(results_aic)

* On observe que les combinaisons qui minimisent les critères d'information
* BIC : ARMA(0,2) ARMA(2,0) et dans une moindre mesure ARMA(1,2) et ARMA(2,1) 
* AIC : ARMA(0,2) ARMA(2,0)

* on regargde la significativité des coefficients :

arima conso, arima(1,1,2) 
estat ic
* deux coefficients non significatifs

arima conso, arima(2,1,1) 
estat ic
* deux coefficients non significatifs

arima conso, arima(0,1,2)
estat ic
* tout significatif à 5%

arima conso, arima(2,1,0) /* ++ */
estat ic
* tout significatif à 10%

**************
* QUESTION 7 *
**************

* Auto-corrélation des résidus :
arima conso, arima(0,1,2)
predict double residu02, residuals
hist residu02, normal frequency bin(20)

arima conso, arima(2,1,0)
predict double residu20, residuals
hist residu20, normal frequency bin(20)
* ARIMA(0,1,2) a l'air plus crédible

* Test du porte-manteau
wntestq residu02, lags(2)
wntestq residu20, lags(2)
* on ne peut pas rejeter l'hypothèse que les résidus sont des bruits blancs

* test de normalité des résidus
qnorm residu02
qnorm residu20
sktest residu02
sktest residu20


**************
* QUESTION 8 *
**************

* on cherche l'ordre d'intégration pour le revenu
* On cherche le lag adapté à la série revenu
* , lags(1) 
regress diff_revenu temps l1.revenu l1.(diff_revenu)
estat ic 

* lags(2) 
regress diff_revenu temps l1.revenu l1.(diff_revenu) l2.(diff_revenu)
estat ic 

* lags(3) 
regress diff_revenu temps l1.revenu l1.(diff_revenu) l2.(diff_revenu) ///
l3.(diff_revenu)
estat ic 

* lags(4) 
regress diff_revenu temps l1.revenu l1.(diff_revenu) l2.(diff_revenu) ///
l3.(diff_revenu) l4.(diff_revenu)
estat ic

* lags(5) 
regress diff_revenu temps l1.revenu l1.(diff_revenu) l2.(diff_revenu) ///
l3.(diff_revenu) l4.(diff_revenu) l5.(diff_revenu)
estat ic


* on retient un lag de 3, car au-delà les coeff ne sont plus significatifs
dfuller revenu, lags(3) trend
* on ne peut pas rejeter H0 -> série revenu différenciée


* SERIE REVENU DIFFERENCIEE
* on cherche le lag adapté à la série revenu différenciée
* lags(1) 
regress diff2_revenu temps l1.diff_revenu l1.(diff2_revenu)
estat ic 

* lags(2) 
regress diff2_revenu temps l1.diff_revenu l1.(diff2_revenu) l2.(diff2_revenu)
estat ic 

* lags(3) 
regress diff2_revenu temps l1.diff_revenu l1.(diff2_revenu) l2.(diff2_revenu) ///
l3.(diff2_revenu)
estat ic

*lags(4) 
regress diff2_revenu temps l1.diff_revenu l1.(diff2_revenu) l2.(diff2_revenu) ///
l3.(diff2_revenu) l4.(diff2_revenu)
estat ic

* On retient un lag de 2, car au-delà les coefficients ne sont plus significatifs 
dfuller diff_revenu, lags(2) trend
* on peut rejeter H0 à 1%

* Le revenu est donc comme la consommation à différencier à l'ordre 1


* On regarde si la combinaison linaire du revenu et de la conso permet d'obtenir
* une série différenciée d'ordre 0 

reg conso revenu
predict double residus, residuals
gen diff_residus = residus[_n] - residus[_n-1]
* on cherche l'ordre d'intégration pour le residus
* On cherche le lag adapté :
*lags(1)
regress diff_residus temps l1.residus l1.(diff_residus)
estat ic 

*lags(2)
regress diff_residus temps l1.residus l1.(diff_residus) l2.(diff_residus)
estat ic 

* on retient un lag de 1, car au-delà les coeff ne sont plus significatifs
dfuller residus, lags(1) 

* on calcule les valeurs critiques de MacKinnon :
display (-3.9) + (-10.5)/115 + (-30)/13225    /*pour 1%*/
display (-3.34) + (-5.96)/115 + (-8.98)/13225 /*pour 5%*/
display (-3.04) + (-4.06)/115 + (-5.73)/13225 /*pour 10%*/
* on rejette l'hypothèse de non-stationnarité à 10%
* donc les résidus sont stationnaires, et il y a bien cointégration 
pac residus
* AR 1
arima residus, arima(0,0,0) 
estat ic
arima residus, arima(1,0,0) 
estat ic
arima residus, arima(2,0,0) 
estat ic
arima residus, arima(3,0,0) 
estat ic
arima residus, arima(4,0,0) 
estat ic
* AR1 confirmé par BIC

***************
* QUESTION 10 *
***************
gen residus_lag1 = residus[_n-1]
reg diff_conso diff_revenu residus_lag1
