* this file first implements a stripped-down version of de Chaisemartin & d'Haultfoeuille's fuzzy DID in Mata, then runs the simulations in the appendix

mata
mata clear
mata set matastrict on
mata set mataoptimize on
mata set matalnum off

// based on late.m in Chaisemartin and d'Haultfoeuille (2018). Code should be accessible to subscribers at https://doi.org/10.1093/restud/rdx049

// Compute F2^-1 (F1(x)), where Fj is the empirical cdf of yj, x is a vector
// and val_inf is the minimal value of the support of y2
real colvector Q(real colvector y1, real colvector y2, real colvector x, real scalar val_inf) {
  real colvector indic_nul, fdr, n2; real scalar m, j
  if (rows(y1)==0) return(.)
  m = length(x)
  fdr = J(m,1,0)
  for(j=m;j;j--) 
    fdr[j] = mean(y1 :<= x[j])
  n2 = ceil(length(y2) * fdr)
  indic_nul = !fdr
  return(val_inf * indic_nul + (n2:>0) :* sort(y2,1)[n2 + indic_nul])
}
  
// Function computing the Wald-DID, Wald-TC and Wald-CIC estimators. 
// Input : 
// - data = database including D, Y, T, G
// D can be finitely supported and ordered, not only binary.
real rowvector late(real matrix data) {
  real rowvector Wald_x, W_neg1
  real colvector D, Y, T, G, dY0, Yd, Gd, Td, Y00_d, Y01_d, sum_Q, sum_Q_minus, wt
  real matrix info
  real scalar ΔED0, ΔED1, ΔEY0, ΔEY1, ΔED_neg1, ΔEY_neg1, minY, EY11, EY_neg11, i, ΔΔED, ΔΔED_neg1
  
  W_neg1 = Wald_x = J(1,3,.)

  _sort(data, 1)
  D = data[,1]
  Y = data[,2]
  T = data[,3]
  G = data[,4]

  ΔED0 = mean(D, G:==0 :& T) - mean(D, G:==0 :& !T)
  ΔED1 = mean(D, G:==1 :& T) - mean(D, G:==1 :& !T)

  // Wald-DID
  ΔEY0 =         mean(Y, G:==0 :& T)  - mean(Y, G:==0 :& !T)
  ΔEY1 = (EY11 = mean(Y, G:==1 :& T)) - mean(Y, G:==1 :& !T)
  
  ΔED_neg1 =             mean(D, G:==-1 :& T)  - mean(D, G:==-1 :& !T)
  ΔEY_neg1 = (EY_neg11 = mean(Y, G:==-1 :& T)) - mean(Y, G:==-1 :& !T)

  ΔΔED      = sum(G:== 1) * (ΔED1 - ΔED0)
  ΔΔED_neg1 = sum(G:==-1) * (ΔED0 - ΔED_neg1)
  Wald_x[1] = sum(G:== 1) * (ΔEY1 - ΔEY0) + sum(G:==-1) * (ΔEY0 - ΔEY_neg1)

  // Wald-CIC and Wald-TC
  minY = min(Y)

  info = panelsetup(D,1)

  sum_Q_minus = sum_Q = dY0 = J(rows(info),1,0)
  for (i=rows(info);i;i--) {
    Yd = panelsubmatrix(Y, i, info)
    Gd = panelsubmatrix(G, i, info)
    Td = panelsubmatrix(T, i, info)
    Y00_d = select(Yd, Gd:==0 :& !Td)
    Y01_d = select(Yd, Gd:==0 :&  Td)

    dY0[i] = editmissing(mean(Y01_d) - mean(Y00_d), 0)
    sum_Q      [i] = mean(Q(Y00_d, Y01_d, select(Yd, Gd:== 1 :& !Td), minY))
    sum_Q_minus[i] = mean(Q(Y00_d, Y01_d, select(Yd, Gd:==-1 :& !Td), minY))
  }

  wt = panelsum(G:== 1 :& !T, info)
  Wald_x[2] = (ΔEY1 - mean(dY0  , wt)) / ΔED1  // (before-after change in G=1 group - before-after change in G=0 group, reweighted to G=1-before-treatment distribution) / before-after change in treatment level in G=1 group
  Wald_x[3] = (EY11 - mean(sum_Q, wt)) / ΔED1

  wt = panelsum(G:==-1 :& !T, info)
  W_neg1[2] = ΔEY_neg1 - mean(dY0        , wt)
  W_neg1[3] = EY_neg11 - mean(sum_Q_minus, wt)

  Wald_x[2..3] = ΔΔED * Wald_x[2..3] + ΔΔED_neg1 * W_neg1[2..3] / ΔED_neg1

  return(Wald_x / (ΔΔED + ΔΔED_neg1))
}
end

* based on construction_groups.m in Chaisemartin and d'Haultfoeuille (2018).
// Generate a column with a variable taking values -1, 0 and 1 according to whether
// a district belongs to G_d, G_s or G_i.
cap program drop construction_groups
program define construction_groups, rclass sortpreserve
  syntax varlist [if] [in], THRESHold(real) GENerate(name)  // varlist must be D G T
  tokenize `varlist'
  local D `1'
  local G `2'
  local T `3'

  tempname min_T_gr max_T_gr DT1 pvalue evol
  marksample touse
  quietly {
    gen `generate' = .

    bysort `G': egen long `min_T_gr' = min(`T') if `touse'
    by     `G': egen long `max_T_gr' = max(`T') if `touse'
    replace `touse' = 0 if `min_T_gr'!=0 | `max_T_gr'!=1  // Remove districts for which no data at T=0 or T=1

    levelsof `G' if `touse'
    foreach g in `r(levels)' {
      tab2 `D' `T' if `touse' & `G'==`g', chi2
      scalar `pvalue' = r(p)
      sum `D' if `touse' & `T'==1 & `G'==`g', meanonly
      scalar `DT1' = r(mean)
      sum `D' if `touse' & `T'==0 & `G'==`g', meanonly
      scalar `evol' = `DT1' - r(mean)
      replace `generate' = (`evol'>0 & `pvalue'<`threshold') - (`evol'<0 & `pvalue'<`threshold') if `G'==`g' & `touse'
    }
  }
end

use "D:\OneDrive\Documents\Work\Clients & prospects\GiveWell\Education\Duflo 2001\inpresdata", clear
gen byte age74 = 74 - p504thn
gen byte young = age74 <= 6
gen byte old = age74 <= 17 & age74 >= 12
gen byte sample = (young | old) & lhwage<.
replace yeduc = 18 if yeduc==19  // ? to allow CIC; IPUMS YRSCHOOL never goes as high as 19

cap program drop est
program define est, rclass
  cap drop G
  construction_groups yeduc birthpl young if sample, gen(G) thresh(.5)
  mata st_matrix("b", late(st_data(.,"yeduc lhwage young G", "sample")))
  return scalar W_Wald = b[1,1]
  return scalar W_TC   = b[1,2]
  return scalar W_CIC  = b[1,3]
end

est
ret list

preserve
collapse G, by(birthpl)
tab G
restore

bootstrap W_Wald=r(W_Wald) W_TC=r(W_TC) W_CIC=r(W_CIC), cluster(birthpl) idcluster(birthpl2) group(birthpl) force seed(0124810) reps(100): est

gen G1 = G + 1
ivregress 2sls lhwage young i.G1 (yeduc = young#G1) if sample, cluster(birthpl)  // good approximation of DID

* basic data driving result
table G young if sample, stat(mean yeduc) stat(mean lhwage) nototals nformat(%4.2f)

//                        |      young   
//                        |      0      1
// -----------------------+--------------
// G                      |              
//   -1                   |              
//     years of education |  10.23   9.45
//     log(hourly wage)   |   7.11   6.67
//   0                    |              
//     years of education |   9.62   9.47
//     log(hourly wage)   |   7.03   6.68
//   1                    |              
//     years of education |   8.73   9.73
//     log(hourly wage)   |   6.97   6.75
// --------------------------------------


***
*** simulations
***

global Ng 100  // number of groups
global N  100  // units per group
scalar σG = 1  // s.d. of group-level time trend 
scalar beta = 0  // impact parameter
global thresh .5  // threshold p value for defining supergroups

cap program drop sim
program define sim, rclass
  clear
  set obs $Ng
  gen int G = _n  // group index
  gen dD = σG * rnormal()  // group-level treatment trend
  gen Z = rnormal()  // group-level exogenous component of treatment
  expand $N
  sort G
  xtset G
  gen byte T = runiform()<.5  // random 0/1 time period
  replace Z = 0 if T==0  // no treatment in pre-treatment period
  drawnorm Y D, cov(C) cstor(lower)  // components have variance 1, potentially correlated
  replace D = D + dD if T==1  // add group trend when T = 1
  replace D = autocode(D + Z, 6, -3, 3)  // quantize treatment
  replace Y = Y + beta * D 

  xtivreg2 Y (D = Z) T, fe  // DID with perfect treatment instrument
  return scalar W_IV = _b[D]

  // construct groups for G = -1, 0, 1
  qui areg D 1.T#ibn.G, a(G)
  mata b = st_matrix("e(b)")[|. \ $Ng|]'
  mata st_view(G=., ., "G")
  mata G[,] = (sign(b) :* (Ftail(1, $Ng*$N-2*$Ng, b :* b :/ diagonal(st_matrix("e(V)"))[|. \ $Ng|]) :< $thresh)) # J($N,1,1)  // Overwrite G with -1, 0 and 1 based on whether average D rises, falls, or stays about the same

  mata st_matrix("b", late(st_data(.,"D Y T G")))
  return scalar W_DID = b[1,1]
  return scalar W_TC  = b[1,2]
  return scalar W_CIC = b[1,3]
  
  tab G, matcell(M)  // number of groups in each supergroup
  return scalar Nm1 = M[1,1]/100
  return scalar N0  = M[2,1]/100
  return scalar Np1 = M[3,1]/100
end

mat C = 1 \ 0 \ 1  // lower triangle of error covariance matrix
simulate Nm1=r(Nm1) N0=r(N0) Np1=r(Np1) W_DID=r(W_DID) W_CIC=r(W_CIC) W_IV=r(W_IV), reps(100) seed(12983710): sim
estpost tabstat *, stat(mean sd) columns(statistics)
est store rho00

mat C = 1 \ .95 \ 1
simulate Nm1=r(Nm1) N0=r(N0) Np1=r(Np1) W_DID=r(W_DID) W_CIC=r(W_CIC) W_IV=r(W_IV), reps(100) seed(12983710): sim
estpost tabstat *, stat(mean sd) columns(statistics)
est store rho95

esttab rho?? using CH.rtf, replace cells("mean(fmt(3)) sd(fmt(3))") nostar unstack nonumber msign("–") stat(N, lab("Simulations") fmt(0)) fonttbl(\f0\fnil Cambria;)
