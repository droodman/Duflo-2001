* dependencies: ivreg2, xtivreg2, ranktest, boottest, reghdfejl, julia, estout, coefplot, blindschemes, palettes, colrspace, moremata, xlincom, cic, and qrprocess
* all are from SSC except the last two are from https://sites.google.com/site/blaisemelly/home/computer-programs/cic_stata

cap cd "D:/OneDrive - Open Philanthropy Project"
cap cd "/Users/davidroodman/Library/CloudStorage/OneDrive-OpenPhilanthropyProject"
cap cd "W:/"
cd "Education/Duflo 2001"

global source95 NBER  // if "NBER", use primary data owned by NBER; otherwise use processed file for birth years 1950-72, https://github.com/NathanLazarus/Duflo2001/blob/main/inpresdata.dta

cap set scheme plotplain
set odbcdriver ansi

if c(os)=="Windows" {
	global procs 6  // set to number of performance cores; will slightly affect bootstrapped Hausman tests
	global font LM Roman 9
}
else {
	global procs 12  // set to number of performance cores; will slightly affect bootstrapped Hausman tests
	global font Latin Modern Roman
	global odbc opts u(sa) p(VeryStr0ngP@ssw0rd)
}

cap set processors $procs
graph set window fontface "$font"  // https://www.1001fonts.com/latin-modern-roman-font.html?
est drop _all

cap log close
cap log using Public/Output/duflo2001, text replace

global age74kink 12
global retireage 56


***
*** Data prep
***
{
  cap noi odbc load, $odbcopts clear dsn("Duflo 2001") table("Regency-level vars")
  if !_rc {
    reg totinnew ch71new  // Duflo (2001, note 2)
    predict e, resid
    gen byte recpnew = e > 0 if e < .
    drop e

    foreach var in en71new ch71new dens71 ninnew {
      gen double l`var' = ln(`var')
    }
    gen byte java = inrange(birthpl,3000,3999)

    regress ninnew lch71new
    predict nin_lch71new
    
    gen nen71new = 1 - en71new
    gen nen71newish = 1 - en71newish
    
    gen byte poor = inlist(floor(birthpl/100), 33, 34, 35, 53, 72, 73, 74, 81, 82) & mod(birthpl,100) < 70  // Duflo (2001), table 6, note b

    label var birthpl "Birth province & regency code, 1995"
    label var nin "New schools per 5-14 population, Duflo"
    label var recp "High treatment dummy, Duflo"
    label var ch71 "Children 5–14, 1971, Duflo"
    label var en71 "Population enrollment rate, 1971, Duflo"
    label var wsppc "Water & sanitation spending per capita, Duflo"
    label var dens71 "Population density, 1971, Duflo"
    label var moldyed "Average schooling among old, Duflo"
    label var birthlat "Birth regency centroid latitude"
    label var birthlong "Birth regency centroid longitude"
    label var Schools73new "Number of new schools, 1973/74"
    label var Schools74new "Number of new schools, 1974/75"
    label var Schools75new "Number of new schools, 1975/76"
    label var Schools76new "Number of new schools, 1976/77"
    label var Schools77new "Number of new schools, 1977/78"
    label var Schools78new "Number of new schools, 1978/79"
    label var ch71new "Population 5-14, 1971"
    label var totinnew "New schools"
    label var recpnew "High treatment dummy"
    label var ninnew "New schools per 5-14 population"
    label var nch71new "Population 5+, 1971, same source as for attendance"
    label var atsc71new "Population 5+ attending school, 1971"
    label var en71newish "Population enrollment rate, 1971, ch71new denominator"
    label var en71new "Population 5+ enrollment rate, 1971, nch71new denominator"
    label var nen71newish "Population non-enrollment rate, 1971, ch71new denominator"
    label var nen71new "Population 5+ non-enrollment rate, 1971, nch71new denominator"
    label var poor "Poor according to Duflo (2001, table 6, note b)"
    label var nin_lch71new "Component of treatment explained by log number of children"
    saveold "Public/Regency-level vars/Regency-level vars", replace ver(11)
  }
  else use "Public/Regency-level vars/Regency-level vars", clear

  drop if inlist(birthpl,1472,1804,3275,5171,5271,7173,7271,8271,8104)  // drop split child regencies--duplicates in this context

  pwcorr ninnew nen71new, sig  // correlation between non-enrolment & treatment, reported in text
  pwcorr nen71new ninnew*, sig

  // Duflo (2001, note 2) on definition of recp--doesn't fully match
  gen totin = round(nin * ch71/1000)
  scatter totin ch71 if recp==0 || scatter totin ch71 if recp==1 || lfit totin ch71, scheme(plottig) legend(off)

  * SUPAS 2005
  cap noi odbc load, $odbcopts clear dsn("Duflo 2001") exec("SELECT * FROM dbo.[IPUMS-based dataset (SUPAS)] where year=2005 and female=0 and birthprov<>96")
  if _rc use SUPAS05, clear
  else {
    recode educatt (11=1) (12=2) (13=3) (14=4) (15=5) (16 17=6) ///  // primary school
                   (21=7) (22=8) (23 27=9) (31=10) (32=11) (33=12)         (37=12) ///  // junior & senior
                                           (41=10) (42=11) (43=12) (44=13) (47=12) ///  //          senior, vocational
                   (51=13) (52 57=14) ///  // Diploma I/II
                   (61=13) (62=14) (63 67=15) ///  // Academy/Diploma III
                   (71=13) (72=14) (73=15) (74=16) (75=17) (76=18) (77=17) ///  // University/Diploma IV
                                                           (86=18) (87=17) ///  // Postgraduate-S2/S3 -- not broken out in 1995
                   (99=0) (.=0), gen(yeduc)

    gen birthyr = year - age
    keep year urban yeduc birthyr wt indgen birthpl classwk
    compress
    save SUPAS05, replace
  }

  * SUSENAS
  cap noi odbc load, $odbcopts clear dsn("Duflo 2001") exec("select * from [SUSENAS dataset] where female=0")
  if _rc use SUSENAS1119, clear
  else {
    gen birthyr = year - age
    replace wage = . if !inlist(classwk,4,5) | wage < 20000  // SUSENAS 2012 alone appears to trim observations below 20000; seems to good to do, and need consistency
  
    * for consistency convert to SUSENAS 2019 highest grade attended (R615) typology--most detailed
    recode edlevatt (3=1) (1=2) (2=4) (6=5) (4=6) (5=7) (10=8) (7=9) (8=12) (9=13) (11=15) (12=16) (13=17) (14=20) if inrange(year,2011,2014)
    replace edlevatt = edlevatt + 1 if edlevatt>=19 & inrange(year,2017,2018)  // inserted new "profesi" category (19)
    recode edlevatt (1/4 = 0) (5/7 = 6) (8/14 = 9) (15/21 = 12), gen(yeduc)  // years of schooling *before* each schooling level
    mat completionyears = 6,6,6,6, 3,3,3, 3,3,3,3,3,3,3, 2,3,5,5,1,7,7  // max years in schooling levels: table yeduc gradeatt if gradeatt<8
    replace yeduc = yeduc + cond(gradeatt<8, gradeatt, completionyears[1, edlevatt]) if edlevatt
 
    keep year yeduc birthyr wt birthpl urban classwk wage hrswork
    compress
    save SUSENAS1119, replace
  }

  * 1995 data
  if "$source95"=="NBER" {
    use supp95_04 if p503==1 & p509prop!=96, clear  // male, not born abroad
    ren (p605 p606 p608 p504thn p509prop kp) (hrswork occ classwk birthyr birthprov urban)

    gen int year = 1995
    destring prop kab urban, replace
    replace urban = 2 - urban  // place of residence, not birth
    gen int birthpl = birthprov * 100 + p509kab
    gen wage = p609uang + p609brng
    
    recode p517 (1 = 0) (2 4 = 6) (3 5 = 9) (6 7 8 = 12) (99 . = 0), gen(yeduc)  // years of schooling *before* each schooling level
    mat completionyears = 6, 3, 3, 3, 3, 2, 3, 5  // max years in schooling levels; used when p518=8, meaning "completed"
    replace yeduc = yeduc + cond(p518<8, p518, completionyears[1, p517] - (p517==6 & p520==2)) if p518<.  // 1 year less for completing Diploma I/II in teaching
  }
  else {
    use inpresdata, clear  // https://github.com/NathanLazarus/Duflo2001/blob/main/inpresdata.dta
    replace birthpl = p509pro * 100 + p509kab
    ren (p608 p504thn) (classwk birthyr)
    gen int year = 1995
    gen hrswork = wage / exp(lhwage) / 4
  }
  
  replace birthyr = birthyr + 1900
  ren weight wt
  recode p607 (11 12 13 14 15 16 17 18 = 10) (21 22 23 24 25 26 = 20) ///  // recode to IPUMS INDGEN
              (31 32 33 34 35 36 37 38 39 = 30) (41=40) (42 43=40) (51 52=50) (61 62=60 ) (63 64 = 70) ///
              (71 72 73 74 75 = 80) (81 82 = 90) (83=111) (91=100) (92=40) (93=114) ///
              (94=114) (96=120) (98=999) (99=0), gen(indgen)

  recode birthpl (7271=7204)  // Duflo (2001) recoding

  keep year urban yeduc birthyr wt indgen birthpl classwk wage wage hrswork
  compress

  append using SUPAS05 SUSENAS1119
  drop if floor(birthpl/100) == 54  // East Timor--gained independence

  merge m:1 birthpl using "Public/Regency-level vars/Regency-level vars", nogen update

  gen int dum = cond(birthyr<1962, 1900+100, birthyr) 
  gen byte age74 = 1974 - birthyr
  gen byte age = year - birthyr
  gen byte old = inrange(age74,12,17)
  gen byte young = inrange(age74,2,6)
  gen byte reallyold = inrange(age74,18,24)
  gen byte primary = yeduc>=6  // completed primary school

  recode birthpl (1472=1403) (1804=1803) (3275=3219) (5171=5103) (5271=5201) (7173=7103) (7271=7203) (8271=8203) (8104=8103), gen(birthplnew) // group new child regencies with parents

  gen byte migration = inlist(floor(birthpl/100), 12,13,61,62,63,64,71,72,73,74,81,82)  // Kalimanton, Sumatra, Sulawesi, Maluku, and Irian Jaya https://www.downtoearth-indonesia.org/old-site/ctrans.htm

  xtset birthplnew

  // adjust for inflation, https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2013&locations=US%E2%89%A4%2FSEURLD-ID&start=1995
  mat cpi = 9.339845464,10.20645761,11.0276046,11.73508494,12.6526737,13.84443641,14.88602496,16.32578542,17.71870229,19.38786131,20.93370995,22.23707238,35.23487344,42.45021132,44.01603795,49.07793288,54.91826459,58.62926593,62.18457972,68.68485723,77.68852994,82.66569441,91.11963768,95.11652363,100,105.3560478,109.8647599,116.9098522,124.38615,132.3009914,136.9656666,142.1824123,146.7298984,151.1766751,154.0807306,156.4845902,163.0717524,sqrt(163.0717524/156.4845902)*163.0717524
  replace wage = wage / cpi[1,year-1994]
  gen lwage = ln(wage)
  gen lhwage = ln(wage / 4 / hrswork)
  gen byte part = lhwage<. | (year==2005 & classwk==4)  // labor force participation

  label var yeduc "Years of schooling"
  label var primary "Primary completion"
  label var part "Employment"
  label var lwage "Log monthly wages"
  label var lhwage "Log hourly wage"
  
  save analysisdata, replace
}  // end data prep


***
*** replicate most of original
***
{
preserve
keep if age74>=2 & age74<=24 & year==1995
xtset birthpl

* Table 3: DID
reg yeduc  young##recp                                if (old | young) & lhwage<. [aw=wt]
reg lhwage young##recp                                if (old | young)            [aw=wt]
ivregress 2sls lhwage young recp (yeduc = young#recp) if (old | young)            [aw=wt], small  // correct Wald DID estimator

reg yeduc  old##recp                              if (reallyold | old) & lhwage<. [aw=wt]
reg lhwage old##recp                              if  reallyold | old             [aw=wt]
ivregress 2sls lhwage old recp (yeduc = old#recp) if  reallyold | old             [aw=wt], small

* Table 4
xtreg yeduc  1.young#c.nin birthyr##c.ch71              if  old | young        , fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71)       if  old | young        , fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71 wsppc) if  old | young        , fe
xtreg yeduc  1.young#c.nin birthyr##c.ch71              if (old | young) & part, fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71)       if (old | young) & part, fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71 wsppc) if (old | young) & part, fe
xtreg lhwage 1.young#c.nin birthyr##c.ch71              if  old | young        , fe
xtreg lhwage 1.young#c.nin birthyr##c.(ch71 en71)       if  old | young        , fe
xtreg lhwage 1.young#c.nin birthyr##c.(ch71 en71 wsppc) if  old | young        , fe

xtreg yeduc  1.old#c.nin   birthyr##c.ch71              if  old | reallyold        , fe
xtreg yeduc  1.old#c.nin   birthyr##c.(ch71 en71)       if  old | reallyold        , fe
xtreg yeduc  1.old#c.nin   birthyr##c.(ch71 en71 wsppc) if  old | reallyold        , fe
xtreg yeduc  1.old#c.nin   birthyr##c.ch71              if (old | reallyold) & part, fe
xtreg yeduc  1.old#c.nin   birthyr##c.(ch71 en71)       if (old | reallyold) & part, fe
xtreg yeduc  1.old#c.nin   birthyr##c.(ch71 en71 wsppc) if (old | reallyold) & part, fe
xtreg lhwage 1.old#c.nin   birthyr##c.ch71              if  old | reallyold        , fe
xtreg lhwage 1.old#c.nin   birthyr##c.(ch71 en71)       if  old | reallyold        , fe
xtreg lhwage 1.old#c.nin   birthyr##c.(ch71 en71 wsppc) if  old | reallyold        , fe

* Table 5
xtreg yeduc  dum#c.nin birthyr##c.ch71                     , fe
xtreg yeduc  dum#c.nin birthyr##c.(ch71 en71)              , fe
xtreg yeduc  dum#c.nin birthyr##c.(ch71 en71 wsppc)        , fe
xtreg yeduc  dum#c.nin birthyr##c.ch71              if part, fe
xtreg yeduc  dum#c.nin birthyr##c.(ch71 en71)       if part, fe
xtreg yeduc  dum#c.nin birthyr##c.(ch71 en71 wsppc) if part, fe
xtreg lhwage dum#c.nin birthyr##c.ch71                     , fe
xtreg lhwage dum#c.nin birthyr##c.(ch71 en71)              , fe
xtreg lhwage dum#c.nin birthyr##c.(ch71 en71 wsppc)        , fe

* Table 6
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if  old | young                         , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if (old | young) & dens71<308           , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if (old | young) & dens71>308 & dens71<., fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if (old | young) &  poor                , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if (old | young) & !poor                , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if (old | young) & moldyed<=6.97        , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if (old | young) & moldyed> 6.97        , fe

* Table 7, panels A1-B1
xtreg   lhwage  yeduc                  birthyr##c.ch71                            , fe
xtivreg lhwage (yeduc =     dum#c.nin) birthyr##c.ch71                            , fe
xtivreg lhwage (yeduc = 1.young#c.nin) birthyr##c.ch71 if old | young             , fe
xtreg   lhwage  yeduc                  birthyr##c.(ch71 en71)                     , fe
xtivreg lhwage (yeduc =     dum#c.nin) birthyr##c.(ch71 en71)                     , fe
xtivreg lhwage (yeduc = 1.young#c.nin) birthyr##c.(ch71 en71) if old | young      , fe
xtreg   lhwage  yeduc                  birthyr##c.(ch71 en71 wsppc)               , fe
xtivreg lhwage (yeduc =     dum#c.nin) birthyr##c.(ch71 en71 wsppc)               , fe
xtivreg lhwage (yeduc = 1.young#c.nin) birthyr##c.(ch71 en71 wsppc) if old | young, fe

xtreg   lwage   yeduc                  birthyr##c.ch71             , fe
xtivreg lwage  (yeduc =     dum#c.nin) birthyr##c.ch71             , fe
xtreg   lwage   yeduc                  birthyr##c.(ch71 en71)      , fe
xtivreg lwage  (yeduc =     dum#c.nin) birthyr##c.(ch71 en71)      , fe
xtreg   lwage   yeduc                  birthyr##c.(ch71 en71 wsppc), fe
xtivreg lwage  (yeduc =     dum#c.nin) birthyr##c.(ch71 en71 wsppc), fe


xtreg   part    yeduc                  birthyr##c.ch71             , fe
xtivreg part   (yeduc =     dum#c.nin) birthyr##c.ch71             , fe
xtreg   part    yeduc                  birthyr##c.(ch71 en71)      , fe
xtivreg part   (yeduc =     dum#c.nin) birthyr##c.(ch71 en71)      , fe
xtreg   part    yeduc                  birthyr##c.(ch71 en71 wsppc), fe
xtivreg part   (yeduc =     dum#c.nin) birthyr##c.(ch71 en71 wsppc), fe

* close replica of Figure 2
forvalues e=0/19 {
  gen byte S`e' = yeduc>`e'
  eststo est`e': xtreg S`e' 1.young#1.recp i.age74 if old | young, fe
}
coefplot e*, keep(1.young#1.recp) vertical ylab(-.04(.02).08) omitted

restore
}


***
*** ~ Mincer 1974, chart 4.4
***
{
preserve
keep if age>=15 & age<56 & year==1995
gen _yeduc = floor((yeduc - 1) / 3) + (yeduc==0) - (yeduc>18)
gen hwage = exp(lhwage)
colorpalette viridis, n(6) range(.8 0) saturate(1) nograph
twoway lpoly hwage age if _yeduc==0 [aw=wt], bw(5) lcolor("`r(p1)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==1 [aw=wt], bw(5) lcolor("`r(p2)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==2 [aw=wt], bw(5) lcolor("`r(p3)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==3 [aw=wt], bw(5) lcolor("`r(p4)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==4 [aw=wt], bw(5) lcolor("`r(p5)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==5 [aw=wt], bw(5) lcolor("`r(p6)'") lwidth(medium)    ///
       xtitle(Age, size(medium) margin(medium)) ytitle("Hourly wage (rupiah)", size(medium)) graphregion(margin(zero)) plotregion(fcolor(white)) ///
       legend(on size(medium) order(6 5 4 3 2 1) label(1 "0–3 years") label(2 "4–6 years")  label(3 "7–9 years") label(4 "10–12 years") label(5 "13–15 years") label(6 "16+ years of schooling") cols(1) pos(11) ring(0) margin(zero) region(margin(zero) style(none) lstyle(none))) ///
       xlab(15(10)55, nogrid labsize(medium)) ylab(, labsize(medium)) yscale(log) scheme(plottig)
graph export "Public/Output/Mincer4.4 left.png", replace width(3000)
restore
}


***
*** Simulation of weighted & unweighted OLS
***
{
preserve
clear
set obs 5000
set seed 20394857
drawnorm x _e
qui forvalues h=1/3 {
  local errstruct : word `h' of _e _e*(x+3)/5 _e+x^2
  local structname: word `h' of Homoskedastic Heteroskedastic Misspecified
  local wtexp     : word `h' of "" [pw=1] [pw=1]  // weight expression for unweighted fits; "pw=1" triggers het-robust CIs
  cap drop e
  gen e = `errstruct'
  sum e, meanonly
  replace e = e - r(mean)
  cap drop y 
  gen y = x + e
  forvalues v=1/4 {
    local sampler    : word `v' of x>0 (x>0)==(e>0) e>0 y>0
    local samplerdesc: word `v' of {it:x} "{it:x} & {it:e}" {it:e} {it:y}
    cap drop p
    cap drop samp
    gen p = cond(`sampler', 1, .1)
    gen byte samp = runiform() < p
    scatter y x if samp & y<10, msym(O) msize(vtiny) mcolor(gs8) yaxis(1 2) ||  ///
         function y=x, range(-4 4) lwidth(medthick) yaxis(2) lpat(dash) ||  ///
         lfitci y x [pw=1/p] if samp, acolor(plg1%50) yaxis(2) level(99.999995) || ///
         lfitci y x `wtexp' if samp, acolor(pll1%50) yaxis(2) level(99.999995)  ///
       plotregion(fcolor(white) lcolor(gs12)) ///
       legend(order(2 4 6) lab(2 "Linear fit in population") lab(4 "Weighted fit in sample") lab(6 "Unweighted fit in sample") margin(zero) bmargin(zero)) ///
       `=cond(`h'>1,"","title(Sampling depends on `samplerdesc')")' xtitle({it:x}) ///
       yscale(off axis(1)) xlab(, notick nogrid labgap(zero)) ylab(, axis(1) nogrid) ylab(-5 0 5, axis(2) notick nogrid) `=cond(`v'==4, "ytitle({it:y}, orient(hor) axis(2)) fxsize(40)", "yscale(off axis(2))")' ///
       `=cond(`h'==3,"","xscale(off)")' ///
       name(v`v'h`h', replace) nodraw
  }
  graph combine v1h`h' v2h`h' v3h`h' v4h`h', xcommon imargin(zero) rows(1) l1title(`structname', size(small)) ycommon name(h`h', replace) nodraw `=cond(`h'==3,"fysize(35)","")'
}
grc1leg2 h1 h2 h3, xcommon scheme(plottig) imargin(zero) cols(1) lrows(1) graphregion(margin(zero)) legscale(*1) iscale(*1.25) labsize(vsmall)
graph export "Public/Output/weightsim.png", replace width(2000)
restore
}


***
*** Perform partial diagnostic checks for inconsistency of unweighted OLS, described in text
***
{
preserve
gen T = young#c.ninnew
gen p = 1 / wt
foreach var in yeduc lhwage {
  cap drop e _T
  cap drop _`var'
  partialhdfejl `var' T if (young | old) & year==1995, a(birthplnew birthyr##c.ch71new) prefix(_)  // under null that unweighted OLS is consistent...
  qui reg _`var' _T, nocons
  predict e if e(sample), resid
  
  reg  e p [pw=wt] if e(sample), cluster(birthplnew)                                               // speaks to exogeneity condition: is e mean-independent of p, T jointly?
  reg _T p [pw=wt] if e(sample), cluster(birthplnew)                                               // speaks to endogeneity condition: is p mean-independent of T?
}
restore
}


***
*** 2x2 DID
***
{
preserve
keep if year==1995 & lhwage<.
cap erase Public/Output/DID2x2.rtf

foreach depvar in yeduc lhwage {
  eststo E`depvar'wt   : reg `depvar'  young##recp    if old | young     [aw=wt]
  eststo E`depvar'newwt: reg `depvar'  young##recpnew if old | young     [aw=wt], cluster(birthplnew)
  eststo P`depvar'wt   : reg `depvar'  old##recp      if reallyold | old [aw=wt]
  eststo P`depvar'newwt: reg `depvar'  old##recpnew   if reallyold | old [aw=wt], cluster(birthplnew)

  esttab E`depvar'wt E`depvar'newwt using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(DID) rename(1.young#1.recp DID 1.young#1.recpnew DID) fonttbl(\f0\fnil $font;)
  esttab P`depvar'wt P`depvar'newwt using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(DID) rename(1.old#1.recp   DID 1.old#1.recpnew   DID) fonttbl(\f0\fnil $font;)
}
eststo EWaldwt   : ivregress 2sls lhwage young recp    (yeduc = young#recp   ) if old | young     [aw=wt], small
eststo EWaldnewwt: ivregress 2sls lhwage young recpnew (yeduc = young#recpnew) if old | young     [aw=wt], cluster(birthplnew) small
eststo PWaldwt   : ivregress 2sls lhwage old recp      (yeduc = old#recp     ) if reallyold | old [aw=wt], small
eststo PWaldnewwt: ivregress 2sls lhwage old recpnew   (yeduc = old#recpnew  ) if reallyold | old [aw=wt], cluster(birthplnew) small

esttab EWaldwt EWaldnewwt using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(yeduc) fonttbl(\f0\fnil $font;)
esttab PWaldwt PWaldnewwt using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(yeduc) fonttbl(\f0\fnil $font;) 
restore
}


* Store a scalar or (labeled) row vector as a regression estimate without standard errors. Destroys its argument.
cap program drop myestpost
program define myestpost, eclass
  mat b = `1'
  ereturn post b
end


***
*** reduced form/OLS
***

{
cwf default
cap frame create bs
tempfile bs
set seed 2039458

preserve
keep if inrange(age74,2,24) & inrange(age,23,55)
gen t1 = $age74kink - age74
gen t2 = max(0,t1)
global age74minplot 2
global age74maxplot 22
global youngmin 2
global youngmax 6
global oldmin 12
global oldmax 17
global reallyoldmin 18
global reallyoldmax 24
scalar placscale = ($oldmax+$oldmin-$youngmax-$youngmin)/($reallyoldmax+$reallyoldmin-$oldmax-$oldmin)  // factor to scale placebo effect by before comparison to experiment: 10.5/6.5
scalar tauscale = $age74kink - ($youngmax-$youngmin)  // factor to multiply kink estimate by to get tau: 8
ren wt _wt

forvalues y=1/3 {
  local years   : word `y' of 1995,1995 2005,2012 1995,2012
  local yearname: word `y' of 1995      Post-1995    All
  replace       old = inrange(age74,      $oldmin      ,$oldmax)
  replace reallyold = inrange(age74,$reallyoldmin,$reallyoldmax)

  sum _wt if inrange(year,`years'), detail
  cap drop wt
  gen wt = min(_wt, r(p50)+5*(r(p75)-r(p50)))  // clip extreme weights to median + 5 * IQR (Potter and Zheng 2015)

  foreach depvars in "primary yeduc" "part lhwage" {
    cap erase "Public/Output/RF`yearname' `depvars'.rtf"
    forvalues c=1(-1)0 {  // control sets, 0=none, 1=minimal, 2=intermediate, 3=full
      eststo clear
      foreach depvar in `depvars' {
        local edvar = inlist("`depvar'","primary","yeduc")
        foreach new in `=cond(`y'==1 & `c' & inlist("`depvar'","yeduc","lhwage"), `""""', "")' new {
          local controls: word `=`c'+1' of "" ch71`new' "ch71`new' en71`new'" "ch71`new' en71`new' wsppc"

          local seed `c(seed)'  // save to give weighted & unweighted, experiment and placebo, Hausman bootstraps same DGPs
          foreach wt in 1 `=cond("`new'"=="","","wt")' {
            reg `depvar' 1.young#c.nin`new' ib1974.birthyr##c.(`controls') i.birthpl`new' i.year [pw=`wt'] if inrange(year,`years') & (`edvar' | year<1974-$oldmax+$retireage) & (old | young)
            est store exp
            qui count if e(sample)
            scalar N`depvar'`new'`wt'exp = r(N)

            * bootstrap distribution, for Hausman test of weighted vs unweighted
            reghdfejl `depvar' 1.young#c.nin`new' [pw=`wt'] if e(sample), a(birthyr##c.(`controls') birthpl`new' year) vce(bs, cluster(birthpl`new') procs($procs) reps(1000) seed(`seed') saving("`bs'", replace)) nosamp
            scalar b`depvar'`new'`wt'exp = _b[1.young#c.nin`new']
            frame bs: use "`bs'.dta", clear
            frame bs: putmata bs`wt'exp = _bs_1, replace

            cap noi reg `depvar' 1.old#c.nin`new' ib1974.birthyr##c.(`controls') i.birthpl`new' i.year [iw=`wt'] if inrange(year,`years') & (`edvar' | year<1974-$reallyoldmax+$retireage) & (old | reallyold)
            if _rc {
              est restore exp
              eststo: xlincom Experiment = 1.young#c.nin`new', post
              scalar N`depvar'`new'`wt'plac = .
            }
            else {
              est sto placebo
              qui count if e(sample)
              scalar N`depvar'`new'`wt'plac = r(N)

              * bootstrap distribution, for Hausman test of weighted vs unweighted
              reghdfejl `depvar' 1.old#c.nin`new' [pw=`wt'] if e(sample), a(birthyr##c.(`controls') birthpl`new' year) vce(bs, cluster(birthpl`new') procs($procs) reps(1000) seed(`seed') saving("`bs'", replace)) nosamp
              scalar b`depvar'`new'`wt'plac = _b[1.old#c.nin`new']
              frame bs: use "`bs'.dta", clear
              frame bs: putmata bs`wt'plac = _bs_1, replace

              reg `depvar' 1.young#c.nin`new' ib1974.birthyr##c.(`controls') i.birthpl`new' i.year [iw=`wt'] if inrange(year,`years') & (`edvar' | year<1974-$reallyoldmax+$retireage) & (old | young)
              suest . placebo, `=cond("`new'"=="","","cluster(birthplnew)")'
              eststo: xlincom (Experiment = [_LAST_mean]1.young#c.nin`new') ///
                              (Placebo    = [placebo_mean]1.old#c.nin`new') ///
                              (Difference = [_LAST_mean]1.young#c.nin`new' - placscale * [placebo_mean]1.old#c.nin`new'), post
            }
          }
          if "`new'"!="" {  // add Hausman p values in the form of another estimation result
            mata st_numscalar("Vexp", variance(bs1exp - bswtexp))  // bootstrap-based Hausman χ²(1) stat. (Cameron and Travedi 2005, p. 378)
            mat χ²p = chi2tail(1, (b`depvar'`new'1exp - b`depvar'`new'wtexp)^2 / Vexp)
            if `depvar'`new'wtNplac < . {
              mata st_numscalar("Vplac", variance(bs1plac - bswtplac))
              mata st_numscalar("Vdiff", variance(bs1exp - bswtexp - `=placscale' * (bs1plac - bswtplac)))
              mat χ²p = χ²p, chi2tail(1, (b`depvar'`new'1plac - b`depvar'`new'wtplac)^2 / Vplac), chi2tail(1, (b`depvar'`new'1exp - b`depvar'`new'wtexp - placscale * (b`depvar'`new'1plac - b`depvar'`new'wtplac))^2 / Vdiff)
              mat colnames χ²p = Experiment Placebo Difference
            }
            else mat colnames χ²p = Experiment
            eststo: myestpost χ²p  // fake estimation result for esttab to include, with Hausman p values for unweighted vs weighted
          }
        }
      }
      esttab using "Public/Output/RF`yearname' `depvars'.rtf", append b(a2) se(a2) title(Controls: `controls') eqlabels(,none) nostar nolines nonotes nomtitles noeqlines nogap nonumber msign("–") noobs fonttbl(\f0\fnil $font;)

      eststo clear
      forvalues d=1/`:word count `depvars'' {
        local depvar: word `d' of `depvars'
        local edvar = inlist("`depvar'","primary","yeduc")
        local pretrendlen = cond(`y'==1 | `edvar', 10, 5)
        foreach new in `=cond(`y'==1 & `c' & inlist("`depvar'","yeduc","lhwage"), `""""', "")' new {
          local controls: word `=`c'+1' of "" ch71`new' "ch71`new' en71`new'" "ch71`new' en71`new' wsppc"
          local seed `c(seed)'  // save to give weighted & unweighted, experiment and placebo, Hausman bootstraps same DGPs
          foreach wt in 1 `=cond("`new'"=="","","wt")' {
            // event study
            reghdfejl `depvar' ibn.birthyr#c.nin`new' [pw=`wt'] if inrange($age74kink-age74, -`pretrendlen', 10) & inrange(year,`years') & (`edvar' | year<1974-$age74kink-`pretrendlen'+$retireage), a(birthpl`new' birthyr##c.(`controls') year) cluster(birthpl`new')
            mata dots = st_matrix("e(b)")'
            coefplot, keep(*.birthyr#c.nin`new') omitted rename(([0-9]+)[ob]?.birthyr#c.nin`new' = \1, regex) vertical at(_coef, transform(1974 - @)) xscale(reverse) msym(smcircle) msize(small) gen replace
            global graph `r(graph)'
          
            // spline fit with bootstrapping for Hausman test
            qui reghdfejl `depvar' c.t?#c.nin`new'    [pw=`wt'] if e(sample), a(birthpl`new' birthyr##c.(`controls') year) vce(bs, cluster(birthpl`new') procs($procs) reps(1000) seed(`seed') saving("`bs'", replace))
            scalar b`depvar'`new'`wt' = _b[c.t2#c.nin`new']
            frame bs: use "`bs'.dta", clear
            frame bs: putmata bs`wt' = _bs_1, replace

            // spline fit with non-bootstrap standard errors
            reghdfejl `depvar' c.t?#c.nin`new'        [pw=`wt'] if e(sample), a(birthpl`new' birthyr##c.(`controls') year) cluster(birthpl`new') nosamp
            mata splinefit = `=_b[c.t1#c.nin`new']' * (-`pretrendlen'::10) + `=_b[c.t2#c.nin`new']' * (J(`pretrendlen',1,0) \ 0::10)
            mata st_numscalar("splineshift", mean(dots - splinefit))
            local splinefn `=splineshift - _b[c.t1#c.nin`new'] * `pretrendlen'' `=$age74kink+`pretrendlen'' `=splineshift' $age74kink `=splineshift + _b[c.t1#c.nin`new'] * 10 + _b[c.t2#c.nin`new'] * 10' `=$age74kink-10'

            eststo: xlincom Kink = tauscale * _b[t2#c.nin`new'], post  // slope increase * mean years
            estadd scalar Nexp  = N`depvar'`new'`wt'exp      // for display, tack the N's for the experiment & placebo to the bottom of the kink estimate
            estadd scalar Nplac = N`depvar'`new'`wt'plac

            local caption: display "{it:{&tau}} = " (_b[Kink]<0)*"{&minus}" %4.3f abs(_b[Kink]) " (" %4.3f abs(_se[Kink]) ")"
            $graph || scatteri 0 $age74minplot, mstyle(p1) msym(smcircle) msize(small) ///  // zero for base year
                   || scatteri `splinefn', lcolor(maroon) lwidth(medium) mstyle(p1) msym(diamond) msize(small) mcolor(maroon) lpat(solid) recast(connected) ///
                      xlab($age74minplot $age74kink $age74maxplot, nogrid) ///
                      `=cond("`wt'"=="1", "xlab(, nolab notick nogrid) xscale(off fill)", "")' ///
                      `=cond(`d'==1 & `c'==1, `"ytitle(`=cond("`wt'"=="1","Unweighted","Weighted")')"', "")' ///
                      plotregion(margin(0 1 0 0)) ///
                      name(RF`depvar'`wt'y`y', replace) nodraw ///
                   || scatteri 0 0, msymbol(none) xaxis(2) yaxis(2) xscale(axis(2) off) yscale(axis(2) off) ///  // fake plot to set up extra axes with range [-1,1] for placing text
                        text(-.9 1 "`caption'", xaxis(2) yaxis(2) place(w) color(black))
          }
          if "`new'"!="" {  // add Hausman p values in the form of another estimation result
            mata st_numscalar("Vkink", variance(bs1 - bswt))  // bootstrap-based Hausman χ²(1) stat. (Cameron and Travedi 2005, p. 378)
            mat χ²p = chi2tail(1, (b`depvar'`new'1 - b`depvar'`new'wt)^2 / Vkink)
            mat colnames χ²p = Kink
            eststo: myestpost χ²p
          }
        }
        graph combine RF`depvar'1y`y' RF`depvar'wty`y', cols(1) graphregion(margin(zero)) /*`=cond(`d'==1 & `c'==1,"fxsize(55)","")'*/ name(`depvar', replace) imargin(zero) ycommon nodraw t1title("     `:var label `depvar' '", size(small))
      }
      esttab using "Public/Output/RF`yearname' `depvars'.rtf", append b(a2) se(a2) nostar nolines nonotes noeqlines nomtitles nogap nonumber noobs msign("–") fonttbl(\f0\fnil $font;) ///
          `=cond(`c', "", `"stat(Nexp Nplac N, label("Experiment N" "Placebo N" "Kink N") fmt(%10.0fc %10.0fc %10.0fc))"')'

      local t1title: word `=`c'+1' of "No extra controls" "With controls based on number of children in regency of birth"
      graph combine `depvars', rows(1) title(`t1title', size(small)) graphregion(margin(zero)) name(RFy`y'ctl`c', replace)
    }
    graph combine RFy`y'ctl1 RFy`y'ctl0, imargin(zero) xsize(9) ysize(5.5) graphregion(margin(zero)) b1title(Age in 1974, xoffset(4) size(vsmall)) altshrink iscale(*1.5)
    graph export "Public/Output/RF`yearname' `depvars' spline.png", replace width(2000)
  }
}
restore
}


***
*** OLS & 2SLS returns to schooling
***
{
preserve

* ~original Table 7 regressions
keep if inrange(age74,2,24) & inrange(age,23,55) & inlist(year,1995,2005,2011,2012)
gen t1_nin =    (   $age74kink - age74) * ninnew
gen t2_nin = max(0, $age74kink - age74) * ninnew
replace young = . if !(young | old)  // to restrict young/old samples to those 2 groups
xi i.young|ninnew i.dum|ninnew i.birthyr*ch71new i.birthyr*en71new i.birthyr*wsppc i.year i.birthplnew
set seed 230498257
xtset birthpl
ren wt _wt
* new regressions
xtset birthplnew
foreach edvar in /*yeduc*/ primary {
  forvalues y=1/3 {
    local years: word `y' of 1995,1995 2005,2012 1995,2012
    sum _wt if inrange(year,`years'), detail
    cap drop wt
    gen wt = min(_wt, r(p50)+5*(r(p75)-r(p50)))  // clip extreme weights to median + 5 * IQR (Potter and Zheng 2015)
    
    local graphs
    cap erase "Public/Output/`edvar' y`y'.rtf"
    forvalues c=1(-1)0 /*1/3*/ {
      local controls _Iyear* _Ibirthyr_* `:word `=`c'+1' of "" _IbirXch7* "_IbirXch7* _IbirXen7_*" "_IbirXch7_* _IbirXen7_* _IbirXwsp_*" '
      forvalues d=1/2 {
        local depvar: word `d' of part lhwage
        foreach wt in 1 wt {
          eststo OLS`depvar'`edvar'c`c'w`wt'y`y': areg `depvar' `edvar' `controls' [pw=`wt'] if inrange(year,`years'), cluster(birthplnew) a(birthplnew)
          forvalues i=1/3 {
            local insts: word `i' of _IyouXninne_1 _IdumXnin_* t2_nin
            eststo TSLS`depvar'`edvar'c`c'`i'w`wt'y`y': xtivreg2 `depvar' (`edvar' = `insts') `controls' `=cond(`i'==3,"t1_nin","")' [pw=`wt'] if inrange(year,`years'), cluster(birthplnew) partial(`controls') small fe
            boottest, ar reps(99999) gridmin(-.8) gridmax(1.1) format(%4.2f) ///
                     graphopt(xlab(-.8(.2)1.1, nogrid) ylab(.05 .2(.2)1, nogrid) xline(0) lwidth(thin) nodraw ///
                              `=cond(`d'==2 & `c', `"xtitle("")"', "xscale(off)")' ytitle("") `=cond("`wt'"=="1" & `i'==1,"","yscale(off)")' `=cond(`d'==2 | `c'==0,"",`"title(`=cond("`wt'"=="1", "Unweighted", "Weighted")')"')') ///
                     graphname(`depvar'`edvar'c`c'2SLS`i'w`wt'y`y', replace)
            estadd local CIstr "`r(CIstr)'"
          }
        }
        esttab /*OLS`depvar'`edvar'c`c'w*y`y'* */ TSLS`depvar'`edvar'c`c'1w*y`y'* /*TSLS`depvar'`edvar'c`c'2w*y`y'* */ TSLS`depvar'`edvar'c`c'3w*y`y'* ///
               using "Public/Output/TSLS`edvar' y`y'.rtf", append ///
               keep(`edvar') b(a2) se(a2) msign("–") nonotes nonumber nogaps nomtitles nostar fonttbl(\f0\fnil $font;) ///
               stat(CIstr /*jp*/ widstat N, labels("Bootstrap CI" /*"Hansen p"*/ "KP F" Observations) fmt(%~1s /*a2*/ a2 %11.0gc))

        graph combine `depvar'`edvar'c`c'2SLS1w1y`y' `depvar'`edvar'c`c'2SLS1wwty`y', ///
              rows(1) imargin(1 0 0 0) `=cond(`d'==1 & `c'==1, "title(Instrument by young/old)", "")' name(g1, replace) nodraw
        graph combine `depvar'`edvar'c`c'2SLS2w1y`y' `depvar'`edvar'c`c'2SLS2wwty`y', ///
              rows(1) imargin(1 0 0 0) `=cond(`d'==1 & `c'==1, "title(Instruments by birth year)", "")' name(g2, replace) nodraw
        graph combine `depvar'`edvar'c`c'2SLS3w1y`y' `depvar'`edvar'c`c'2SLS3wwty`y', ///
              rows(1) imargin(1 0 0 0) `=cond(`d'==1 & `c'==1, "title(Kink instrument)", "")' name(g3, replace) nodraw
        graph combine g1 g2 g3, l1title(`:var label `depvar'') rows(1) imargin(1 1 0 0) iscale(1) name(`depvar'`edvar'c`c'2SLSy`y', replace) nodraw fysize(30)
        local graphs `graphs' `depvar'`edvar'c`c'2SLSy`y'
      }
    }
    graph combine `graphs', cols(1) ycommon b1title(Coefficient on `=lower("`:var label `edvar''")', size(small)) xsize(8.5) ysize(5.5) name(TSLS`edvar'y`y', replace) imargin(0 0 1 0) iscale(*1)
    graph export Public/Output/TSLS`edvar'y`y'.png, replace width(2000)
  }
}
restore
}


***
*** CIC
***
{
cap program drop mycic
program define mycic
  syntax [pw/]
  preserve  // double-nested preserve, which is why code is in this subprogram
  forvalues y=1/3 {
    local years : word `y' of 1995,1995 2005,2012 1995,2012
    if "`exp'"!="1" {
      sum _wt if inrange(year,`years'), detail
      replace wt = min(_wt, r(p50)+5*(r(p75)-r(p50)))  // clip extreme weights to median + 5 * IQR (Potter and Zheng 2015)
    }
    eststo cicw`exp'y`y': cic lhwage _Ibirthyr* _Ibirthpl* _Iyear* `=cond("`exp'"=="1","","[pw=`exp']")' if inrange(year,`years'), group(recpnew) time(young) reps(1000)
    foreach stat in `:rownames e(tests)' {
      estadd scalar `stat' = e(tests)["`stat'",2], replace
    }
  }
  drop _all  // speeds up coefplot, gen
  coefplot cicw`exp'y1 cicw`exp'y2 cicw`exp'y3, scheme(plottig) ylabel(1 "10" 2 "20" 3 "30" 4 "40" 5 "50" 6 "60" 7 "70" 8 "80" 9 "90") `=cond("`exp'"=="1","ytitle(Percentile)","")' ///
    legend(rowgap(zero) lab(11 "1995") lab(22 "2011-12") lab(33 "All") pos(4) region(margin(zero)) bmargin(1 0 0 0) size(7pt)) ///
    title("`=cond("`exp'"=="1","Unw","W")'eighted", margin(vsmall)) graphregion(margin(zero)) cismooth(n(10)) gen replace
  global graph `r(graph)'
  cap drop label x
  gen label = subinstr(string(__b, "%5.3f") + " (" + string(__se, "%5.3f")+ ")", "-", "–", .) if __at<.
  gen x = .17 if __at<.
  $graph || scatter __at x, msym(none) mlab(label) mlabcolor(black) mlabsize(7pt) mlabpos(9) xscale(range(-.1 .17)) xlab(-.1(.05).1) xline(0, lcolor(gs10) lpat(solid)) xsize(5.5) ysize(5) name(cicw`exp', replace)

  esttab cicw`exp'y? using Public/Output/cic.rtf, append rename(q9 90 q8 80 q7 70 q6 60 q5 50 q4 40 q3 30 q2 20 q1 10) order(90 80 70 60 50 40 30 20 10) nogaps nomtitle msign("–") b(3) se(3) nostar fonttbl(\f0\fnil $font;) ///
                             stats(constant_0 constant_m stoch_dom_pos stoch_dom_neg, labels("No effect (p)" "Constant effect (p)" "All >0 (p)" "All <0 (p)") fmt(%4.2f))
  restore
end

preserve
keep if (young | old) & inrange(age,23,55) & inlist(year,1995,2011,2012)
xi i.year i.birthyr i.birthplnew
set seed 30948573
cap erase Public/Output/cic.rtf
gen _wt = wt
foreach wt in 1 wt {
  mycic [pw=`wt']
}
grc1leg2 cicw1 cicwwt, rows(1) imargin(zero)
gr_edit .legend.plotregion1.key[1].view.style.editstyle marker(symbol(circle)) editcopy
gr_edit .legend.plotregion1.key[2].view.style.editstyle marker(symbol(circle)) editcopy
gr_edit .legend.plotregion1.key[3].view.style.editstyle marker(symbol(circle)) editcopy
gr_edit .legend.plotregion1.key[1].view.style.editstyle marker(fillcolor(black)) editcopy
gr_edit .legend.plotregion1.key[2].view.style.editstyle marker(fillcolor(plb1)) editcopy
gr_edit .legend.plotregion1.key[3].view.style.editstyle marker(fillcolor(plg1)) editcopy
gr_edit .legend.plotregion1.key[1].view.style.editstyle marker(linestyle(color(black))) editcopy
gr_edit .legend.plotregion1.key[2].view.style.editstyle marker(linestyle(color(plb1 ))) editcopy
gr_edit .legend.plotregion1.key[3].view.style.editstyle marker(linestyle(color(plg1 ))) editcopy
graph export Public/Output/cic.png, replace width(2000)
restore
}


***
*** Jakiela diagnostic
***
{
preserve
keep if year==1995 & (young | old)
xi i.young|ninnew

regress _IyouXninne_1 i.birthyr i.birthplnew i.birthyr#c.lch71new if lhwage<. [aw=wt]
predict _T if e(sample), resid
regress lhwage        i.birthyr i.birthplnew i.birthyr#c.lch71new if lhwage<. [aw=wt]
predict _Y if e(sample), resid

eststo untreated: reg _Y _T if !young [aw=wt], nocons
eststo   treated: reg _Y _T if  young [aw=wt], nocons
suest untreated treated, cluster(birthplnew)
test [untreated_mean]_T = [treated_mean]_T  // test that two slopes are same, as reported in text

collapse T=_IyouXninne_1 _T _Y (rawsum) wt [aw=wt], by(birthyr birthplnew)

scatter _Y _T if T==0, msym(Oh) mcolor(%20) mlwidth(medthick) || ///
scatter _Y _T if T!=0, msym(Oh) mcolor(%20) mlwidth(medthick) || ///
lpoly   _Y _T if T==0 [aw=wt], bw(.5) pstyle(p1) || ///
lpoly   _Y _T if T!=0 [aw=wt], pstyle(p2) bw(.5) || ///
lfit    _Y _T if T==0 [aw=wt], pstyle(p1) || ///
lfit    _Y _T if T!=0 [aw=wt], pstyle(p2) ///
  legend(order(2 1) cols(1) label(2 "Age 2{&minus}6 in 1974") label(1 "Age 12{&minus}17 in 1974") ring(0) pos(8) region(style(none))) ///
  scheme(plottig) xtitle(Residualized treatment) ytitle(Residualized log hourly wage) graphregion(margin(zero))
graph export Public/Output/Jakiela.png, replace width(2680) height(1552)
restore
}

log close
