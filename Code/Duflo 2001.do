* dependencies: boottest, reghdfejl, julia, xlincom, estout, coefplot, blindschemes, palettes, colrspace, moremata, xlincom, cic, and qrprocess
* all are from SSC except the last two are from https://sites.google.com/site/blaisemelly/home/computer-programs/cic_stata

cap cd "D:/OneDrive - Open Philanthropy Project"
cap cd "/Users/davidroodman/Library/CloudStorage/OneDrive-OpenPhilanthropyProject"
cap cd "W:/"
cd "Education/Duflo 2001"

global source95 NBER  // if "NBER", use primary data owned by NBER; otherwise use processed file for birth years 1950-72, https://github.com/NathanLazarus/Duflo2001/blob/main/inpresdata.dta

cap set scheme stmono1  // plotplain
set odbcdriver ansi

if c(os)=="Windows" global font LM Roman 9
               else global font Latin Modern Roman

graph set window fontface "$font"  // https://www.1001fonts.com/latin-modern-roman-font.html?
est drop _all

cap log close
cap log using Public/Output/duflo2001, text replace

scalar age74kink = 12


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

    foreach var in ch71 en71new ch71new dens71 ninnew {
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

  // Table 2--doesn't fully match
  gen ltotin = ln(nin*ch71)
  gen lnen71 = ln(1-en71)
  reg ltotin lch71 lnen71

  gen ltotinnew = ln(totinnew)
  gen lnen71new = ln(1-en71new)
  reg ltotinnew lch71new lnen71new  // corrected version

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
    replace wage = . if !inlist(classwk,4,5) | wage/cpi[1,year-1985] < 100000/cpi[1,2007-1985]  // to minimize survey effects, truncate all to highest threshold, 100,000 2007 Rp ~= $10/month today
  
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
  gen byte yeducp = clip(yeduc,0,6)

  recode birthpl (1472=1403) (1804=1803) (3275=3219) (5171=5103) (5271=5201) (7173=7103) (7271=7203) (8271=8203) (8104=8103), gen(birthplnew) // group new child regencies with parents

  gen byte migration = inlist(floor(birthpl/100), 12,13,61,62,63,64,71,72,73,74,81,82)  // Kalimanton, Sumatra, Sulawesi, Maluku, and Irian Jaya https://www.downtoearth-indonesia.org/old-site/ctrans.htm

  xtset birthplnew

  // adjust for inflation, https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2013&locations=US%E2%89%A4%2FSEURLD-ID&start=1995
  mat cpi = 9.339845464,10.20645761,11.0276046,11.73508494,12.6526737,13.84443641,14.88602496,16.32578542,17.71870229,19.38786131,20.93370995,22.23707238,35.23487344,42.45021132,44.01603795,49.07793288,54.91826459,58.62926593,62.18457972,68.68485723,77.68852994,82.66569441,91.11963768,95.11652363,100,105.3560478,109.8647599,116.9098522,124.38615,132.3009914,136.9656666,142.1824123,146.7298984,151.1766751,154.0807306,156.4845902,163.0717524,sqrt(163.0717524/156.4845902)*163.0717524
  replace wage = wage / cpi[1,year-1994]
  gen lwage = ln(wage)
  gen lhwage = ln(wage / 4 / hrswork)
  gen byte part = lhwage<. | (year==2005 & classwk==4) if inlist(year,1995,2005,2011,2012,2013,2014) // labor force participation
  gen lhwagep = lhwage if yeduc<=6  // wage var restricted to primary-only or some-secondary people
  gen lhwages = lhwage if yeduc> 6

  label var yeduc "Years of schooling"
  label var yeducp "Years of primary schooling"
  label var primary "Primary completion"
  label var part "Employment"
  label var lwage "Log monthly wages"
  label var lhwage "Log hourly wage"
  label var lhwagep "Primary schooling only"
  label var lhwages "Some secondary schooling"
  
  save analysisdata, replace
}  // end data prep


***
*** replicate most of original
***
{
preserve
keep if inrange(age74,2,24) & year==1995
xtset birthpl

* Table 3: DID
reg yeduc  young##recp                                if (      old | young) & lhwage<. [aw=wt]
reg lhwage young##recp                                if (      old | young)            [aw=wt]
ivregress 2sls lhwage young recp (yeduc = young#recp) if (      old | young)            [aw=wt], small  // correct Wald DID estimator

reg yeduc  old##recp                                  if (reallyold | old  ) & lhwage<. [aw=wt]
reg lhwage old##recp                                  if  reallyold | old               [aw=wt]
ivregress 2sls lhwage old   recp (yeduc = old#recp  ) if  reallyold | old               [aw=wt], small

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
  local errstruct : word `h' of _e _e*(x+3)/3 _e+x^2
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
    local samplerdesc: word `v' of {it:x} "{it:x} & {it:e}" {it:e} "{it:y} = {it:x} + {it:e}"
    cap drop p
    cap drop samp
    gen p = cond(`sampler', 1, .05)
    gen byte samp = runiform() < p
    scatter y x if samp & y<10, msym(O) msize(vtiny) mcolor(gs8) yaxis(1 2) ||  ///
         function y=x, range(-4 4) lwidth(medthick) yaxis(2) lpat(dash) ||  ///
         lfitci y x [pw=1/p] if samp, acolor(plg1%50) yaxis(2) level(99.999995) || ///
         lfitci y x `wtexp' if samp, acolor(pll1%50) yaxis(2) level(99.999995)  ///
       plotregion(fcolor(gs`=cond(`v'==1 & `h'<3 | `v'==3 & `h'!=2,15,16)') lcolor(gs12)) ///
       legend(order(2 4 6) lab(2 "Linear fit in population") lab(4 "Weighted fit in sample") lab(6 "Unweighted fit in sample") margin(zero) bmargin(zero)) ///
       `=cond(`h'>1,"","title(Sampling depends on `samplerdesc')")' xtitle({it:x}) ///
       yscale(off axis(1)) xlab(, notick nogrid labgap(zero)) ylab(, axis(1) nogrid) ylab(-5 0 5, axis(2) notick nogrid) `=cond(`v'==4, "ytitle({it:y}, orient(hor) axis(2)) fxsize(40)", "yscale(off axis(2))")' ///
       `=cond(`h'==2,"","xscale(off)")' ///
       name(v`v'h`h', replace) nodraw
  }
  graph combine v1h`h' v2h`h' v3h`h' v4h`h', xcommon imargin(zero) rows(1) l1title(`structname', size(small)) ycommon name(h`h', replace) nodraw `=cond(`h'==3,"fysize(35)","")'
}
grc1leg2 h1 h2 /*h3*/, xcommon scheme(plottig) imargin(zero) cols(1) lrows(1) graphregion(margin(zero)) legscale(*1) iscale(*1.25) labsize(vsmall)
graph export "Public/Output/weightsim.png", replace width(2000)
restore
}

      
***
*** Partial diagnostic checks for inconsistency of unweighted OLS, described in text
***

{
preserve
keep if year==1995 & (young | old)
gen T = young#c.ninnew
gen p = 1 / wt

reghdfejl p age [aw=wt], a(birthplnew)  // cited in discussion of whether weighting needed: in Duflo (2001) sample, no clear association between age and sampling probability 

reghdfejl p yeduc  1.young#c.ninnew, a(birthplnew birthyr##c.ch71new) cluster(birthplnew)  // reported in text
sum wt if e(sample), detail
di "Trimming level example: " r(p50) + 4 * (r(p75) - r(p25))  // trimming level mentioned in text


reghdfejl p lhwage 1.young#c.ninnew, a(birthplnew birthyr##c.ch71new) cluster(birthplnew)
restore
}


***
*** 2x2 DID
***
{
preserve
keep if year==1995 & (reallyold | old | young) & lhwage<.

sum wt, detail
gen double wtnew = min(wt, r(p50)+4*(r(p75)-r(p25)))  // clip extreme weights to median + 4 * IQR (Potter and Zheng 2015)

cap erase Public/Output/DID2x2.rtf
foreach depvar in yeduc lhwage {
  eststo clear
  eststo: reg `depvar'  young##recp    if old | young
  eststo: reg `depvar'  young##recp    if old | young     [aw=wt   ]
  eststo: reg `depvar'  young##recpnew if old | young               , cluster(birthplnew)
  eststo: reg `depvar'  young##recpnew if old | young     [aw=wtnew], cluster(birthplnew)
  esttab using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(DID) rename(1.young#1.recp DID 1.young#1.recpnew DID) fonttbl(\f0\fnil $font;)

  eststo clear
  eststo: reg `depvar'  old##recp      if reallyold | old
  eststo: reg `depvar'  old##recp      if reallyold | old [aw=wt   ]
  eststo: reg `depvar'  old##recpnew   if reallyold | old           , cluster(birthplnew)
  eststo: reg `depvar'  old##recpnew   if reallyold | old [aw=wtnew], cluster(birthplnew)
  esttab using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(DID) rename(1.old#1.recp   DID 1.old#1.recpnew   DID) fonttbl(\f0\fnil $font;)
}
eststo clear
eststo: ivregress 2sls lhwage young recp    (yeduc = young#recp   ) if old | young               , small
eststo: ivregress 2sls lhwage young recp    (yeduc = young#recp   ) if old | young     [aw=wt]   , small
eststo: ivregress 2sls lhwage young recpnew (yeduc = young#recpnew) if old | young               , cluster(birthplnew) small
eststo: ivregress 2sls lhwage young recpnew (yeduc = young#recpnew) if old | young     [aw=wtnew], cluster(birthplnew) small
esttab using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(yeduc) fonttbl(\f0\fnil $font;)

eststo clear
eststo: ivregress 2sls lhwage old recp      (yeduc = old#recp     ) if reallyold | old           , small
eststo: ivregress 2sls lhwage old recp      (yeduc = old#recp     ) if reallyold | old [aw=wt   ], small
eststo: ivregress 2sls lhwage old recpnew   (yeduc = old#recpnew  ) if reallyold | old           , cluster(birthplnew) small
eststo: ivregress 2sls lhwage old recpnew   (yeduc = old#recpnew  ) if reallyold | old [aw=wtnew], cluster(birthplnew) small
esttab using Public/Output/DID2x2.rtf, append b(a2) se(a2) nogap nonotes nonumbers nomtitles noobs msign("–") keep(yeduc) fonttbl(\f0\fnil $font;)

restore
}


* check for attrition-related trends in outcomes in later surveys
* final years are base years (=0)
{
foreach depvar in yeduc part lhwage {
  reghdfejl `depvar' year#c.ninnew if inrange(age74,12,24) [aw=wt], cluster(birthplnew) a(birthplnew birthyr year)
  coefplot, omit vertical rename(([0-9]+)[ob]?.year#c.ninnew = \1, regex) at(_coef) plotregion(lstyle(none)) xlab(1995 2005 2011 2017) title(`:var label `depvar'') ///
    nodraw name(`depvar'attrition, replace)
}
graph combine yeducattrition partattrition lhwageattrition, rows(1) imargin(small) graphregion(margin(zero)) iscale(*2) ysize(2) b1title(Survey year, size(vlarge)) name(attritioncheck, replace)
graph export "Public/Output/Attrition check.png", replace width(2000)
}



***
*** reduced form/OLS
***

{
* Store a scalar or (labeled) row vector as a regression estimate without standard errors. Destroys its argument.
cap program drop myestpost
program define myestpost, eclass
  mat b = `1'
  ereturn post b
end

preserve
keep if inrange(age74,2,24)
gen t1 = age74kink - age74
gen t2 = max(0,t1)

* kink specification closest to Duflo (2001), Figure 1, just with corrections and clustering
reghdfejl yeduc c.t?#c.ninnew if abs(t1)<11 & year==1995, a(birthplnew birthyr##c.(ch71new en71new))
reghdfejl yeduc c.t?#c.ninnew if abs(t1)<11 & year==1995, a(birthplnew birthyr##c.(ch71new en71new)) cluster(birthplnew)

global age74minplot 2
global age74maxplot 22
global youngmin 2
global youngmax 6
global oldmin 12
global oldmax 17
global reallyoldmin 18
global reallyoldmax 24
scalar placscale = ($oldmax+$oldmin-$youngmax-$youngmin)/($reallyoldmax+$reallyoldmin-$oldmax-$oldmin)  // factor to scale placebo effect by before comparison to experiment: 10.5/6.5
scalar tauscale = age74kink - ($youngmax+$youngmin)/2  // factor to multiply kink estimate by to get tau: 8
ren wt _wt

set seed 2039458

forvalues y=1/3 {
  local years   : word `y' of 1995,1995 2005,2019 1995,2019
  local yearname: word `y' of 1995      Post-1995    All   
  replace       old = inrange(age74,      $oldmin      ,$oldmax) / placscale  // trick: divide placebo treatment by 10.5/6.5 in order to multiply coefficients by that, to compare to experiment
  replace reallyold = inrange(age74,$reallyoldmin,$reallyoldmax)

  sum _wt if inrange(year,`years'), detail
  cap drop wt
  gen double wt = min(_wt, r(p50)+4*(r(p75)-r(p25)))  // clip extreme weights to median + 4 * IQR (Potter and Zheng 2015)

  foreach depvars in yeduc "part lhwage" `=cond(`y'==3,`""yeducp primary""',"")' "lhwagep lhwages" {
    cap erase "Public/Output/RF`yearname' `depvars'.rtf"
    forvalues c=1(-1)0 {  // control sets, 0=none 1=minimal 2=minimal logged 3=intermediate 4=full
      eststo clear
      foreach depvar in `depvars' {
        foreach new in `=cond(`y'==1 & inlist("`depvar'","yeduc","lhwage"), `""""', "")' new {
          local controls: word `=`c'+1' of "" ch71`new' lch71`new' "ch71`new' en71`new'" "ch71`new' en71`new' wsppc"
          local seed `c(seed)'  // save to give weighted & unweighted Hausman bootstraps same DGPs
          foreach wt in 1 `=cond("`new'"=="","","wt")' {
            reg `depvar' 1.young#c.nin`new' ib1974.birthyr##c.(`controls') i.birthpl`new' i.year [pw=`wt'] if inrange(year,`years') & (old | young)
            est store exp
            scalar N`depvar'`new'`wt'exp = e(N)  // save to tack to the bottom of the kink results

            * bootstrap distribution, for Hausman test of weighted vs unweighted
            scalar b`depvar'`new'`wt'exp = _b[1.young#c.nin`new']
            boottest 1.young#c.nin`new', seed(`seed') reps(9999) noci nonull cluster(birthpl`new') svmat(numer)
            mata bs`wt'exp = st_matrix("r(dist)")

            cap noi reg `depvar' c.old#c.nin`new' ib1974.birthyr##c.(`controls') i.birthpl`new' i.year [iw=`wt'] if inrange(year,`years') & (old | reallyold)
            if _rc {
              est restore exp
              eststo: xlincom Experiment = 1.young#c.nin`new', post
              scalar N`depvar'`new'`wt'plac = .
            }
            else {
              est sto placebo
              qui count if e(sample)
              scalar N`depvar'`new'`wt'plac = r(N)  // save to tack to the bottom of the kink results

              * bootstrap distribution, for Hausman test of weighted vs unweighted
              scalar b`depvar'`new'`wt'plac = _b[c.old#c.nin`new']
              boottest c.old#c.nin`new', seed(`seed') reps(9999) noci nonull cluster(birthpl`new') svmat(numer)
              mata bs`wt'plac = st_matrix("r(dist)")

              reg `depvar' 1.young#c.nin`new' ib1974.birthyr##c.(`controls') i.birthpl`new' i.year [iw=`wt'] if inrange(year,`years') & (old | young)
              suest . placebo, `=cond("`new'"=="","","cluster(birthplnew)")'
              eststo: xlincom (Experiment = [_LAST_mean]1.young#c.nin`new') ///
                              (Placebo    = [placebo_mean]c.old#c.nin`new') ///
                              (Difference = [_LAST_mean]1.young#c.nin`new' - [placebo_mean]c.old#c.nin`new'), post
            }
          }
          if "`new'"!="" {  // add Hausman p values in the form of another estimation result
            mata st_numscalar("Vexp", variance(bs1exp - bswtexp))  // bootstrap-based Hausman χ²(1) stat. (Cameron and Travedi 2005, p. 378)
            mat χ²p = chi2tail(1, (b`depvar'`new'1exp - b`depvar'`new'wtexp)^2 / Vexp)
            if N`depvar'`new'wtplac < . {
              mata st_numscalar("Vplac", variance(bs1plac - bswtplac))
              mata st_numscalar("Vdiff", variance(bs1exp - bswtexp - (bs1plac - bswtplac)))
              mat χ²p = χ²p, chi2tail(1, (b`depvar'`new'1plac - b`depvar'`new'wtplac)^2 / Vplac), chi2tail(1, (b`depvar'`new'1exp - b`depvar'`new'wtexp - (b`depvar'`new'1plac - b`depvar'`new'wtplac))^2 / Vdiff)
              mat colnames χ²p = Experiment Placebo Difference
            }
            else mat colnames χ²p = Experiment
            eststo: myestpost χ²p  // fake estimation result for esttab to include, with Hausman p values for unweighted vs weighted
          }
        }
      }
      esttab using "Public/Output/RF`yearname' `depvars'.rtf", append b(a2) se(a2) title(Controls: `controls') eqlabels(,none) nostar nolines nonotes nomtitles noeqlines nogap nonumber msign("–") noobs fonttbl(\f0\fnil $font;)

      eststo clear
      foreach depvar in `depvars' {
        foreach new in `=cond(`y'==1 & inlist("`depvar'","yeduc","lhwage"), `""""', "")' new {
          local controls: word `=`c'+1' of "" ch71`new' lch71`new' "ch71`new' en71`new'" "ch71`new' en71`new' wsppc"
          local seed `c(seed)'  // save to give weighted & unweighted, experiment and placebo, Hausman bootstraps same DGPs
          foreach wt in 1 `=cond("`new'"=="","","wt")' {
            * event study
            reghdfejl `depvar' ibn.birthyr#c.nin`new' [pw=`wt'] if abs(t1)<11 & inrange(year,`years'), a(birthpl`new' birthyr##c.(`controls') year) cluster(birthpl`new')
            mata dots = st_matrix("e(b)")'
            coefplot, keep(*.birthyr#c.nin`new') omitted rename(([0-9]+)[ob]?.birthyr#c.nin`new' = \1, regex) vertical at(_coef, transform(1974 - @)) xscale(reverse) msym(smcircle) msize(small) plotregion(lstyle(none)) gen replace
            global graph `r(graph)'

            * spline fit
            reghdfejl `depvar' c.t?#c.nin`new' birthyr##c.(`controls') i.year [pw=`wt'] if e(sample), a(birthpl`new') cluster(birthpl`new')
            mata splinefit = `=_b[c.t1#c.nin`new']' * (-10::10) + `=_b[c.t2#c.nin`new']' * (J(10,1,0) \ 0::10)
            mata st_numscalar("splineshift", mean(dots - splinefit))
            local splinefn `=splineshift - _b[c.t1#c.nin`new'] * 10' `=age74kink+10' `=splineshift' `=age74kink' `=splineshift + _b[c.t1#c.nin`new'] * 10 + _b[c.t2#c.nin`new'] * 10' `=age74kink-10'

            * bootstrapping for Hausman test
            scalar b`depvar'`new'`wt' = _b[c.t2#c.nin`new']
            boottest c.t2#c.nin`new', seed(`seed') reps(9999) noci nonull cluster(birthpl`new') svmat(numer)
            mata bs`wt' = st_matrix("r(dist)")

            eststo, addscalar(Nexp N`depvar'`new'`wt'exp Nplac N`depvar'`new'`wt'plac): xlincom Kink = tauscale * _b[t2#c.nin`new'], post
            if "`new'"!="" {
              estadd local corrected ✓              
              estadd local clustered ✓              
            }
            if "`wt'"!="1" estadd local weights ✓

            local caption: display "kink = " (_b[Kink]<0)*"{&minus}" %4.3f abs(_b[Kink]) " (" %4.3f abs(_se[Kink]) ")"
            $graph || scatteri 0 $age74minplot, mstyle(p1) msym(smcircle) msize(small) ///  // zero for base year
                   || scatteri `splinefn', lcolor(maroon) lwidth(medium) mstyle(p1) msym(diamond) msize(small) mcolor(maroon) lpat(solid) recast(connected) ///
                   xlab($age74minplot `=age74kink' $age74maxplot, nogrid) plotregion(margin(0 1 0 0)) `=cond("`wt'"=="1",`"fxsize(`=cond(wordcount("`depvars'")==1,95,87)')"',"yscale(off)")' ///
                   `=cond(`c'==1, `"xlab(, nolab notick nogrid) xscale(off fill) title(`=cond("`wt'"=="1","Unweighted","Weighted")')"', "")' ///
                      name(RF`wt', replace) nodraw ///
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
        graph combine RF1 RFwt, name(`depvar', replace) rows(1) graphregion(margin(zero)) imargin(1 1 0 0) ycommon nodraw `=cond(`c'==1 & wordcount("`depvars'")>1, `"t1title("`:var label `depvar''", size(small))"', "")'
      }
      esttab using "Public/Output/RF`yearname' `depvars'.rtf", append b(a2) se(a2) nostar nolines nonotes noeqlines nomtitles nogap nonumber noobs msign("–") fonttbl(\f0\fnil $font;) ///
          `=cond(`c', "", `"stat(corrected clustered weights Nexp Nplac N, label("Data corrections" Clustered Weighted "Experiment N" "Placebo N" "Kink N") fmt(%-1s %-1s %-1s %10.0fc %10.0fc %10.0fc))"')'

      local t1title: word `=`c'+1' of "Without the extra controls" "With controls based on number of children"
      graph combine `depvars', rows(1) l1title(`t1title', size(vsmall)) graphregion(margin(zero)) name(RFctl`c', replace)
    }
    graph combine RFctl1 RFctl0, cols(1) imargin(0 0 1 1) xsize(`=1+4*wordcount("`depvars'")') ysize(5.5) graphregion(margin(zero)) b1title(Age in 1974, xoffset(4) size(vsmall)) altshrink iscale(*3)
    graph export "Public/Output/RF`yearname' `depvars' spline.png", replace width(2000)
  }
}
restore
}


***
*** returns to schooling
***
{
preserve
keep if inrange(age74,2,24) & inlist(year,1995,2005,2011,2012,2013,2014)
gen t1 = age74kink - age74
gen t2 = max(0,t1)

replace young = . if !(young | old)  // to restrict young-old samples to those 2 groups
set seed 230498257

ren wt _wt

foreach edvar in yeduc primary {
  forvalues y=1/3 {
    local years   : word `y' of 1995,1995 2005,2014 1995,2014
    local yearname: word `y' of 1995      Post-1995    All   

    sum _wt if inrange(year,`years'), detail
    cap drop wt
    gen wt = min(_wt, r(p50)+4*(r(p75)-r(p25)))  // clip extreme weights to median + 4 * IQR (Potter and Zheng 2015)

    cap erase "Public/Output/TSLS`edvar' `yearname'.rtf"
    cap erase "Public/Output/TSLS`edvar' `yearname' by birth year.rtf"
    forvalues c=1(-1)0 /*1/4*/ {
      local controls: word `=`c'+1' of "" ch71new lch71new /*"ch71new en71new"*/ "ch71new en71new wsppc"
      forvalues d=1/2 {
        local depvar: word `d' of part lhwage
        forvalues i=1/3 {  // 1=by young/old; 2=by birth year; 3=kink
          local insts : word `i' of 1.young#c.ninnew dum#c.ninnew c.t2#c.ninnew
          foreach wt in 1 wt {
            eststo OLS`depvar'c`c'w`wt': reghdfejl `depvar' `edvar' [pw=`wt'] if inrange(year,`years'), cluster(birthplnew) a(birthplnew birthyr##c.(`controls') year) nosamp

            reghdfejl `depvar' (`edvar' = `insts') `=cond(`i'==3,"c.t1#c.ninnew","")' birthyr##c.(`controls') i.year [pw=`wt'] if inrange(year,`years'), cluster(birthplnew) a(birthplnew)
            boottest, ar reps(99999) gridmin(-.45) gridmax(1.1) julia format(%4.2f) ///
                      graphopt(xlab(-.4(.4).8, nogrid `=cond(`c', "nolab", "")') ylab(.05 .2(.2)1, nogrid) xline(0) lwidth(thin) nodraw ///
                      plotregion(lstyle(none)) xtitle("") ytitle("") `=cond("`wt'"=="1" & `i'==1 & `d'==1,"","yscale(off)")' `=cond(`c'==0,"",`"title(`=cond("`wt'"=="1", "Unweighted", "Weighted")')"')') ///
                      graphname(`depvar'`i'w`wt', replace)
            estadd local CIstr "`r(CIstr)'"
            estadd local corrected ✓
            estadd local clustered ✓
            if "`wt'"!="1" estadd local weights ✓
            if `i'==3 estadd local trends ✓
            eststo TSLS`depvar'c`c'`i'w`wt'
          }
        }
        graph combine `depvar'1w1 `depvar'1wwt, rows(1) imargin(1 0 0 0) `=cond(`c'==1, "title(Instrument by young/old, size(medium))", "")' name(g1`depvar', replace) nodraw
        graph combine `depvar'3w1 `depvar'3wwt, rows(1) imargin(1 0 0 0) `=cond(`c'==1, "title(Kink instrument, size(medium))"        , "")' name(g3`depvar', replace) nodraw
      }
      esttab /*OLS`depvar'c`c'w* */ TSLSpartc`c'1w* TSLSpartc`c'3w* TSLSlhwagec`c'1w* TSLSlhwagec`c'3w* ///
             using "Public/Output/TSLS`edvar' `yearname'.rtf", append ///
             keep(`edvar') b(a2) se(a2) msign("–") nonotes nonumber nogaps nomtitles nostar fonttbl(\f0\fnil $font;) ///
             stat(CIstr widstat `=cond(`c',"","corrected clustered weights trends N")' /*jp*/, labels("Bootstrap CI" "KP F" `=cond(`c',"",`""Data corrections" Clustered Weighted Observations "Pre-trend control""')' /*"Hansen p"*/) fmt(%-1s a2 `=cond(`c',"","%-1s %-1s %~1s %~1s %11.0gc")') /*a2*/)

      esttab TSLSpartc`c'2w* TSLSlhwagec`c'2w* ///  // Instruments by birth year: not in main table
             using "Public/Output/TSLS`edvar' `yearname' by birth year.rtf", append ///
             keep(`edvar') b(a2) se(a2) msign("–") nonotes nonumber nogaps nomtitles nostar fonttbl(\f0\fnil $font;) ///
             stat(CIstr widstat `=cond(`c',"","corrected clustered weights trends N")' /*jp*/, labels("Bootstrap CI" "KP F" `=cond(`c',"",`""Data corrections" Clustered Weighted Observations "Pre-trend control""')' /*"Hansen p"*/) fmt(%-1s a2 `=cond(`c',"","%-1s %-1s %~1s %~1s %11.0gc")') /*a2*/)

      graph combine g1part   /*g2part*/   g3part  , imargin(1 1 0 0) iscale(1) name(part  , replace) `=cond(`c'==1,"title(Employment     )","")' nodraw
      graph combine g1lhwage /*g2lheage*/ g3lhwage, imargin(1 1 0 0) iscale(1) name(lhwage, replace) `=cond(`c'==1,"title(Log hourly wage)","")' nodraw
      graph combine part lhwage, name(`edvar'c`c'2SLSy`y', replace)
    }
    graph combine `edvar'c12SLSy`y' `edvar'c02SLSy`y', cols(1) ycommon b1title(Coefficient on `=lower("`:var label `edvar''")', size(small)) graphregion(margin(zero)) xsize(8.5) ysize(5) name(TSLS`edvar'y`y', replace) imargin(0 0 1 0) iscale(*1.3)
    graph export "Public/Output/TSLS`edvar' `yearname'.png", replace width(2000)
  }
}
restore
}


***
*** Jakiela diagnostic
***
{
preserve
keep if year==1995 & (young | old)
xi i.young|ninnew

cap program drop hetcheck
program define hetcheck
  qui {
    preserve
    regress _IyouXninne_1 i.birthyr i.birthplnew i.birthyr#c.ch71new [aw=wt] if `1'<.
    predict _T if e(sample), resid
    regress `1'      i.birthyr i.birthplnew i.birthyr#c.ch71new [aw=wt]
    predict _Y if e(sample), resid

    eststo untreated: reg _Y _T if !recpnew [aw=wt], nocons
    eststo   treated: reg _Y _T if  recpnew [aw=wt], nocons
    noi suest untreated treated, cluster(birthplnew)
    noi test [untreated_mean]_T = [treated_mean]_T  // test that two slopes are same, as reported in text

    local label = lower("`:var label `1''")
    collapse T=_IyouXninne_1 _T _Y (rawsum) wt [aw=wt], by(birthyr birthplnew)

    scatter _Y _T if T==0, msym(Oh) mcolor(%20) mlwidth(medthick) || ///
    scatter _Y _T if T!=0, msym(Oh) mcolor(%20) mlwidth(medthick) || ///
    lfit    _Y _T if T==0 [aw=wt], pstyle(p1) || ///
    lfit    _Y _T if T!=0 [aw=wt], pstyle(p2) || ///
    lpoly   _Y _T if T==0 [aw=wt], bw(.5) pstyle(p1) || ///
    lpoly   _Y _T if T!=0 [aw=wt], pstyle(p2) bw(.5) ///
      legend(order(2 1) cols(1) label(2 "High treatment") label(1 "Low treatment") ring(0) pos(8) region(style(none))) ///
      scheme(plottig) xtitle(Residualized treatment) ytitle(Residualized `label') name(hetcheck`1', replace)
    restore
  }
end
foreach depvar in yeduc lhwage {
  hetcheck `depvar'
}
grc1leg2 hetcheckyeduc hetchecklhwage, lrows(1) imargin(2 2 0 0) graphregion(margin(zero)) 
graph export "Public/Output/hetcheck.png", replace width(2680) height(1552)
restore
}


* Duflo (2001), Figure 2
{
preserve
keep if inrange(age74,2,30)

forvalues g=1/20 {
  gen byte S`g' = yeduc>=`g'
}

ren wt _wt
forvalues s=1/4 {
  local new        : word `s' of "" new new new
  local years      : word `s' of 1995,1995 1995,2019 1995,2019 1995,2019
  local cluster    : word `s' of "" birthpl`new' birthpl`new' birthpl`new'
  local wt         : word `s' of 1 wt wt wt wt
  local treatvar   : word `s' of recp nin nin nin
  local control    : word `s' of "" "" "" c.t1#c.`treatvar'`new'
  local conditional: word `s' of 0 0 1 1
  local title      : word `s' of Original "+ corrected, clustered, weighted, continuous treatment, all years with data" "+ conditional on attending previous grade" "+ kink model"
  local maxg       : word `s' of 20 20 14 14

  forvalues e=1/2 {
    local trial   : word `e' of Experiment Placebo
    local oldvar  : word `e' of old reallyold
    local youngvar: word `e' of young old
    local kinkpt  : word `e' of 12 18
    local rescale : word `e' of 1 `=10.5/6.5'

    local if      : word `s' of "`oldvar' | `youngvar'" "`oldvar' | `youngvar'" "`oldvar' | `youngvar'" abs(t1)<=10
    local timevar : word `s' of `youngvar' `youngvar' `youngvar' t2
    
    cap drop t?
    gen t1 = `kinkpt' - age74
    gen t2 = max(0,t1)
  
    cap drop wt
    sum _wt if `if' & inrange(year,`years'), detail
    gen double wt = min(_wt, r(p50)+4*(r(p75)-r(p25)))

    local ests
    forvalues g=1/`maxg' {
      local ests `ests' est`g' \
      eststo est`g': reghdfejl S`g' c.`timevar'#c.`treatvar'`new' `control' [aw=`wt'] if inrange(year,`years') & (`if') `=cond(`conditional' & `g'>1,"& S`=`g'-1'","")', a(birthpl`new' birthyr year) cluster(`cluster')
      estadd mat grade = J(1, colsof(e(b)), `g'): est`g'
    }
    coefplot (`ests'), keep(c.`timevar'#c.`treatvar'`new') rescale(`rescale') omitted at(grade) yline(0, lpat(solid) lcolor(gs8)) plotregion(lstyle(none)) xscale(range(1 20)) xlab(1/20) `=cond(`e'==1,"fxsize(51)","yscale(off)")' `=cond(`s'==4,"","xscale(noline) xlab(,nolab notick)")' nodraw name(g`e', replace)
  }
  graph combine g1 g2, rows(1) imargin(small) ycommon title("`title'", pos(11) span size(medsmall)) `=cond(`s'==4,"fysize(30)","")' name(r`s', replace) nodraw
}
graph combine r1 r2 r3 r4, cols(1) graphregion(margin(zero)) imargin(small) xcommon xsize(6.5) ysize(8) t1title("Experiment                                   Placebo", size(small)) b1title(Years of schooling, size(small) margin(zero)) name(DIDinCDF, replace)
graph export Public/Output/DIDinCDF.png, replace width(4000)

restore
}


* kinks in schooling continuation

{
preserve
keep if inrange(age74,-3,22) & year>1995  // in 1995 data some cohorts still children

gen byte S1 = yeduc>=1
forvalues g=2/6 {
  gen byte S`g' = yeduc>=`g' if yeduc>=`g'-1
}

ren wt _wt
sum _wt, detail
cap drop wt
gen double wt = min(_wt, r(p50)+4*(r(p75)-r(p25)))  // clip extreme weights to median + 4 * IQR (Potter and Zheng 2015)

local plots
forvalues g=1/6 {
  local plots `plots' S`g'
  
  cap drop t?
  scalar _age74kink   = age74kink - 6 + `g'
  global age74minplot = _age74kink - 10
  global age74maxplot = _age74kink + 10

  gen t1 = _age74kink - age74  // expected kink in continuation rate is at lower ages for earlier grades
  gen t2 = max(0,t1)

  * event study
  reghdfejl S`g' ibn.birthyr#c.ninnew [pw=wt] if abs(t1)<11, a(birthplnew birthyr year) cluster(birthplnew)
  mata dots = st_matrix("e(b)")'
  coefplot, keep(*.birthyr#c.ninnew) omitted rename(([0-9]+)[ob]?.birthyr#c.ninnew = \1, regex) vertical at(_coef, transform(1974 - @)) xscale(reverse) msym(smcircle) msize(small) plotregion(lstyle(none)) gen replace
  global graph `r(graph)'

  * spline fit
  reghdfejl S`g' c.t?#c.ninnew [pw=wt] if e(sample), a(birthplnew birthyr year) cluster(birthplnew)
  mata splinefit = `=_b[c.t1#c.ninnew]' * (-10::10) + `=_b[c.t2#c.ninnew]' * (J(10,1,0) \ 0::10)
  mata st_numscalar("splineshift", mean(dots - splinefit))
  local splinefn `=splineshift - _b[c.t1#c.ninnew] * 10' `=_age74kink+10' `=splineshift' `=_age74kink' `=splineshift + _b[c.t1#c.ninnew] * 10 + _b[c.t2#c.ninnew] * 10' `=_age74kink-10'

  local caption: display "kink = " (_b[t2#c.ninnew]<0)*"{&minus}" %5.4f 8*abs(_b[t2#c.ninnew]) " (" %5.4f 8*abs(_se[t2#c.ninnew]) ")"
  $graph || scatteri 0 $age74minplot, mstyle(p1) msym(smcircle) msize(small) ///  // zero for base year
         || scatteri `splinefn', lcolor(maroon) lwidth(medium) mstyle(p1) msym(diamond) msize(medium) mcolor(maroon) lpat(solid) recast(connected) ///
         xlab($age74minplot `=_age74kink' $age74maxplot, nogrid) plotregion(margin(zero)) ///
         `=cond(`g'<6, `"xlab(, nolab notick nogrid) xscale(off fill)"',"xlab(-3/22)")' ///
            name(S`g', replace) nodraw ///
         || scatteri 0 0, msymbol(none) xaxis(2) yaxis(2) xscale(axis(2) off) yscale(axis(2) off) ///  // fake plot to set up extra axes with range [-1,1] for placing text
              text(-.75 .9 "`caption'", xaxis(2) yaxis(2) place(w) color(black) size(medlarge)) title(Grade `g')
}
graph combine `plots', cols(1) xcommon imargin(0 0 1 1) xsize(4) ysize(7) graphregion(margin(zero)) b1title(Age in 1974, xoffset(4) size(vsmall)) name(CDFshiftkink, replace)
graph export Public/Output/CDFshiftkink.png, replace width(2000)
restore
}


***
*** CIC
***
{
cap program drop mycic
program define mycic
  syntax [pw/]
  preserve  // nested preserve, which is why code is in this subprogram
  forvalues y=1/3 {
    local years : word `y' of 1995,1995 2005,2014 1995,2014
    if "`exp'"!="1" {
      sum _wt if inrange(year,`years'), detail
      replace wt = min(_wt, r(p50)+4*(r(p75)-r(p25)))  // clip extreme weights to median + 4 * IQR (Potter and Zheng 2015)
    }
    eststo cicw`exp'y`y': cic lhwage _Ibirthyr* _Ibirthpl* _Iyear* `=cond("`exp'"=="1","","[pw=`exp']")' if inrange(year,`years'), group(recpnew) time(young) reps(1000)
    foreach stat in `:rownames e(tests)' {
      estadd scalar `stat' = e(tests)["`stat'",2], replace
    }
  }
  coefplot cicw`exp'y1 cicw`exp'y2 cicw`exp'y3, scheme(plottig) ylabel(1 "10" 2 "20" 3 "30" 4 "40" 5 "50" 6 "60" 7 "70" 8 "80" 9 "90") `=cond("`exp'"=="1","ytitle(Percentile)","")' ///
    legend(rowgap(zero) lab(11 "1995") lab(22 "2011-14") lab(33 "All") pos(4) region(margin(zero)) bmargin(1 0 0 0) size(7pt)) ///
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
keep if (young | old) & inlist(year,1995,2011,2012,2013,2014)
xi i.year i.birthyr i.birthplnew
set seed 30948573
cap erase Public/Output/cic.rtf
gen _wt = wt
foreach wt in 1 wt {
  mycic [pw=`wt']
}
grc1leg2 cicw1 cicwwt, rows(1) imargin(zero) xsize(5.5) ysize(4)
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

log close
