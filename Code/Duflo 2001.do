* dependencies: ivreg2, xtivreg2, ranktest, ftools, reghdfe, boottest, estout, coefplot, blindschemes, palettes, moremata, cmp, cic, and qrprocess
* all are from SSC except the last two are from https://sites.google.com/site/blaisemelly/home/computer-programs/cic_stata

global source95 NBER  // source for SUPAS 1995: should be NBER or IPUMS

cap set scheme plotplain
cap set processors 8
graph set window fontface Cambria
est drop _all

cap cd "D:\OneDrive\Documents\Work\Clients & prospects\GiveWell\Education\Duflo 2001"
cap cd /disk/homedirs/nber/feenberg/roodman

cap log close
cap log using Public\Output\duflo2001, text replace


***
*** Data prep
***
{
  cap noi odbc load, clear dsn("Duflo 2001 old") table("Regency-level vars")
  if !_rc {
    gen nen71new = 1 - en71new
    gen nen71newish = 1 - en71newish
    label var birthpl "Birth province & regency code, 1995"
    label var nin "Number of new schools, Duflo"
    label var recp "High treatment dummy, Duflo"
    label var ch71 "Number of children 5-14, 1971, Duflo"
    label var en71 "Population enrollment rate, 1971, Duflo"
    label var wsppc "Water & sanitation spending per capita, Duflo"
    label var dens71 "Population density, 1971, Duflo"
    label var dens71 "Population density, 1971, Duflo"
    label var moldyed "Average education among old, Duflo"
    label var birthlat "Birth regency centroid latitude"
    label var birthlong "Birth regency centroid longitude"
    label var Schools73new "Number of new schools, 1973/74"
    label var Schools74new "Number of new schools, 1974/75"
    label var Schools75new "Number of new schools, 1975/76"
    label var Schools76new "Number of new schools, 1976/77"
    label var Schools77new "Number of new schools, 1977/78"
    label var Schools78new "Number of new schools, 1978/79"
    label var pop71new "Regency population, 1971"
    label var ch71new "Population 5-14, 1971"
    label var totinnew "Number of new schools"
    label var ninnew "Number of new schools per 5-14 population"
    label var nch71new "Population 5+, 1971, same source as for attendance"
    label var atsc71new "Population 5+ attending school, 1971"
    label var en71newish "Population enrollment rate, 1971, ch71new denominator"
    label var en71new "Population 5+ enrollment rate, 1971, nch71new denominator"
    label var nen71newish "Population non-enrollment rate, 1971, ch71new denominator"
    label var nen71new "Population 5+ non-enrollment rate, 1971, nch71new denominator"
    saveold "Public\Regency-level vars\Regency-level vars", replace ver(11)
  }
  else use "Public\Regency-level vars\Regency-level vars", clear

  scatter ninnew nen71new, msym(Oh) || lfit ninnew nen71new, legend(off) scheme(plottig) xtitle("Non-enrollment rate, ages 5 and up, 1971") ytitle("Planned new schools 1973/74–78/79 per 1,000 children") graphregion(margin(zero)) name(targeting, replace)
  graph save Public\Output\targeting, replace

  gen ninnew7374 = (Schools73new + Schools74new)/ch71
  gen ninnew7576 = (Schools75new + Schools76new)/ch71
  gen ninnew7778 = (Schools77new + Schools78new)/ch71
  corr nen71new ninnew*
  

  * post-1995 data

  if "$source95" != "NBER" {
    #delimit;
    odbc load, clear dsn("Duflo 2001 old") exec("
      SELECT YEAR as year, RELATE as relate, 2-URBAN as urban, YRSCHOOL, EDUCATT, BIRTHYR as birthyr, PERWT as wt, INDGEN as indgen, OCC as occ, CLASSWK as classwk, BPLPROV as birthprov, BPLREG as birthpl, birthlat, birthlong
      FROM dbo.[IPUMS-based dataset]
      where year=2005 and Male=1 and BPLPROV<>96
    ");
    #delimit cr

    replace occ = floor(occ/10)  // 2005 4-digit classification => 1995 3-digit
    recode EDUCATT (11=1) (12=2) (13=3) (14=4) (15=5) (16 17=6) ///  // primary school
                   (21=7) (22=8) (23 27=9) (31=10) (32=11) (33=12)         (37=12) ///  // junior & senior
                                           (41=10) (42=11) (43=12) (44=13) (47=12) ///  //          senior, vocational
                   (51=13) (52 57=14) ///  // Diploma I/II
                   (61=13) (62=14) (63 67=15) ///  // Academy/Diploma III
                   (71=13) (72=14) (73=15) (74=16) (75=17) (76=18) (77=17) ///  // University/Diploma IV
                                                           (86=18) (87=17) ///  // Postgraduate-S2/S3 -- not broken out in 1995
                   (99=0) (.=0), gen(yeduc)
  //   replace yeduc = yeduc - 1 if EDUCATT==57 & occ==13  // 1 less year for Diploma I/II in teaching; go by occupation for lack of study field in 2005

    gen lwage = .
    gen lhwage = .
    keep  year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong
    order year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong
    compress
    save SUPAS05, replace

    * SAKERNAS
    #delimit;
    odbc load, clear dsn("Duflo 2001 old") exec("
      SELECT B5P1A, B5P6B, 2010-UMUR as birthyr, WEIGHT as wt, B5P12A+B5P12B as wage, ID1995A_BPLREG AS birthpl, floor(ID1995A_BPLREG/100) as birthprov
      FROM  [SAKERNAS 2010] LEFT OUTER JOIN
               [SUPAS 1995-2010 regency concordance] ON respl = [SUPAS 1995-2010 regency concordance].regy2010
      where JK=1 and B1P01<>31  --men outside Jakarta, copying Duflo (2004)
    ");
    #delimit cr

    replace wage = wage * .194  // adjust for inflation https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2010&locations=US%E2%89%A4%2FSEURLD-ID&start=1995
    gen hwage = wage / 4 / B5P6B
    gen lwage = ln(wage)
    gen lhwage = ln(hwage)
    gen byte relate = .
    gen byte indgen = .
    gen byte occ = .
    gen byte urban = .
    gen byte birthlat = .
    gen byte birthlong = .
    gen byte classwk = .
    gen int year = 2010

    recode B5P1A (1=0) (2=3) (3=6) ///  // no/some/completed primary school
                 (4 5=9) (6 7=12) ///  // junior & senior
                 (8=14) ///  // Diploma I/II
                 (9=15) ///  // Academy/Diploma III
                 (10=17) ///  // University/Diploma IV
                 (11=17), gen(yeduc)  // Postgraduate-S2/S3 -- not broken out in 1995
    keep  year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong  // "birth" fields are actually for residence
    order year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong
    compress
    save SAKERNAS10, replace
    
    * SUSENAS
    #delimit;
    odbc load, clear dsn("Duflo 2001 old") exec("
      SELECT year, c2.ID1995A_BPLREG AS birthpl, B5_TL1 AS birthprov, B5R15, B5R29, B5R28B, year - UMUR AS birthyr, FWT_TAHUN AS wt, HB as relate
      FROM  [SUPAS95-geo2_id1995 regency concordance] AS c2 RIGHT OUTER JOIN
               [SUPAS 1995-2010 regency concordance] AS c1 ON c2.ID1995A_BPLREG = c1.ID1995A_BPLREG RIGHT OUTER JOIN
               SUSENAS ON c1.regy2010 = birthpl
      WHERE JK = 1  --men
    ");
    #delimit cr

    replace B5R29 = B5R29 * (.194 / cond(year==2103, 1.169, 1.244))  // adjust for inflation, 2013/14->1995 https://data.worldbank.org/indicator/FP.CPI.TOTL?end=2013&locations=US%E2%89%A4%2FSEURLD-ID&start=1995
    gen hwage = B5R29 / 4 / B5R28B
    gen lwage = ln(B5R29)
    gen lhwage = ln(hwage)
    gen byte indgen = .
    gen byte occ = .
    gen byte classwk = .
    gen byte urban = .
    gen byte birthlat = .
    gen byte birthlong = .
  
    recode B5R15 (1 2 3 = 0) (4 5 6 = 6) (7 8 9 10 = 9) (11 12 13 14 = 12), gen(yeduc)  // years of schooling *before* each schooling level
    mat completionyears = 6, 6, 6, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 5  // max years in schooling levels; used when p518=8, meaning "completed"
    replace yeduc = yeduc + cond(B5R15<8, B5R15, completionyears[1, B5R15]) if B5R15>0

    keep  year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong  // "birth" fields are actually for residence
    order year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong
    save SUSENAS1314, replace
  }


  * 1995 data

  if "$source95"=="IPUMS" {
    odbc load, clear dsn("Duflo 2001 old") exec("select RELATE, YRSCHOOL, 2-URBAN as urban, ID1995A_BIRTHYR as birthyr, PERWT, BPLREG, BPLPROV, OCC, INDGEN, CLASSWK, SALCASH, SALGOODS, HRSWORK, birthlat, birthlong from [IPUMS-based dataset] where Male=1 and YEAR=1995")
    ren (YRSCHOOL RELATE PERWT BPLREG BPLPROV OCC INDGEN CLASSWK) (yeduc relate weight birthpl birthprov occ indgen classwk)  // match names in Duflo data set
    replace yeduc=11 if yeduc==93  // 9 people marked as being in 4th year of vocational senior high school; call it 11 years
    gen wage = SALCASH + SALGOODS
    gen hwage = wage / 4 / HRSWORK
    gen lwage = ln(wage / 1000)
    gen lhwage = ln(hwage)
  }
  else {
    if "$source95" == "NBER" {
      use supp95_04 if p503==1 & p509prop!=96, clear  // male, not born abroad
      ren (p606 p608 p504thn p509prop kp p502) (occ classwk birthyr birthprov urban relate)

      gen int birthpl = birthprov * 100 + p509kab
      destring urban, replace
      replace urban = 2 - urban  // place of residence, not birth

      gen wage = p609uang + p609brng
      gen double lwage = ln(wage)
      gen double lhwage = ln(wage / p605 / 4)
      
      recode p517 (1 = 0) (2 4 = 6) (3 5 = 9) (6 7 8 = 12) (99 . = 0), gen(yeduc)  // years of schooling *before* each schooling level
      mat completionyears = 6, 3, 3, 3, 3, 2, 3, 5  // max years in schooling levels; used when p518=8, meaning "completed"
      replace yeduc = yeduc + cond(p518<8, p518, completionyears[1, p517] - (p517==6 & p520==2)) if p518<.  // 1 less year for completing Diploma I/II in teaching
    }
    else {
      use inpresdata, clear
      replace birthpl = p509pro * 100 + p509kab
      ren (p608 p504thn p509pro) (classwk birthyr birthprov)
      gen occ = .
      replace urban = .  // place of residence, not birth
      gen byte relate = .
    }

    gen birthlat = .
    gen birthlong = .
    recode p607 (11 12 13 14 15 16 17 18 = 10) (21 22 23 24 25 26 = 20) ///  // recode to IPUMS INDGEN
                (31 32 33 34 35 36 37 38 39 = 30) (41=40) (42 43=40) (51 52=50) (61 62=60 ) (63 64 = 70) ///
                (71 72 73 74 75 = 80) (81 82 = 90) (83=111) (91=100) (92=40) (93=114) ///
                (94=114) (96=120) (98=999) (99=0), gen(indgen)
  }

  ren weight wt
  replace birthyr = birthyr + 1900
  gen int year = 1995

  keep  year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong
  order year relate urban yeduc birthyr wt indgen occ birthpl birthprov classwk lwage lhwage birthlat birthlong
  compress
  append using SUPAS05 SAKERNAS10 SUSENAS1314

  gen int dum = cond(birthyr<1962, 1900+100, birthyr) 
  gen byte age74 = 1974 - birthyr
  gen byte old = age74 <= 17 & age74 >= 12
  gen byte young = !old if (age74>=2 & age74 <= 6) | old  // young dummy missing outside of ages 2-6, 12-17, so will restrict samples
  gen byte reallyold = age74 <= 24 & age74 >= 18
  gen byte part = lhwage<. | (year==2005 & classwk==4)  // labor force participation
  gen byte primary = yeduc>=6  // completed primary school
  gen byte poor = inlist(birthprov, 33, 34, 35, 53, 72, 73, 74, 81, 82) & mod(birthpl, 100) < 70  // Poor dummy, Duflo (2001), table 6, note b

  recode birthpl (1472=1403) (1804=1803) (3275=3219) (5171=5103) (5271=5201) (7173=7103) (7271=7203) (8271=8203) (8104=8103), gen(birthplnew) // group new child regencies with parents
  replace birthpl = 7204 if birthpl==7271  // Duflo recoding

  merge m:1 birthpl using "Regency-level vars", nogen update

  bysort birthplnew: gen byte samp = _n==1
  reg totinnew ch71new if samp
  predict recpnew if totinnew<., resid
  replace recpnew = recpnew > 0 if recpnew<.
  drop samp

  xtset birthplnew

  regress lhwage c.age74##c.age74##c.age74##c.age74##(birthpl occ indgen urban) [aw=wt] if year==1995 // imputation regression
  est save Public\Output\IS, replace
  predict double IS if part  // Income Score
  gen double _lwage = cond(year==1995, lhwage, cond(year==2005, IS, cond(year==2010, lhwage, lwage)))

  label var yeduc "Years of education"
  label var primary "Finished primary school"
  label var part "Formal employment"
  label var lwage "Log monthly wages"
  label var lhwage "Log hourly wage"
  label var IS "Imputed log hourly wage"
  label var _lwage "Consolidated log wage variable"
}  // end data prep


***
*** cross-survey, regency-level correlations in key variables
***
{
preserve
gen _year = cond(year<2013, year, 2014)
collapse ninnew ch71new en71new pop71new year part yeduc primary lhwage lwage IS (rawsum) wt [aw=wt] if young<., by(young birthplnew _year)
gen _lwage = cond(inlist(year,1995,2010), lhwage, cond(year==2005, IS, lwage))
reshape wide ninnew ch71new en71new pop71new year part yeduc primary lhwage lwage IS _lwage wt, i(birthplnew young) j(_year)

foreach depvar in primary yeduc part lhwage IS lwage _lwage {
  graph matrix `depvar'*, scheme(plotplain) msym(Oh) msize(tiny) name(mat`depvar', replace)
  graph matrix `depvar'*, scheme(plotplain) msym(Oh) msize(tiny) name(matby`depvar', replace) by(young)
  pwcorr `depvar'*
  bysort young: pwcorr `depvar'*
}
restore
}


***
*** replicate most of original
***
{
preserve
keep if age74>=2 & age74<=24 & year==1995
xtset birthpl

* Table 3: DID
reg yeduc  young##recp                    if lhwage<. [aw=wt]
reg lhwage young##recp                                [aw=wt]
ivregress 2sls lhwage young recp (yeduc = young#recp) [aw=wt], small  // correct Wald DID estimator

reg yeduc  old##recp                              if (reallyold | old) & lhwage<. [aw=wt]
reg lhwage old##recp                              if  reallyold | old             [aw=wt]
ivregress 2sls lhwage old recp (yeduc = old#recp) if  reallyold | old             [aw=wt], small

* Table 4
xtreg yeduc  1.young#c.nin birthyr##c.ch71                     , fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71)              , fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71 wsppc)        , fe
xtreg yeduc  1.young#c.nin birthyr##c.ch71              if part, fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71)       if part, fe
xtreg yeduc  1.young#c.nin birthyr##c.(ch71 en71 wsppc) if part, fe
xtreg lhwage 1.young#c.nin birthyr##c.ch71                     , fe
xtreg lhwage 1.young#c.nin birthyr##c.(ch71 en71)              , fe
xtreg lhwage 1.young#c.nin birthyr##c.(ch71 en71 wsppc)        , fe

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
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71)                         , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if dens71<308           , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if dens71>308 & dens71<., fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if  poor                , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if !poor                , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if moldyed<=6.97        , fe
xtreg yeduc 1.young#c.nin birthyr##c.(ch71 en71) if moldyed> 6.97        , fe

* Table 7, panels A1-B1
xtreg   lhwage  yeduc                  birthyr##c.ch71             , fe
xtivreg lhwage (yeduc =     dum#c.nin) birthyr##c.ch71             , fe
xtivreg lhwage (yeduc = 1.young#c.nin) birthyr##c.ch71             , fe
xtreg   lhwage  yeduc                  birthyr##c.(ch71 en71)      , fe
xtivreg lhwage (yeduc =     dum#c.nin) birthyr##c.(ch71 en71)      , fe
xtivreg lhwage (yeduc = 1.young#c.nin) birthyr##c.(ch71 en71)      , fe
xtreg   lhwage  yeduc                  birthyr##c.(ch71 en71 wsppc), fe
xtivreg lhwage (yeduc =     dum#c.nin) birthyr##c.(ch71 en71 wsppc), fe
xtivreg lhwage (yeduc = 1.young#c.nin) birthyr##c.(ch71 en71 wsppc), fe

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
  eststo est`e': xtreg S`e' 1.young#1.recp i.age74, fe
}
coefplot e*, keep(1.young#1.recp) vertical ylab(-.04(.02).08) omitted

restore
}


* investigate scope for distortion from heterogeneous treatment effects
{
preserve
gen byte T = 1.young#c.ninnew
reghdfe T [pw=wt] if lhwage<. & year==1995 & (young | old), cluster(birthplnew) a(ib1974.birthyr##c.(ch71new) i.birthplnew) resid  // in context of reduced-form log hourly wage regressions
predict Te if e(sample), resid
scatter Te T  // instances of negative treatment after partialling out controls, when treatment is actually positive, are when treatment is small
restore
}


***
*** 2x2 DID
***
{
est drop _all
preserve
keep if age74>=2 & age74<=24 & year==1995 & lhwage<.

eststo Eyeduc : reg yeduc  young##recp                                [aw=wt]
eststo Elhwage: reg lhwage young##recp                                [aw=wt]
eststo EWald  : ivregress 2sls lhwage young recp (yeduc = young#recp) [aw=wt], small

eststo Pyeduc : reg yeduc  old##recp                              if reallyold | old [aw=wt]
eststo Plhwage: reg lhwage old##recp                              if reallyold | old [aw=wt]
eststo PWald  : ivregress 2sls lhwage old recp (yeduc = old#recp) if reallyold | old [aw=wt], small

eststo Eyeducnew : reg yeduc  young##recpnew                                   [aw=wt]
eststo Elhwagenew: reg lhwage young##recpnew                                   [aw=wt]
eststo EWaldnew  : ivregress 2sls lhwage young recpnew (yeduc = young#recpnew) [aw=wt], small

eststo Pyeducnew : reg yeduc  old##recpnew                                 if reallyold | old [aw=wt]
eststo Plhwagenew: reg lhwage old##recpnew                                 if reallyold | old [aw=wt]
eststo PWaldnew  : ivregress 2sls lhwage old recpnew (yeduc = old#recpnew) if reallyold | old [aw=wt], small

eststo Eyeducnewcl : reg yeduc  young##recpnew                                   [aw=wt], cluster(birthplnew)
eststo Elhwagenewcl: reg lhwage young##recpnew                                   [aw=wt], cluster(birthplnew)
eststo EWaldnewcl  : ivregress 2sls lhwage young recpnew (yeduc = young#recpnew) [aw=wt], cluster(birthplnew) small

eststo Pyeducnewcl : reg yeduc  old##recpnew                                 if reallyold | old [aw=wt], cluster(birthplnew)
eststo Plhwagenewcl: reg lhwage old##recpnew                                 if reallyold | old [aw=wt], cluster(birthplnew)
eststo PWaldnewcl  : ivregress 2sls lhwage old recpnew (yeduc = old#recpnew) if reallyold | old [aw=wt], cluster(birthplnew) small

* tests that experiment and control experiment yield different results
cmp (experiment: yeduc = young##recp) (placebo: yeduc = old##recp), ind("young | old" "old | reallyold") covariance(independent) nolr qui  // like suest but classical errors
test _b[experiment:1.young#1.recp] = _b[placebo:1.old#1.recp]
estadd scalar EPp = r(p): Pyeduc
cmp (experiment: lhwage = young##recp) (placebo: lhwage = old##recp), ind("young | old" "old | reallyold") cov(ind) nolr qui
test _b[experiment:1.young#1.recp] = _b[placebo:1.old#1.recp]
estadd scalar EPp = r(p): Plhwage

cmp (experiment: yeduc = young##recpnew) (placebo: yeduc = old##recpnew), ind("young | old" "old | reallyold") cov(ind) nolr qui
test _b[experiment:1.young#1.recpnew] = _b[placebo:1.old#1.recpnew]
estadd scalar EPp = r(p): Pyeducnew
cmp (experiment: lhwage = young##recpnew) (placebo: lhwage = old##recpnew), ind("young | old" "old | reallyold") cov(ind) nolr qui
test _b[experiment:1.young#1.recpnew] = _b[placebo:1.old#1.recpnew]
estadd scalar EPp = r(p): Plhwagenew

cmp (experiment: yeduc = young##recpnew) (placebo: yeduc = old##recpnew), ind("young | old" "old | reallyold") cov(ind) nolr qui cluster(birthplnew)
test _b[experiment:1.young#1.recpnew] = _b[placebo:1.old#1.recpnew]
estadd scalar EPpcl = r(p): Pyeducnew
cmp (experiment: lhwage = young##recpnew) (placebo: lhwage = old##recpnew), ind("young | old" "old | reallyold") cov(ind) nolr qui cluster(birthplnew)
test _b[experiment:1.young#1.recpnew] = _b[placebo:1.old#1.recpnew]
estadd scalar EPpcl = r(p): Plhwagenew

esttab Eyeduc*  using Public\Output\DID2x2.rtf, replace b(3) se(3) nogap nonotes nonumbers nomtitles noobs msign("–") fonttbl(\f0\fnil Cambria;) keep(DID) rename(1.young#1.recp DID 1.young#1.recpnew DID)
esttab Pyeduc*  using Public\Output\DID2x2.rtf, append  b(3) se(3) nogap nonotes nonumbers nomtitles noobs msign("–") fonttbl(\f0\fnil Cambria;) keep(DID) rename(1.old#1.recp DID 1.old#1.recpnew DID) stat(ECp ECpcl, fmt(2) layout(@ "[@]"))
esttab Elhwage* using Public\Output\DID2x2.rtf, append  b(3) se(3) nogap nonotes nonumbers nomtitles noobs msign("–") fonttbl(\f0\fnil Cambria;) keep(DID) rename(1.young#1.recp DID 1.young#1.recpnew DID)
esttab Plhwage* using Public\Output\DID2x2.rtf, append  b(3) se(3) nogap nonotes nonumbers nomtitles noobs msign("–") fonttbl(\f0\fnil Cambria;) keep(DID) rename(1.old#1.recp DID 1.old#1.recpnew DID) stat(ECp ECpcl, fmt(2) layout(@ "[@]"))
esttab EWald*   using Public\Output\DID2x2.rtf, append  b(3) se(3) nogap nonotes nonumbers nomtitles noobs msign("–") fonttbl(\f0\fnil Cambria;) keep(yeduc)
esttab PWald*   using Public\Output\DID2x2.rtf, append  b(3) se(3) nogap nonotes nonumbers nomtitles noobs msign("–") fonttbl(\f0\fnil Cambria;) keep(yeduc)
restore
}


***
*** enter weights as controls in unweighted regressions--Duflo (2001), table 5, cols 5 & 8, used in figure 3
***
{
areg yeduc  wt birthyr##c.(nin ch71 en71) if 24>=age74 & age74>=2 & year==1995 & lhwage<., a(birthpl) cluster(birthpl)
areg lhwage wt birthyr##c.(nin ch71 en71) if 24>=age74 & age74>=2 & year==1995           , a(birthpl) cluster(birthpl)
}


***
*** reduced form/OLS
***
{
cap erase Public\Output\RF.rtf
preserve
keep if young==1 | old | reallyold
forvalues y=1/4 {
  local years: word `y' of 1995 2005 2010 2013,2014
  local wagevar: word `y' of lhwage IS lhwage lwage
  local depvars : word `y' of "primary yeduc part lhwage" "primary yeduc part IS" "primary yeduc part lhwage" "primary yeduc lwage"
  local Ndepvars: word count `depvars'
  forvalues c=1/1 /*0/3*/ {  // control sets, from none to full
    local ests
    local controls: word `=`c'+1' of "" ch71new "ch71new en71new" "ch71new en71new wsppc"
    forvalues d=1/`Ndepvars' {
      local depvar: word `d' of `depvars'
      foreach wt in 1 wt {
        if `y'==1 {
          eststo RF`depvar'c`c'w`wt'y`y': cmp (experiment: `depvar' = 1.young#c.ninnew ib1974.birthyr##c.(`controls') i.birthplnew) ///
                                              (placebo   : `depvar' =   1.old#c.ninnew ib1974.birthyr##c.(`controls') i.birthplnew) ///
                                              [pw=`wt'] if ("`depvar'"=="part" | `wagevar'<.) & inlist(year,`years'), ind("young | old" "old | reallyold") cluster(birthplnew) cov(ind) qui
          test _b[experiment:1.young#c.ninnew] = _b[placebo:1.old#c.ninnew]
          estadd scalar EPp = r(p)
        }
        else {
          eststo RF`depvar'c`c'w`wt'y`y': areg `depvar' 1.young#c.ninnew ib1974.birthyr##c.(`controls') [pw=wt] if ("`depvar'"=="part" | `wagevar'<.) & inlist(year,`years') & young<., cluster(birthplnew) a(birthplnew)
        }
        local ests `ests' RF`depvar'c`c'w`wt'y`y'
      }
    }
    esttab `ests' using Public\Output\RF.rtf, append keep(*#c.ninnew) se nostar nolines nonotes noeqlines varwidth(22) nonumber msign("–") ///
       stat(N1 N2 EPp, label("Experiment observations" "Placebo observations" "Experiment=placebo (p)") fmt(%7.0fc %7.0fc %4.2f))
  }
}
restore
}


***
*** ~ Mincer 1974, chart 4.4
***
{
preserve
gen age = 1995 - birthyr
keep if age>=15 & age<=65 
gen potexp = max(0, age - yeduc - 7)
gen _yeduc = floor((yeduc - 1) / 3) + (yeduc==0) - (yeduc>18)

colorpalette viridis, n(6) range(.9 0) saturate(.5) nograph
twoway lpoly lhwage age if _yeduc==0 [aw=wt], bw(5) lcolor("`r(p1)'") lwidth(medium) || ///
       lpoly lhwage age if _yeduc==1 [aw=wt], bw(5) lcolor("`r(p2)'") lwidth(medium) || ///
       lpoly lhwage age if _yeduc==2 [aw=wt], bw(5) lcolor("`r(p3)'") lwidth(medium) || ///
       lpoly lhwage age if _yeduc==3 [aw=wt], bw(5) lcolor("`r(p4)'") lwidth(medium) || ///
       lpoly lhwage age if _yeduc==4 [aw=wt], bw(5) lcolor("`r(p5)'") lwidth(medium) || ///
       lpoly lhwage age if _yeduc==5 [aw=wt], bw(5) lcolor("`r(p6)'") lwidth(medium) || if year==1995 & age>=15 & age<=65, ///
       xtitle(Age) ytitle("Hourly wage (rupiah)") graphregion(margin(zero)) ///
       legend(on order(6 5 4 3 2 1) label(1 "0–3 years") label(2 "4–6 years")  label(3 "7–9 years") label(4 "10–12 years") label(5 "13–15 years") label(6 "16+ years of schooling") cols(1) pos(11) ring(0) margin(zero) region(margin(zero) style(none) lstyle(none))) ///
       ylab(`=ln(500)' "500" `=ln(1000)' "1,000" `=ln(2000)' "2,000" `=ln(3000)' "3,000" `=ln(4000)' "4,000", nogrid) xlab(15(10)65, nogrid) scheme(plottig) name(MincerAge, replace)
graph save "Public\Output\Mincer4.4 left.gph", replace

* for blog
graph set window fontface Montserrat
gen hwage = exp(lhwage)
table age _yeduc [aw=wt] if year==1995 & age>=15 & age<=65, stat(mean hwage)

colorpalette viridis, n(6) range(.8 0) saturate(1) nograph
twoway lpoly hwage age if _yeduc==0 [aw=wt], bw(5) lcolor("`r(p1)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==1 [aw=wt], bw(5) lcolor("`r(p2)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==2 [aw=wt], bw(5) lcolor("`r(p3)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==3 [aw=wt], bw(5) lcolor("`r(p4)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==4 [aw=wt], bw(5) lcolor("`r(p5)'") lwidth(medium) || ///
       lpoly hwage age if _yeduc==5 [aw=wt], bw(5) lcolor("`r(p6)'") lwidth(medium) || if year==1995 & age>=15 & age<=65, ///
       xtitle(Age, size(medium)) ytitle("Hourly wage (rupiah)", size(medium)) graphregion(margin(zero)) ///
       legend(on size(medium) order(6 5 4 3 2 1) label(1 "0–3 years") label(2 "4–6 years")  label(3 "7–9 years") label(4 "10–12 years") label(5 "13–15 years") label(6 "16+ years of schooling") cols(1) pos(11) ring(0) margin(zero) region(margin(zero) style(none) lstyle(none))) ///
       xlab(15(10)65, nogrid labsize(medium)) ylab(, labsize(medium)) scheme(plottig)
graph set window fontface Cambria
graph export "Public\Output\Mincer4.4 left.png", replace width(2680) height(1552)

colorpalette viridis, n(6) range(.9 0) saturate(.5) nograph
twoway lpoly lhwage potexp if _yeduc==0 [aw=wt], bw(5) lcolor("`r(p1)'") lwidth(medium) || ///
       lpoly lhwage potexp if _yeduc==1 [aw=wt], bw(5) lcolor("`r(p2)'") lwidth(medium) || ///
       lpoly lhwage potexp if _yeduc==2 [aw=wt], bw(5) lcolor("`r(p3)'") lwidth(medium) || ///
       lpoly lhwage potexp if _yeduc==3 [aw=wt], bw(5) lcolor("`r(p4)'") lwidth(medium) || ///
       lpoly lhwage potexp if _yeduc==4 [aw=wt], bw(5) lcolor("`r(p5)'") lwidth(medium) || ///
       lpoly lhwage potexp if _yeduc==5 [aw=wt], bw(5) lcolor("`r(p6)'") lwidth(medium) || if year==1995 & potexp<=45, ///
       xtitle(Years of potential experience) yscale(off fill) graphregion(margin(zero)) ///
       legend(off) ///
       xlab(0(5)45, nogrid) ylab(,nogrid) scheme(plottig) name(MincerExp, replace) nodraw
graph combine MincerAge MincerExp, imargin(0 0 0 0) ycommon xsize(7) ysize(4) iscale(*1.5) graphregion(margin(zero))
graph save Public\Output\Mincer4.4.gph, replace

eststo lhwage95 : reg lhwage c.age74##c.age74 c.yeduc##c.yeduc c.age74#c.yeduc [aw=wt] if year==1995, robust
eststo lhwage953: reg lhwage c.(age74 yeduc)##c.(age74 yeduc)##c.(age74 yeduc) [aw=wt] if year==1995, robust
eststo lhwage954: reg lhwage c.(age74 yeduc)##c.(age74 yeduc)##c.(age74 yeduc)##c.(age74 yeduc) [aw=wt] if year==1995, robust
eststo IS95     : reg IS     c.age74##c.age74 c.yeduc##c.yeduc c.age74#c.yeduc [aw=wt] if year==1995, robust
eststo IS05     : reg IS     c.age74##c.age74 c.yeduc##c.yeduc c.age74#c.yeduc [aw=wt] if year==2005, robust
esttab lhwage95 IS95 IS05 using "Public\Output\wage compression.rtf", replace b(a2) se(a2) nonumber nocons nostar stats(N, fmt(%7.0fc)) msign("–") nogaps fonttbl(\f0\fnil Cambria;)
esttab lhwage95 lhwage953 lhwage954, varwidth(30)  // quadratic vs cubic model, not reported in text

regress lhwage ibn.age age#c.yeduc if year==1995 [pw=wt], nocons cluster(birthplnew)  // Slopes of linear fits of log hourly wage to years of schooling, by age
coefplot, keep(*age#c.yeduc) rename(([0-9]+)[ob]?.age#c.yeduc = \1, regex) vertical omitted at(_coef) ///
          xline(23 45, lpat(dash)) text(0 23 "Age 2 in 1974", place(se)) text(0 45 "Age 24 in 1974", place(se)) ///
          xtitle(Age in 1995) graphregion(margin(zero)) xlab(15(10)65) name(gradlhwageyeduc1995, replace)

graph save Public\Output\gradlhwageyeduc1995, replace

eststo WSDlhwage95E : areg lhwage c.yeduc##c.age74 i.birthyr##c.ch71 [aw=wt] if year==1995 & young<., cluster(birthplnew) a(birthplnew)
eststo WSDlhwage95P : areg lhwage c.yeduc##c.age74 i.birthyr##c.ch71 [aw=wt] if year==1995 & (old | reallyold), cluster(birthplnew) a(birthplnew)
eststo WSDIS05     : areg IS     c.yeduc##c.age74 i.birthyr##c.ch71 [aw=wt] if year==2005 & young<., cluster(birthplnew) a(birthplnew)
eststo WSDlhwage10 : areg lhwage c.yeduc##c.age74 i.birthyr##c.ch71 [aw=wt] if year==2010 & young<., cluster(birthplnew) a(birthplnew)
eststo WSDlwage1314: areg lwage c.yeduc##c.age74 i.birthyr##c.ch71 [aw=wt] if inlist(year,2013,2014) & young<., cluster(birthplnew) a(birthplnew)
esttab WSDlhwage95E WSDlhwage95P WSDIS05 WSDlhwage10 WSDlwage1314 using "Public\Output\wage compression.rtf", replace keep(c.yeduc#c.age74 yeduc) se nonumber nocons nostar stats(N, fmt(%7.0fc)) msign("–") nogaps


restore
}

qui est dir
qui foreach est in `r(names)' {
  est restore `est'
  est save Public\Output\`est', replace
}


***
*** Figure with wage scale dilation and OLS reduced-form impacts, all follow-ups
***
{
preserve
drop _all
cap drop lab
gen lab = ""
coefplot (RFprimaryc1wwty1, rename(1.young#c.ninnew=1995)) (RFprimaryc1wwty2, rename(1.young#c.ninnew=2005)) (RFprimaryc1wwty3, rename(1.young#c.ninnew=2010)) (RFprimaryc1wwty4, rename(1.young#c.ninnew=2013–14)), bylabel(Primary school completion) || ///
         (RFyeducc1wwty1, rename(1.young#c.ninnew=1995)) (RFyeducc1wwty2, rename(1.young#c.ninnew=2005)) (RFyeducc1wwty3, rename(1.young#c.ninnew=2010)) (RFyeducc1wwty4, rename(1.young#c.ninnew=2013–14)), bylabel(Years of schooling) || ///
         (WSDlhwage95E, rename(c.yeduc#c.age74=1995)) (WSDIS05, rename(c.yeduc#c.age74=2005)) (WSDlhwage10, rename(c.yeduc#c.age74=2010)) (WSDlwage1314, rename(c.yeduc#c.age74=2013–14)), bylabel(Wage scale dilation) || ///
         (RFpartc1wwty1, rename(1.young#c.ninnew=1995)) (RFpartc1wwty2, rename(1.young#c.ninnew=2005)) (RFpartc1wwty3, rename(1.young#c.ninnew=2010)), bylabel(Wage sector participation) || ///
         (RFlhwagec1wwty1, rename(1.young#c.ninnew=1995)) (RFISc1wwty2, rename(1.young#c.ninnew=2005)) (RFlhwagec1wwty3, rename(1.young#c.ninnew=2010)) (RFlwagec1wwty4, rename(1.young#c.ninnew=2013–14)), bylabel(Log earnings) ///
           keep(c.yeduc#c.age74 1.young#c.ninnew) scheme(plottig) cismooth(n(20)) grid(none) mlab(lab) mlabpos(12) mlabcolor(black) ///
           byopts(xrescale legend(off) rows(2) holes(1) style(econ) iscale(*1.25)) ///
           ysize(4) xsize(7) addplot(scatteri .5 0, msym(none)) graphregion(margin(zero)) plotregion(color(gs14)) xlab(,labcolor(black)) ylab(,labcolor(black)) ///
           gen replace
local graph `r(graph)'
replace lab = subinstr(strofreal(__b, "%6.4f") + " (" + strofreal(__se, "%6.4f") + ")", "-", "−", .) if __b<.
`graph'
restore
}



***
*** spline fit demo with exact match to Duflo figure 1
***
{
preserve
keep if year==1995 & 24>=age74 & age74>=2

areg yeduc i(1950/1971)bn.birthyr#c.nin birthyr##c.(ch71 en71), absorb(birthpl)
mata dots = st_matrix("e(b)")'[|.\22|] \ 0
coefplot, keep(*.birthyr#c.nin) omitted rename(([0-9]+)[ob]?.birthyr#c.nin = \1, regex) vertical xlab(2(2)24) at(_coef, transform(1974 - @)) xscale(reverse) msym(smcircle) msize(small) gen replace
global graph `r(graph)'

gen t1 = 24 - age74
gen t2 = max(0, 12 - age74)

areg yeduc c.t?#c.nin  birthyr##c.(ch71 en71), absorb(birthpl)
mata segmentfit = `=_b[c.t1#c.nin]' * (24 :- (24::2)) + `=_b[c.t2#c.nin]' * (J(24-12,1,0) \ 12 :- (12::2))
mata st_numscalar("segmentshift", mean(dots - segmentfit))
local segmentfn `=segmentshift' 24 `=_b[c.t1#c.nin] * 12 + segmentshift' 12  `=_b[c.t1#c.nin] * 22 + _b[c.t2#c.nin] * 10 + segmentshift' 2

local kinkpt1  = segmentshift + _b[c.t1#c.nin] * (24 - 12)
local kinkpt2  = segmentshift + _b[c.t1#c.nin] * (24 -  2) + _b[c.t2#c.nin] * max(0, 12 - 2)
local kinkpt2b = segmentshift + _b[c.t1#c.nin] * (24 -  2)
$graph || scatteri  `segmentfn', recast(connected) lcolor(maroon) lwidth(medium) mstyle(p1) mcolor(maroon) msym(diamond) msize(small) ///
          ylab(, format(%3.1f) nogrid) xtitle(Age in 1974) xlab(2(2)24, nogrid) graphregion(margin(zero)) ///
          name(Fig1yeduc1995, replace) ///
       || scatteri 0 2, mstyle(p1) msym(smcircle) msize(small) /// // zero for base year
       || scatteri `kinkpt1' 12, msym(diamond) mcolor(maroon) ///
       || scatteri `kinkpt1' 12 `kinkpt2b' 2, recast(line) pstyle(p3) lcolor(gs8) /// 
       || scatteri `=.8*(`kinkpt2'-`kinkpt1')+`kinkpt1'' 4 `=.8*(`kinkpt2'-`kinkpt1')+`kinkpt1'' 3.7 `=.8*(`kinkpt2b'-`kinkpt1')+`kinkpt1'' 3.7 `=.8*(`kinkpt2b'-`kinkpt1')+`kinkpt1'' 4, recast(line) lcolor(maroon) lpat(solid) /// 
       || scatteri 0 0, msym(none) text(`=.8*((`kinkpt2'+`kinkpt2b')/2-`kinkpt1')+`kinkpt1'' 1.5 "{it:{&tau}}", place(c) color(maroon)) /// 
       || scatteri 0 0, msym(none) xaxis(2) yaxis(2) xscale(axis(2) off) yscale(axis(2) off) /// // fake plot to set up extra axes with range [-1,1] for placing text
            text(1 -1 "{it:{&tau}} = `:display %4.3f 8*_b[t2#c.nin]' (`:display %4.3f 8*_se[t2#c.nin]')", xaxis(2) yaxis(2) place(e) color(maroon))
graph save Public\Output\Fig1yeduc1995, replace


* with quadratic fit
areg yeduc c.t1##c.t1#c.nin i.birthyr##c.(ch71 en71), absorb(birthpl)
mata quadfit    = `=_b[c.t1#c.nin]' * (24 :- (24::2)) + `=_b[c.t1#c.t1#c.nin]' * (24 :- (24::2)):^2
mata st_numscalar("quadshift", mean(dots - quadfit))
local quadfn `=_b[c.t1#c.nin]' * (24 - x) + `=_b[c.t1#c.t1#c.nin]' * (24 - x)^2 + quadshift

$graph || scatteri  `segmentfn', recast(connected) lcolor(maroon) lwidth(medium) mstyle(p1) mcolor(maroon) msym(diamond) msize(small) ///
       || function y = `quadfn', pstyle(p3) lcolor(blue) lwidth(medium) range(2 24) ///
          ylab(, format(%3.1f) nogrid) xtitle(Age in 1974) xlab(2(2)24, nogrid) graphregion(margin(zero)) ///
          name(Fig1yeduc1995quad, replace) ///
       || scatteri 0 2, mstyle(p1) msym(smcircle) msize(small)  // zero for base year
graph save Public\Output\Fig1yeduc1995quad, replace

graph combine Fig1yeduc1995 Fig1yeduc1995quad, imargin(zero) graphregion(margin(zero)) xsize(7) ysize(4) iscale(*1.5) name(Fig1, replace) ycommon
graph save Public\Output\Fig1, replace
restore
}


***
*** piecewise-linear spline fits
***
{
preserve
keep if 24>=age74 & age74>=2
gen t1 = 24 - age74  // spline components -1974 + birth year
gen t2 = max(0, 12 - age74)  // 12-year-olds in '74 couldn't benefit

twoway scatteri 0 0, msym(none) yscale(off) ylab(,nogrid) nodraw xscale(off) xlab(,nogrid) name(hole1, replace)
twoway scatteri 0 0, msym(none) yscale(off) ylab(,nogrid) nodraw xscale(reverse) xlab(2(2)24,nogrid) xtitle("") name(hole2, replace)

forvalues c=1/1 /*0/3*/ {  // control sets, from none to full
  local controls: word `=`c'+1' of "" ch71new "ch71new en71new" "ch71new en71new wsppc"

  forvalues y=1/5 {
    local years   : word `y' of 1995 2005 2010 2013,2014 2005,2010,2013,2014
    local yearname: word `y' of "Survey: Intercensal, 1995" "Intercensal, 2005" "Labor, 2010" "Socioeconomic, 2013–14" "Post-1995"
    local depvars : word `y' of "primary yeduc part lwage lhwage IS" "primary yeduc part IS" "primary yeduc part lhwage" "primary yeduc lwage" "primary yeduc part _lwage"
    local Ndepvars: word count `depvars'

    foreach wt in 1 wt {
      local wtexp = cond("`wt'"=="1", "", "[pw=wt]")  // reghdfe can't handle weights that are not variables

      forvalues d=1/`Ndepvars' {
        local depvar: word `d' of `depvars'

        eststo RF`depvar'c`c'`wt'y`y'dummies: reghdfe `depvar' i(1950/`=1974-2-1')bn.birthyr#c.ninnew `wtexp' if inlist(year,`years'), absorb(birthplnew birthyr##c.(`controls') `=cond(`y'==5,"year","")') cluster(birthplnew)  // RF = reduced form
        est save Public\Output\RF`depvar'c`c'`wt'y`y'dummies, replace

        mata dots = st_matrix("e(b)")'; dots[length(dots)] = 0  // average of dots in plot, including base level's 0

        coefplot, keep(*.birthyr#c.ninnew) omitted rename(([0-9]+)[ob]?.birthyr#c.ninnew = \1, regex) vertical xlab(2(2)24) at(_coef, transform(1974 - @)) xscale(reverse) msym(smcircle) msize(small) gen replace
        global graph `r(graph)'

        eststo RF`depvar'c`c'`wt'y`y'pwlspline: reghdfe `depvar' c.t?#c.ninnew `wtexp' if inlist(year,`years'), absorb(birthplnew birthyr##c.(`controls') `=cond(`y'==5,"year","")') cluster(birthplnew)
        est save Public\Output\RF`depvar'c`c'`wt'y`y'pwlspline, replace

        mata segmentfit = `=_b[c.t1#c.ninnew]' * (24 :- (24::2)) + `=_b[c.t2#c.ninnew]' * (J(24 - 12,1,0) \ 12 :- (12::2))
        mata st_numscalar("segmentshift", mean(dots - segmentfit))
        local segmentfn `=segmentshift'                                                                24 ///
                        `=segmentshift + _b[c.t1#c.ninnew] * (24 - 12)'                                12 ///
                        `=segmentshift + _b[c.t1#c.ninnew] * (24 -  2) + _b[c.t2#c.ninnew] * (12 - 2)'  2 

        local kinkpt = segmentshift + _b[c.t1#c.ninnew] * (24 - 12)

        scalar b = _b[t2#c.ninnew] * 8  // slope increase * mean years
        test t2#c.ninnew
        scalar se = abs(b) / sqrt(r(F))
        local caption: display "{it:{&tau}} = " (b<0)*"{&minus}" %4.3f abs(b) " (" %4.3f se ")"

        $graph || scatteri `segmentfn', lcolor(maroon) lwidth(medium) mstyle(p1) msym(diamond) msize(small) mcolor(maroon) lpat(solid) recast(connected) ///
                  title(`=cond(`d'==1, `""`yearname'", size(medium) span"', "")') `=cond(`y'==1, "fxsize(21)", "yscale(off)")' ylab(, format(%3.2f) nogrid) ///
                  `=cond("`depvar'"=="IS", `"xtitle("") xlab(2(2)24, nogrid)"', "xlab(, nolab notick nogrid) xscale(off fill)")' ///
                  graphregion(margin(zero)) ///
                  name(RF`depvar'`wt'y`y', replace) nodraw ///
               || scatteri 0 2, mstyle(p1) msym(smcircle) msize(small) ///  // zero for base year
               || scatteri `kinkpt' 12, msym(diamond) mcolor(maroon) ///
               || scatteri 0 0, msymbol(none) xaxis(2) yaxis(2) xscale(axis(2) off) yscale(axis(2) off) ///  // fake plot to set up extra axes with range [-1,1] for placing text
                    text(.95 -1 "`caption'", xaxis(2) yaxis(2) place(e) color(black))
      }
    }
  }
  foreach wt in 1 wt {
    graph combine RFprimary`wt'y1 RFprimary`wt'y2 RFprimary`wt'y3 RFprimary`wt'y4 RFprimary`wt'y5, cols(5) graphregion(margin(zero)) name(RFprimary, replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label primary', size(small))
    graph combine RFyeduc`wt'y1   RFyeduc`wt'y2   RFyeduc`wt'y3   RFyeduc`wt'y4   RFyeduc`wt'y5  , cols(5) graphregion(margin(zero)) name(RFyeduc  , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label yeduc  ', size(small))
    graph combine RFpart`wt'y1    RFpart`wt'y2    RFpart`wt'y3    hole1           RFpart`wt'y5   , cols(5) graphregion(margin(zero)) name(RFpart   , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label part   ', size(small))
    graph combine RFlhwage`wt'y1  hole1           RFlhwage`wt'y3  RFlwage`wt'y4   RF_lwage`wt'y5 , cols(5) graphregion(margin(zero)) name(RFlhwage , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label lhwage ', size(small))
    graph combine RFIS`wt'y1      RFIS`wt'y2      hole2           hole2           hole2          , cols(5) graphregion(margin(zero)) name(RFIS     , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label IS     ', size(small)) fysize(25)

    graph combine RFprimary RFyeduc RFpart RFlhwage RFIS, cols(1) graphregion(margin(zero)) name(RFc`c'w`wt', replace) xsize(7.5) ysize(8.34) imargin(1 1 0 0) b1title(Age in 1974, xoffset(4) size(vsmall))
    graph save Public\Output\RFc`c'w`wt'pwlspline, replace
  }
}
restore
}

* piecewise-linear fit for blog
{
twoway function y = max(0,12-x), range(2 24) xscale(reverse) xtitle(Age in 1974) ytitle(Average Inpres schooling opportunity) xlab(2/24) yscale(noline) ylab(, notick nolab) scheme(plotplain) graphregion(margin(zero))
graph export "Public\Output\Dose.png", replace width(2680) height(1552)

preserve
keep if 24>=age74 & age74>=2
gen t1 = 24 - age74  // spline components -1974 + birth year
gen t2 = max(0, 12 - age74)  // 12-year-olds in '74 couldn't benefit

forvalues y=5/5 {
  local years: word `y' of 1995 2005 2010 "2013, 2014" "2005, 2010, 2013, 2014"
  local wagevar: word `y' of lhwage IS lhwage lwage _lwage

  forvalues d=1/3 {
    local depvar    : word `d' of yeduc primary `wagevar'
    local depvarname: word `d' of Schooling "Primary completion" Wages

    reghdfe `depvar' i(1950/1971)bn.birthyr#c.ninnew [pw=wt] if inlist(year,`years'), absorb(birthplnew birthyr##c.ch71new `=cond(`y'==5,"year","")') cluster(birthplnew)
    mata dots = st_matrix("e(b)")'; dots[length(dots)] = 0  // average of dots in plot, including base level's 0
    coefplot, keep(*.birthyr#c.ninnew) omitted rename(([0-9]+)[ob]?.birthyr#c.ninnew = \1, regex) vertical xlab(2(2)24) at(_coef, transform(1974 - @)) xscale(reverse) msym(square) msize(medium) mcolor(blue) /*mcolor("68 82 125")*/  ///
      cismooth(n(20) lwidth(10 10) color(gray)) title("`depvarname' vs. Inpres intensity in native regency, by age," "men surveyed in `years'", size(large)) xtitle(Age in 1974, size(medium)) plotregion(color(white)) xlab(, labcolor(black) labsize(medium)) ylab(, labcolor(black) labsize(medium)) gen replace
    global graph `r(graph)'

    reghdfe `depvar' c.t?#c.ninnew [pw=wt] if inlist(year,`years'), absorb(birthplnew birthyr##c.ch71new `=cond(`y'==5,"year","")') cluster(birthplnew)
    mata segmentfit = `=_b[c.t1#c.ninnew]' * (24 :- (24::2)) + `=_b[c.t2#c.ninnew]' * (J(24 - 12,1,0) \ 12 :- (12::2))
    mata st_numscalar("segmentshift", mean(dots - segmentfit))
    local segmentfn `=segmentshift'                                                                24 ///
                    `=segmentshift + _b[c.t1#c.ninnew] * (24 - 12)'                                12 ///
                    `=segmentshift + _b[c.t1#c.ninnew] * (24 -  2) + _b[c.t2#c.ninnew] * (12 - 2)'  2 
    local kinkpt = segmentshift + _b[c.t1#c.ninnew] * (24 - 12)
    test c.t2#c.ninnew
    local caption: display "{it:p} = " %4.2f r(p)
    $graph || scatteri `segmentfn', lcolor(maroon) lwidth(medium) mstyle(p1) msym(diamond) msize(small) mcolor(maroon) lpat(solid) recast(connected) ylab(, format(%3.2f) nogrid) graphregion(margin(zero)) ///
           || scatteri 0 2, mstyle(p1) msym(square) msize(medium) mcolor(blue) /*mcolor("68 82 125")*/ ///  // zero for base year
           || scatteri `kinkpt' 12, msym(diamond) mcolor(maroon) ///
           || scatteri 0 0, msymbol(none) xaxis(2) yaxis(2) xscale(axis(2) off) yscale(axis(2) off) ///  // fake plot to set up extra axes with range [-1,1] for placing text
                text(.95 -1 "`caption'", xaxis(2) yaxis(2) place(e) color(black) size(medium))
    graph export "Public\Output\\`depvar'`years'blog.png", replace width(2680) height(1552)
  }
}
restore
}


***
*** quadratic spline fits
***
{
preserve
keep if 24>=age74 & age74>=2
gen t1 = 24 - age74  // spline components -1974 + birth year
gen t2 = max(0, 12 - age74)  // 12-year-olds in '74 couldn't benefit

twoway scatteri 0 0, msym(none) yscale(off) ylab(,nogrid) nodraw xscale(off) xlab(,nogrid) name(hole1, replace)
twoway scatteri 0 0, msym(none) yscale(off) ylab(,nogrid) nodraw xscale(reverse) xlab(2(2)24,nogrid) xtitle("") name(hole2, replace)

forvalues c=1/1 {
  local controls: word `=`c'+1' of "" ch71new "ch71new en71new" "ch71new en71new wsppc"

  forvalues y=1/5 {
    local years   : word `y' of 1995 2005 2010 2013,2014 2005,2010,2013,2014
    local yearname: word `y' of "Survey: Intercensal, 1995" "Intercensal, 2005" "Labor, 2010" "Socioeconomic, 2013–14" "Post-1995"
    local depvars : word `y' of "primary yeduc part lhwage IS" "primary yeduc part IS" "primary yeduc part lhwage" "primary yeduc lwage" "primary yeduc part _lwage"
    local Ndepvars: word count `depvars'

    foreach wt in 1 wt {
      local wtexp = cond("`wt'"=="1", "", "[pw = wt]")

      forvalues d=1/`Ndepvars' {
        local depvar: word `d' of `depvars'

        eststo RF`depvar'c`c'`wt'y`y'dummies: reghdfe `depvar' i(1950/`=1974-2-1')bn.birthyr#c.ninnew `wtexp' if inlist(year,`years'), absorb(birthplnew birthyr##c.(`controls')) cluster(birthplnew)
        mata dots = st_matrix("e(b)")'[|.\length(st_matrix("e(b)"))-1|] \ 0  // average of dots in plot, including base level's 0
        coefplot, keep(*.birthyr#c.ninnew) omitted rename(([0-9]+)[ob]?.birthyr#c.ninnew = \1, regex) vertical xlab(2(2)24) at(_coef, transform(1974 - @)) xscale(reverse) msym(smcircle) msize(small) gen replace
        global graph `r(graph)'

        eststo RF`depvar'c`c'`wt'y`y'qspline: reghdfe `depvar' c.t?#c.ninnew `wtexp' if inlist(year,`years'), absorb(birthplnew birthyr##c.(`controls')) cluster(birthplnew)  // piecewise-linear fit
        est save Public\Output\RF`depvar'c`c'`wt'y`y'qspline, replace

        mata segmentfit = `=_b[c.t1#c.ninnew]' * (24 :- (24::2)) + `=_b[c.t2#c.ninnew]' * (J(24 - 12,1,0) \ 12 :- (12::2))
        mata st_numscalar("segmentshift", mean(dots - segmentfit))
        local segmentfn `=segmentshift'                                                                24 ///
                        `=segmentshift + _b[c.t1#c.ninnew] * (24 - 12)'                                12 ///
                        `=segmentshift + _b[c.t1#c.ninnew] * (24 -  2) + _b[c.t2#c.ninnew] * (12 - 2)'  2
        
        reghdfe `depvar' c.t1#c.ninnew c.t1#c.t1#c.ninnew c.t?#c.ninnew `wtexp' if inlist(year,`years'), absorb(birthplnew birthyr##c.(`controls')) cluster(birthplnew)  // omnibus fit
        test c.t1#c.ninnew c.t1#c.t1#c.ninnew
        local caption1: display "{it:p} = " %4.2f r(p)
        test c.t1#c.ninnew c.t2#c.ninnew
        local caption2: display "{it:p} = " %4.2f r(p)
        
        eststo RF`depvar'c`c'`wt'y`y'poly: reghdfe `depvar' c.t1#c.ninnew c.t1#c.t1#c.ninnew `wtexp' if inlist(year,`years'), absorb(birthplnew birthyr##c.(`controls')) cluster(birthplnew)  // p fit
        mata polyfit = `=_b[c.t1#c.ninnew]' * (24 :- (24::2)) + `=_b[c.t1#c.t1#c.ninnew]' * (24 :- (24::2)):^2
        local polyfn polyshift + `=_b[c.t1#c.ninnew]' * (24 - x) + `=_b[c.t1#c.t1#c.ninnew]' * (24 - x)^2
        mata st_numscalar("polyshift", mean(dots - polyfit))

        $graph || scatteri  `segmentfn', lcolor(maroon) lwidth(medium) mstyle(p1) msym(diamond) msize(small) mcolor(maroon) lpat(solid) recast(connected) ///
               || function y = `polyfn', lcolor(blue  ) lwidth(medium) lpat(solid) range(2 24) ///
                  title(`=cond(`d'==1, `""`yearname'", size(medium) span"', "")') `=cond(`y'==1, "fxsize(21)", "yscale(off)")' ylab(, format(%3.2f) nogrid) ///
                  `=cond("`depvar'"=="IS", `"xtitle("") xlab(2(2)24, nogrid)"', "xlab(, nolab notick nogrid) xscale(off fill)")' ///
                  graphregion(margin(zero)) ///
                  name(RF`depvar'`wt'y`y', replace) nodraw ///
               || scatteri 0 2, mstyle(p1) msym(smcircle) msize(small) /// // zero for base year
               || scatteri 0 0, msymbol(none) xaxis(2) yaxis(2) xscale(axis(2) off) yscale(axis(2) off) ///  // fake plot to set up extra axes with range [-1,1] for placing text
                    text(1 -1 "`caption1'", xaxis(2) yaxis(2) place(e) color(blue)) text(1 -.1 "`caption2'", xaxis(2) yaxis(2) place(e) color(maroon))
      }
    }
  }
  foreach wt in 1 wt {
    graph combine RFprimary`wt'y1 RFprimary`wt'y2 RFprimary`wt'y3 RFprimary`wt'y4 RFprimary`wt'y5, cols(5) graphregion(margin(zero)) name(RFprimary, replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label primary', size(small))
    graph combine RFyeduc`wt'y1   RFyeduc`wt'y2   RFyeduc`wt'y3   RFyeduc`wt'y4   RFyeduc`wt'y5  , cols(5) graphregion(margin(zero)) name(RFyeduc  , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label yeduc'  , size(small))
    graph combine RFpart`wt'y1    RFpart`wt'y2    RFpart`wt'y3    hole1           RFpart`wt'y5   , cols(5) graphregion(margin(zero)) name(RFpart   , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label part'   , size(small))
    graph combine RFlhwage`wt'y1  hole1           RFlhwage`wt'y3  RFlwage`wt'y4   RF_lwage`wt'y5 , cols(5) graphregion(margin(zero)) name(RFlhwage , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label lhwage' , size(small))
    graph combine RFIS`wt'y1      RFIS`wt'y2      hole2           hole2           hole2          , cols(5) graphregion(margin(zero)) name(RFIS     , replace) imargin(1 0 0 0) ycommon nodraw l1title(`:var label IS'     , size(small)) fysize(25)

    graph combine RFprimary RFyeduc RFpart RFlhwage RFIS, cols(1) graphregion(margin(zero)) name(RFc`c'w`wt'poly, replace) xsize(7.5) ysize(8.34) imargin(1 1 0 0) b1title(Age in 1974, xoffset(4) size(vsmall))
    graph save Public\Output\RFc`c'w`wt'qspline, replace
  }
}
restore
}


est drop _all  // avoid system limit on # of stored estimates

***
*** OLS & 2SLS
***
{
preserve
keep if 24>=age74 & age74>=2
xi i.young|ninnew i.dum|ninnew i.birthyr*ch71new i.birthyr*en71new i.birthyr*wsppc i.year  // xtivreg2 doesn't take factor vars, but is faster than ivreg2...i.birthplnew and does weak inst tests
set seed 230498257

forvalues c=1/1 /*3*/ {
  local controls _Ibirthyr_* `: word `c' of _IbirXch7* "_IbirXch7* _IbirXen7*" "_IbirXch7* _IbirXen7* _IbirXwsp*" '
  foreach edvar in yeduc primary {
    forvalues y=1/5 {
      local years   : word `y' of 1995 2005 2010 2013,2014 2005,2010,2013,2014
      local depvars : word `y' of "part lwage lhwage" "part IS" "part lhwage" lwage "part _lwage" 
      local Ndepvars: word count `depvars'
      local graphs
      forvalues d=1/`Ndepvars' {
        local depvar: word `d' of `depvars'
        foreach wt in 1 wt {
          eststo OLS`depvar'`edvar'c`c'w`wt'y`y': areg `depvar' `edvar' `controls' `=cond(`y'==5,"_Iyear*","")' [pw=`wt'] if inlist(year,`years'), cluster(birthplnew) a(birthplnew)
          forvalues i=1/2 {
            local insts: word `i' of _IyouXninne_1 _IdumXnin*  // instruments by birth year or young/old
            eststo TSLS`depvar'`edvar'c`c'`i'w`wt'y`y': xtivreg2 `depvar' (`edvar' = `insts') `controls' `=cond(`y'==5,"_Iyear*","")' [pw=`wt'] if inlist(year,`years'), cluster(birthplnew) partial(`controls') small fe
            boottest, ar reps(99999) gridmin(-.8) gridmax(1.1) format(%4.2f) ///
                      graphopt(xlab(-.8(.2)1.1) ylab(.05 .2(.2)1, nogrid) xline(0) nodraw ///
                              `=cond(`d'==`Ndepvars', `"xtitle("")"', "xscale(off)")' xscale(range(-.4 .7)) ytitle("") `=cond("`wt'"=="1" & `i'==1,"","yscale(off)")' `=cond(`d'==1, `"title(`=cond("`wt'"=="1", "Unweighted", "Weighted")')"', "")') ///
                      graphname(`depvar'`edvar'c`c'2SLS`i'w`wt'y`y', replace)
            estadd local CIstr "`r(CIstr)'"
            est save Public\Output\TSLS`depvar'`edvar'c`c'`i'w`wt'y`y', replace
          }
        }
        esttab OLS`depvar'`edvar'c`c'w1y`y' OLS`depvar'`edvar'c`c'wwty`y' TSLS`depvar'`edvar'c`c'1w1y`y' TSLS`depvar'`edvar'c`c'1wwty`y' TSLS`depvar'`edvar'c`c'2w1y`y' TSLS`depvar'`edvar'c`c'2wwty`y' ///
               using "Public\Output\c`c' `edvar' y`y'.rtf", `=cond(`d'==1,"replace","append")' ///
               keep(`edvar') b se msign("–") nonotes nonumber nogaps nomtitles nostar ///
               stat(CIstr jp widstat N, labels("Bootstrap CI" "Hansen p" "KP F" Observations) fmt(%~1s %4.2f %4.2f %7.0fc)) fonttbl(\f0\fnil Cambria;)

        graph combine `depvar'`edvar'c`c'2SLS1w1y`y' `depvar'`edvar'c`c'2SLS1wwty`y', ///
              rows(1) imargin(1 0 0 0) `=cond(`d'==1, "title(Instrument by young/old)", "")' name(g1, replace) nodraw
        graph combine `depvar'`edvar'c`c'2SLS2w1y`y' `depvar'`edvar'c`c'2SLS2wwty`y', ///
              rows(1) imargin(1 0 0 0) `=cond(`d'==1, "title(Instruments by birth year)", "")' name(g2, replace) nodraw
        graph combine g1 g2, l1title(`:var label `depvar'': {it:p}) rows(1) imargin(1 1 0 0) iscale(1) name(`depvar'`edvar'c`c'2SLSy`y', replace) nodraw
        local graphs `graphs' `depvar'`edvar'c`c'2SLSy`y'
      }
      graph combine `graphs', cols(1) ycommon b1title(Coefficient on `=lower("`:var label `edvar''")', size(small)) xsize(7.5) ysize(8.34) name(`edvar'c`c'y`y', replace) imargin(0 0 1 0)
      graph save Public\Output\TSLS`edvar'c`c'y`y', replace
    }
  }
}
restore
}


***
*** OLS & 2SLS with spline specification
***
{
preserve
keep if 24>=age74 & age74>=2
gen t1 = 24 - age74  // spline components -1974 + birth year
gen t2 = max(0, 12 - age74)  // 12-year-olds in '74 couldn't benefit
gen t1_nin = t1 * ninnew
gen t2_nin = t2 * ninnew
gen t1t1_nin = t1 * t1_nin
xi i.young|ninnew i.dum|ninnew i.birthyr*ch71new i.birthyr*en71new i.birthyr*wsppc i.year  // xtivreg2 doesn't take factor vars, but is faster than ivreg2...i.birthplnew and does weak inst tests
set seed 230498257

forvalues c=1/1 /*3*/ {
  local controls _Ibirthyr_* `: word `c' of _IbirXch7* "_IbirXch7* _IbirXen7*" "_IbirXch7* _IbirXen7* _IbirXwsp*" '
  foreach edvar in yeduc primary {
    forvalues y=1/5 {
      local years  : word `y' of 1995 2005 2010 2013,2014 2005,2010,2013,2014
      local depvars : word `y' of "part lhwage" "part IS" "part lhwage" lwage _lwage
      local Ndepvars: word count `depvars'
      local graphs
      forvalues d=1/`Ndepvars' {
        local depvar: word `d' of `depvars'
        foreach wt in 1 wt {
          forvalues i=1/2 {
            local timecontrols = cond(`i'==1, "t1_nin", "t1_nin t1t1_nin")
            eststo TSLSPWL`depvar'`edvar'c`c'`i'`wt'y`y': xtivreg2 `depvar' (`edvar' = t2_nin) `timecontrols' `controls' `=cond(`y'==5,"_Iyear*","")' [pw=`wt'] if inlist(year,`years'), cluster(birthplnew) partial(`controls') small fe
            boottest, ar reps(99999) gridmin(-.8) gridmax(1.1) format(%4.2f) ///
                      graphopt(xlab(-.8(.2)1.1) ylab(.05 .2(.2)1, nogrid) xline(0) nodraw ///
                              `=cond(`d'==`Ndepvars', `"xtitle("")"', "xscale(off)")' ytitle("") `=cond("`wt'"=="1" & `i'==1,"","yscale(off)")' `=cond(`d'==1, `"title(`=cond("`wt'"=="1", "Unweighted", "Weighted")')"', "")') ///
                      graphname(`depvar'`edvar'c`c'2SLS`i'w`wt'y`y', replace)
            estadd local CIstr "`r(CIstr)'"
            est save Public\Output\TSLSPWL`depvar'`edvar'c`c'`i'`wt'y`y', replace
          }
        }
        esttab TSLSPWL`depvar'`edvar'c`c'21y`y' TSLSPWL`depvar'`edvar'c`c'2wty`y' TSLSPWL`depvar'`edvar'c`c'11y`y' TSLSPWL`depvar'`edvar'c`c'1wty`y' ///
               using "Public\Output\spline c`c' `edvar' y`y'.rtf", `=cond(`d'==1,"replace","append")' ///
               rename(t2_nin `edvar') keep(`edvar') b se msign("–") nonotes nonumber nogaps nomtitles nostar ///
               stat(CIstr widstat N, labels("Bootstrap CI" "KP F" Observations) fmt(%~1s %4.2f %7.0fc)) fonttbl(\f0\fnil Cambria;)

        graph combine `depvar'`edvar'c`c'2SLS1w1y`y' `depvar'`edvar'c`c'2SLS1wwty`y', ///
              rows(1) imargin(1 1 0 0) `=cond(`d'==1, "title(Linear time control)", "")' name(g1, replace) nodraw
        graph combine `depvar'`edvar'c`c'2SLS2w1y`y' `depvar'`edvar'c`c'2SLS2wwty`y', ///
              rows(1) imargin(1 1 0 0) `=cond(`d'==1, "title(Quadratic time controls)", "")' name(g2, replace) nodraw
        
        graph combine g1 g2, l1title(`:var label `depvar'': {it:p}, size(small)) rows(1) imargin(1 0 0 0) iscale(1) name(`depvar'`edvar'c`c'2SLSy`y', replace) nodraw
        local graphs `graphs' `depvar'`edvar'c`c'2SLSy`y'
      }

      graph combine `graphs', cols(1) ycommon b1title(Coefficient on `=lower("`:var label `edvar''")', size(small)) xsize(7.5) ysize(8.34) name(spline`edvar'c`c'y`y', replace) imargin(zero)
      graph save Public\Output\TSLSPWL`edvar'c`c'y`y', replace
    }
  }
}
restore
}


***
*** CIC
***
{
preserve
keep if young<. & 2<=age74 & age74<=24
xi i.birthyr|ch71new i.birthyr|en71new i.birthyr|wsppc i.year
set seed 30948573
forvalues y=1/5 {
  local years : word `y' of 1995 2005 2010 2013,2014 2005,2010,2013,2014
  local depvar: word `y' of lhwage IS lhwage lwage _lwage
  eststo cic`y': cic `depvar' _IbirXch7* `=cond(`y'==5,"_Iyear*","")' [pw=wt] if inlist(year,`years'), group(recpnew) time(young) reps(`=cond(`y'<5,1000,100)')
  foreach stat in `:rownames e(tests)' {
    estadd scalar `stat' = e(tests)["`stat'",2], replace
  }
}
drop _all  // speeds up coefplot, gen
coefplot cic1 cic2 cic3 cic4 cic5, scheme(plottig) ylabel(1 "10" 2 "20" 3 "30" 4 "40" 5 "50" 6 "60" 7 "70" 8 "80" 9 "90") ytitle(Percentile) ///
  order(q9 q8 q7 q6 q5 q4 q3 q2 q1) ///
  legend(rowgap(zero) lab(21 "Log hourly wage, 1995") lab(42 "Imputed log hourly wage, 2005") lab(63 "Log hourly wage, 2010") lab(84 "Log typical monthly earnings, 2013–14") lab(105 "Log earnings, post-1995") pos(4) region(/*fcolor(gs14%100) lstyle(none)*/ margin(zero)) bmargin(1 0 0 0) size(7pt)) ///
  xtitle("Changes-in-changes impact estimate") graphregion(margin(zero)) cismooth(n(20)) gen replace
local graph `r(graph)'
cap drop label x
gen label = subinstr(string(__b, "%5.3f") + " [" + string(__ll1, "%5.3f") + ", " + string(__ul1, "%5.3f") + "]", "-", "–", .) if __at<.
gen x = .5 if __at<.
`graph' || scatter __at x, msym(none) mlab(label) mlabcolor(black) mlabsize(7pt) mlabpos(9) xscale(range(-.2 .5)) xlab(-.3(.1).1) xline(0, lcolor(gs10) lpat(solid)) xsize(5.5) ysize(5) name(cic, replace)
graph save Public\Output\cic, replace

esttab cic? using Public\Output\cic.rtf, replace rename(q9 90 q8 80 q7 70 q6 60 q5 50 q4 40 q3 30 q2 20 q1 10) order(90 80 70 60 50 40 30 20 10) nogaps nomtitle msign("–") b(3) se(3) nostar ///
                           stats(constant_0 constant_m stoch_dom_pos stoch_dom_neg, labels("No effect (p)" "Constant effect (p)" "All >0 (p)" "All <0 (p)") fmt(%4.2f)) fonttbl(\f0\fnil Cambria;)
restore
}


***
*** Jakiela diagnostic
***
{
preserve
keep if year==1995 & (young==1 | old==1)
xi i.young|ninnew

regress _IyouXninne_1 i.birthyr i.birthplnew i.birthyr#c.ch71new if lhwage<. [aw=wt]
predict _T if e(sample), resid
regress lhwage        i.birthyr i.birthplnew i.birthyr#c.ch71new if lhwage<. [aw=wt]
predict _Y if e(sample), resid

eststo untreated: reg _Y _T if !young [aw=wt], nocons
eststo   treated: reg _Y _T if  young [aw=wt], nocons
suest untreated treated, cluster(birthplnew)
test [untreated_mean]_T = [treated_mean]_T  // test that two slopes are same

collapse T=_IyouXninne_1 _T _Y (rawsum) wt [aw=wt], by(birthyr birthplnew)

scatter _Y _T if T==0, msym(Oh) mcolor(%20) mlwidth(medthick) || ///
scatter _Y _T if T!=0, msym(Oh) mcolor(%20) mlwidth(medthick) || ///
lpoly   _Y _T if T==0 [aw=wt], bw(.5) pstyle(p1) || ///
lpoly   _Y _T if T!=0 [aw=wt], pstyle(p2) bw(.5) || ///
lfit    _Y _T if T==0 [aw=wt], pstyle(p1) || ///
lfit    _Y _T if T!=0 [aw=wt], pstyle(p2) ///
  legend(order(2 1) cols(1) label(2 "Age 2{&minus}6 in 1974") label(1 "Age 12{&minus}17 in 1974") ring(0) pos(8) region(style(none))) ///
  scheme(plottig) xtitle(Residualized treatment) ytitle(Residualized log hourly wage) graphregion(margin(zero))

restore
}

log close
