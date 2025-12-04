*Codes implementing the estimators proposed in Melly and Santangelo

cap prog drop cic
program cic, eclass
	version 9.2
	capt findfile lmoremata.mlib
	if _rc {
      	di as error "-moremata- is required; type {stata ssc install moremata} and restart Stata."
		error 499
	}
	else {
		syntax varlist [if] [in] [pweight iweight fweight/], Group(varname) Time(varname) [Method(string) Quantiles(numlist >0 <1 sort) NReg(integer 100) Reps(integer 100) Level(cilevel) First(real 0.1) Last(real 0.9) noboot noprint noprinttest SAVing(string) CONS_test(string) beta(real 0.9995) small(real 0.00001) max_it(real 100)]
		if `nreg'<1{
			dis as error "The option nreg must be a strictly positive integer."
			exit
		}
		if "`method'"==""{
			local method "qr"
		}
		if "`method'"!="qr"{
			dis as error "The selected method has not been implemented yet"
			exit
		}
		marksample touse
		markout `touse' `group' `time'
		quietly tab `group' if `touse'
		local temp=r(r)
		quietly sum `group' if `touse'
		if r(min)!=0 | r(max)!=1 | `temp'!=2{ 
			dis as error "The variable in the option group must be a 0/1 binary variable."
			exit
		}
		quietly tab `time' if `touse'
		local temp=r(r)
		quietly sum `time' if `touse'
		if r(min)!=0 | r(max)!=1 | `temp'!=2{ 
			dis as error "The variable in the option time must be a 0/1 binary variable."
			exit
		}
		gettoken dep varlist : varlist
		if "`exp'"==""{
			tempvar exp
			quiet gen `exp'=1
		}
		quiet count if `touse' & `group'==0 & `time'==0
		local n00=r(N)
		quiet count if `touse' & `group'==0 & `time'==1
		local n01=r(N)
		quiet count if `touse' & `group'==1 & `time'==0
		local n10=r(N)
		quiet count if `touse' & `group'==1 & `time'==1
		local n11=r(N)
		quiet _rmcoll `varlist' [aw=`exp'] if `touse' & `group'==0 & `time'==0
		local varlist `r(varlist)'
		quiet _rmcoll `varlist' [aw=`exp'] if `touse' & `group'==1 & `time'==0
		local varlist `r(varlist)'
		quiet _rmcoll `varlist' [aw=`exp'] if `touse' & `group'==0 & `time'==1
		local varlist `r(varlist)'
		quiet _rmcoll `varlist' [aw=`exp'] if `touse' & `group'==1 & `time'==1
		local varlist `r(varlist)'
		tempname quants dist qte effects covariance tests
		if "`quantiles'"==""{
			numlist "0.1(0.1)0.9"
			local quantiles "`r(numlist)'"
		}
		tokenize "`quantiles'", parse(" ")
		local i=1
		while "`1'" != "" {
			matrix `quants'=nullmat(`quants')\(`1')
			mac shift 
			local i=`i'+1
		}
		mata: sample=cic("`dep'", "`varlist'", "`group'", "`time'", "`exp'", "`touse'", `nreg', "`quants'", `beta', `small', `max_it')
		mata: st_matrix("`dist'",sample)
		mata: st_matrix("`qte'",(sample[.,2]-sample[.,1])')
		if "`boot'"==""{
			if "`cons_test'"=="" | "`constest'"=="0" {
				tempname constest
				mat `constest'=0
			}
			else{
				tokenize `cons_test'
				local i=1
				while "``i''"!=""{
					if ``i''!=0{
						mat `constest'=(nullmat(`constest'))\(``i'')
					}
					local i=`i'+1
				}
			}
			if `"`saving'"'!="" {
				_prefix_saving `saving'
				local saving `"`s(filename)'"'
				if "`double'" == "" {
					local double `"`s(double)'"'
				}
				if "`double'" == "" {
					local double "double"
				}
				local every	`"`s(every)'"'
				if "`every'"==""{
					local every=1
				}
				local replace `"`s(replace)'"'
			}
			else local every=999999
			mata: CIC_boot=J(0,rows(sample),.)
			preserve
			local actual_more=c(more)
			set more off
			di in gr "(bootstrapping " _c
			forvalues i=1/`reps'{
				bsample
				capture mata: boot=cic("`dep'", "`varlist'", "`group'", "`time'", "`exp'", "`touse'", `nreg', "`quants'", `beta', `small', `max_it')
				if _rc==0{
					mata: CIC_boot=CIC_boot\(boot[.,2]-boot[.,1])'
					if round(`i'/`every')==(`i'/`every'){
						drop _all
						mata: st_addobs(rows(CIC_boot))
						mata: idx = st_addvar(st_local("double"), st_tempname(cols(CIC_boot)))
						mata: st_store(.,idx,CIC_boot)
						if `i'==1{	
							quietly save `saving', `replace'
						}
						else{ 
							quietly save `saving', replace
						}
					}
					di in gr "." _c
				}
				else{
					dis in red "x" _continue
					local i=`i'-1
				}
				restore, preserve
			}
			set more `actual_more'
			di in gr ")"
			mata: ev_boot(CIC_boot, (sample[.,2]-sample[.,1])', st_matrix("`quants'"), `last', `first', `level', "`constest'", "`effects'", "`covariance'", "`tests'")
*			mata: mata drop qte_cov_boot qte_cov_coef qte_cov_counter qte_cov_def qte_cov_defc qte_cov_deff qte_cov_fitted qte_cov_obs qte_cov_quant qte_cov_res
		}
		else{
			mata: st_matrix("`effects'",(st_matrix("`quants'"),sample[.,2]-sample[.,1],J(rows(sample),3,.)))
			mata: st_matrix("`covariance'",J(rows(sample),rows(sample),0))
*			mata: mata drop qte_cov_coef qte_cov_counter qte_cov_fitted qte_cov_obs qte_cov_quant qte_cov_res
		}
		local nq=rowsof(`quants')
		forvalues j=1/`nq'{
			local names "`names' q`j'"
		}
		mat rownames `covariance' = `names'
		mat colnames `covariance' = `names'
		mat rownames `effects' = `names'
		mat colnames `effects' = quantile effect point_se uniform_lb uniform_ub
		mat rownames `dist' = `names'
		mat colnames `dist' = counter estimated observed
		mat colnames `qte' = `names'
		mat rownames `qte' = `dep'
*Display the results
*header
		if "`print'"!="noprint"{
			dis
			dis as text _column(0) "Conditional model" _c
			if "`method'"=="qr"{
				di as result _column(43) "linear quantile regression"
			}
			di
			if "`boot'"==""{
				di as text "The variance has been estimated by bootstraping the results " as result `reps'  as text " times."
			}
			else{
				di as text "The variance has not been computed." _new "Do not turn the option boot off if you want to compute it."
			}
			dis
			dis as text _column(0) "No. of obs. in the control group in period 0: " _c
			dis as result _column(43) %-8.0f `n00'
			dis as text _column(0) "No. of obs. in the control group in period 1: " _c
			dis as result _column(43) %-8.0f `n01'
			dis as text _column(0) "No. of obs. in the treated group in period 0: " _c
			dis as result _column(43) %-8.0f `n10'
			dis as text _column(0) "No. of obs. in the treated group in period 1: " _c
			dis as result _column(43) %-8.0f `n11'
			dis
			display as text _n "Changes-in-changes estimates" 
			dis as text "{hline 12}" "{c TT}" "{hline 65}"
			dis as text _column(0) "Quantile" _column(13) "{c |}" _column(16) %~10s "QTE"  _column(26) %~10s "Pointwise" _column(40) %~8s "Pointwise" _column(58) %~21s "Functional"
			dis as text /*_column(2) "quantile" */ _column(13) "{c |}" _column(26) %~10s "Std. Err." _column(35) %~8s "[`level'% Conf. Interval]" _column(58) %~21s "[`level'% Conf. Interval]"
			dis as text "{hline 12}" "{c +}" "{hline 65}"
			forvalues k=1/`nq'{
				dis as text _column(2) %17s `quants'[`k',1] as text _column(13) "{c |}" _column(16) as result %8.0g (`effects'[`k',2]) _column (26) as result %8.0g (`effects'[`k',3]) _column (37) as result %8.0g (`effects'[`k',2]-invnormal(0.5*(1+`level'/100))*`effects'[`k',3]) _column(47) as result %8.0g (`effects'[`k',2]+invnormal(0.5*(1+`level'/100))*`effects'[`k',3]) _column(60) as result %8.0g (`effects'[`k',4]) _column(70) as result %8.0g (`effects'[`k',5]) 
			}
			dis as text "{hline 12}" "{c BT}" "{hline 65}" 
		}
*Tests
		if "`printtest'"!="noprinttest" & "`boot'"!="noboot"{
			dis
			dis as text _n "Bootstrap inference"
			dis as text "{hline 47}" "{c TT}" "{hline 30}"
			dis as text _column(48) "{c |}"  _column(60) %~10s "P-values"
			dis as text _column(0) "Null-hypothesis" _column(48) "{c |}" _column(50) %~10s "KS-statistic" _column(65) %~10s "CMS-statistic"
			dis as text "{hline 47}" "{c +}" "{hline 30}"
			if `constest'[1,1]!=0 local nct=rowsof(`constest')
			else local nct=0
			dis as text _column(0) %17s "No effect: QTE(tau)=0 for all taus" as text _column(48) "{c |}" _column(50) as result %8.0g (`tests'[1,1]) _column(65) as result %8.0g (`tests'[1,2]) 
			if `nct'!=0{
				forvalues j=1/`nct'{
					local temp=`constest'[`j',1]
					dis as text _column(0) %17s "Constant effect: QTE(tau)=`temp' for all taus" as text _column(48) "{c |}" _column(50) as result %8.0g (`tests'[1+`j',1]) _column(65) as result %8.0g (`tests'[1+`j',2]) 
				}
			}
			dis as text _column(0) %17s "Constant effect: QTE(tau)=QTE(0.5) for all taus" as text _column(48) "{c |}" _column(50) as result %8.0g (`tests'[2+`nct',1]) _column(65) as result %8.0g (`tests'[2+`nct',2]) 
			dis as text _column(0) %17s "Stochastic dominance: QTE(tau)>0 for all taus" as text _column(48) "{c |}" _column(50) as result %8.0g (`tests'[3+`nct',1]) _column(65) as result %8.0g (`tests'[3+`nct',2]) 
			dis as text _column(0) %17s "Stochastic dominance: QTE(tau)<0 for all taus" as text _column(48) "{c |}" _column(50) as result %8.0g (`tests'[4+`nct',1]) _column(65) as result %8.0g (`tests'[4+`nct',2]) 
			dis as text "{hline 47}" "{c BT}" "{hline 30}"
		}
		ereturn post `qte' `covariance', dep(`dep') esample(`touse')
		ereturn matrix qfunctions = `dist'
		ereturn matrix qte = `effects'
		ereturn matrix quantiles = `quants'
		if "`boot'"==""{
			mat colnames `tests'=KS CMS
			local conam "constant_0"
			if `constest'[1,1]!=0{
				local nct=rowsof(`constest')
				forvalues i=1/`nct'{
					local conam "`conam' constant_`i'"
				}
			}
			local conam "`conam' constant_m stoch_dom_pos stoch_dom_neg"
			mat rownames `tests'=`conam'
			ereturn matrix tests=`tests'
		}
	}
end

cap mata mata drop ev_boot()
*Mata function doing the evaluation of the bootstrap results
version 9.2
mata void ev_boot(real matrix bootstrap, real rowvector sample, real colvector quantiles, real scalar max, real scalar min, real scalar level, string scalar contest, string scalar out_results, string scalar out_cov, string scalar out_test)
{
	cov=variance(bootstrap)
	st_matrix(out_cov,cov)
	constest=st_matrix(contest)
	test=J(0,2,.)
	results=quantiles,sample',sqrt(diagonal(cov)),J(cols(sample),2,.)
	if(constest==J(1,1,0)) nc=0 
	else nc=rows(constest)
	sel=(sum(quantiles:<min)+1)..sum(quantiles:<=max)
	qte_cov_boot=bootstrap[.,sel]
	qte_cov_def=sample[sel]
	nr=rows(qte_cov_boot)
	nq=length(qte_cov_def)
	test=J(4+nc,2,.)
	Vuqf=diagonal(variance(qte_cov_boot))'
	seuqf=colmax((sqrt(Vuqf)\J(1,cols(sel),0.00000001)))
	//test of no effect, KS
	Kuqf=((qte_cov_boot-qte_cov_def#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1))):^0.5
	Kmaxuqf=rowmax(Kuqf)
	Kalpha=mm_quantile(Kmaxuqf,1,level/100)
	results[sel,4]=(qte_cov_def-seuqf*Kalpha)'
	results[sel,5]=(qte_cov_def+seuqf*Kalpha)'
	KSstat=max((qte_cov_def:^2:/Vuqf):^0.5)
	test[1,1]=mean(Kmaxuqf:>=KSstat)
	if(nc>0){
		for(i=1;i<=nc;i++){
			KSstat=max(abs(qte_cov_def:-constest[i]):/seuqf)
			test[1+i,1]=mean(Kmaxuqf:>=KSstat)
		}
	}
	//test of no effects, CMS
	Kuqf=((qte_cov_boot-qte_cov_def#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1)))
	Kmeanuqf=mean(Kuqf')
	CMSstat=mean((qte_cov_def:^2:/Vuqf)')
	test[1,2]=mean(Kmeanuqf':>=CMSstat)
	if(nc>0){
		for(i=1;i<=nc;i++){
			CMSstat=mean(((qte_cov_def:-constest[i]):^2:/Vuqf)')
			test[i+1,2]=mean(Kmeanuqf':>=CMSstat)
		}
	}
	//test of constant effect, coef by coef, KS
	qte_cov_def1=qte_cov_def:-mean(qte_cov_def')
	qte_cov_boot1=qte_cov_boot-J(1,nq,1)#mean(qte_cov_boot')'
	Kuqf=((qte_cov_boot1-qte_cov_def1#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1))):^0.5
	Kmaxuqf=rowmax(Kuqf)
	KSstat=max((qte_cov_def1:^2:/Vuqf):^0.5)
	test[2+nc,1]=mean(Kmaxuqf:>=KSstat)
	//test of constant effects, coef by coef, CMS
	Kuqf=((qte_cov_boot1-qte_cov_def1#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1)))
	Kmeanuqf=mean(Kuqf')
	CMSstat=mean((qte_cov_def1:^2:/Vuqf)')
	test[2+nc,2]=mean(Kmeanuqf':>=CMSstat)	
	//test of stochastic dominance, KS
	qte_cov_boot1=qte_cov_boot-qte_cov_def#J(nr,1,1)
	qte_cov_boot1=qte_cov_boot1:*(qte_cov_boot1:<=0)
	qte_cov_def1=qte_cov_def:*(qte_cov_def:<=0)
	Kuqf=(qte_cov_boot1:^2:/(Vuqf#J(nr,1,1))):^0.5
	Kmaxuqf=rowmax(Kuqf)
	KSstat=max((qte_cov_def1:^2:/Vuqf):^0.5)
	test[3+nc,1]=mean(Kmaxuqf:>=KSstat)
	//test of stochastic dominance, CMS
	Kuqf=qte_cov_boot1:^2:/(Vuqf#J(rows(qte_cov_boot),1,1))
	Kmeanuqf=mean(Kuqf')
	CMSstat=mean((qte_cov_def1:^2:/Vuqf)')
	test[3+nc,2]=mean(Kmeanuqf':>=CMSstat)	
	//test of being stochastically dominated, KS
	qte_cov_boot1=qte_cov_boot-qte_cov_def#J(nr,1,1)
	qte_cov_boot1=qte_cov_boot1:*(qte_cov_boot1:>=0)
	qte_cov_def1=qte_cov_def:*(qte_cov_def:>=0)
	Kuqf=(qte_cov_boot1:^2:/(Vuqf#J(nr,1,1))):^0.5
	Kmaxuqf=rowmax(Kuqf)
	KSstat=max((qte_cov_def1:^2:/Vuqf):^0.5)
	test[4+nc,1]=mean(Kmaxuqf:>=KSstat)
	//test of being stochastically dominated, CMS
	Kuqf=qte_cov_boot1:^2:/(Vuqf#J(nr,1,1))
	Kmeanuqf=mean(Kuqf')
	CMSstat=mean((qte_cov_def1:^2:/Vuqf)')
	test[4+nc,2]=mean(Kmeanuqf':>=CMSstat)
	st_matrix(out_test,test)
	st_matrix(out_results,results)
}

cap mata mata drop cic()
version 9.2
mata real matrix cic(string scalar dep, string scalar reg, string scalar group, string scalar time, string scalar weights, string scalar touse, real scalar nreg, string scalar quants, real scalar beta, real scalar small, real scalar max_it)
{
	y=st_data(.,dep,touse)
	if(reg==""){
		x=J(rows(y),0,.)
	}
	else{
		x=st_data(.,reg,touse)
	}
	g=st_data(.,group,touse)
	t=st_data(.,time,touse)
	w=st_data(.,weights,touse)
	quantiles=st_matrix(quants)
	yw1=select(y,t:*g)
	nw1=rows(yw1)
	xw1=select(x,t:*g),J(nw1,1,1)
	ww1=select(w,t:*g)
	if(cols(xw1)>1){
		stata("qrprocess "+dep+" "+reg+" if "+touse+" & "+time+"==0 & "+group+"==0 [pweight="+weights+"], vce(novar) method(qreg, beta("+strofreal(beta)+") small("+strofreal(small)+") max_it("+strofreal(max_it)+")) quantile("+strofreal(0.5/nreg)+"("+strofreal(1/nreg)+")"+strofreal(1-0.5/nreg)+")",1)
		coefm0=st_matrix("e(coefmat)")
		stata("qrprocess "+dep+" "+reg+" if "+touse+" & "+time+"==0 & "+group+"==1 [pweight="+weights+"], vce(novar) method(qreg, beta("+strofreal(beta)+") small("+strofreal(small)+") max_it("+strofreal(max_it)+")) quantile("+strofreal(0.5/nreg)+"("+strofreal(1/nreg)+")"+strofreal(1-0.5/nreg)+")",1)
		coefw0=st_matrix("e(coefmat)")
		stata("qrprocess "+dep+" "+reg+" if "+touse+" & "+time+"==1 & "+group+"==0 [pweight="+weights+"], vce(novar) method(qreg, beta("+strofreal(beta)+") small("+strofreal(small)+") max_it("+strofreal(max_it)+")) quantile("+strofreal(0.5/nreg)+"("+strofreal(1/nreg)+")"+strofreal(1-0.5/nreg)+")",1)
		coefm1=st_matrix("e(coefmat)")
		stata("qrprocess "+dep+" "+reg+" if "+touse+" & "+time+"==1 & "+group+"==1 [pweight="+weights+"], vce(novar) method(qreg, beta("+strofreal(beta)+") small("+strofreal(small)+") max_it("+strofreal(max_it)+")) quantile("+strofreal(0.5/nreg)+"("+strofreal(1/nreg)+")"+strofreal(1-0.5/nreg)+")",1)
		coefw1=st_matrix("e(coefmat)")
		fitw1w0=cross(xw1',coefw0)
		fitw1m0=cross(xw1',coefm0)
		r0=J(nw1,nreg,.)
		for(i=1;i<=nreg;i++){
			r0[.,i]=rowmax((J(nw1,1,1),rowsum((fitw1m0:<=fitw1w0[.,i]))))
		}
		fitw1m1=cross(xw1',coefm1)
		counter=J(nw1,nreg,.)
		for(i=1;i<=nw1;i++){
			counter[i,.]=fitw1m1[i,r0[i,.]]
		}
		qcounter=mm_quantile(vec(counter),J(nreg,1,ww1),quantiles)
		fitw1w1=cross(xw1',coefw1)
		qw1=mm_quantile(vec(fitw1w1),J(nreg,1,ww1),quantiles)
		qobsw1=mm_quantile(yw1,ww1,quantiles)
	} else{
		fitw1m0=mm_quantile(select(y,(1:-t):*(1:-g)),select(w,(1:-t):*(1:-g)),(1..nreg):/nreg:-0.5/nreg)
		fitw1w0=mm_quantile(select(y,(1:-t):*g),select(w,(1:-t):*g),(1..nreg):/nreg:-0.5/nreg)
		r0=J(1,nreg,.)
		for(i=1;i<=nreg;i++) r0[i]=sum(fitw1m0:<=fitw1w0[i])
		r0=colmax((r0\J(1,nreg,1)))
		fitw1m1=mm_quantile(select(y,t:*(1:-g)),select(w,t:*(1:-g)),(1..nreg):/nreg:-0.5/nreg)
		counter=fitw1m1[r0]'
		qcounter=mm_quantile(counter,1,quantiles)
		fitw1w1=mm_quantile(yw1,ww1,(1..nreg):/nreg:-0.5/nreg)'
		qw1=qobsw1=mm_quantile(fitw1w1,1,quantiles)
	}
	return((qcounter,qw1,qobsw1))
}
