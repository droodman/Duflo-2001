*qrprocess: quantile regression process
*! version 1.1.3  12.04.2023  Blaise Melly

program qrprocess, eclass byable(recall) sortpreserve
	version 9.2
*check that moremata is installed
	capt findfile lmoremata.mlib
	if _rc {
      	di as error "-moremata- is required; type {stata ssc install moremata}"
		error 499
	}
	if replay() {
		if "`e(cmd)'"!="qrprocess" { 
			error 301 
		} 
		if _by() {
			error 190 
		}
        syntax [, Level(cilevel)]
		ereturn display, level(`level')
 	}
	else {
		if _caller() >= 11{
			version 11.1: syntax varlist(numeric fv) [if] [in] [pweight iweight fweight/] [, Quantiles(numlist >0 <1 sort) Method(string)  Vce(string) Functional Level(cilevel) noPrint QLow(real -1234) QHigh(real -1234) QStep(real -1234)]
		}
		else{
			syntax varlist(numeric) [if] [in] [pweight iweight fweight/] [, Quantiles(numlist >0 <1 sort) Method(string)  Vce(string) Functional Level(cilevel) noPrint QLow(real -1234) QHigh(real -1234) QStep(real -1234)]
		}
		local fvops = "`s(fvops)'" == "true" & _caller() >= 11
		if `fvops' {
			local vv: di "version " string(max(11,_caller())) ", missing: " 
		}
		local quantile "`quantiles'"
*sample definition
		marksample touse
*weights
		local isweight=("`exp'"!="")
		if `isweight'==0{
			tempvar exp
			quietly gen `exp'=1 if `touse'
			local weight "pweight"
		}
*separate dependent from regressors
		gettoken dep varlist : varlist
		if `fvops' {
			`vv' _fv_check_depvar `dep'
			`vv'  quiet _rmcoll `varlist' [aw=`exp'] if `touse'
			local varlist "`r(varlist)'"
			`vv' fvexpand `varlist' if `touse'
			local varlistt "`r(varlist)'"
			local varlist ""
			local i=1
			foreach vn of local varlistt{						
				`vv' _ms_parse_parts `vn'
				if ~`r(omit)'{
					if r(type)=="variable"{
						local varlist `varlist' `vn'
						local variable_names `variable_names' `vn'
					}
					else{
						tempvar factor`i'
						quiet gen `factor`i''=`vn' if `touse'
						local varlist `varlist' `factor`i''
	*					local temp=strtoname("`vn'",0)
	*					local variable_names `variable_names' `temp'
						local variable_names `variable_names' `vn'
					}
					local i=`i'+1
				}
			}
		}
		else{
			*check multicolinearity
			quiet _rmcoll `varlist' [aw=`exp'] if `touse'
			local varlist `r(varlist)'
			local variable_names `varlist'
		}

*number of regressors
		if "`varlist'"!=""{
			local k=wordcount("`varlist'")+1
		} 
		else{
			local k=1
		}
*temporary names
		tempname quants covariance convergence coefficients rawdev mindev coefmat nc ns
			
*Estimation of the variance, cleaning
		if strpos("`vce'",",")==0 VCEParse `vce', weight(`isweight') funcint(`functional')
		else VCEParse `vce' weight(`isweight') funcint(`functional')
		local variance `r(var)'
		local reps `r(reps)'
		local boot_method `r(boot_method)'
		local bofinger `r(bofinger)'
		local cluster `r(cluster)'
		local strata `r(strata)'
		local sub_size `r(sub_size)'
		local noreplacement `r(noreplacement)'
		local mboot `r(mboot)'
		local misspecification `r(misspecification)'
		local true_clust=("`cluster'"!="")
		local memory `r(memory)'
		if "`misspecification'"!="" & "`cluster'"=="" & "`strata'"==""{
			tempvar cluster
			quiet gen long `cluster'=_n
		}
		else if "`cluster'"!="" | "`strata'"!="" local misspecification "misspecification"
		local HC1 `r(HC1)'
		local nodfadjust `r(nodfadjust)'
		markout `touse' `cluster' `strata'
*number of observations
		quietly sum `dep' if `touse'
		local obs=r(N)
*cleaning of the quantiles
*first, the default values are -1234 but are meant as missing
*Second, the values must be between 0 and 1
		if `qlow'==-1234{
			local qlow ""
		}
		else if `qlow'<=0 | `qlow'>=1{
			dis as error "The option qlow must be strictly between 0 and 1."
			error 400
		}
		if `qhigh'==-1234{
			local qhigh ""
		}
		else if `qhigh'<=0 | `qhigh'>=1{
			dis as error "The option qhigh must be strictly between 0 and 1."
			error 400
		}
		if `qstep'==-1234{
			local qstep ""
		}
		else if `qstep'<=0 | `qstep'>=1{
			dis as error "The option qstep must be strictly between 0 and 1."
			error 400
		}
		if ("`qlow'"!="" | "`qhigh'"!="" | "`qstep'"!="") & ("`qlow'"=="" | "`qhigh'"=="" | "`qstep'"==""){
			dis as error "The options qlow, qhigh and qstep are interdependent; either all of them or none of them must be specified."
			error 400
		}
		if ("`qlow'"!="" | "`qhigh'"!="" | "`qstep'"!="") & "`quantile'"!=""{
			dis as error "The options quantile and (qlow, qhigh and qstep) cannot used simultaneously."
			error 400
		}
*default quantiles
**median if no functional
		if "`quantile'"=="" & ("`qlow'"=="" | "`qhigh'"=="" | "`qstep'"=="") & "`functional'"==""{
			local quantile "0.5"
		}
**if functional: a rule that take into account the sample size and the number of regressors
**the rule is based on Chernozhukov and Fernandez-Val (2011, REStud) for conitnuous regressors.
**At least the range 0.3-0.7 is used. Maybe a warning should be printed when the sample size is too small for functional inference???
		else if "`quantile'"=="" & ("`qlow'"=="" | "`qhigh'"=="" | "`qstep'"=="") & "`functional'"=="functional"{
			local qlow=min(15*`k'/`obs',0.3)
			local qhigh=1-`qlow'
			local qstep=0.01
		}
		if "`qlow'"=="" | "`qhigh'"=="" | "`qstep'"==""{
			mata: st_matrix("`quants'",strtoreal(tokens(st_local("quantile")))')
		}
		else{
			mata: st_matrix("`quants'",range(`qlow',`qhigh',`qstep'))
		}
*Algorithm, cleaning		
		local nq=rowsof(`quants')
		if strpos("`method'",",")==0 methodParse `method', n(`obs') k(`k') nq(`nq')
		else methodParse `method' n(`obs') k(`k') nq(`nq')
		local method `r(method)'
		local beta `r(beta)'
		local small `r(small)'
		local max_it `r(max_it)'
		local m `r(m)'
		local error_tol `r(error_tol)'
		local step `r(step)'
		local first `r(first)'
		if "`functional'"=="functional"{
			if `nq'<2{
				dis as error "It is not possible to perform functional inference with only 1 quantile regression."
				error 400
			}
			else if `nq'<5{
				dis as error "Warning: we recommend to use many quantile regressions to perform functional inference."
			}
		}
*Names of the columns of the matrices, for later. Taken from sqreg.
		capture{
			tokenize "`variable_names'"
			local nq=rowsof(`quants')
			forvalues j=1/`nq' {
				local i=1
				while "``i''" != "" {
					local conams "`conams' ``i''"
					local eqnams "`eqnams' q`j'"
					local i = `i' + 1
				}
				local conams "`conams' _cons"
				local eqnams "`eqnams' q`j'"
				local ueqnams "`ueqnams' q`j'"
 			}
		}
		if _rc{
			dis in red "Due to the high number of regressors and quantiles the rows and columns of the matrices cannot be named correctly."
			tokenize "`variable_names'"
			local nq=rowsof(`quants')
			local conams ""
			local eqnams ""
			forvalues j=1/`nq' {
				local i=1
				while "``i''" != "" {
					local conams "`conams' x`i'"
					local eqnams "`eqnams' q`j'"
					local i = `i' + 1
				}
				local conams "`conams' c"
				local eqnams "`eqnams' q`j'"
				local ueqnams "`ueqnams' q`j'"
 			}
		}
*Estimation
		preserve
		quiet keep if `touse'
		if "`method'"=="pqreg" | "`method'"=="pfn" | "`method'"=="proqreg" | "`method'"=="profn" | "`variance'"=="bootstrap"{
			foreach t in byte int long {
				foreach x of local varlist {
					cap confirm `t' variable `x'
					if _rc==0 {
						recast float `x'
					}
				}
			}
		}
		if "`method'"=="qreg"{
			mata: rq_qreg("`dep'", "`varlist'", "`weight'","`exp'", "`touse'","`quants'", "`variance'", "`bofinger'", "`cluster'", "`strata'", `level', "`coefmat'", "`covariance'", "`convergence'", "`rawdev'", "`mindev'", `small', "`nc'", "`ns'")
		}
		else if "`method'"=="fn"{
			mata: rq_fn("`dep'","`varlist'","`weight'","`exp'","`touse'","`quants'", `beta',`small',`max_it',"`variance'","`bofinger'","`cluster'","`strata'",`level',"`coefmat'", "`covariance'", "`convergence'","`rawdev'","`mindev'", "`nc'", "`ns'")
		}
		else if "`method'"=="pfn"{
			mata: rq_pfn("`dep'","`varlist'","`weight'","`exp'","`touse'","`quants'", `beta', `small', `max_it', "`variance'", "`bofinger'", "`cluster'", "`strata'", `level', "`coefmat'", "`covariance'", "`convergence'", "`rawdev'", "`mindev'", `m', `error_tol', "`nc'", "`ns'")
		}
		else if "`method'"=="pqreg"{
			mata: rq_pqreg("`dep'","`varlist'","`weight'","`exp'","`touse'","`quants'", "`variance'", "`bofinger'", "`cluster'", "`strata'", `level', "`coefmat'", "`covariance'", "`convergence'", "`rawdev'", "`mindev'", `m', `error_tol', `small', "`nc'", "`ns'")
		}
		else if "`method'"=="proqreg"{
			mata: rq_proqreg("`dep'","`varlist'","`weight'","`exp'","`touse'","`quants'", "`variance'", "`bofinger'", "`cluster'", "`strata'", `level', "`coefmat'", "`covariance'", "`convergence'", "`rawdev'", "`mindev'", `m', `error_tol', `small', "`first'", "`nc'", "`ns'")
		}
		else if "`method'"=="profn"{
			mata: rq_profn("`dep'","`varlist'","`weight'","`exp'","`touse'","`quants'", `beta',`small',`max_it', "`variance'", "`bofinger'", "`cluster'", "`strata'", `level', "`coefmat'", "`covariance'", "`convergence'", "`rawdev'", "`mindev'", `m', `error_tol', `step',"`first'", "`nc'", "`ns'")
		}
		else if "`method'"=="1step" | "`method'"=="onestep"{
			mata: rq_1step("`dep'","`varlist'","`weight'","`exp'","`touse'","`quants'", `beta', `small', `max_it', "`variance'", "`bofinger'", "`cluster'", "`strata'", `level', "`coefmat'", "`covariance'", "`convergence'", "`rawdev'", "`mindev'", `m', `error_tol', keep_for_boot=J(1,1,"`variance'"=="bootstrap"), "`first'", "`nc'", "`ns'")
		}
		mata: st_matrix("`coefficients'",vec(st_matrix("`coefmat'"))')
	
*degrees of freedom
		if `true_clust'==1{
			cap confirm scalar `nc'
			if _rc{
				mata: st_numscalar("`nc'", rows(uniqrows(st_data(., "`cluster'", "`touse'"))))
			}
		}
		if "`strata'"!=""{
			cap confirm scalar `ns'
			if _rc{
				mata: st_numscalar("`ns'", rows(uniqrows(st_data(., "`strata'", "`touse'"))))
			}
		}		
		if "`nodfadjust'"=="nodfadjust" | (`true_clust'==0 & "`strata'"==""){
			local df=`obs'-colsof(`coefficients')/`nq'
		}
		else if "`nodfadjust'"=="" & `true_clust'==1 & "`strata'"==""{
			local df=`nc'-1
		}
		else if "`nodfadjust'"=="" & `true_clust'==0 & "`strata'"!=""{
			local df=`obs'-`ns'-colsof(`coefficients')/`nq'+1
		}
		else if "`nodfadjust'"=="" & `true_clust'==1 & "`strata'"!=""{
			local df=`nc'-`ns'-colsof(`coefficients')/`nq'+1
		}
*Bootstrap
		if "`variance'"=="bootstrap" | "`variance'"=="multiplier"{
			if "`boot_method'"!="subsampling"{
				local sub_size `obs'
			}
			tempname pointwise uniform tests
			local actual_more=c(more)
			set more off
			if "`strata'"!="" & "`cluster'"!=""{
				sort `strata' `cluster', stable
				mata: st_view(strata=., ., "`strata'","`touse'")
				mata: st_view(cluster=., ., "`cluster'","`touse'")
				mata: mm_panels(strata, Sinfo=., cluster, Cinfo=.)
			}
			else if "`strata'"!="" & "`cluster'"==""{
				sort `strata', stable
				mata: st_view(strata=., ., "`strata'","`touse'")
				mata: mm_panels(strata, Sinfo=.)
				mata: Cinfo=.
			}
			else if "`strata'"=="" & "`cluster'"!=""{
				sort `cluster', stable
				mata: st_view(cluster=., ., "`cluster'","`touse'")
				mata: mm_panels(cluster, Cinfo=.)
				mata: Sinfo=.
			}
			else{
				mata: Sinfo=.
				mata: Cinfo=.
			}
			di in gr "(bootstrapping " _c
			mata: Qreg2_Results=st_matrix("`coefficients'")
			if "`variance'"=="bootstrap"{
				if "`method'"!="1step" & "`method'"!="onestep"{		
					mata: Qreg2_Boot=rq_boot("`dep'", "`varlist'","`exp'", "`touse'", "`quants'", `mboot', `error_tol', `small', st_matrix("`coefmat'"), `reps', "`boot_method'", "`cluster'", "`strata'", `noreplacement', `sub_size', Sinfo, Cinfo, 2)
				}
				else{
					mata: Qreg2_Boot=rq_boot_1step("`dep'", "`varlist'","`exp'", "`touse'", "`quants'", `small', `reps', "`boot_method'", "`cluster'", "`strata'", `noreplacement', `sub_size', Sinfo, Cinfo, `beta', `max_it', `m', `error_tol', keep_for_boot, "`first'")
				}
			}
			else{
				mata: Qreg2_Boot=rq_mult("`dep'", "`varlist'","`exp'", "`touse'", "`quants'", st_matrix("`coefmat'"), `reps', "`boot_method'", "`cluster'", "`strata'", `noreplacement', `sub_size', Sinfo, Cinfo, "`bofinger'", `level', `memory')
			}
			set more `actual_more'
			di in gr ")"
			mata: ev_boot("`variance'"=="multiplier", Qreg2_Boot, Qreg2_Results, "`quants'", `level', "`covariance'", "`pointwise'", "`uniform'", "`tests'", "`functional'"=="functional",`obs', `sub_size', `df', "`nodfadjust'"=="nodfadjust")
			mata: mata drop Qreg2_Results Qreg2_Boot
			cap restore
			mata mata drop Cinfo Sinfo
		}
		else{
			restore
			if "`HC1'"=="HC1"{
				mat `covariance'=(`nc'/(`nc'-`ns'+1))*((`obs'-1)/(`obs'-colsof(`coefficients')/`nq'))*(`nc'/(`nc'-1))*`covariance'
			}
		}

		if "`print'"!="noprint"{
			dis
			dis as text _column(0) "Quantile regression"
			dis as text _column(0) "No. of obs." _c
			dis as result _column(20) %-8.0f `obs'
			dis as text _column(0) "Algorithm:" _c
			if "`method'"=="qreg" dis as result _column(20) %-8.0f "qreg."
			if "`method'"=="pqreg" dis as result _column(20) %-8.0f "qreg with preprocessing."
			if "`method'"=="fn" dis as result _column(20) %-8.0f "Frisch-Newton interior point."
			if "`method'"=="pfn" dis as result _column(20) %-8.0f "Frisch-Newton interior point with preprocessing."
			if "`method'"=="proqreg" dis as result _column(20) %-8.0f "discretized quantile regression process with qreg."
			if "`method'"=="profn" dis as result _column(20) %-8.0f "discretized quantile regression process with Frisch-Newton."
			dis as text _column(0) "Variance:" _continue
			if "`variance'"=="novar" dis as result _column(20) %-8.0f "the variance has not been estimated."
			if "`variance'"=="bootstrap" & "`boot_method'"=="empirical" dis as result _column(20) %-8.0f "empirical bootstrap."
			if "`variance'"=="bootstrap" & "`boot_method'"=="weighted" dis as result _column(20) %-8.0f "weighted bootstrap (standard exponential weights)."
			if "`variance'"=="bootstrap" & "`boot_method'"=="subsampling" & "`replacement'"=="" dis as result _column(20) %-8.0f "subsampling with replacement (subsamples size is `sub_size')."
			if "`variance'"=="bootstrap" & "`boot_method'"=="subsampling" & "`replacement'"=="noreplacement" dis as result _column(20) %-8.0f "subsampling without replacement (subsamples size is `sub_size')."
			if "`variance'"=="kernel" dis as result _column(20) %-8.0f "kernel estimate of the sandwich as proposed by Powell(1990)."
			if "`variance'"=="nid" dis as result _column(20) %-8.0f "estimate of the sandwich as in Hendricks and Koenker (1991)."
			if "`variance'"=="iid" dis as result _column(20) %-8.0f "assuming iid error terms (not recommended!)."
			dis
			dis as text "{hline 13}" "{c TT}" "{hline 64}"
			if "`functional'"=="functional"{
				dis as text _column(0) %11s "`dep'" _column(14) "{c |}"  _column(19) %~10s "Coef." _column(29) %~10s "Pointwise" _column(45) %~8s "Pointwise" _column(60) %~21s "Functional"
				dis as text _column(14) "{c |}" _column(29) %~10s "Std. Err." _column(42) %~8s "[`level'% Conf. Int.]" _column(60) %~21s "[`level'% Conf. Int.]"
				dis as text "{hline 13}" "{c +}" "{hline 64}"
				forvalues i=1/`nq'{
					dis as text _column(0) "Quant. 0" as result round(`quants'[`i',1],0.001)  _column(14) "{c |}" 
	*				dis as text _column(0) "Ps. R2: " as result round(1-(`mindev'[1,`i']/`rawdev'[1,`i']),0.001) _column(14) "{c |}"
					forvalues j=1/`k'{
							dis as text _column(0) %11s abbrev(word("`conams'",`j'),12) _column(14) "{c |}" as result _column(18) %8.0g (`coefmat'[`j',`i']) _continue
							dis as result _column(29) %8.0g (`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5 _continue
							dis as result _column(40) %8.0g (`pointwise'[(`i'-1)*`k'+`j',1]) _continue
							dis as result _column(50) %8.0g (`pointwise'[(`i'-1)*`k'+`j',2]) _continue
							dis as result _column(61) %8.0g (`uniform'[(`i'-1)*`k'+`j',1]) _continue
							dis as result _column(71) %8.0g (`uniform'[(`i'-1)*`k'+`j',2])
					}
					if `i'<`nq' dis as text "{hline 13}" "{c +}" "{hline 64}"
					else dis as text "{hline 13}" "{c BT}" "{hline 64}"
				}
				dis
				dis as text _n "Bootstrap inference on the quantile regression process"
				dis as text "{hline 51}" "{c TT}" "{hline 26}"
				dis as text _column(52) "{c |}"  _column(62) %~10s "P-values"
				dis as text _column(0) "Null-hypothesis" _column(40) %11s "Coef." _column(52) "{c |}" _column(55) %~10s "KS-stat." _column(69) %~10s "CMS-stat."
				dis as text "{hline 51}" "{c +}" "{hline 26}"
				local i=1
				foreach null in "No effect: beta(tau)=0 for all taus" "Constant effect: beta(tau)=B for all taus" "Positive effect: beta(tau)>=0 for all taus" "Negative effect: beta(tau)<=0 for all taus" "Location-scale shift"{
					dis as text _column(0) "`null'" as text _column(52) "{c |}" 
					if `i'<5 local max_k=`k'
					else local max_k=`k'-1
					forvalues j=1/`max_k'{
						dis as text _column(30) %20s abbrev(word("`conams'",`j'),20) as text _column(52) "{c |}" as result _column(57) as result %4.3f `tests'[`j',(`i'-1)*2+1] _column(71) as result %4.3f `tests'[`j',(`i'-1)*2+2]
					}
					dis as text _column(30) %20s "all slopes"  as text _column(52) "{c |}" as result _column(57) as result %4.3f `tests'[`k'+1,(`i'-1)*2+1] _column(71) as result %4.3f `tests'[`k'+1,(`i'-1)*2+2]
					local i=`i'+1
				}
				dis as text "{hline 51}" "{c BT}" "{hline 26}"
			}
			else{
				if "`nodfadjust'"==""{
					dis as text _column(0) %11s "`dep'" _column(14) "{c |}"  _column(19) %~10s "Coef." _column(29) %~10s "Std. Err." _column(41) %~8s "t" _column(45) %~8s "P>|t|" _column(59) %~8s "[`level'% Conf. Interval]"
				}
				else{
					dis as text _column(0) %11s "`dep'" _column(14) "{c |}"  _column(19) %~10s "Coef." _column(29) %~10s "Std. Err." _column(41) %~8s "z" _column(45) %~8s "P>|z|" _column(59) %~8s "[`level'% Conf. Interval]"			
				}
				dis as text "{hline 13}" "{c +}" "{hline 64}"
				forvalues i=1/`nq'{
	*				dis as text _column(0) "Quant. 0" as result round(`quants'[`i',1],0.001)  _column(14) "{c |}" 
					dis as text _column(0) "Quant. 0" as result %-5.0g `quants'[`i',1]  _column(14) "{c |}" 
					forvalues j=1/`k'{
							dis as text _column(0) %11s abbrev(word("`conams'",`j'),12) _column(14) "{c |}" as result _column(17) %9.0g (`coefmat'[`j',`i']) _continue
							if "`variance'"!="novar"{
								dis as result _column(28) %9.0g (`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5 _continue
								dis as result _column(40) %5.2f `coefmat'[`j',`i']/(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5 _continue
								if "`nodfadjust'"==""{
									dis as result _column(49) %4.3f 2*ttail(`df',abs(`coefmat'[`j',`i']/(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5)) _continue
									dis as result _column(58) %9.0g `coefmat'[`j',`i']-invttail(`df',(100-`level')/200)*(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5 _continue
									dis as result _column(70) %9.0g `coefmat'[`j',`i']+invttail(`df',(100-`level')/200)*(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5
								}
								else{
									dis as result _column(49) %4.3f 2-2*normal(abs(`coefmat'[`j',`i']/(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5)) _continue
									dis as result _column(58) %9.0g `coefmat'[`j',`i']+invnormal((100-`level')/200)*(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5 _continue
									dis as result _column(70) %9.0g `coefmat'[`j',`i']-invnormal((100-`level')/200)*(`covariance'[(`i'-1)*`k'+`j',(`i'-1)*`k'+`j'])^0.5							
								}
							}
							else dis
					}
					dis as text "{hline 13}" "{c BT}" "{hline 64}"
				}
			}
		}
		`vv' mat colnames `coefficients' = `conams'
		mat coleq `coefficients' = `eqnams'
		mat rownames `coefficients'=`dep'	
		if "`variance'"!="novar"{
			`vv' mat rownames `covariance' = `conams'
			mat roweq `covariance' = `eqnams'
			`vv' mat colnames `covariance' = `conams'
			mat coleq `covariance' = `eqnams'
			ereturn post `coefficients' `covariance', depname(`dep') obs(`obs') dof(`df') esample(`touse')
		}
		else ereturn post `coefficients', depname(`dep') obs(`obs') dof(`df') esample(`touse')
		ereturn matrix convergence `convergence'
		`vv' mat rownames `coefmat' = `variable_names' _cons
		`vv' mat colnames `coefmat' = `ueqnams'
		ereturn matrix coefmat `coefmat'
		mat rownames `quants' = `ueqnams'
		mat colnames `quants' = "tau"
		ereturn matrix quantiles `quants'
		ereturn local predict "qrprocess_p"
		if "`bofinger'"=="" & "`variance'"=="kernel" ereturn local bwmethod "hsheather"
		else if "`variance'"=="kernel" ereturn local bwmethod "bofinger"
		if "`boot_method'"=="subsampling"{
			ereturn scalar subsize=`sub_size'
			if `noreplacement'==0 ereturn local replacement "with replacement"
			else ereturn local replacement "without replacement"
		}
		ereturn local bmethod `"`boot_method'"'
		ereturn local vce `"`variance'"'
		ereturn local method `"`method'"'
		ereturn scalar df_m=`k'-1
		mat colnames `rawdev' = `ueqnams'
		mat rownames `rawdev' = "rawdev"
		ereturn matrix sum_rdev `rawdev'
		mat colnames `mindev' = `ueqnams'
		mat rownames `mindev' = "mindev"
		ereturn matrix sum_mdev `mindev'
		ereturn local misspecification "`misspecification'"
		if `true_clust'{
			ereturn local clustvar `"`cluster'"'
			ereturn scalar N_clust=`nc'
		}
		if "`strata'"!=""{
			ereturn local stratvar `"`strata'"'
			ereturn scalar N_strat=`ns'
		}
		if `isweight'==1{
			ereturn local wtype `"`weight'"'
			ereturn local wexp `"`exp'"'
		}
		ereturn local xvar `"`variable_names'"'
		ereturn local depvar `"`dep'"'
		if "`functional'"!=""{
			`vv' mat rownames `pointwise' = `conams'
			mat roweq `pointwise' = `eqnams'
			`vv' mat rownames `uniform' = `conams'
			mat roweq `uniform' = `eqnams'
			mat colnames `pointwise'="lower_bound upper_bound"
			mat colnames `uniform'="lower_bound upper_bound"
			tokenize "`varlist'"
			local i=1
			local conam ""
			while "``i''" != "" {
				local conam "`conam' ``i''"
				local i = `i' + 1
			}
			local conam "`conam' _cons"
			`vv' mat rownames `tests'=`conam' "all slopes"
			mat colnames `tests'=KS_0 CVM_0 KS_constant CVM_constant KS_pos CVM_pos KS_neg CVM_neg KS_loc_scale CVM_loc_scale
			ereturn matrix pointwise `pointwise'
			ereturn matrix uniform `uniform'
			ereturn matrix tests `tests'
		}
		if "`variance'"=="bootstrap" | "`variance'"=="multiplier"{
			ereturn scalar rep=`reps'
		}
		ereturn local cmdline `"qrprocess `0'"'
		ereturn local title "Quantile regression"
		ereturn local cmd "qrprocess"
		ereturn local plotprocess "e(quantiles) Quantile"
		cap mata: mata drop keep_for_boot
	}
end

prog define putbinr1, rclass
	version 9.2
	syntax 
	tempname b
	mat `b'=e(b)
	return matrix b=`b'
end

program my_qreg1, sortpreserve
	version 9.2
	syntax varlist [if] [iweight pweight fweight], quantile(real)
	marksample touse
	tempname r
	quiet predict `r' if `touse', resid
	quiet replace `r'=abs(`r')
	sort `r'
	putbinr1
*for _qreg: starting values in r(b), residual sorted by their absolute values
	qui _qreg `varlist' if `touse' [aweight`exp'], quant(`quantile')
end
	
*Mata function doing the evaluation of the bootstrap results
version 9.2
mata void ev_boot(real scalar score, numeric matrix qte_cov_boot, numeric rowvector qte_cov_def, string scalar quantile, numeric scalar level, string scalar boot_cov, string scalar boot_poit_ci, string scalar boot_unif_ci, string scalar tests, real scalar ptests, real scalar obs, real scalar sub_size, real scalar df, real scalar nodfadjust)
{
	real colvector quants, sel, Kmaxuqf, Kmeanuqf, y, b
	real scalar nr, nq, k, i, Kalpha, KSstat, CMSstat, j
	real matrix Vuqf, uniform, test, Kuqf, tempvar, temp, qte_cov_boot1, qte_cov_boot2, X
	real rowvector seuqf, qte_cov_def1, qte_cov_def2, center, center1, Vuqf1
	quants=st_matrix(quantile)
	nr=rows(qte_cov_boot)
	nq=rows(quants)
	k=cols(qte_cov_def)/nq
	Vuqf=(sub_size/obs):*variance(qte_cov_boot)
	st_matrix(boot_cov,Vuqf)
	Vuqf=diagonal(Vuqf)'
	seuqf=sqrt(Vuqf)	
	if(nodfadjust==0) st_matrix(boot_poit_ci,(qte_cov_def'-invttail(df,(100-level)/200):*seuqf',qte_cov_def'+invttail(df,(100-level)/200):*seuqf'))
	if(nodfadjust==1) st_matrix(boot_poit_ci,(qte_cov_def'-invnormal((100-level)/200):*seuqf',qte_cov_def'+invnormal((100-level)/200):*seuqf'))	
	if(ptests==1){
		uniform=J(nq*k,2,0)
		test=J(k+1,10,.)
		//test of no effect, coef by coef, KS
		if(score==0) center=qte_cov_def 
		else center=J(1,cols(qte_cov_def),0)
		Kuqf=((qte_cov_boot-center#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1))):^0.5
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			Kmaxuqf=sqrt(sub_size):*rowmax(Kuqf[.,sel])
			Kalpha=mm_quantile(Kmaxuqf:/sqrt(obs),1,level/100)			
			uniform[sel',1]=(qte_cov_def[1,sel]-seuqf[1,sel]*Kalpha)'
			uniform[sel',2]=(qte_cov_def[1,sel]+seuqf[1,sel]*Kalpha)'
			KSstat=sqrt(obs):*max((qte_cov_def[1,sel]:^2:/Vuqf[sel]):^0.5)			
			test[i,1]=mean(Kmaxuqf:>=KSstat)
		}
		st_matrix(boot_unif_ci,uniform)
		//test of no effects, coef by coef, CMS
		Kuqf=((qte_cov_boot-center#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1)))
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			Kmeanuqf=sub_size:*mean(Kuqf[.,sel]')
			CMSstat=obs*mean((qte_cov_def[sel]:^2:/Vuqf[sel])')
			test[i,2]=mean(Kmeanuqf':>=CMSstat)
		}
		//Test of no effect for all coefficient (except the constant)
		if(k>1){
			qte_cov_def1=J(1,nq,.)
			qte_cov_boot1=J(nr,nq,.)
			for(i=1;i<=nq;i++){
				sel=(i-1)*k:+(1..(k-1))
				tempvar=variance(qte_cov_boot[.,sel])
				tempvar=invsym(tempvar)
				qte_cov_def1[i]=sqrt(qte_cov_def[sel]*tempvar*qte_cov_def[sel]')
				for(j=1;j<=nr;j++){
					qte_cov_boot1[j,i]=sqrt((qte_cov_boot[j,sel]-center[sel])*tempvar*(qte_cov_boot[j,sel]-center[sel])')
				}
			}
			Kmaxuqf=sqrt(sub_size):*rowmax(qte_cov_boot1)
			KSstat=sqrt(obs)*max(qte_cov_def1)
			test[k+1,1]=mean(Kmaxuqf:>=KSstat)
			Kmeanuqf=sub_size:*mean(qte_cov_boot1':^2)
			CMSstat=obs*mean(qte_cov_def1':^2)
			test[k+1,2]=mean(Kmeanuqf':>=CMSstat)		
		}	
		//test of constant effect, coef by coef, KS
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			qte_cov_def1=qte_cov_def[sel]:-mean(qte_cov_def[sel]')
			qte_cov_boot1=qte_cov_boot[.,sel]-J(1,nq,1)#mean(qte_cov_boot[.,sel]')' 
			if(score==0){
				center1=qte_cov_def1
			} else{
				center1=J(1, cols(qte_cov_def1),0)
			}
			Vuqf1=diagonal(variance(qte_cov_boot1))'
//			Kuqf=((qte_cov_boot1-qte_cov_def1#J(nr,1,1)):^2:/(Vuqf[sel]#J(nr,1,1))):^0.5
			Kuqf=((qte_cov_boot1-center1#J(nr,1,1)):^2:/(Vuqf1#J(nr,1,1))):^0.5
			Kmaxuqf=sqrt(sub_size):*rowmax(Kuqf)
			KSstat=sqrt(obs)*max((qte_cov_def1:^2:/Vuqf1):^0.5)
			test[i,3]=mean(Kmaxuqf:>KSstat)
		//test of constant effects, coef by coef, CMS
//			Kuqf=((qte_cov_boot1-qte_cov_def1#J(nr,1,1)):^2:/(Vuqf[sel]#J(nr,1,1)))
			Kuqf=((qte_cov_boot1-center1#J(nr,1,1)):^2:/(Vuqf1#J(nr,1,1)))
			Kmeanuqf=sub_size:*mean(Kuqf')
			CMSstat=obs*mean((qte_cov_def1:^2:/Vuqf1)')
			test[i,4]=mean(Kmeanuqf':>=CMSstat)
		}
		//test of constant effect, all coef except the constant, KS
		if(k>1){
			qte_cov_def1=colshape(qte_cov_def,k)[.,1..(k-1)]
			qte_cov_def1=qte_cov_def1:-mean(qte_cov_def1)#J(nq,1,1)
			qte_cov_def1=rowshape(qte_cov_def1,1)
			if(score==0){
				center1=qte_cov_def1
			} else{
				center1=J(1,(k-1)*nq,0)
			}
			qte_cov_boot1=J(nr,(k-1)*nq,.)
			for(i=1;i<=nr;i++){
				temp=colshape(qte_cov_boot[i,.],k)[.,1..(k-1)]
				temp=temp:-mean(temp)#J(nq,1,1)
				qte_cov_boot1[i,.]=rowshape(temp,1)
			}
			qte_cov_def2=J(1,nq,.)
			qte_cov_boot2=J(nr,nq,.)
			for(i=1;i<=nq;i++){
				sel=(i-1)*(k-1):+(1..(k-1))
				tempvar=variance(qte_cov_boot1[.,sel])
				tempvar=invsym(tempvar)
				qte_cov_def2[i]=sqrt(qte_cov_def1[sel]*tempvar*qte_cov_def1[sel]')
				for(j=1;j<=nr;j++){
					qte_cov_boot2[j,i]=sqrt((qte_cov_boot1[j,sel]-center1[sel])*tempvar*(qte_cov_boot1[j,sel]-center1[sel])')
				}
			}
			Kmaxuqf=sqrt(sub_size):*rowmax(qte_cov_boot2)
			KSstat=sqrt(obs)*max(qte_cov_def2)
			test[k+1,3]=mean(Kmaxuqf:>=KSstat)
			Kmeanuqf=sub_size:*mean(qte_cov_boot2':^2)
			CMSstat=obs*mean(qte_cov_def2':^2)
			test[k+1,4]=mean(Kmeanuqf':>=CMSstat)		
		}	
		//test of stochastic dominance, coef by coef, KS
		qte_cov_boot1=qte_cov_boot-center#J(nr,1,1)
		qte_cov_boot1=qte_cov_boot1:*(qte_cov_boot1:<=0)
		qte_cov_def1=qte_cov_def:*(qte_cov_def:<=0)
		Kuqf=(qte_cov_boot1:^2:/(Vuqf#J(nr,1,1))):^0.5
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			Kmaxuqf=sqrt(sub_size):*rowmax(Kuqf[.,sel])
			KSstat=sqrt(obs)*max((qte_cov_def1[1,sel]:^2:/Vuqf[sel]):^0.5)
			test[i,5]=mean(Kmaxuqf:>=KSstat)
		}
		//test of stochastic dominance, coef by coef, CMS
		Kuqf=qte_cov_boot1:^2:/(Vuqf#J(rows(qte_cov_boot),1,1))
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			Kmeanuqf=sub_size:*mean(Kuqf[.,sel]')
			CMSstat=obs*mean((qte_cov_def1[1,sel]:^2:/Vuqf[sel])')
			test[i,6]=mean(Kmeanuqf':>=CMSstat)
		}
		//Test of stochastic dominance for all coefficient (except the constant)
		if(k>1){
			qte_cov_def2=J(1,nq,.)
			qte_cov_boot2=J(nr,nq,.)
			for(i=1;i<=nq;i++){
				sel=(i-1)*k:+(1..(k-1))
				tempvar=variance(qte_cov_boot[.,sel])
				tempvar=invsym(tempvar)
				qte_cov_def2[i]=sqrt(qte_cov_def1[sel]*tempvar*qte_cov_def1[sel]')
				for(j=1;j<=nr;j++){
					qte_cov_boot2[j,i]=sqrt(qte_cov_boot1[j,sel]*tempvar*qte_cov_boot1[j,sel]')
				}
			}
			Kmaxuqf=sqrt(sub_size):*rowmax(qte_cov_boot2)
			KSstat=sqrt(obs)*max(qte_cov_def2)
			test[k+1,5]=mean(Kmaxuqf:>=KSstat)
			Kmeanuqf=sub_size:*mean(qte_cov_boot2':^2)
			CMSstat=obs*mean(qte_cov_def2':^2)
			test[k+1,6]=mean(Kmeanuqf':>=CMSstat)		
		}			
		//test of being stochastically dominated, KS
		qte_cov_boot1=qte_cov_boot-center#J(nr,1,1)
		qte_cov_boot1=qte_cov_boot1:*(qte_cov_boot1:>=0)
		qte_cov_def1=qte_cov_def:*(qte_cov_def:>=0)
		Kuqf=(qte_cov_boot1:^2:/(Vuqf#J(nr,1,1))):^0.5
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			Kmaxuqf=sqrt(sub_size):*rowmax(Kuqf[.,sel])
			KSstat=sqrt(obs)*max((qte_cov_def1[1,sel]:^2:/Vuqf[sel]):^0.5)
			test[i,7]=mean(Kmaxuqf:>=KSstat)
		}
		//test of being stochastically dominated, CMS
		Kuqf=qte_cov_boot1:^2:/(Vuqf#J(nr,1,1))
		for(i=1;i<=k;i++){
			sel=(1..nq):*k:+i:-k
			Kmeanuqf=sub_size:*mean(Kuqf[.,sel]')
			CMSstat=obs*mean((qte_cov_def1[1,sel]:^2:/Vuqf[sel])')
			test[i,8]=mean(Kmeanuqf':>=CMSstat)
		}
		//Test of being stochastically dominated for all coefficient (except the constant)
		if(k>1){
			qte_cov_def2=J(1,nq,.)
			qte_cov_boot2=J(nr,nq,.)
			for(i=1;i<=nq;i++){
				sel=(i-1)*k:+(1..(k-1))
				tempvar=variance(qte_cov_boot[.,sel])
				tempvar=invsym(tempvar)
				qte_cov_def2[i]=sqrt(qte_cov_def1[sel]*tempvar*qte_cov_def1[sel]')
				for(j=1;j<=nr;j++){
					qte_cov_boot2[j,i]=sqrt(qte_cov_boot1[j,sel]*tempvar*qte_cov_boot1[j,sel]')
				}
			}
			Kmaxuqf=sqrt(sub_size):*rowmax(qte_cov_boot2)
			KSstat=sqrt(obs)*max(qte_cov_def2)
			test[k+1,7]=mean(Kmaxuqf:>=KSstat)
			Kmeanuqf=sub_size:*mean(qte_cov_boot2':^2)
			CMSstat=obs*mean(qte_cov_def2':^2)
			test[k+1,8]=mean(Kmeanuqf':>=CMSstat)		
		}			
		//Location scale shift hypothesis
		if(k>1 & nq>2){
			sel=(1..nq):*k
			X=J(nq,1,1),qte_cov_def[sel]'
			for(i=1;i<k;i++){
				sel=(1..nq):*k:+i:-k
				y=qte_cov_def[sel]'
				b=invsym(X'X)*X'y 
				qte_cov_def1[sel]=(y-X*b)'
			}
			if(score==0){
				for(j=1;j<=nr;j++){
					sel=(1..nq):*k
					X=J(nq,1,1),qte_cov_boot[j,sel]'
					for(i=1;i<k;i++){
						sel=(1..nq):*k:+i:-k
						y=qte_cov_boot[j,sel]'
						b=invsym(X'X)*X'y 
						qte_cov_boot1[j,sel]=(y-X*b)'
					}
				}
				center1=qte_cov_def1
			} else{
				qte_cov_boot1=qte_cov_boot
				center1=J(1,cols(qte_cov_def1),0)
			}
			Vuqf=diagonal(variance(qte_cov_boot1))'
			//KS
			Kuqf=((qte_cov_boot1-center1#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1))):^0.5
			for(i=1;i<k;i++){
				sel=(1..nq):*k:+i:-k
				Kmaxuqf=sqrt(sub_size):*rowmax(Kuqf[.,sel])
				KSstat=sqrt(obs)*max((qte_cov_def1[1,sel]:^2:/Vuqf[sel]):^0.5)
				test[i,9]=mean(Kmaxuqf:>=KSstat)
			}
			//CMS
			Kuqf=((qte_cov_boot1-center1#J(nr,1,1)):^2:/(Vuqf#J(nr,1,1)))
			for(i=1;i<k;i++){
				sel=(1..nq):*k:+i:-k
				Kmeanuqf=sub_size:*mean(Kuqf[.,sel]')
				CMSstat=obs*mean((qte_cov_def1[1,sel]:^2:/Vuqf[sel])')
				test[i,10]=mean(Kmeanuqf':>=CMSstat)
			}
			//Location scale shift model for all coefficient (except the constant)
			qte_cov_def2=J(1,nq,.)
			qte_cov_boot2=J(nr,nq,.)
			for(i=1;i<=nq;i++){
				sel=(i-1)*k:+(1..(k-1))
				tempvar=variance(qte_cov_boot1[.,sel])
				tempvar=invsym(tempvar)
				qte_cov_def2[i]=sqrt(qte_cov_def1[sel]*tempvar*qte_cov_def1[sel]')
				for(j=1;j<=nr;j++){
					qte_cov_boot2[j,i]=sqrt((qte_cov_boot1[j,sel]-center1[sel])*tempvar*(qte_cov_boot1[j,sel]-center1[sel])')
				}
			}
			Kmaxuqf=sqrt(sub_size):*rowmax(qte_cov_boot2)
			KSstat=sqrt(obs)*max(qte_cov_def2)
			test[k+1,9]=mean(Kmaxuqf:>=KSstat)
			Kmeanuqf=sub_size:*mean(qte_cov_boot2':^2)
			CMSstat=obs*mean(qte_cov_def2':^2)
			test[k+1,10]=mean(Kmeanuqf':>=CMSstat)		
		}
		st_matrix(tests,test)
	}
}

version 9.2
mata void rq_qreg(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, real scalar small, string scalar nc, string scalar ns)
{
	real colvector quants, y, w, uy
	real rowvector conv, rdev, mdev
	real matrix x, coef, cov
	real scalar alpha, i
	string rowvector treg
	quants=st_matrix(quantile)
	conv=rdev=mdev=J(1,rows(quants),.)
	y=st_data(.,dep,touse)
	treg=tokens(reg)
	if(length(treg)>0){
		x=st_data(.,treg,touse)
		x=x,J(rows(x),1,1)
	}
	else x=J(rows(y),1,1)
	w=st_data(.,weight,touse)
	coef=J(cols(x),rows(quants),.)
	for (i=1; i<=rows(quants); i++) {
		uy=y:-mm_quantile(y,w,quants[i])
		rdev[i]=colsum(uy:*(quants[i]:-(uy:<0)))
		if(i==1){
			stata("qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants[i])+")",1)
			conv[i]=st_numscalar("e(convcode)")
		}
		else{
			stata("my_qreg1 "+dep+" "+reg+" [iweight="+weight+"], quantile("+strofreal(quants[i])+")",1) 
			conv[i]=st_numscalar("r(convcode)")
		}
		coef[.,i]=st_matrix("e(b)")'	
		uy=y:-x*coef[.,i]
		mdev[i]=colsum(uy:*(quants[i]:-(uy:<0)))

	}
	if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
		alpha=1-0.01*level
		cov=rqcov(y, x, weight_type, w, coef, quants, "my_qreg1 "+dep+" "+reg+" [iweight="+weight+"], quantile(", alpha, ., small, ., variance, bofinger, cluster, strata, touse, nc, ns)
	}
	else cov=0
	st_matrix(results,coef)
	st_matrix(covariance,cov)
	st_matrix(convergence,conv)
	st_matrix(rawdev,rdev)
	st_matrix(mindev,mdev)
}

version 9.2
mata void rq_fn(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, real scalar beta, real scalar small, real scalar max_it, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, string scalar nc, string scalar ns)
{
	real colvector quants, y, w, uy
	real rowvector conv, rdev, mdev
	string rowvector reg1
	real matrix x, coef, cov
	real scalar alpha, i, n
	quants=st_matrix(quantile)
	conv=rdev=mdev=J(1,rows(quants),.)
	y=st_data(.,dep,touse)
	n=rows(y)
	reg1=tokens(reg)
	if(length(reg1)>0) x=st_data(.,reg1,touse),J(n,1,1)
	else x=J(n,1,1)
	w=st_data(.,weight,touse)
	coef=J(cols(x),rows(quants),.)
	for (i=1; i<=rows(quants); i++) {
		uy=y:-mm_quantile(y,w,quants[i])
		rdev[i]=colsum(uy:*(quants[i]:-(uy:<0)))
		if(i==1){
			coef[.,i]=int_fnm(y, x, w, quants[i], beta, small, max_it, conv[i],0)
		}
		else{
			coef[.,i]=int_fnm(y, x, w, quants[i], beta, small, max_it, conv[i],coef[.,i-1])
		}
		if(colmissing(coef[.,i]) | conv[i]==0){
			stata("qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants[i])+")",1)
			coef[.,i]=st_matrix("e(b)")'
			conv[i]=st_numscalar("e(convcode)")	
		}
		uy=y:-x*coef[.,i]
		mdev[i]=colsum(uy:*(quants[i]:-(uy:<0)))
	}
	if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
		alpha=1-0.01*level
		cov=rqcov(y, x, weight_type, w, coef, quants, "qrprocess "+dep+" "+reg+" [iweight="+weight+"], vce(novar) method(fn)  quantile(", alpha, beta, small, max_it, variance, bofinger, cluster, strata, touse, nc, ns)
	}
	else cov=0
	st_matrix(results,coef)
	st_matrix(covariance,cov)
	st_matrix(convergence,conv)
	st_matrix(rawdev,rdev)
	st_matrix(mindev,mdev)
}

version 9.2
mata real colvector int_fnm(real colvector dep, real matrix X, real colvector wei, real scalar p, real scalar beta, real scalar small, real scalar max_it, real scalar convergence, real colvector start)
{
	real colvector weight, c, b, x, s, y, r, z, w, q, rhs, dy, dx, ds, dz, dw, fx, fs, fw, fz, fp, fd, dxdz, dsdw, xinv, sinv, xi
	real scalar n, gap, it, mu, g
	real matrix A, AQ, invAQ
	weight=wei:/mean(wei)
	n=rows(X)
	A=(X:*weight)
	c=-(dep:*weight)
	b=colsum(A):*(1-p)
	x=J(n,1,1-p)
	s=J(n,1,p)
	if(start==0){
		y = (invsym(cross(A, A))*cross(A,c))
		y[cols(X)]=y[cols(X)]+mm_quantile(c - cross(A' , y),1,p)
	}
	else{
		y=start
	}
	r = c - cross(A' , y)
	r = r + 0.001 * (r :== 0)
	z = r :* (r :> 0)
	w = z - r
	it = 0
	while(it < max_it){
    	it = it + 1
    	q=1:/(z:/x+w:/s)
    	r = z - w
		AQ = A:*sqrt(q)
		rhs=r:*sqrt(q)
		invAQ=invsym(cross(AQ, AQ))
		dy = invAQ*cross(AQ,rhs)
		dx = q :* (A*dy - r)    
		ds = -dx
		dz = -z :* (1 :+ dx:/x)
		dw = -w :* (1 :+ ds:/s)
		fx = bound(x, dx)
		fs = bound(s, ds)
		fw = bound(w, dw)
		fz = bound(z, dz)
		fp = rowmin((fx, fs))
		fd = colmin((fw, fz))
		fp = min((beta * fp\ 1))
		fd = min((beta * fd, 1))
		if(min((fp, fd)) < 1){
			mu = z ' x + w ' s
			g = (z + fd * dz) ' (x + fp * dx) + (w + fd * dw) ' (s + fp * ds)
			mu = mu * (g / mu) ^3 / ( 2* n)
			dxdz = dx :* dz
			dsdw = ds :* dw
			xinv = 1 :/ x
			sinv = 1 :/ s
			xi = mu * (xinv - sinv)
			rhs = rhs + sqrt(q):*(dxdz - dsdw :- xi)
			dy = invAQ*cross(AQ,rhs)
			dx = q :* (A*dy  - r -dxdz + dsdw:+ xi)
			ds = -dx
			dz = mu * xinv - z - xinv :* z :* dx - dxdz
			dw = mu * sinv - w - sinv :* w :* ds - dsdw
 			fx = bound(x, dx)
			fs = bound(s, ds)
			fw = bound(w, dw)
			fz = bound(z, dz)
			fp = rowmin((fx, fs))
			fd = colmin((fw, fz))
			fp = min((beta * fp\ 1))
			fd = min((beta * fd, 1))
		}  
		x = x + fp * dx
		s = s + fp * ds
		gap=fd * dy
		y = y + gap
		if(max(abs(gap)) < small){
			if(c'x-b*y+sum(w)<small){
				break
			}
		}
		w = w + fd * dw
		z = z + fd * dz
	}
	convergence=(it==max_it)
	return(-y)
}

version 9.2
mata real colvector bound(real colvector x, real colvector dx)
{
	real colvector b, f
	f=(dx:>=0)
	b=f:*1e20-(1:-f):*x:/dx
	return(b)
}

version 9.2
mata void rq_pfn(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, real scalar beta, real scalar small, real scalar max_it, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, real scalar mfactor, real scalar error_tol, string scalar nc, string scalar ns)
{
	real colvector y, quants, w
	real rowvector conv, rdev, mdev
	real matrix x, coef, cov
	real scalar n, k, m, minmaxy, miny, maxy, i, alpha
	string rowvector treg
	y=st_data(.,dep,touse)
	n=rows(y)
	treg=tokens(reg)
	if(length(treg)>0){
		x=st_data(.,treg,touse)
		x=x,J(n,1,1)
	}
	else x=J(n,1,1)
	k=cols(x)
	m=round((k*n)^(2/3))
	if(m>=n | mfactor*m>=n){
		rq_fn(dep,reg,weight_type,weight,touse,quantile, beta,small,max_it,variance,bofinger,cluster,strata,level,results, covariance, convergence,rawdev,mindev)
	}
	else{
		quants=st_matrix(quantile)
		conv=rdev=mdev=J(1,rows(quants),.)
		w=st_data(.,weight,touse)
		coef=J(cols(x),rows(quants),.)
		minmaxy=minmax(y)
		miny=2*minmaxy[1]-minmaxy[2]
		maxy=2*minmaxy[2]-minmaxy[1]
		for (i=1; i<=rows(quants); i++) {
			if(i==1) coef[.,i]=int_pfnm(y, x, w, quants[i], beta, small, max_it, conv[i],0,m,mfactor,n,error_tol,miny,maxy,rdev[i],mdev[i])
			else coef[.,i]=int_pfnm(y, x, w, quants[i], beta, small, max_it, conv[i],coef[.,i-1],m,mfactor,n,error_tol,miny,maxy,rdev[i],mdev[i])
			if(colmissing(coef[.,i]) | conv[i]==0){
				stata("qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants[i])+")",1)
				coef[.,i]=st_matrix("e(b)")'
				conv[i]=st_numscalar("e(convcode)")	
			}
		}
		if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
			alpha=1-0.01*level
			cov=rqcov(y, x, weight_type, w, coef, quants, "qrprocess "+dep+" "+reg+" [iweight="+weight+"], vce(novar) method(pfn, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+"))  quantile(", alpha, beta, small, max_it, variance, bofinger, cluster, strata, touse, nc, ns)
		}
		else cov=0
		st_matrix(results,coef)
		st_matrix(covariance,cov)
		st_matrix(convergence,conv)
		st_matrix(rawdev,rdev)
		st_matrix(mindev,mdev)
	}
}
			
version 9.2
mata real colvector int_pfnm(real colvector y, real matrix x, real colvector w, real scalar p, real scalar beta, real scalar small, real scalar max_it, real scalar convergence, real colvector start, real scalar m, real scalar mfactor, real scalar n, real scalar error_tol, real scalar miny, real scalar maxy, real scalar rdev, real scalar mdev)
{
	real scalar not_optimal, m_temp, iter, M, lo_q, hi_q, s_bad
	real colvector s, yy, ww, z, band, r, kappa, sl, su, su_bad, sl_bad, uy
	real matrix xx, xxinv
	not_optimal=1
	m_temp=m
	iter=1
	while(not_optimal){
		if(start==0 | iter==2){
			s=mm_sample(min((m_temp\n)),n,.,.,1)
			xx=x[s,.]
			yy=y[s]
			ww=w[s]	
			z=int_fnm(yy, xx, ww, p, beta, small, max_it, convergence,0)
			xxinv=luinv(cross(xx,xx))
		}
		else{
			z=start
			xxinv=luinv(cross(x,x))
		}
		band=sqrt(colsum((cross(x',xxinv)':^2))')
		r=y-cross(x',z)
		M=mfactor*m_temp
		lo_q=max(((1/n)\p-M/(2*n)))
		hi_q=min((p+M/(2*n)\(n-1)/n))
		kappa=mm_quantile(r:/rowmax((band,J(n,1,small))),1,(lo_q\hi_q))
		sl=(r:<band:*kappa[1,1])
		su=(r:>band:*kappa[2,1])
		while(not_optimal){
			xx=select(x,(!sl):*(!su))
			yy=select(y,(!sl):*(!su))
			ww=select(w,(!sl):*(!su))
			if(any(sl)){
				xx=xx\mean(select(x,sl),select(w,sl))
				yy=yy\miny
				ww=ww\sum(select(w,sl))
			}
			if(any(su)){
				xx=xx\mean(select(x,su),select(w,su))
				yy=yy\maxy
				ww=ww\sum(select(w,su))
			}
			z=int_fnm(yy, xx, ww, p, beta, small, max_it, convergence,z)
			r=y-cross(x',z)
			su_bad=(r:<0):*su
			sl_bad=(r:>0):*sl
			s_bad=sum((su_bad\sl_bad))
			if(s_bad>error_tol){
				if(sum(s_bad>0.1*M)){
					m_temp=2*m_temp
					iter=2
					break
				}
				su=su:*(!su_bad)
				sl=sl:*(!sl_bad)
			}
			else{
				not_optimal=0
			}
		}
	}
	uy=y:-mm_quantile(y,w,p)
	rdev=colsum(uy:*(p:-(uy:<0)))
	uy=y:-x*z
	mdev=colsum(uy:*(p:-(uy:<0)))
	return(z)
}

version 9.2
mata void rq_pqreg(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, real scalar mfactor, real scalar error_tol, real scalar small, string scalar nc, string scalar ns)
{
	real colvector y, w, sl,su, itemptouse, quants, uy, z, band, r, kappa, su_bad, sl_bad
	real rowvector index_reg, conv, rdev, mdev
	real matrix x, coef, xxinv, cov
	real scalar index_dep, index_touse, index_weight, n, k, m, index_temptouse, nq, minmaxy, miny, maxy, n_obs_data, i, not_optimal, m_temp, lo_q, hi_q, s_bad, alpha, rc
	string scalar temptouse
	index_dep=st_varindex(dep)
	index_reg=st_varindex(tokens(reg))
	index_touse=st_varindex(touse)
	index_weight=st_varindex(weight)
//views on the data
	y=st_data(.,index_dep,index_touse)
	n=rows(y)
	x=st_data(.,(index_reg,index_touse),index_touse)
	k=cols(x)
//number of observations in the estimation subsample (before multiplication with mfactor
	m=round((k*n)^(2/3))
	if(m>=n | mfactor*m>=n){
		rq_qreg(dep, reg, weight_type, weight, touse, quantile, variance, bofinger, cluster, strata, level, results, covariance, convergence, rawdev, mindev, small, nc, ns)
	}
	else{
		w=st_data(.,weight,touse)
		stata("recast double "+weight)
		stata("recast double "+reg)
//add two pseudo observations
		st_addobs(2)
		n_obs_data=st_nobs()
//put them in th estimation sample
		st_store((n_obs_data-1\n_obs_data),index_touse,(1\1))
//temporary using sample
		index_temptouse = st_addvar("byte", temptouse=st_tempname())
		st_view(itemptouse=.,.,index_temptouse,index_touse)
//weights and regressors for the pseudo-observations
		quants=st_matrix(quantile)
		nq=rows(quants)
		conv=rdev=mdev=J(1,nq,.)
		coef=J(cols(x),nq,.)
		minmaxy=minmax(y)
		miny=2*minmaxy[1]-minmaxy[2]
		maxy=2*minmaxy[2]-minmaxy[1]
		st_store((n_obs_data-1\n_obs_data),index_dep,(miny\maxy))
		xxinv=invsym(cross(x,x))
		band=sqrt(colsum((cross(x',xxinv)':^2))')
		band=rowmax((band,J(n,1,small)))
		for (i=1; i<=rows(quants); i++) {
			uy=y:-mm_quantile(y,w,quants[i])
			rdev[i]=colsum(uy:*(quants[i]:-(uy:<0)))
			not_optimal=1
			m_temp=m*mfactor
			while(not_optimal){
				itemptouse[.,1]=mm_srswor(min((m_temp\n)),n,1)\0\0
				rc=_stata("_qreg "+dep+" "+reg+" if "+temptouse+" [aweight="+weight+"], quantile("+strofreal(quants[i])+")",1)
				z=st_matrix("e(b)")'
				while(sum(z:<maxdouble())+sum(z:>mindouble())<2*k | rc>0){
					m_temp=2*m_temp
					itemptouse[.,1]=mm_sample(min((m_temp\n)),n,.,.,1,1)\0\0
					rc=_stata("_qreg "+dep+" "+reg+" if "+temptouse+" [aweight="+weight+"], quantile("+strofreal(quants[i])+")",1)
					z=st_matrix("e(b)")'
				}	
				r=y-cross(x',z)
				lo_q=max(((1/n)\quants[i]-m_temp/(2*n)))
				hi_q=min((quants[i]+m_temp/(2*n)\(n-1)/n))
				kappa=mm_quantile(r:/band,1,(lo_q\hi_q))
				sl=(r:<band:*kappa[1,1])
				su=(r:>band:*kappa[2,1])
				while(not_optimal){
					itemptouse[(1..n)',1]=(!sl):*(!su)
					if(any(sl)){
						if(k>1) st_store(n_obs_data-1, index_reg, mean(select(x[.,1..(k-1)],sl), select(w,sl)))
						st_store(n_obs_data-1,index_weight,sum(select(w,sl)))
						itemptouse[n+1,1]=1
					}
					else itemptouse[n+1,1]=0
					if(any(su)){
						if(k>1) st_store(n_obs_data,index_reg,mean(select(x[.,1..(k-1)],su),select(w,su)))
						st_store(n_obs_data,index_weight,sum(select(w,su)))
						itemptouse[n+2,1]=1
					}
					else itemptouse[n+2,1]=0
					rc=_stata("_qreg "+dep+" "+reg+" if "+temptouse+" [aweight="+weight+"], quantile("+strofreal(quants[i])+")",1)
					z=st_matrix("e(b)")'
					if(rows(z)<k | rc>0){
						m_temp=2*m_temp
						break
					}					
					r=y-cross(x',z)					
					su_bad=(r:<0):*su
					sl_bad=(r:>0):*sl
					s_bad=sum((su_bad\sl_bad))
					if(s_bad>error_tol){
						if(s_bad>0.1*m){
							m_temp=2*m_temp
							m=m*2
							break
						}
						m=m*1.02
						su[.,1]=su:*(!su_bad)
						sl[.,1]=sl:*(!sl_bad)
					}
					else{
						m=0.98*m
						not_optimal=0
					}
				}
			}
			conv[i]=st_numscalar("r(convcode)")	
			coef[.,i]=z
			uy=y:-cross(x',z)
			mdev[i]=colsum(uy:*(quants[i]:-(uy:<0)))
		}
		st_dropobsin((n_obs_data-1\n_obs_data))
		if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
			alpha=1-0.01*level
			cov=rqcov(y, x, weight_type, w, coef, quants, "qrprocess "+dep+" "+reg+" [iweight="+weight+"], vce(novar) method(pqreg, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+")) quantile(", alpha, ., small, ., variance, bofinger, cluster, strata, touse, nc, ns)
		}
		else cov=0
		st_matrix(results,coef)
		st_matrix(covariance,cov)
		st_matrix(convergence,conv)
		st_matrix(rawdev,rdev)
		st_matrix(mindev,mdev)
	}
}

version 9.2
mata real matrix rq_boot(string scalar dep, string scalar reg, string scalar weight, string scalar touse, string scalar quantile, real scalar mfactor, real scalar error_tol, real scalar small, real matrix start, real scalar reps, string scalar boot_method, string scalar cluster, string scalar strata, real scalar noreplacement, real scalar subsize, real matrix Sinfo, real colvector Cinfo, real scalar memory)
{
	real colvector y, quants, w, band, s, resid, kappa, bweight, z, sl, su, su_bad, sl_bad
	real matrix x, xxinv, sel, boot_res, temp_coef
	real scalar index_dep, index_weight, index_touse, n, k, m, M, nq, miny, maxy, index_tempname, index_temp_bw, n_obs_data, i, lo_q, hi_q, r, not_optimal, m_temp, iter, s_bad, rc
	real rowvector index_reg, minmaxy, sel_index
	string scalar tempname, temp_bw
	index_dep=st_varindex(dep)
	index_reg=st_varindex(tokens(reg))
	index_weight=st_varindex(weight)
	index_touse=st_varindex(touse)
	y=st_data(.,index_dep,index_touse)	
	x=st_data(.,(index_reg,index_touse),index_touse)
	n=rows(y)
	k=cols(x)
	m=round((k*n)^(1/2))
	M=mfactor*m
	quants=st_matrix(quantile)
	nq=rows(quants)
	w=st_data(.,index_weight,index_touse)
	minmaxy=minmax(y)
	miny=2*minmaxy[1]-minmaxy[2]
	maxy=2*minmaxy[2]-minmaxy[1]
	index_tempname=st_addvar("byte", tempname=st_tempname())
	index_temp_bw=st_addvar("double", temp_bw=st_tempname())
	st_addobs(2)
	n_obs_data=st_nobs()
	xxinv=invsym(cross(x,x))
	band=sqrt(colsum((cross(x',xxinv)':^2))')
	band=rowmax((band,J(n,1,small)))
	if(memory>0){
		if(memory==2) sel=J(n,nq,.) 
		else sel_index=J(nq,1,.)
		for (i=1; i<=nq; i++) {
			lo_q=max(((1/n)\quants[i]-M/(2*n)))
			hi_q=min((quants[i]+M/(2*n)\(n-1)/n))
			resid=y-cross(x',start[.,i])
			kappa=mm_quantile(resid:/band,1,(lo_q\hi_q))
			if(memory==2) sel[.,i]=0:-(resid:<band*kappa[1,1]):+(resid:>band*kappa[2,1])
			else{
				sel_index[i]=st_addvar("byte",st_tempname())
				st_store((1..(n_obs_data-2))',sel_index[i],index_touse,0:-(resid:<band*kappa[1,1]):+(resid:>band*kappa[2,1]))
			}
		}
	}
	boot_res=J(reps,nq*k,.)
	if(Cinfo!=. & Sinfo==.){
		subsize=rows(Cinfo)*subsize/n
	}
	else if(Cinfo==. & Sinfo!=.){
		subsize=Sinfo*subsize/n
	}
	else if(Cinfo!=. & Sinfo!=.){
		subsize=Sinfo[.,2]*subsize/n
	}	
	for (r=1; r<=reps; r++){
sl:		if(boot_method=="weighted"){
			st_store((n_obs_data-1\n_obs_data),index_touse,(0\0))
			bweight=prepare_weights(w[1..(n_obs_data-2)], index_touse, cluster, strata, "")
		}
		else{
			if(cluster=="" & strata==""){
				if(noreplacement==0){
					bweight=mm_srswr(subsize, n, 1):*w
				}
				else{
					bweight=mm_srswor(subsize, n, 1):*w
				}
			}
			else{
				bweight=mm_sample(subsize, Sinfo, Cinfo, 1, noreplacement, 1):*w
			}
		}
		st_store((1..(n_obs_data-2))',index_temp_bw,index_touse,bweight)
		stata("_rmcoll "+reg+" [iweight="+temp_bw+"]",1)
		if(st_global("r(varlist)")!=reg){
			stata(`"dis in red "x" _continue"')
			goto sl
		}
		else{
			temp_coef=J(k,nq,.)
			for (i=1; i<=rows(quants); i++) {
				not_optimal=1
				m_temp=M
				iter=1
				while(not_optimal){
					if(iter>1 | memory==0){
						lo_q=max(((1/n)\quants[i]-m_temp/(2*n)))
						hi_q=min((quants[i]+m_temp/(2*n)\(n-1)/n))
						resid=y-cross(x',start[.,i])
						kappa=mm_quantile(resid:/band,1,(lo_q\hi_q))
						sl=(resid:<band*kappa[1,1])
						su=(resid:>band*kappa[2,1])
					}
					else if(memory==2){
						sl=(sel[.,i]:==-1)
						su=(sel[.,i]:==1)
					}
					else if(memory==1){
						sel=st_data((1..(n_obs_data-2))',sel_index[i],index_touse)
						sl=(sel:==-1)
						su=(sel:==1)
					}
					while(not_optimal){
						s=(!sl):*(!su):*(bweight:>0)
						if(any(sl)){
							if(k>1) st_store(n_obs_data-1,index_reg,mean(select(x[.,1..(k-1)],sl),select(bweight,sl)))
							st_store(n_obs_data-1,index_dep,miny)
							st_store(n_obs_data-1,index_temp_bw,sum(select(bweight,sl)))
							st_store(n_obs_data-1,index_touse,1)
							s=s\1
						}
						else s=s\0
						if(any(su)){
							if(k>1) st_store(n_obs_data,index_reg,mean(select(x[.,1..(k-1)],su),select(bweight,su)))
							st_store(n_obs_data,index_dep,maxy)
							st_store(n_obs_data,index_temp_bw,sum(select(bweight,su)))
							st_store(n_obs_data,index_touse,1)
							s=s\1
						}
						else s=s\0
						st_store(.,index_tempname,index_touse,s)
						rc=_stata("_qreg "+dep+" "+reg+" if "+tempname+" [aweight="+temp_bw+"], quantile("+strofreal(quants[i])+")",1)
						z=st_matrix("e(b)")'
						st_store((n_obs_data-1\n_obs_data),index_tempname,(0\0))
						if(rows(z)==0) z=0
						if(rows(z)<k | rc>0 | colmissing(z)){
							if(m_temp>10*M){
								goto sl
							}
							m_temp=2*m_temp
							iter=2
							break
						}					
						resid=y-cross(x',z)					
						su_bad=(resid:<0):*su:*(bweight:>0)
						sl_bad=(resid:>0):*sl:*(bweight:>0)
						s_bad=sum((su_bad\sl_bad))
						if(s_bad>error_tol){
							if(s_bad>0.1*m_temp){
								m_temp=2*m_temp
								iter=2
								break
							}
							su=su:*(!su_bad)
							sl=sl:*(!sl_bad)
						}
						else{
							not_optimal=0
						}
					}
				}
				if(colmissing(z)==0) temp_coef[.,i]=z
				else goto sl
			}		
			boot_res[r,.]=vec(temp_coef)'
			stata(`"di in gr "." _c"')
		}
	}
	return(boot_res)
}

version 9.2
mata real matrix rq_mult(string scalar dep, string scalar reg, string scalar weight, string scalar touse, string scalar quantile, real matrix start, real scalar reps, string scalar boot_method, string scalar cluster, string scalar strata, real scalar noreplacement, real scalar subsize, real matrix Sinfo, real colvector Cinfo, string scalar bofinger, real scalar level, real scalar memory)
{
	real colvector y, quants, w
	real matrix x, boot_res
	real scalar index_dep, index_weight, index_touse, k, nq
	real rowvector index_reg
	index_dep=st_varindex(dep)
	index_reg=st_varindex(tokens(reg))
	index_weight=st_varindex(weight)
	index_touse=st_varindex(touse)
	w=st_data(.,index_weight,index_touse)
	y=st_data(., index_dep, index_touse)
	x=st_data(., (index_reg,index_touse), index_touse)
	quants=st_matrix(quantile)
	k=cols(x)
	nq=rows(quants)
	if(memory==0 | (memory==1 & nq*k<reps)) boot_res=rq_multiplier(y, x, w, quants, index_touse, start, reps, boot_method, cluster, strata, noreplacement, subsize, Sinfo, Cinfo, bofinger, level)
	else if(memory<=1) boot_res=rq_multiplier1(y, x, w, quants, index_touse, start, reps, boot_method, cluster, strata, noreplacement, subsize, Sinfo, Cinfo, bofinger, level)
	else if(memory==2) boot_res=rq_multiplier2(y, x, w, quants, index_touse, start, reps, boot_method, cluster, strata, noreplacement, subsize, Sinfo, Cinfo, bofinger, level)
	return(boot_res)
}

version 9.2
mata real matrix rq_multiplier(real colvector y, real matrix x, real colvector w, real colvector quants, real scalar index_touse, real matrix start, real scalar reps, string scalar boot_method, string scalar cluster, string scalar strata, real scalar noreplacement, real scalar subsize, real matrix Sinfo, real colvector Cinfo, string scalar bofinger, real scalar level)
{
	real colvector bweight
	real matrix boot_res, score, temp
	real scalar n, k, nq, i, r, alpha, h
	real colvector fit, uhat, den
	n=rows(y)
	k=cols(x)
	nq=rows(quants)
	score=J(n,nq*k,.)
	alpha=1-0.01*level	
	for (i=1; i<=nq; i++) {
		fit=x*start[.,i]
		uhat=y-fit
		h=bandwidth_rq(quants[i],n,(bofinger==""),alpha)
		if(quants[i]-h<0.001) h=quants[i]*0.5
		if(quants[i]+h>0.999) h=(1-quants[i])*0.5
		h=(invnormal(quants[i]+h)-invnormal(quants[i]-h))*min((sqrt(variance(uhat,w)),mm_iqrange(uhat,w)/1.34))
		den = normalden(uhat:/h):/h
		temp=cross(x,den:*w,x):/n
		temp=luinv(temp)
		if(missing(temp)){
			errprintf("The multiplier bootstrap is unable to estimate the variance. Try the empirical or weighted bootrap.")
			exit(error(506))
		}	
		score[.,(i-1)*k:+(1..k)]=((quants[i]:-(y:<=fit)):*x)*temp
		score[.,(i-1)*k:+(1..k)]=score[.,(i-1)*k:+(1..k)]-mean(score[.,(i-1)*k:+(1..k)])#J(n,1,1)
	}
	boot_res=J(reps,nq*k,.)
	if(Cinfo!=. & Sinfo==.){
		subsize=rows(Cinfo)*subsize/n
	}
	else if(Cinfo==. & Sinfo!=.){
		subsize=Sinfo*subsize/n
	}
	else if(Cinfo!=. & Sinfo!=.){
		subsize=Sinfo[.,2]*subsize/n
	}	
	for (r=1; r<=reps; r++){
		if(boot_method=="exponential" | boot_method=="Gaussian" | boot_method=="wild"){
			bweight=prepare_weights(w, index_touse, cluster, strata, boot_method)
		}
		else{
			if(cluster=="" & strata==""){
				if(noreplacement==0){
					bweight=mm_srswr(subsize, n, 1):*w*n/subsize
				}
				else{
					bweight=mm_srswor(subsize, n, 1):*w*n/subsize
				}
			}
			else{
				bweight=mm_sample(subsize, Sinfo, Cinfo, 1, noreplacement, 1):*w*n/subsize
			}
		}
		boot_res[r,.]=mean(score:*bweight)
		stata(`"di in gr "." _c"')
	}
	return(boot_res)
}

version 9.2
mata real matrix rq_multiplier1(real colvector y, real matrix x, real colvector w, real colvector quants, real scalar index_touse, real matrix start, real scalar reps, string scalar boot_method, string scalar cluster, string scalar strata, real scalar noreplacement, real scalar subsize, real matrix Sinfo, real colvector Cinfo, string scalar bofinger, real scalar level)
{
	real colvector bweight
	real matrix boot_res, score, temp
	real scalar n, k, nq, i, r, alpha, h
	real colvector fit, uhat, den
	n=rows(y)
	k=cols(x)
	nq=rows(quants)
	bweight=J(n,reps,.)
	boot_res=J(reps,nq*k,.)
	if(Cinfo!=. & Sinfo==.){
		subsize=rows(Cinfo)*subsize/n
	}
	else if(Cinfo==. & Sinfo!=.){
		subsize=Sinfo*subsize/n
	}
	else if(Cinfo!=. & Sinfo!=.){
		subsize=Sinfo[.,2]*subsize/n
	}	
	for (r=1; r<=reps; r++){
		if(boot_method=="exponential" | boot_method=="Gaussian" | boot_method=="wild"){
			bweight[.,r]=prepare_weights(w, index_touse, cluster, strata, boot_method)
		}
		else{
			if(cluster=="" & strata==""){
				if(noreplacement==0){
					bweight[.,r]=mm_srswr(subsize, n, 1):*w*n/subsize
				}
				else{
					bweight[.,r]=mm_srswor(subsize, n, 1):*w*n/subsize
				}
			}
			else{
				bweight[.,r]=mm_sample(subsize, Sinfo, Cinfo, 1, noreplacement, 1):*w*n/subsize
			}
		}
	}
	score=J(n,k,.)
	alpha=1-0.01*level	
	for (i=1; i<=nq; i++) {
		fit=x*start[.,i]
		uhat=y-fit
		h=bandwidth_rq(quants[i],n,(bofinger==""),alpha)
		if(quants[i]-h<0.001) h=quants[i]*0.5
		if(quants[i]+h>0.999) h=(1-quants[i])*0.5
		h=(invnormal(quants[i]+h)-invnormal(quants[i]-h))*min((sqrt(variance(uhat,w)),mm_iqrange(uhat,w)/1.34))
		den = normalden(uhat:/h):/h
		temp=cross(x,den:*w,x):/n
		temp=luinv(temp)
		if(missing(temp)){
			errprintf("The multiplier bootstrap is unable to estimate the variance. Try the empirical or weighted bootrap.")
			exit(error(506))
		}	
		score=((quants[i]:-(y:<=fit)):*x)*temp
		score=score-mean(score)#J(n,1,1)
		for(r=1;r<=reps;r++){
			boot_res[r,(i-1)*k:+(1..k)]=mean(score:*bweight[.,r])
		}
		stata(`"di in gr "." _c"')		
	}
	return(boot_res)
}

version 9.2
mata real matrix rq_multiplier2(real colvector y, real matrix x, real colvector w, real colvector quants, real scalar index_touse, real matrix start, real scalar reps, string scalar boot_method, string scalar cluster, string scalar strata, real scalar noreplacement, real scalar subsize, real matrix Sinfo, real colvector Cinfo, string scalar bofinger, real scalar level)
{
	real rowvector keep_seeds
	real matrix boot_res, score, temp
	real scalar n, k, nq, i, r, alpha, h
	real colvector bweight, fit, uhat, den
	n=rows(y)
	k=cols(x)
	nq=rows(quants)
	bweight=J(n,1,.)
	boot_res=J(reps,nq*k,.)
	if(Cinfo!=. & Sinfo==.){
		subsize=rows(Cinfo)*subsize/n
	}
	else if(Cinfo==. & Sinfo!=.){
		subsize=Sinfo*subsize/n
	}
	else if(Cinfo!=. & Sinfo!=.){
		subsize=Sinfo[.,2]*subsize/n
	}	
	score=J(n,k,.)
	alpha=1-0.01*level
	keep_seeds=J(1,reps,"")
	for (i=1; i<=nq; i++) {
		fit=x*start[.,i]
		uhat=y-fit
		h=bandwidth_rq(quants[i],n,(bofinger==""),alpha)
		if(quants[i]-h<0.001) h=quants[i]*0.5
		if(quants[i]+h>0.999) h=(1-quants[i])*0.5
		h=(invnormal(quants[i]+h)-invnormal(quants[i]-h))*min((sqrt(variance(uhat,w)),mm_iqrange(uhat,w)/1.34))
		den = normalden(uhat:/h):/h
		temp=cross(x,den:*w,x):/n
		temp=luinv(temp)
		if(missing(temp)){
			errprintf("The multiplier bootstrap is unable to estimate the variance. Try the empirical or weighted bootrap.")
			exit(error(506))
		}	
		score=((quants[i]:-(y:<=fit)):*x)*temp
		score=score-mean(score)#J(n,1,1)
		for(r=1;r<=reps;r++){
			if(i==1) keep_seeds[1,r]=rngstate()
			rngstate(keep_seeds[1,r])
			if(boot_method=="exponential" | boot_method=="Gaussian" | boot_method=="wild"){
				bweight=prepare_weights(w, index_touse, cluster, strata, boot_method)
			}
			else{
				if(cluster=="" & strata==""){
					if(noreplacement==0){
						bweight=mm_srswr(subsize, n, 1):*w*n/subsize
					}
					else{
						bweight=mm_srswor(subsize, n, 1):*w*n/subsize
					}
				}
				else{
					bweight=mm_sample(subsize, Sinfo, Cinfo, 1, noreplacement, 1):*w*n/subsize
				}
			}		
			boot_res[r,(i-1)*k:+(1..k)]=mean(score:*bweight)
		}
		stata(`"di in gr "." _c"')		
	}
	return(boot_res)
}

mata void rq_proqreg(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, real scalar mfactor, real scalar error_tol, real scalar small, string scalar first, string scalar nc, string scalar ns)
{
	real colvector y, quants, w, temp_coef, s, uy, band, r, kappa, sl, su, su_bad, sl_bad
	real rowvector index_reg, conv, rdev, mdev
	real matrix x, coef, xxinv, cov
	real scalar index_dep, index_touse, index_weight, n, k, m, nq, minmaxy, miny, maxy, idx, n_obs_data, i, not_optimal, m_temp, M, lo_q, hi_q, s_bad, alpha
	string scalar tempname
	index_dep=st_varindex(dep)
	index_reg=st_varindex(tokens(reg))
	index_touse=st_varindex(touse)
	index_weight=st_varindex(weight)
	y=st_data(.,index_dep,index_touse)
	x=st_data(.,(index_reg,index_touse),index_touse)
	n=rows(y)
	k=cols(x)
	quants=st_matrix(quantile)
	nq=rows(quants)
	if(nq>1) m=(k*n)^(1/2)*sqrt(n)*min(quants[2..nq,1]-quants[1..(nq-1),1]) 
	else m=1000
	conv=rdev=mdev=J(1,rows(quants),.)
	w=st_data(.,index_weight,index_touse)
	stata("recast double "+weight)
	stata("recast double "+reg)
	coef=J(k,nq,.)
	minmaxy=minmax(y)
	miny=2*minmaxy[1]-minmaxy[2]
	maxy=2*minmaxy[2]-minmaxy[1]
//first regression
	if(first=="pqreg"){
		stata("qrprocess "+dep+" "+reg+" [iweight="+weight+"], method(pqreg, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+")) vce(novar) quantile("+strofreal(quants[1,1])+")",1)
		conv[1]=st_matrix("e(convergence)")
	}
	else{
		stata("qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants[1,1])+")",1)
		conv[1]=st_numscalar("e(convcode)")		
	}
	idx = st_addvar("double", tempname=st_tempname())
	st_addobs(2)
	n_obs_data=st_nobs()
	st_store(n_obs_data-1,index_dep,miny)
	st_store(n_obs_data,dep,maxy)
	st_store((n_obs_data-1\n_obs_data),index_touse,(1\1))
	coef[.,1]=temp_coef=st_matrix("e(b)")'
	uy=y:-mm_quantile(y,w,quants[1,1])
	rdev[1]=colsum(uy:*(quants[1,1]:-(uy:<0)))
	uy=y:-x*coef[.,1]
	mdev[1]=colsum(uy:*(quants[1,1]:-(uy:<0)))
//to calculate the bands
	xxinv=luinv(cross(x,x))
	band=sqrt(colsum((cross(x',xxinv)':^2))')
	band=rowmax((band,J(n,1,small)))
//following regressions
	for (i=2; i<=nq; i++){
		not_optimal=1
		m_temp=m
		while(not_optimal){
			r=y-cross(x',coef[.,i-1])
			M=mfactor*m_temp
			lo_q=max(((1/n)\quants[i,1]-M/(2*n)))
			hi_q=min((quants[i,1]+M/(2*n)\(n-1)/n))
			kappa=mm_quantile(r:/band,1,(lo_q\hi_q))
			sl=(r:<band:*kappa[1,1])
			su=(r:>band:*kappa[2,1])
			while(not_optimal){
				s=(!sl):*(!su)
				if(any(sl)){
					if(k>1) st_store(n_obs_data-1,index_reg,mean(select(x[.,1..(k-1)],sl),select(w,sl)))
					st_store(n_obs_data-1,index_weight,sum(select(w,sl)))
					s=s\1
				}
				else s=s\0
				if(any(su)){
					if(k>1) st_store(n_obs_data,index_reg,mean(select(x[.,1..(k-1)],su),select(w,su)))
					st_store(n_obs_data,index_weight,sum(select(w,su)))
					s=s\1
				}
				else s=s\0
				st_store(.,idx,index_touse,s)
				stata("_qreg "+dep+" "+reg+" if "+tempname+" [aweight="+weight+"], quantile("+strofreal(quants[i,1])+")",1)
				temp_coef=st_matrix("e(b)")'
				if(rows(temp_coef)<k){
					m_temp=2*m_temp
					M=1.2*M
					break
				}					
				r=y-cross(x',temp_coef)
				su_bad=(r:<0):*su
				sl_bad=(r:>0):*sl
				s_bad=sum((su_bad\sl_bad))
				s=s[1..n]
				if(s_bad>error_tol){
					if(sum(s_bad>0.1*M)){
						m_temp=2*m_temp
						break
					}
					su=su:*(!su_bad)
					sl=sl:*(!sl_bad)
				}
				else{
					not_optimal=0
					coef[.,i]=temp_coef
					conv[i]=st_numscalar("r(convcode)")		
					uy=y:-mm_quantile(y,w,quants[i,1])
					rdev[i]=colsum(uy:*(quants[i,1]:-(uy:<0)))
					uy=y:-x*coef[.,i]
					mdev[i]=colsum(uy:*(quants[i,1]:-(uy:<0)))
				}
			}
		}
	}
	st_dropobsin((n_obs_data-1\n_obs_data))
	if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
		alpha=1-0.01*level
		cov=rqcov(y, x, weight_type, w, coef, quants, "qrprocess "+dep+" "+reg+" [iweight="+weight+"], vce(novar) method(pqreg, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+")) quantile(", alpha, ., small, ., variance, bofinger, cluster, strata, touse, nc, ns)
	}
	else cov=J(rows(quants)*cols(x),rows(quants)*cols(x),0)
	st_matrix(results,coef)
	st_matrix(covariance,cov)
	st_matrix(convergence,conv)
	st_matrix(rawdev,rdev)
	st_matrix(mindev,mdev)
}
		
mata void rq_profn(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, real scalar beta, real scalar small, real scalar max_it, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, real scalar mfactor, real scalar error_tol, real scalar step, string scalar first, string scalar nc, string scalar ns)
{
	real colvector y, quants_temp, add_quant, w, temp_coef, y_temp, w_temp, s, uy, band, r, kappa, sl, su, su_bad, sl_bad
	real rowvector conv, rdev, mdev
	real matrix x, quants, coef, xxinv, x_temp, cov
	real scalar n, k, m, nq, minmaxy, miny, maxy, i, not_optimal, m_temp, M, lo_q, hi_q, s_bad, alpha
	string rowvector treg
	y=st_data(.,dep,touse)
	n=rows(y)
	treg=tokens(reg)
	if(length(treg)>0){
		x=st_data(.,treg,touse)
		x=x,J(n,1,1)
	}
	else x=J(n,1,1)
	k=cols(x)
	m=round((k*n)^(1/2))
//	m=round((k*n)^(2/3))
	quants_temp=st_matrix(quantile)
	quants=quants_temp,J(rows(quants_temp),1,1)
	add_quant=range(min(quants_temp),max(quants_temp),step)
	for(i=1;i<=rows(add_quant);i++){
		if(all(abs(add_quant[i]:-quants_temp):>small)) quants=quants\(add_quant[i],0)
	}
	quants=sort(quants,1)
	nq=rows(quants)
	conv=rdev=mdev=J(1,rows(quants),.)
	w=st_data(.,weight,touse)
	coef=J(cols(x),rows(quants),.)
	minmaxy=minmax(y)
	miny=2*minmaxy[1]-minmaxy[2]
	maxy=2*minmaxy[2]-minmaxy[1]
//first regression
	if(first=="fn"){
		coef[.,1]=int_fnm(y, x, w, quants[1,1], beta, small, max_it, conv[1],0)
	}
	else{
		coef[.,1]=int_pfnm(y, x, w, quants[1,1], beta, small, max_it, conv[1],0,m,mfactor,n,error_tol,miny,maxy,rdev[1],mdev[1])		
	}
	if(colmissing(coef[.,1]) | conv[1]==0){
			stata("qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants[1,1])+")",1)
			coef[.,1]=st_matrix("e(b)")'
			conv[1]=st_numscalar("e(convcode)")	
	}
	temp_coef=coef[.,1]
	uy=y:-mm_quantile(y,w,quants[1,1])
	rdev[1]=colsum(uy:*(quants[1,1]:-(uy:<0)))
	uy=y:-x*coef[.,1]
	mdev[1]=colsum(uy:*(quants[1,1]:-(uy:<0)))
//to calculate the bands
	xxinv=luinv(cross(x,x))
	band=sqrt(colsum((cross(x',xxinv)':^2))')
//pseudo observations
	y_temp=y\miny\maxy
	x_temp=x\J(2,k,.)
	w_temp=w\J(2,1,0)
//following regressions
	for (i=2; i<=nq; i++){
		not_optimal=1
		m_temp=m
		while(not_optimal){	
			r=y-cross(x',temp_coef)
			M=mfactor*m_temp
			lo_q=max(((1/n)\quants[i,1]-M/(2*n)))
			hi_q=min((quants[i,1]+M/(2*n)\(n-1)/n))
			kappa=mm_quantile(r:/rowmax((band,J(n,1,small))),1,(lo_q\hi_q))
			sl=(r:<band:*kappa[1,1])
			su=(r:>band:*kappa[2,1])
			while(not_optimal){
				s=(!sl):*(!su)\0\0
				if(any(sl)){			
					if(k>1) x_temp[n+1,.]=mean(select(x[.,1..(k-1)],sl),select(w,sl)),1
					w_temp[n+1]=sum(select(w,sl))
					s[n+1]=1
				}
				if(any(su)){
					if(k>1) x_temp[n+2,.]=mean(select(x[.,1..(k-1)],su),select(w,su)),1
					w_temp[n+2]=sum(select(w,su))
					s[n+2]=1
				}
				temp_coef=int_fnm(select(y_temp,s), select(x_temp,s), select(w_temp,s), quants[i,1], beta, small, max_it, conv[i],temp_coef)
				r=y-cross(x',temp_coef)
				su_bad=(r:<0):*su
				sl_bad=(r:>0):*sl
				s_bad=sum((su_bad\sl_bad))	
				if(s_bad>error_tol){
					if(sum(s_bad>0.1*M)){
						m_temp=2*m_temp
						break
					}
					su=su:*(!su_bad)
					sl=sl:*(!sl_bad)
				}
				else{
					not_optimal=0
					if(quants[i,2]){
						coef[.,i]=temp_coef
						uy=y:-mm_quantile(y,w,quants[i,1])
						rdev[i]=colsum(uy:*(quants[i,1]:-(uy:<0)))
						uy=y:-x*coef[.,i]
						mdev[i]=colsum(uy:*(quants[i,1]:-(uy:<0)))
					}
				}
			}
		}
		if(colmissing(coef[.,i]) | conv[i]==0){
			stata("_qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants[i,1])+")",1)
			coef[.,i]=st_matrix("e(b)")'
			conv[i]=st_numscalar("r(convcode)")	
		}

	}
	coef=select(coef',quants[.,2])'
	conv=select(conv',quants[.,2])'
	rdev=select(rdev',quants[.,2])'
	mdev=select(mdev',quants[.,2])'
	quants=select(quants[.,1],quants[.,2])
	if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
		alpha=1-0.01*level
		cov=rqcov(y, x, weight_type, w, coef, quants, "qrprocess "+dep+" "+reg+" [iweight="+weight+"], vce(novar) method(pqreg, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+"))  quantile(", alpha, beta, small, max_it, variance, bofinger, cluster, strata, touse, nc, ns)
	}
	else cov=0
	st_matrix(results,coef)
	st_matrix(covariance,cov)
	st_matrix(convergence,conv)
	st_matrix(rawdev,rdev)
	st_matrix(mindev,mdev)
}

*function to compute bandwidth for sparsity estimation
version 9.2
mata real scalar bandwidth_rq(real scalar p, real scalar n, real scalar hs, real scalar alpha)
{
	real scalar x0, f0, band
	x0 = invnormal(p)
	f0 = normalden(x0)
	if (hs==1){
		band=n^(-1/3) * invnormal(1 - alpha/2)^(2/3) * ((1.5 * f0^2)/(2 * x0^2 + 1))^(1/3)
	} 
	else {
		band=n^-0.2 * ((4.5 * f0^4)/(2 * x0^2 + 1)^2)^0.2
	}
	return(band)
}

*Estimation of the variance-covariance matrix
version 9.2
mata real matrix rqcov(real colvector dep1, real matrix reg1, string scalar weight_type, real colvector weights, real matrix coef, real colvector tau, string scalar qreg, real scalar level, real scalar beta, real scalar small, real scalar max_it, string scalar variance, string scalar bofinger, string scalar cluster1, string scalar strata1, string scalar touse, string scalar ncluster, string scalar nstrata)
{
	real scalar k, n, nq, ns, i, q, h, n_effective, c, s, nc, j
	real matrix info, fxxinv, den
	real colvector cluster, uniq_clust, strata, uniq_strata, reg, dep, uhat, betahi, betalo, dyhat, residq, residi, residqi, residii, sel_vector
	real rowvector tempmq, tempmi
	real matrix temp, cov, resid, regc, tempq, tempi, temp2q, temp2i
	k=cols(reg1)
	n=rows(reg1)
	nq=length(tau)
	variance=J(nq,1,variance)
	fxxinv=J(k,k*length(tau),.)
	den=J(n,nq,0)
	if(cluster1!=""){
		cluster=st_data(., cluster1, touse)
		info = panelsetup(cluster, 1)
        nc   = rows(info)
		resid=J(n,nq,.)
		st_numscalar(ncluster, nc)
	}
	else st_numscalar(ncluster, n)
	if(strata1!=""){
		strata=st_data(.,strata1,touse)
		uniq_strata=uniqrows(strata)
		ns=rows(uniq_strata)
		resid=J(n,nq,.)
		st_numscalar(nstrata, ns)
	}
	else st_numscalar(nstrata, 1)
	i=1
	reg=reg1:*weights
	dep=dep1:*weights
	while(i<=nq){
		q=tau[i]
		uhat=dep-reg*coef[.,i]
		if(cluster1!="" | strata1!=""){
			resid[,i]=q:-(uhat:<small)

		}
		if(variance[i]=="nid"){
			h=bandwidth_rq(q,n,(bofinger==""),level)
			if(q-h<0.001) h=q*0.5
			if(q+h>0.999) h=(1-q)*0.5
			if(qreg==""){
				betahi=int_fnm(dep1, reg1, weights, q+h, beta, small, max_it, .,0)
				betalo=int_fnm(dep1, reg1, weights, q-h, beta, small, max_it, .,0)
			}
			else{
				stata(qreg+strofreal(q+h)+")",1)
				betahi=st_matrix("e(b)")'
				stata(qreg+strofreal(q-h)+")",1)
				betalo=st_matrix("e(b)")'
			}
			dyhat=reg*(betahi:-betalo)
			temp=my_selectindex(dyhat:>small^(2/3))			
			den[temp,i]=(2*h:/dyhat[temp])
		}
		if(variance[i]=="kernel"){
			h=bandwidth_rq(q,n,(bofinger==""),level)
			if(q-h<0.001) h=q*0.5
			if(q+h>0.999) h=(1-q)*0.5
			h=(invnormal(q+h)-invnormal(q-h))*min((sqrt(variance(uhat,weights)),mm_iqrange(uhat,weights)/1.34))
			den[.,i] = normalden(uhat:/h):/h
		}
		if(variance[i]=="iid"){
			h=bandwidth_rq(q,n,(bofinger==""),level)
			betahi=betalo=0
			while(betahi-betalo==0){
				betahi=mm_quantile(uhat,weights, min((q+h,1-1/n)))
				betalo=mm_quantile(uhat,weights, max((q-h,1/n)))
				if(betahi-betalo==0) h=1.2*h
			}
			den[.,i] = J(n,1,2*h/(betahi:-betalo))
		}
		temp=sqrt(den[,i]) :* reg
		temp=cross(temp,temp)
		temp=luinv(temp)
		if(max(missing(temp))==0){
			fxxinv[.,((i-1)*k+1)..(i*k)]=temp
			i=i+1
		} 
		else{
//			message=`"dis as error "Warning: the selected vce method could not estimate the variance for the "'+strofreal(q)+`" quantile regression.""'
			stata(`"dis as error "Warning: the selected vce method could not estimate the variance for the "'+strofreal(q)+`" quantile regression.""')	
			stata(`"dis "The vce method iid is used instead for this quantile regression.""')
			variance[i]="iid"
		}
	}
//	temp=cross(reg,reg)	
	den=den'
	cov=J(nq*k,nq*k,.)
	for(q=1; q<=nq; q++){
		for(i=1; i<=q; i++){
			if(cluster1=="" & strata1==""){
				temp=(min(tau[q]\tau[i])-tau[q]*tau[i])*cross(reg,reg)
			}
			else if(strata1==""){
				temp=J(k,k,0)
				residq = resid[.,q]
				residi = resid[.,i]
				for(c=1; c<=nc; c++){				
					regc = panelsubmatrix(reg, c, info)
					residqi = panelsubmatrix(residq, c, info)
					residii = panelsubmatrix(residi, c, info)
					temp=temp:+colsum(regc:*residqi)'colsum(regc:*residii)
				}
			}
			else if(cluster1==""){
				temp=J(k,k,0)
				tempq=reg:*resid[.,q]
				tempi=reg:*resid[.,i]
				for(s=1; s<=ns; s++){
					temp2q=select(tempq,strata:==uniq_strata[s])
					temp2i=select(tempi,strata:==uniq_strata[s])
					tempmq=mean(temp2q)
					tempmi=mean(temp2i)
					for(j=1;j<=rows(temp2q);j++){
						temp=temp+(temp2q[j,.]-tempmq)'(temp2i[j,.]-tempmi)
					}
				}
			}
			else{
				temp=J(k,k,0)
				tempq=reg:*resid[.,q]
				tempi=reg:*resid[.,i]
				for(s=1; s<=ns; s++){
					uniq_clust=uniqrows(select(cluster,strata:==uniq_strata[s]))
					nc=rows(uniq_clust)
					tempmq=mean(select(tempq,strata:==uniq_strata[s]))
					tempmi=mean(select(tempi,strata:==uniq_strata[s]))
					for(c=1; c<=nc; c++){
						temp2q=select(tempq,(cluster:==uniq_clust[c]):*(strata:==uniq_strata[s]))
						temp2i=select(tempi,(cluster:==uniq_clust[c]):*(strata:==uniq_strata[s]))
						temp=temp:+colsum(temp2q-tempmq#J(rows(temp2q),1,1))'colsum(temp2i-tempmi#J(rows(temp2i),1,1))
					}
				}
			}
			cov[((q-1)*k+1)..(q*k),((i-1)*k+1)..(i*k)]=fxxinv[.,((q-1)*k+1)..(q*k)]*temp*fxxinv[.,((i-1)*k+1)..(i*k)]
			if(i!=q) cov[((i-1)*k+1)..(i*k),((q-1)*k+1)..(q*k)]=cov[((q-1)*k+1)..(q*k),((i-1)*k+1)..(i*k)]'
		}
	}
	if(weight_type=="fweight"){
		n_effective=colsum(weights)
		cov=cov*n/n_effective
	}
	return(cov)
}

*Parsing vce
program define VCEParse, rclass
	syntax [anything(name=var)] [, Reps(integer 100) BMethod(string) Bofinger Cluster(varname numeric) Strata(varname numeric) funcint(string) Subsize(integer -10) noReplacement m(real 4) weight(real 0) MISSpecification hc1 NODFadjust  memory(integer 0)]
	if "`var'"=="" & "`funcint'"==""{
		local var "kernel"
	}
	else if "`var'"==""{
		local var "bootstrap"
	}
	else if "`var'"=="boot" | "`var'"=="boots" | "`var'"=="bootst" | "`var'"=="bootstr" | "`var'"=="bootstra"{
		local var "bootstrap"
	}
	else if "`var'"=="ker" | "`var'"=="kern" | "`var'"=="kerne"{
		local var "kernel"
	}
	else if "`var'"=="mult" | "`var'"=="multi" | "`var'"=="multip" | "`var'"=="multipl" | "`var'"=="multipli" | "`var'"=="multiplie" | "`var'"=="multiplier" | "`var'"=="score"{
		local var "multiplier"
	}
	else if "`var'"!="kernel" & "`var'"!="iid" & "`var'"!="bootstrap" & "`var'"!="nid" & "`var'"!="novar" & "`var'"!="multiplier"{
		dis as error "The option vce(`var') is not allowed."
		error 400
	}
	if ("`var'"=="kernel" | "`var'"=="iid" | "`var'"=="nid" | "`var'"=="novar") & "`funcint'"=="functional"{
		dis as error "Only the (classical or multiplier) bootstrap can be used to estimate the variance when the option functional is activated."
		error 400
	}
	if "`var'"=="iid" & (`weight'==1 | "`strata'"!="" | "`cluster'"!=""){
		dis as error "The VCE-method iid cannot be used in the presence of weights, clustering, or stratification."
		error 400
	}
	if `reps'<2{
		dis as error "At least two bootstrap replications must be performed. We recommend using at least 100 replications."
		error 400
	}
	if "`var'"=="bootstrap"{
		if "`bmethod'"==""{
			local bmethod "empirical"
		}
		if "`bmethod'"=="subsampling" & `subsize'==-10{
			dis as error "The option subsize must be specified if bmethod(subsampling) is selected."
			error 400
		}
		if `subsize'!=-10 & `subsize'<0{
			dis as error "The option subsize(`subsize') is not allowed."
			error 400
		}
		if "`bmethod'"!="empirical" & "`bmethod'"!="subsampling" & "`bmethod'"!="weighted"{
			dis as error "The option bmethod(`bmethod') is not allowed."
			error 400
		}
		if "`bmethod'"!="subsampling" & (`subsize'!=-10 | "`replacement'"!=""){
			dis as error "The options subsize and noreplacement cannot be used when boot_method(`boot_method')."
			error 400
		}
		local noreplacement=("`replacement'"=="noreplacement")		
	}
	else if "`var'"=="multiplier"{
		if "`bmethod'"==""{
			local bmethod "wild"
		}
		if "`bmethod'"=="subsampling" & `subsize'==-10{
			dis as error "The option subsize must be specified if bmethod(subsampling) is selected."
			error 400
		}
		if `subsize'!=-10 & `subsize'<0{
			dis as error "The option subsize(`subsize') is not allowed."
			error 400
		}
		if "`bmethod'"!="empirical" & "`bmethod'"!="subsampling" & "`bmethod'"!="exponential" & "`bmethod'"!="Gaussian" & "`bmethod'"!="wild"{
			dis as error "The option bmethod(`bmethod') is not allowed."
			error 400
		}
		if "`bmethod'"!="subsampling" & (`subsize'!=-10 | "`replacement'"!=""){
			dis as error "The options subsize and noreplacement cannot be used when boot_method(`boot_method')."
			error 400
		}
		local noreplacement=("`replacement'"=="noreplacement")		
	}
	else if "`bmethod'"!="" | "`replacement'"!="" | `subsize'!=-10 {
		dis as error "The options bmethod, subsize and noreplacement can only be used with the (classical or multiplier) bootstrap."
		error 400
	}
	return local var `var'
	return local reps `reps'
	return local boot_method `bmethod'
	return local bofinger `bofinger'
	return local cluster `cluster'
	return local strata `strata'
	return local sub_size `subsize'
	return local noreplacement `noreplacement'
	return local mboot `m'
	return local misspecification `misspecification'
	if "`hc1'"=="hc1" 	return local HC1 "HC1"
	return local nodfadjust `nodfadjust'
	return local memory `memory'
end

*Parsing method
program define methodParse, rclass
	syntax [anything(name=method)] , n(integer) k(integer) nq(integer) [ Beta(real 0.9995) SMall(real 0.00001) MAX_it(real 100) m(real -10) Error_tol(integer 0) Step(real 0.01) First(string)]
	if "`method'"==""{
		if `n'<10000 & `nq'<10{
			local method "qreg"
		}
		else if `n'>=10000 & `nq'<10{
			local method "pfn"
		}
		else if `n'<10000{
			local method "proqreg"
			local first "qreg"
		}
		else if `n'<100000{
			local method "profn"
			local first "pfn"
		}
		else{
			local method "1step"
			local first "pfn"
		}
	}
	if "`method'"=="proqreg" & ("`first'"!="qreg" & "`first'"!="pqreg" & "`first'"!=""){
		dis as error "The option 'first' can only take the values qreg or pqreg when the method proqreg has been selected."
		error 400
	}
	else if "`method'"=="profn" & ("`first'"!="fn" & "`first'"!="pfn" & "`first'"!=""){
		dis as error "The option 'first' can only take the values fn or pfn when the method profn has been selected."
		error 400
	}
	if "`first'"==""{
		if `n'<10000{
			if "`method'"=="proqreg" | "`method'"=="1step" | "`method'"=="onestep"{
				local first "qreg"
			}
			else if "`method'"=="profn"{
				local first "fn"
			}
		}
		else{
			if "`method'"=="proqreg"| "`method'"=="1step" | "`method'"=="onestep"{
				local first "pqreg"
			}
			else if "`method'"=="profn"{
				local first "pfn"
			}
		
		}
	}
	if "`method'"!="qreg" & "`method'"!="fn"  & "`method'"!="pfn" & "`method'"!="pqreg" & "`method'"!="proqreg" & "`method'"!="profn" & "`method'"!="1step" & "`method'"!="onestep"{
		dis as error "The selected method in the option 'method' has not been implemented."
		error 400
	}
	if `m'==-10{
		if "`method'"=="profn" | "`method'"=="proqreg"{
			local m=4
		}
		else local m=0.8
	}
	else if `m'<=0{
		dis as error "The option m must be a strictly positive real number."
		error 400
	}
	return local method `method'
	return local beta `beta'
	return local small `small'
	return local max_it `max_it'
	return local m `m'
	return local error_tol `error_tol'
	return local step `step'
	return local first `first'
end

*bootstrap weights for weighted bootstrap with clustering and/or stratification
version 9.2
mata real colvector prepare_weights(real colvector weight, real scalar touse, string scalar cluster1, string scalar strata1, string scalar method)
{
	real scalar n, nc, c, ns, s
	real colvector bootw, cluster, uniq_clust, clust_w, index, strata, uniq_strata, strata_w
	n=rows(weight)
	if(cluster1=="" & strata1==""){
		if(method=="") bootw=-ln(uniform(n,1)):*weight
		else if(method=="exponential") bootw=(-ln(uniform(n,1)):-1):*weight
		else if(method=="Gaussian") bootw=invnormal(uniform(n,1)):*weight
		else if(method=="wild") bootw=(invnormal(uniform(n,1))/sqrt(2)+(invnormal(uniform(n,1)):^2:-1):/2):*weight
	}
	else if(cluster1!="" & strata1==""){
		bootw=J(n,1,.)
		cluster=st_data(.,cluster1,touse)
		uniq_clust=uniqrows(cluster)
		nc=rows(uniq_clust)
		if(method=="") clust_w=-ln(uniform(nc,1))
		else if(method=="exponential") clust_w=(-ln(uniform(nc,1)):-1)
		else if(method=="Gaussian") clust_w=invnormal(uniform(nc,1))
		else if(method=="wild") clust_w=invnormal(uniform(nc,1))/sqrt(2)+(invnormal(uniform(nc,1)):^2:-1):/2
		for(c=1;c<=nc;c++){
			index=my_selectindex(cluster:==uniq_clust[c])
			bootw[index]=J(rows(index),1,clust_w[c]):*weight[index]
		}
	}
	else if(cluster1=="" & strata1!=""){
		if(method=="") bootw=-ln(uniform(n,1)):*weight
		else if(method=="exponential") bootw=(-ln(uniform(n,1)):-1):*weight
		else if(method=="Gaussian") bootw=invnormal(uniform(n,1)):*weight
		else if(method=="wild") bootw=(invnormal(uniform(n,1))/sqrt(2)+(invnormal(uniform(n,1)):^2:-1):/2):*weight
		strata=st_data(.,strata1,touse)
		uniq_strata=uniqrows(strata)
		ns=rows(uniq_strata)
		for(s=1;s<=ns;s++){
			index=my_selectindex(strata:==uniq_strata[s])
			bootw[index]=bootw[index]:*sum(weight[index])/sum(bootw[index])
		}
	}
	else{
		bootw=J(n,1,.)
		cluster=st_data(.,cluster1,touse)
		strata=st_data(.,strata1,touse)
		uniq_strata=uniqrows(strata)
		ns=rows(uniq_strata)
		if(method=="") strata_w=-ln(uniform(ns,1))
		else if(method=="exponential") strata_w=(-ln(uniform(ns,1)):-1)
		else if(method=="Gaussian") strata_w=invnormal(uniform(ns,1))
		else if(method=="wild") strata_w=invnormal(uniform(ns,1))/sqrt(2)+(invnormal(uniform(ns,1)):^2:-1):/2
		for(s=1;s<=ns;s++){
			uniq_clust=uniqrows(select(cluster,strata:==uniq_strata[s]))
			for(c=1;c<=rows(uniq_clust);c++){
				index=my_selectindex((cluster:==uniq_clust[c]):*(strata:==uniq_strata[s]))
				bootw[index]=J(rows(index),1,strata_w[s]):*weight[index]
			}
		}
		for(s=1;s<=ns;s++){
			index=my_selectindex(strata:==uniq_strata[s])
			bootw[index]=bootw[index]:*sum(weight[index])/sum(bootw[index])
		}
	}
	return(bootw)
}

version 9.2
mata real colvector my_selectindex(real colvector input) return(select((1..rows(input))',input))

version 9.2
mata void rq_1step(string scalar dep, string scalar reg, string scalar weight_type, string scalar weight, string scalar touse, string scalar quantile, real scalar beta, real scalar small, real scalar max_it, string scalar variance, string scalar bofinger, string scalar cluster, string scalar strata, real scalar level, string scalar results, string scalar covariance, string scalar convergence, string scalar rawdev, string scalar mindev, real scalar mfactor, real scalar error_tol, real matrix save_for_boot, string scalar first, string scalar nc, string scalar ns)
{
	real colvector quants, y, w, uy, fit, uhat, den
	real rowvector conv, rdev, mdev, index, direction
	real matrix x, coef, cov, temp
	real scalar alpha, i, n, k, nq, qstart, index1, quants1, h
	string rowvector reg1
	quants=st_matrix(quantile)
	nq=rows(quants)
	if(nq>1){
		if(max(quants[2..nq]:-quants[1..(nq-1)])>0.0501){
			errprintf("The method onestep is only valid for a fine grid of quantiles.\n ")
			exit(498)
		}
		if(max(quants[2..nq]:-quants[1..(nq-1)])>0.0101){
			errprintf("Warning: the method onestep is only valid for a fine grid of quantiles.\n ")
		}
	}
	conv=rdev=mdev=J(1,nq,.)
	y=st_data(.,dep,touse)
	n=rows(y)
	reg1=tokens(reg)
	if(length(reg1)>0) x=st_data(.,reg1,touse),J(n,1,1)
	else x=J(n,1,1)
	k=cols(x)
	w=st_data(.,weight,touse)
	w=w:/mean(w)
	coef=J(k,nq,.)
	qstart=select((1..nq)',abs(quants:-0.5):==min(abs(quants:-0.5)))[1]
	index=(qstart..nq),((qstart-1)..1)
	direction=J(1,nq-qstart+1,1),J(1,qstart-1,-1)
	if(save_for_boot!=J(1,1,0)) save_for_boot=J(nq*k,k,0)
	for(i=1; i<=nq; i++) {
		index1=index[i]
		quants1=quants[index1]
		uy=y:-mm_quantile(y,w,quants1)
		rdev[index1]=colsum(uy:*(quants1:-(uy:<0)))
		if(i>1){
			fit=x*coef[.,index1-direction[i]]
			uhat=y-fit
//			den=J(n,1,0)
//			dyhat=x*(coef[.,i-1]:-coef[.,i-2])
//			temp=my_selectindex(dyhat:>small^(2/3))
//			den[temp]=(quants[i-1]-quants[i-2]):/dyhat[temp]
			h=bandwidth_rq(quants1,n,(bofinger==""),0.05)
			if(quants1-h<0.001) h=quants1*0.5
			if(quants1+h>0.999) h=(1-quants1)*0.5
			h=(invnormal(quants1+h)-invnormal(quants1-h))*min((sqrt(variance(uhat,w)),mm_iqrange(uhat,w)/1.34))
			den = normalden(uhat:/h):/h
//			temp=sqrt(den:*w) :* x
			temp=cross(x,den:*w,x):/n
			temp=luinv(temp)
			if(save_for_boot!=0) save_for_boot[(i-1)*k:+(1..k),.]=temp
			temp=temp*mean((quants1:-(y:<=fit)):*x,w)'
			coef[.,index1]=coef[.,index1-direction[i]]+temp
			conv[index1]=1
		}
		if(i==1 | colmissing(coef[.,index1]) | conv[index1]==0){
			if(first=="qreg"){
				stata("qreg "+dep+" "+reg+" [aweight="+weight+"], quantile("+strofreal(quants1)+")",1)
				coef[.,index1]=st_matrix("e(b)")'
				conv[index1]=st_numscalar("e(convcode)")
			}
			else if(first=="pqreg"){
				stata("qrprocess "+dep+" "+reg+" [iweight="+weight+"], method(pqreg, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+")) vce(novar) quantile("+strofreal(quants1)+")",1)
				conv[index1]=st_matrix("e(convergence)")
				coef[.,index1]=st_matrix("e(b)")'
			}
			else if(first=="fn"){
				coef[.,index1]=int_fnm(y, x, w, quants1, beta, small, max_it, conv[index1],0)
			}
			else{
				coef[.,index1]=int_pfnm(y, x, w, quants1, beta, small, max_it, conv[index1], 0, round((k*n)^(1/2)), mfactor, n, error_tol, 2*min(y)-max(y), 2*max(y)-min(y), rdev[index1], mdev[index1])		
			}
		}
		uy=y:-x*coef[.,index1]
		mdev[index1]=colsum(uy:*(quants[index1]:-(uy:<0)))
	}
	if(variance!="novar" & variance!="boot" & variance!="bootstrap" & variance!="multiplier"){
		alpha=1-0.01*level
		cov=rqcov(y, x, weight_type, w, coef, quants, "qrprocess "+dep+" "+reg+" [iweight="+weight+"], vce(novar) method(fn)  quantile(", alpha, beta, small, max_it, variance, bofinger, cluster, strata, touse, nc, ns)
	}
	else cov=0
	st_matrix(results,coef)
	st_matrix(covariance,cov)
	st_matrix(convergence,conv)
	st_matrix(rawdev,rdev)
	st_matrix(mindev,mdev)
}

version 9.2
mata real matrix rq_boot_1step(string scalar dep, string scalar reg, string scalar weight, string scalar touse, string scalar quantile, real scalar small, real scalar reps, string scalar boot_method, string scalar cluster, string scalar strata, real scalar noreplacement, real scalar subsize, real matrix Sinfo, real colvector Cinfo, real scalar beta, real scalar max_it, real scalar mfactor, real scalar error_tol, real matrix save_for_boot, string scalar first)
{
	real colvector y, quants, w, bweight, fit
	real matrix x, boot_res, temp_coef, temp
	real scalar index_touse, n, k, nq, index_temp_bw, i, r, qstart, index1, quants1
	real rowvector index, direction
	string scalar temp_bw
	string rowvector reg1
	index_touse=st_varindex(touse)
	index_temp_bw=st_addvar("double", temp_bw=st_tempname())
	quants=st_matrix(quantile)
	y=st_data(.,dep,touse)
	n=rows(y)
	reg1=tokens(reg)
	if(length(reg1)>0) x=st_data(.,reg1,touse),J(n,1,1)
	else x=J(n,1,1)
	k=cols(x)
	w=st_data(.,weight,touse)
	w=w:/mean(w)
	nq=rows(quants)
	qstart=select((1..nq)',abs(quants:-0.5):==min(abs(quants:-0.5)))[1]
	index=(qstart..nq),((qstart-1)..1)
	direction=J(1,nq-qstart+1,1),J(1,qstart-1,-1)
	boot_res=J(reps,nq*k,.)
	if(Cinfo!=. & Sinfo==.){
		subsize=rows(Cinfo)*subsize/n
	}
	else if(Cinfo==. & Sinfo!=.){
		subsize=Sinfo*subsize/n
	}
	else if(Cinfo!=. & Sinfo!=.){
		subsize=Sinfo[.,2]*subsize/n
	}	
	for (r=1; r<=reps; r++){
		if(boot_method=="weighted"){
			bweight=prepare_weights(w, index_touse, cluster, strata, "")
		}
		else{
			if(cluster=="" & strata==""){
				if(noreplacement==0){
					bweight=mm_srswr(subsize, n, 1):*w
				}
				else{
					bweight=mm_srswor(subsize, n, 1):*w
				}
			}
			else{
				bweight=mm_sample(subsize, Sinfo, Cinfo, 1, noreplacement, 1):*w
			}
		}
		st_store(.,index_temp_bw,index_touse,bweight)
		stata("_rmcoll "+reg+" [iweight="+temp_bw+"]",1)
		if(st_global("r(varlist)")!=reg){
			stata(`"dis in red "x" _continue"')
			r=r-1
		}
		else{
			temp_coef=J(k,nq,.)
			for (i=1; i<=rows(quants); i++) {
				index1=index[i]
				quants1=quants[index1]
				if(i>1){
					fit=x*temp_coef[.,index1-direction[i]]
//					uhat=y-fit
//					h=bandwidth_rq(quants1,n,(bofinger==""),0.05)
//					if(quants1-h<0.001) h=quants1*0.5
//					if(quants1+h>0.999) h=(1-quants1)*0.5
//					h=(invnormal(quants1+h)-invnormal(quants1-h))*min((sqrt(variance(uhat,bweight)),mm_iqrange(uhat,bweight)/1.34))
//					den = normalden(uhat:/h):/h
//					temp=cross(x,den:*bweight,x):/n
//					temp=luinv(temp)
					temp=save_for_boot[(i-1)*k:+(1..k),.]
					temp=temp*mean((quants1:-(y:<=fit)):*x,bweight)'
					temp_coef[.,index1]=temp_coef[.,index1-direction[i]]+temp
				}
				if(i==1 | colmissing(temp_coef[.,index1])){
					if(first=="qreg"){
						stata("qreg "+dep+" "+reg+" [aweight="+temp_bw+"], quantile("+strofreal(quants1)+")",1)
						temp_coef[.,index1]=st_matrix("e(b)")'
					}
					else if(first=="pqreg"){
						stata("qrprocess "+dep+" "+reg+" [iweight="+temp_bw+"], method(pqreg, m("+strofreal(mfactor)+") error_tol("+strofreal(error_tol)+")) vce(novar) quantile("+strofreal(quants1)+")",1)
						temp_coef[.,index1]=st_matrix("e(b)")'
					}
					else if(first=="fn"){
						temp_coef[.,index1]=int_fnm(y, x, bweight, quants1, beta, small, max_it, ., 0)
					}
					else if(first=="pfn"){
						temp_coef[.,index1]=int_pfnm(y, x, bweight, quants1, beta, small, max_it, ., 0, round((k*n)^(1/2)), mfactor, n, error_tol, 2*min(y)-max(y), 2*max(y)-min(y), ., .)		
					}
				}
			}
			boot_res[r,.]=vec(temp_coef)'
			stata(`"di in gr "." _c"')
		}
	}
	return(boot_res)
}
