*qrprocess_p: predict for the quantile regression process
*! version 1.1.1  14.04.2020  Blaise Melly

program define qrprocess_p
	version 6, missing

		/* Step 1:
			place command-unique options in local myopts
			Note that standard options are
			LR:
				Index XB Cooksd Hat 
				REsiduals RSTAndard RSTUdent
				STDF STDP STDR noOFFset
			SE:
				Index XB STDP noOFFset
		*/
	local myopts "RESIDuals Difference REArranged(numlist >0 <1 min=1 sort) cdf(numlist min=1 sort)"

		/* Step 2:
			call _propts, exit if done, 
			else collect what was returned.
		*/
	_pred_me "`myopts'" `0'
	if `s(done)' { exit }
	local vtyp  `s(typ)'
	local varn `s(varn)'
	local 0 `"`s(rest)'"'


		/* Step 3:
			Parse your syntax.
		*/
	syntax [if] [in] [, `myopts' noOFFset EQuation(string)]
	if "`rearranged'"!=""{
		local cfg="rearrangement"
	}
	else if "`cdf'"!=""{
		local cfg="rcdf"
	}

	if "`equatio'" == "" {	    /* we're version 6 --  7-char locals! */
		tempname b
		mat `b' = get(_b)
		local eqnames : coleq `b'
		gettoken equatio : eqnames
	}

		/* Step 4:
			Concatenate switch options together
		*/
	local type "`residua'`differe'`cfg'"
	

		/* Step 5:
			quickly process default case if you can 
			Do not forget -nooffset- option.
		*/
		

	if "`type'"=="" | "`type'"=="xb"{
		tempname quant
		mat `quant'=e(quantiles)
		if "`type'"=="" {
			di in smcl in gr "(option xb assumed; fitted values)"
		}
		if "`equation'"==""{
			local equation "#1"
		}	
		local nq=wordcount("`equation'")
		if `nq'>1{
			forvalues i=1/`nq'{
				local names "`names' `varn'`i'"
			}
		}
		else{
			local names "`varn'"
		}
		tokenize "`equation'", parse(" ")
		foreach name of local names{
			local eqnum=substr("`1'",2,100)
			local temp=`quant'[`eqnum',1]
			_predict `vtyp' `name' `if' `in', xb `offset' equation(`1')
			label var `name' "0.`temp' QR fitted values"
			mac shift
		}
		exit
	}


		/* Step 6:
			mark sample (this is not e(sample)).
		*/
	marksample touse
	
	if "`type'"=="rearrangement" {
		tempname quants
		local nq=wordcount("`rearranged'")
		if `nq'>1{
			forvalues i=1/`nq'{
				quietly gen `vtyp' `varn'`i'=.
				local pn "`pn' `varn'`i'"
			}
			tokenize "`rearranged'", parse(" ")
			local i=1
			while "`1'" != "" {
				matrix `quants'=nullmat(`quants')\(`1')
				mac shift 
				local i=`i'+1
			}
		}
		else{
				quietly gen `vtyp' `varn'=.
				local pn "`varn'"
				matrix `quants'=`rearranged'				
		}
		mata: rearranged("`e(xvar)'","`touse'","`quants'","`pn'")
		if `nq'==1 {
			di "Rearranged `rearranged' conditional quantile function" 
		}
		else{
			dis "Rearranged conditional quantile functions for the following quantiles:"
			forvalues i=1/`nq'{
				local temp=`quants'[`i',1]
				dis "The `temp' conditional quantile is saved in `varn'`i'"
			}
		}
		exit
	}

		if "`type'"=="rcdf" {
			tempname eval
			local nq=wordcount("`cdf'")
			if `nq'>1{
				forvalues i=1/`nq'{
					quietly gen `vtyp' `varn'`i'=.
					local pn "`pn' `varn'`i'"
				}
				tokenize "`cdf'", parse(" ")
				local i=1
				while "`1'" != "" {
					matrix `eval'=nullmat(`eval')\(`1')
					mac shift 
					local i=`i'+1
				}
			}
			else{
				quietly gen `vtyp' `varn'=.
				local pn "`varn'"
				matrix `eval'=`cdf'
			}
			mata: rcdf("`e(xvar)'","`touse'","`eval'","`pn'")
			if `nq'==1 {
				di "Conditional CDF at `e(depvar)'=`cdf'." 
			}
			else{
				dis "Conditional CDFs at the following values of `e(depvar)':"
				forvalues i=1/`nq'{
					local temp=`eval'[`i',1]
					dis "CDF at `e(depvar)'=`temp' is saved in `varn'`i'"
				}
			}
			exit
		}


		/* Step 7:
			handle options that take argument one at a time.
			Comment if restricted to e(sample).
			Be careful in coding that number of missing values
			created is shown.
			Do all intermediate calculations in double.
		*/


		/* Step 8:
			handle switch options that can be used in-sample or 
			out-of-sample one at a time.
			Be careful in coding that number of missing values
			created is shown.
			Do all intermediate calculations in double.
		*/

	if "`type'"=="residuals" {
		tempvar xb
		qui _predict double `xb' if `touse', xb `offset' eq(`equatio')
		gen `vtyp' `varn' = `e(depvar)'-`xb' if `touse'
		label var `varn' "Residuals:  `equatio'"
		exit
		
	}
	
	tokenize "`equatio'", parse(",") 
	local eq1 `"`1'"'
	local eq2 `"`3'"'
	
	if "`type'"=="difference" {
		tempvar xb1 xb2
		qui _predict double `xb1' if `touse', `offset' eq(`eq1')
		qui _predict double `xb2' if `touse', `offset' eq(`eq2')
		gen `vtyp' `varn' = `xb1' - `xb2' if `touse'
		lab var `varn' "Fitted diff.:  `eq1' - `eq2'"
		exit
	}
	
		/* Step 9:
			handle switch options that can be used in-sample only.
			Same comments as for step 8.
		*/
	*qui replace `touse'=0 if !e(sample)


			/* Step 10.
				Issue r(198), syntax error.
				The user specified more than one option
			*/
	error 198
end

*Mata function doing the rearrangement
version 9.2
mata void rearranged(string scalar reg, string scalar touse, string scalar quant_wished, string scalar out)
{
	real colvector quants, pred_quants, q_index
	real scalar nw, i, n
	real matrix x, coef, fit
	quants=st_matrix("e(quantiles)")
	pred_quants=st_matrix(quant_wished)
	nw=rows(pred_quants)
	q_index=J(nw,1,.)
	for(i=1;i<=nw;i++){
//		if(min(abs(quants:-pred_quants[i]))>0.000001){
//			"The "+strofreal(pred_quants[i])+"th quantile was not included in the estimation."
//			"Include this quantile when you call qrprocess if you want ot obtaint the predicted values at this quantile."
//			exit(400)
//		}
		q_index[i,1]=sum(quants:<=pred_quants[i])
	}
	n = sum(st_data(., touse, touse))
	if(reg!=""){
		x = get_reg(reg, touse, n), J(n,1,1)
	} else {
		x=J(n,1,1)
	}
	coef=st_matrix("e(coefmat)")
	fit=cross(x',coef)'
	for(i=1;i<=n;i++){
		fit[.,i]=sort(fit[.,i],1)
	}
	st_store(.,tokens(out),touse,fit[q_index,.]')
}

*Mata function calculating the cdf
mata void rcdf(string scalar reg, string scalar touse, string scalar eval, string scalar out)
{
	real colvector quants, evalu
	real scalar nw, n, i, j
	real matrix x, coef, fit, cdf
	quants=st_matrix("e(quantiles)")
	evalu=st_matrix(eval)
	nw=rows(evalu)
	n = sum(st_data(., touse, touse))
	if(reg!=""){
		x = get_reg(reg, touse, n), J(n,1,1)
	} else {
		x=J(n,1,1)
	}
	coef=st_matrix("e(coefmat)")
	fit=cross(x',coef)
	cdf=J(n,nw,.)
	for(i=1;i<=n;i++){
		for(j=1;j<=nw;j++){
			cdf[i,j]=quants[max(sum(fit[i,.]:<=evalu[j,1])\1),1]
		}
	}
	st_store(.,tokens(out),touse,cdf)
}

mata real matrix get_reg(string scalar input, string scalar touse, real scalar nobs)
{
	string rowvector varlist
	real scalar nreg, i
	real matrix output
	varlist = tokens(input) 
	nreg = cols(varlist)
	output = J(nobs, nreg, .)
	for(i=1; i <= nreg; i++){		
		output[.,i] = st_data(., varlist[1,i], touse)
	}
	return(output)
}
