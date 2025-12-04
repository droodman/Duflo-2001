*plotprocess: plot quantile regression and distribution regression coefficients
*! version 1.1.2  03.08.2022  Blaise Melly
*New: options compare, compare_ci and compare_color

program plotprocess
*version control
	version 9.2
	*the last estimates must exist en allow for plotprocess
	if "`e(plotprocess)'" == ""{
		if "`e(cmd)'" == "" {
			error 301
		}
		else{ 
			dis in red "plotprocess cannot be used after `e(cmd)'."
			error 332
		}
	}
	syntax [anything(name=namelist)] [, Pointwise Uniform Both None lcolor(string) ucolor(string) pcolor(string) ufcolor(string) pfcolor(string) legend(string) ytitle(string) xtitle(string) title(string) OTHER_graph_options(string) COMBINE_options(string) Level(string) compare(string) compare_ci compare_color(string)]
	local fvops = "`s(fvops)'" == "true" & _caller() >= 11
	if `fvops' {
		local vv: di "version " string(max(11,_caller())) ", missing: " 
	}
	if "`namelist'"==""{
		local namelist "`e(xvar)' _cons"
	}
	if "`pointwise'"=="" & "`uniform'"=="" & "`both'"=="" & "`none'"==""{
		if "`e(vce)'"!="novar"{
			local pointwise "pointwise"
			cap confirm matrix e(uniform)
			if _rc==0{
				local uniform "uniform"
			}
		}
	}
	if	"`both'"=="both"{
		local pointwise "pointwise"
		local uniform "uniform"
	}
	if "`level'"!="" & "`uniform'"=="uniform"{
		dis in red "The option level cannot be specified for uniform confidence bands."
		dis in red "The option level must be specified when `e(cmd)' is called."
		error 400
	}
	if "`uniform'"=="uniform"{
		cap confirm matrix e(uniform)
		if _rc>0{
			dis in red "The uniform confidence bands cannot be plotted because they have not been estimated by `e(cmd)'."
			dis in red "Call `e(cmd)' again with the suboption functional of the option vce() turned on."
			local uniform ""
		}
	}
	if "`pointwise'"=="pointwise"{
		if "`e(vce)'"=="novar"{
			dis in red "The pointwise confidence intervals cannot be plotted because the s.e. have not been estimated by `e(cmd)'."
			dis in red "Call `e(cmd)' without the option vce() set to novar."
			local pointwise ""
		}
	}
	tempvar coef quantiles
	tempname quantile
	quiet gen `coef'=.
	local input "`e(plotprocess)'"
	gettoken index xtitle_i : input
	mat `quantile' = `index'
/*	if "`e(cmd)'"=="qrprocess" | "`e(cmd)'"=="mdqr"{
		mat `quantile' = e(quantiles)
	}
	else{
		mat `quantile' = e(thresholds)
		if "`ols'"=="ols" | "`ols_ci'"=="ols_ci"{
			dis in red "The options ols and ols_ci make sense only with quantile regression."
			exit
		}
	}
*/
	if "`lcolor'"==""{
		local lcolor "black"
	}
	if "`pcolor'"==""{
		local pcolor "gs8"
	}	
	if "`pfcolor'"==""{
		local pfcolor "`pcolor'"
	}
	if "`ucolor'"==""{
		local ucolor "gs13"
	}
	if "`ufcolor'"==""{
		local ufcolor "`ucolor'"
	}
	local nq=rowsof(`quantile')
	quiet svmat `quantile', name(`quantiles')
	mata: PlotProcessCoeF = st_matrix("e(b)")'
	if "`pointwise'" == "pointwise"{
		tempvar pointl pointu
		quie gen `pointl'=.
		quie gen `pointu'=.
		cap confirm matrix e(pointwise)
		if _rc==0 & "`level'"==""{
			mata: PlotProcessPoinT = st_matrix("e(pointwise)")
		}
		else{
			if "`level'"==""{
				local level = c(level)
			}
			mata: PlotProcessPoinT = st_matrix("e(b)")' - invttail(st_numscalar("e(df_r)"),(100-`level')/200)*sqrt(diagonal(st_matrix("e(V)"))),st_matrix("e(b)")'+invttail(st_numscalar("e(df_r)"),(100-`level')/200)*sqrt(diagonal(st_matrix("e(V)")))
		}
	}
	if "`uniform'"=="uniform"{
		tempvar unifl unifu
		quie gen `unifl'=.
		quie gen `unifu'=.
		mata: PlotProcessUniF=st_matrix("e(uniform)")
	}
	local kplot=wordcount("`namelist'")
	local reg "`e(xvar)' _cons"
	local kreg=wordcount("`reg'")
	if "`xtitle'" == ""{
		local xtitle "`xtitle_i'"
	}
	if "`xtitle'"=="off"{
		local xtitle 
	}
	if "`ytitle'"=="off"{
		local ytitle 
	}
	if `kplot'>1 & "`legend'"==""{
		local legend "off"
	}
	if `kplot'>1 & wordcount("`title'")>=`kplot'{
		local different_titles="diff"
		forvalues i=1/`kplot'{
			gettoken title`i' title : title , parse("||")
			gettoken trash title:title , parse("||")
			gettoken trash title:title , parse("||")
		}
	}
	if "`legend'"=="" & ("`pointwise'"=="pointwise" | "`uniform'"=="uniform"){
		if "`pointwise'"=="pointwise" & "`uniform'"=="uniform"{
			local legend "order(3 "Coefficient" 2 "Pointwise CI" 1 "Uniform CB") rows(1)"
		}
		else if "`pointwise'"=="pointwise"{
			local legend "order(2 "Coefficient" 1 "Pointwise CI") rows(1)"
		}
		else{
			local legend "order(2 "Coefficient" 1 "Uniform CB") rows(1)"
		}
	}
	if "`compare'"!=""{
		tempname keep_est
		quietly estimates store `keep_est'
		gettoken compare_fct compare_opt : compare, parse(",")
		if "`e(wtype)'"!=""{
			local compare_weight "`e(wtype)'=`e(wexp)'"
		}
		if _caller() >= 11{
			version 11.1: quietly `compare_fct' `e(depvar)' `e(xvar)' [`compare_weight'] `compare_opt'
		}
		else{
			version 9.2: quietly `compare_fct' `compare_fct' `e(xvar)' [`compare_weight'] `compare_opt'
		}
		if "`compare_color'"==""{
			local compare_color "red*0.6"
		}
	}
	forvalues i=1/`kplot'{
		local temp_var=word("`namelist'",`i')
		local j=1
		while `j'<=`kreg'{
			local temp_reg=word("`reg'",`j')
			if regexm("`temp_reg'","`temp_var'"){
				continue, break
			}
			local j=`j'+1
		}
		mata: PlotProcessSelecT = rangen(`j',(`nq'-1)*`kreg'+`j',`nq')
		mata: st_store((1..`nq')', "`coef'", PlotProcessCoeF[PlotProcessSelecT,1])
		if "`pointwise'"=="pointwise"{
			mata: st_store((1..`nq')', ("`pointl'", "`pointu'"), PlotProcessPoinT[PlotProcessSelecT,.])
			local pointgraph "(rarea `pointl' `pointu' `quantiles'1, fcolor(`pfcolor') lcolor(`pcolor'))"
		}
		if "`uniform'"=="uniform"{
			mata: st_store((1..`nq')', ("`unifl'", "`unifu'"), PlotProcessUniF[PlotProcessSelecT,.])
			local unifgraph "(rarea `unifl' `unifu' `quantiles'1, fcolor(`ufcolor') lcolor(`ucolor'))"
		}
		if "`different_titles'"=="diff"{
			local temptitle `title`i''
		}
		else if "`title'"==""{
			if "`temp_var'"=="_cons"{
				local temptitle "Intercept"
			} 
			else{
				cap local vlabel: var l `temp_var'
				if "`vlabel'"=="" | _rc>0{
					local temptitle "`temp_var'"
				} 
				else{
					local temptitle "`vlabel'"
				}
			}
			local vlabel ""
		} 
		else if "`title'"=="off"{
			local temptitle
		}
		else local temptitle "`title'"
		if "`compare'"!=""{
			local ols_coef = _b[`temp_var']
			local olsgraph "(scatteri `ols_coef' 0 `ols_coef' 1, recast(line) lcolor(`compare_color'))"
		}	
		if "`compare_ci'"!=""{
			if "`level'"==""{
				local level=c(level)
			}
			local ols_lb = _b[`temp_var']+invnormal((100-`level')/200)*_se[`temp_var']
			local ols_ub = _b[`temp_var']+invnormal(1-(100-`level')/200)*_se[`temp_var']
			local olscigraph "(scatteri `ols_lb' 0 `ols_lb' 1, recast(line) lcolor(`compare_color') lpattern(dash)) (scatteri `ols_ub' 0 `ols_ub' 1, recast(line) lcolor(`compare_color') lpattern(dash))"
		}	
		if `kplot'==1{
			twoway `unifgraph' `pointgraph' (line `coef' `quantiles'1, lcolor(`lcolor')) `olsgraph' `olscigraph', ytitle("`ytitle'") xtitle("`xtitle'") title("`temptitle'") legend(`legend') `other_graph_options'
		}
		else{
			tempname graph`i'
			local graphcombine "`graphcombine' `graph`i''"
			twoway `unifgraph' `pointgraph' (line `coef' `quantiles'1, lcolor(`lcolor')) `olsgraph' `olscigraph', ytitle("`ytitle'") xtitle("`xtitle'") title("`temptitle'") legend(`legend') `other_graph_options' nodraw name(`graph`i'')
		}
	}
	if `kplot'>1{
		graph combine `graphcombine', `combine_options'
	}
	if "`compare'"!=""{
		quietly estimates restore `keep_est'
	}
	mata: mata drop PlotProcessCoeF
	capture mata: mata drop PlotProcessPoinT
	capture mata: mata drop PlotProcessUniF
	capture mata: mata drop PlotProcessSelecT
end	
