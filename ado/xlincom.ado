*! 1.2.7                25sep2023
*! Wouter Wakker        wouter.wakker@outlook.com

* 1.2.7     25sep2023   names check removed at suggestion of NHB
* 1.2.6     28nov2020   test used to calculate covariances
* 1.2.5     13nov2020   repost option supported for most if not all estimation commands
* 1.2.4     09nov2020   repost option added
* 1.2.3     07nov2020   estadd option added
* 1.2.2     03nov2020   eform option similar to lincom
* 1.2.1     02nov2020   display options allowed
* 1.2.0     02nov2020   no parentheses necessary and no constrained syntax for single equation
* 1.1.2     26oct2020   up to two decimals in level option allowed
* 1.1.1     21oct2020   allow eqno:coef syntax
* 1.1.0     08jul2020   name specification syntax change (name) --> name=
* 1.0.4     30jun2020   aesthetic changes
* 1.0.3     26jun2020   name change mlincom --> xlincom
* 1.0.2     09jun2020   proper error code when parentheses found in equation
* 1.0.1     07may2020   if statements for display options run slightly faster
* 1.0.0     05may2020   born

program xlincom, eclass
	version 8

	if replay() {
		if "`e(cmd)'" != "xlincom" error 301
		
		syntax [, EForm          ///
		          OR             ///
		          HR             ///
		          SHR            ///
		          IRr            ///
		          RRr            ///
		          Level(cilevel) ///
		          *              ///
		          ]
		
		// Only one display option allowed
		local eformopt : word count `eform' `or' `hr' `shr' `irr' `rrr' 
		if `eformopt' > 1 {
			di as error "only one display option can be specified"
			exit 198
		}
		
		// Get additional display options
		_get_diopts displayopts, `options'

	}
	else {
		syntax anything(equalok id="expression") [, EForm                ///
		                                            OR                   ///
		                                            HR                   ///
		                                            SHR                  ///
		                                            IRr                  ///
		                                            RRr                  ///
		                                            Level(cilevel)       ///
		                                            DF(numlist max=1 >0) ///
		                                            POST                 ///
		                                            REPOST               ///
		                                            COVZERO              ///
		                                            noHEADer             ///
		                                            ESTADD(string asis)  ///
		                                            *                    ///
		                                            ]
		
		// Only one display option allowed
		local eformopt : word count `eform' `or' `hr' `shr' `irr' `rrr' 
		if `eformopt' > 1 {
			di as error "only one display option can be specified"
			exit 198
		}
		
		// Only post or repost allowed
		if "`post'" != "" & "`repost'" != "" {
			di as error "options {bf:post} and {bf:repost} not allowed together"
			exit 198
		}
		
		// Get additional display options
		_get_diopts displayopts, `options'
		
		// Estadd only allowed when not posting results
		if `"`estadd'"' != "" {
			if "`post'`repost'" != "" {
				di as error "option {bf:estadd} not allowed when posting results"
				exit 198
			}
			xlincom_parse_estadd `estadd'
		}
		
		// Header option
		if "`header'" != "" local dont *
		
		// Parse input, must be within parentheses,
		// return list of "[name=] equation" and number of equations
		xlincom_check_parentheses `anything'
		local name_eq_list "`s(name_eq_list)'"
		local n_lc = s(n_lc)
			
		// Store e(b) and e(V) matrix
		tempname eb eV
		mat `eb' = e(b)
		mat `eV' = e(V)
		local rownames : rowfullnames `eV'
		local n_eV = rowsof(`eV')
		
		// Extract estimation output (code based on Roger Newson's lincomest.ado)
		local depname = e(depvar)
		local obs = e(N)
		if "`df'" == "" local dof = e(df_r)
		else local dof = `df' 
		tempvar esample
		gen byte `esample' = e(sample)
		
		// Define tempnames and matrices for results
		tempname estimate se variance
		if "`repost'" == "" {
			tempname beta vcov
			mat def `beta' = J(1, `n_lc', 0)
			mat def `vcov' = J(`n_lc', `n_lc', 0)
		}
		else {
			tempname betarepost vcovrepost
			mat def `betarepost' = J(1, `n_eV' + `n_lc', 0)
			mat def `vcovrepost' = J(`n_eV' + `n_lc', `n_eV' + `n_lc', 0)	
			mat `betarepost'[1,1] = `eb'
			mat `vcovrepost'[1,1] = `eV'
		}
		
		`dont' di
		
		// Call lincom for each equation, extract output
		local i 1
		foreach name_eq of local name_eq_list {
			
			// Parse name/eq, return name and equation for display
			xlincom_parse_name_eq `"`name_eq'"' `i'
			local eq_names "`eq_names' `s(eq_name)'"
			local name "`s(eq_name)'"
			local eq "`s(eq)'"
			
			// Parse equation, return proper equation for test in case of multiple equation models
			xlincom_parse_eq_for_test "`eq'"
			local eq_for_test "`s(eq_for_test)'"
			if "`post'`repost'" != "" & "`covzero'" == "" local eqs_for_cov "`eqs_for_cov' (`eq_for_test' = 0)"
			
			// Check if there are no additive constants in equation when eform is specified, lincom will throw an error
			if `eformopt' > 0 qui lincom `eq_for_test', `eform' `or' `hr' `shr' `irr' `rrr' 
			
			qui lincom `eq_for_test'
			
			`dont' di as txt %13s abbrev("`name':",13)  _column(16) as res "`eq' = 0"
			
			// Get estimate and standard error
			scalar `estimate' = r(estimate)
			scalar `se' = r(se)
			
			// Calculate transformed se and var if previous command is logistic (from Roger Newson's lincomest.ado)
			if "`e(cmd)'" == "logistic" {
				scalar `se' = `se' / `estimate'
				scalar `estimate' = log(`estimate')
				if `eformopt' == 0 local or "or"
			}
			
			scalar `variance' = `se' * `se'
			
			// Store results in matrices
			if "`repost'" == "" {
				mat `beta'[1, `i'] = `estimate'
				mat `vcov'[`i', `i'] = `variance'
			}
			else {
				mat `betarepost'[1, `i' + `n_eV'] = `estimate'
				mat `vcovrepost'[`i' + `n_eV', `i' + `n_eV'] = `variance'
			}
			
			local ++i
		}
		
		// Check if coef doesn't already exist in e(b) for repost
		if "`repost'" != "" {
			foreach eqname of local eq_names {
				if !missing(colnumb(`eb', "xlincom:`eqname'")) {
					di as error "{bf:xlincom:`eqname'} already exists in {bf:e(b)}"
					exit 198
				}
			}
		}
		
		// Fill VCOV matrix with covariances
		if "`covzero'" == "" & "`post'`repost'" != "" {
			if "`post'" != "" & `n_lc' > 1 {
				qui test `eqs_for_cov', matvlc(`vcov')
			}
			else if "`repost'" != "" {
				foreach rowname of local rownames {
					local params_for_cov "`params_for_cov' (_b[`rowname'] = 0)"
				}
				qui test `params_for_cov' `eqs_for_cov', matvlc(`vcovrepost')
			}
		}
			
		// Name rows/cols matrices
		if "`repost'" == "" {
			mat rownames `beta' = y1
			mat colnames `beta' = `eq_names'
			mat rownames `vcov' = `eq_names'
			mat colnames `vcov' = `eq_names'
		}
		else {
			local equations: coleq `eb', quoted // Thanks to Ben Jann for these two lines
			local equations: subinstr local equations `""_""' `""Main""', all word
			forval i = 1/`n_lc' {
				local equations `"`equations' "xlincom""'
			}
			local names: colnames `eb'
			local names `names' `eq_names'
			mat rownames `betarepost' = y1
			mat colnames `betarepost' = `names'
			mat coleq `betarepost' = `equations'
			mat rownames `vcovrepost' = `names'
			mat roweq `vcovrepost' = `equations'
			mat colnames `vcovrepost' = `names'
			mat coleq `vcovrepost' = `equations'
		}
	}
	
	// Eform options 
	if "`eform'" == "" {
		if "`or'" != "" local eform "Odds Ratio"
		else if "`hr'" != "" local eform "Haz. Ratio"
		else if "`shr'" != "" local eform "SHR"
		else if "`irr'" != "" local eform "IRR"
		else if "`rrr'" != "" local eform "RRR"
	}
	else local eform "exp(b)"
	
	di
	
	// Post and display results
	if "`post'" != "" {
		ereturn post `beta' `vcov' , depname("`depname'") obs(`obs') dof(`dof') esample(`esample')
		ereturn local cmd "xlincom"
		ereturn local predict "xlincom_p"
		ereturn display, eform(`eform') level(`level') `displayopts'
	}
	else if "`repost'" != "" {
		cap ereturn repost b = `betarepost' V = `vcovrepost', resize
		if _rc ereturn post `betarepost' `vcovrepost', noclear
		ereturn display, eform(`eform') level(`level') `displayopts'
	}
	else if replay() ereturn display, eform(`eform') level(`level') `displayopts'
	else {
		tempname hold
		nobreak {
			_estimates hold `hold'
			capture noisily break {
				ereturn post `beta' `vcov' , depname("`depname'") obs(`obs') dof(`dof') esample(`esample')
				ereturn local cmd "xlincom"
				ereturn display, eform(`eform') level(`level') `displayopts'
				if `"`estadd'"' != "" {
					tempname rtable
					mat `rtable' = r(table)
				}
			}
			local rc = _rc
			_estimates unhold `hold'
			if `rc' exit `rc'
			if `"`estadd'"' != "" {
				xlincom_parse_estadd `estadd'
				xlincom_estadd "`n_lc'" "`eq_names'" "`rtable'" "`s(star)'" "`s(se)'" "`s(t)'" "`s(p)'" "`s(ci)'" "`s(bfmt)'" ///
				               "`s(sefmt)'" "`s(tfmt)'" "`s(pfmt)'" "`s(cifmt)'" "`s(left)'" "`s(right)'" `"`s(starlevels)'"'
			}
		}
	}
end

// Check if parentheses are properly specified
program xlincom_check_parentheses, sclass
	version 8
	
	local n_lc 0
	gettoken first : 0, parse(" (")
	if `"`first'"' == "(" { 
		while `"`first'"' != "" {
			local ++n_lc
			gettoken first 0 : 0, parse(" (") match(paren)
			local first `first'
			local name_eq_list `"`name_eq_list' `"`first'"'"'
			gettoken first : 0, parse(" (")
			if !inlist(`"`first'"', "(", "") {
				di as error "equation must be contained within parentheses"
				exit 198
			}
		}
	}
	else {
		local name_eq_list `""`0'""'
		local n_lc 1
	}
	
	sreturn local name_eq_list "`name_eq_list'"
	sreturn local n_lc = `n_lc'
end

// Parse name/eq expression
// Return name and equation
program xlincom_parse_name_eq, sclass
	version 8
	args name_eq n
	
	gettoken first eq : name_eq, parse("=")
	if "`first'" != "`name_eq'" {
		if "`first'" != "=" {
			local wc : word count `first'
			if `wc' > 1 {
				di as error "{bf:`first'} invalid name"
				exit 7
			}
			local eq_name `first'
			gettoken equalsign eq : eq, parse("=")
		}
		else local eq_name lc_`n'
	}
	else {
		local eq_name lc_`n'
		local eq `name_eq'
	}
	
	sreturn local eq_name `eq_name'
	sreturn local eq `eq'
end	

// Parse equation, look for multiple equation expressions
// Return equation that is accepted by test
program xlincom_parse_eq_for_test, sclass
	version 8
	args eq

	gettoken first rest : eq , parse(":")
	if `"`first'"' == `"`eq'"' local eq_for_test `"`eq'"'
	else {
		tokenize `"`eq'"', parse(":+-/*()")
		local i 1
		local 0
		while "``i''" != "" {
			local `i' = strtrim("``i''")
			if inlist("``i''", "*", "/", "+", "-", "(", ")") local eq_for_test `"`eq_for_test' ``i''"'
			else if "``=`i'+1''" == ":" & !strpos("``i''", "[") local eq_for_test `"`eq_for_test' [``i'']"'
			else if "``=`i'-1''" == ":" local eq_for_test `"`eq_for_test'``i''"'
			else if "``i''" != ":" & !strpos("``=`i'-1''", "[") local eq_for_test `"`eq_for_test' ``i''"'
			else if !strpos("``=`i'-1''", "]") & strpos("``=`i'-1''", "[") local eq_for_test `"`eq_for_test':"'
			local ++i
		}
	}
	
	sreturn local eq_for_test `eq_for_test'
end

// Parser for estadd option to check if everything is specified properly
program xlincom_parse_estadd, sclass
	version 8
	syntax anything(id="star|nostar") [, SE                      ///
	                                     T                       ///
	                                     P                       ///
	                                     CI                      ///
	                                     FMT(string)             ///
	                                     BFMT(string)            /// 
	                                     SEFMT(string)           /// 
	                                     TFMT(string)            ///
	                                     PFMT(string)            ///
	                                     CIFMT(string)           ///
	                                     PARentheses             ///
	                                     BRAckets                ///
	                                     STARLevels(string asis) ///
	                                     ]
	
	if !inlist("`anything'", "star", "nostar") {
		di as error "{bf:star} or {bf:nostar} must be specified in option {bf:estadd}"
		exit 198
	}
	
	if "`parentheses'" != "" & "`brackets'" != "" {
		di as error "only one option allowed of options: {bf:parentheses}, {bf:brackets}"
		exit 198
	}
	
	if "`fmt'" != "" & ("`bfmt'" != "" | "`sefmt'" != "" | "`tfmt'" != "" | "`pfmt'" != "") {
		di as error "format options wrongly specified"
		exit 198
	}
	
	foreach format in "`fmt'" "`bfmt'" "`sefmt'" "`tfmt'" "`pfmt'" {
		if "`format'" != "" confirm numeric format `format'
	}
	
	if "`fmt'" != "" {
		local bfmt `fmt'
		local sefmt `fmt'
		local tfmt `fmt'
	}
	
	if "`parentheses'`brackets'" != "" {
		if "`parentheses'" != "" {
			local left "("
			local right ")"
		}
		else {
			local left "["
			local right "]"
		}
	}
	
	if `"`starlevels'"' == "" local starlevels "* 0.05 ** 0.01 *** 0.001"
	xlincom_parse_starlevels `"`starlevels'"'
	
	sreturn local star `anything'
	sreturn local se `se'
	sreturn local t `t'
	sreturn local p `p'
	sreturn local ci `ci'
	sreturn local bfmt `bfmt'
	sreturn local sefmt `sefmt'
	sreturn local tfmt `tfmt'
	sreturn local pfmt `pfmt'
	sreturn local cifmt `cifmt'
	sreturn local left `left'
	sreturn local right `right'
	sreturn local starlevels "`s(starl_list)'"
end

// Parser for starlevels suboption of estadd option
// Check if specified properly and return list of sign/pval combinations
program xlincom_parse_starlevels, sclass
	version 8
	args starlevels
	
	local cnt : word count `starlevels'
	if mod(`cnt', 2) != 0 {
		di as error "option {bf:starlevels} wrongly specified"
		exit 198
	}
	
	forval i = 1/`cnt' {
		if mod(`i', 2) == 0 {
			confirm number `:word `i' of `starlevels''
			if `i' >= 4 {
				if `:word `i' of `starlevels'' >= `:word `=`i'-2' of `starlevels'' {
					di as error "pvalues in option {bf:starlevels} must be specified in descending order"
					exit 198
				}
			}
		}
		else {
			cap confirm number `:word `i' of `starlevels''
			if !_rc {
				di as error "{bf:`:word `i' of `starlevels''} found where string expected
				exit 198
			}
		}
	}
	
	forval i = 1(2)`cnt' {
		local starl_list `"`starl_list' `""`:word `i' of `starlevels''" "`:word `=`i'+1' of `starlevels''""'"'
	}

	sreturn local starl_list "`starl_list'"
end

// Add results as local or scalar to e()
program xlincom_estadd, eclass
	version 8
	args n_lc names rtable star se t p ci bfmt sefmt tfmt pfmt cifmt left right starlevels
	
	// Default formatting
	local defaultfmt %4.3f
	if "`star'" == "star" & "`bfmt'" == "" local bfmt `defaultfmt'
	if "`left'" != "" {
		foreach format in sefmt tfmt pfmt cifmt {
			if "``format''" == "" local `format' `defaultfmt'
		}
	}
	
	forval i = 1/`n_lc' {
		if "`left'" == "" {
			if "`ci'" != "" {
				if "`cifmt'" == "" ereturn local p_`:word `i' of `names'' = "`=`rtable'[5, `i']',`=`rtable'[6, `i']'"
				else ereturn local ci_`:word `i' of `names'' = "`:di `cifmt' `rtable'[5, `i']',`:di `cifmt' `rtable'[6, `i']'"
			}
			if "`p'" != "" {
				if "`pfmt'" == "" ereturn scalar p_`:word `i' of `names'' = `rtable'[4, `i']
				else ereturn local p_`:word `i' of `names'' = `:di `pfmt' `rtable'[4, `i']'
			}
			if "`t'" != "" {
				if "`tfmt'" == "" ereturn scalar t_`:word `i' of `names'' = `rtable'[3, `i']
				else ereturn local t_`:word `i' of `names'' = `:di `tfmt' `rtable'[3, `i']'
			}
			if "`se'" != "" {
				if "`sefmt'" == "" ereturn scalar se_`:word `i' of `names'' = `rtable'[2, `i']
				else ereturn local se_`:word `i' of `names'' = `:di `sefmt' `rtable'[2, `i']'
			}
		}
		else {
			if "`ci'" != "" {
				if "`cifmt'" == "" ereturn local p_`:word `i' of `names'' = "`left'" + "`=`rtable'[5, `i']',`=`rtable'[6, `i']'" + "`right'"
				else ereturn local ci_`:word `i' of `names'' = "`left'" + "`:di `cifmt' `rtable'[5, `i']',`:di `cifmt' `rtable'[6, `i']'" + "`right'"
			}
			if "`p'" != "" {
				if "`pfmt'" == "" ereturn local p_`:word `i' of `names'' = "`left'" + "`=`rtable'[4, `i']'" + "`right'"
				else ereturn local p_`:word `i' of `names'' = "`left'" + "`:di `pfmt' `rtable'[4, `i']'" + "`right'"
			}
			if "`t'" != "" {
				if "`tfmt'" == "" ereturn local t_`:word `i' of `names'' = "`left'" + "`=`rtable'[3, `i']'" + "`right'"
				else ereturn local t_`:word `i' of `names'' = "`left'" + "`:di `tfmt' `rtable'[3, `i']'" + "`right'"
			}
			if "`se'" != "" {
				if "`sefmt'" == "" ereturn local se_`:word `i' of `names'' = "`left'" + "`=`rtable'[2, `i']'" + "`right'"
				else ereturn local se_`:word `i' of `names'' = "`left'" + "`:di `sefmt' `rtable'[2, `i']'" + "`right'"
			}
		}
		if "`star'" == "nostar" {
			if "`bfmt'" == "" ereturn scalar b_`:word `i' of `names'' = `rtable'[1, `i']
			else ereturn local b_`:word `i' of `names'' = `:di `bfmt' `rtable'[1, `i']'
		}
		else {
			local addstar
			foreach level of local starlevels {
				local sign `:word 1 of `level''
				if `rtable'[4, `i'] < `:word 2 of `level'' local addstar `sign'
			}
			if "`bfmt'" == "" ereturn local b_`:word `i' of `names'' = "`=`rtable'[1, `i']'`addstar'"
			else ereturn local b_`:word `i' of `names'' = "`:di `bfmt' `rtable'[1, `i']'`addstar'"
		}
	}
end
