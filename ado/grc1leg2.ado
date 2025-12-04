*! version 2.26 (4Nov2023), by Mead Over
*  Enhanced version of -grc1leg- version 1.0.5  02jun2010, by Vince Wiggins
program grc1leg2
	version 13.1
	syntax [anything(id="graph list")] [, LEGendfrom(string)  ///
			     POSition(string) RING(integer -1) SPAN	///
			     NAME(passthru) SAVing(string asis)   ///
				 XTOB1title XTItlefrom(string) XTSize(string)  ///  These and following options by MO
				 LABSize(string) LSize(string)  ///  lsize() is retained fo backward compatibility
				 YTOL1title y1tol1title YTItlefrom(string) YTSize(string)  ///
				 y2tor1title y2titlefrom(string) y2tsize(string)  ///
				 l1tol1title l1titlefrom(string)  ///
				 l2tol2title l2titlefrom(string)  ///
				 r1tor1title r1titlefrom(string)  ///
				 r2tor2title r2titlefrom(string)  ///
				 MAINTOtoptitle MAINTItlefrom(string) MAINFrom(string) MTSize(string)  ///  
				 SUBTOsubtitle SUBFrom(string) STSize(string)  ///  
				 NOTETOnote SUBTONote NOTEFrom(string) NTSize(string)  ///  
				 SYMXsize(string) SYMYsize(string) LMSize(string) /// 
				 LTItle(string) LTSize(string) LSUBTItle(string) LSUBTSize(string)  ///
				 LEGScale(string)  ///
				 LXOffset(real 0) LYOffset(real 0) LOFF HIDElegendfrom  ///
				 LCols(string) LRows(string) LHoles(string)  ///
				 ysize(passthru) xsize(passthru) margins(passthru) scale(passthru) scheme(passthru) ///
				 holes(string) ///
				 GRAPHon DOTS DEBUG notrap noAUTOsize * ]
				 
	if strpos(`"`anything'"',"*") | strpos(`"`anything'"',"?") {
	    di as err `"The "wildcard" characters "*" and "?" are not permitted in the list of graph names to be combined."'
		error 198
	}

	local Npnls : list sizeof anything
	if "`holes'"~="" {
		local Nholes : list sizeof holes
		local Npnls = `Npnls' + `Nholes'
		local holes holes(`holes')
	}
	
					//  For compatibility with version 2.13, allow only one or neither of following two options
	local opt1 = cond("`labsize'"~="","labsize","") 
	local opt2 = cond("`lsize'"~="","lsize","") 
	//  Allow only one of these options (lsize is retained for backward compatibility)
	opts_exclusive "`opt1' `opt2'"
	local labsize `lsize'`labsize'
		
					//  For compatibility with version 2.13, allow only one or neither of following two options
	local opt1 = cond("`maintitlefrom'"~="","maintitlefrom()","")
	local opt2 = cond("`mainfrom'"~="","mainfrom()","") 
	opts_exclusive "`opt1' `opt2'"  //  Allow only one of these options
	local mainfrom `maintitlefrom'`mainfrom'
	if "`mainfrom'"~=""   local maintotoptitle maintotoptitle
		
	if "`dots'"=="dots"  {
	    nois _dots 0, title(-grc1leg2- working...)
		global grc1leg2_stage 1
	}
	else {
		di as txt "-grc1leg2- working..."
	}
	if strpos(`"`saving'"',"asis") {
		local isasis isasis
	}
	
					// If -grc1leg2- has previously set graphics off,
					// and exited without setting graphics back on,
					// set graphics on now.
	if "${grc1leg2_set_graph}"=="off" {
	    set graphics on
		global grc1leg2_set_graph 
	}
	local usergrset "`c(graphics)'"
	if "`graphon'"=="" & "`usergrset'"=="on" {
	    set graphics off
		global grc1leg2_set_graph off
	}

	gr_setscheme , refscheme	// So we can have temporary styles

					// location and alignment in cell
	tempname clockpos
	if ("`position'" == "") local position 6
	.`clockpos' = .clockdir.new , style(`position')
	local location `.`clockpos'.relative_position'

					// specify -ring()- for combined legend
	if `ring' > -1 {
		if (`ring' == 0) {
			local location "on"
			local ring ""
		}
		else	local ring "ring(`ring')"
	}
	else	local ring ""

	if "`span'" != "" {
		if "`location'" == "above" | "`location'" == "below" {
			local span spancols(all)
		}
		else	local span spanrows(all)
	}

					// allow legend to be from any graph
	if `"`legendfrom'"' != "" {			
		local lfrom : list posof `"`legendfrom'"' in anything
		if `lfrom' == 0 {
		    di as err `"-`legendfrom'- not found in graph name list"'
		    error 198
		}
	}
	else {  		// use graph 1 for legend by default
		local lfrom 1
		gettoken legendfrom : anything
	}
	
	if "`dots'"=="dots"  make10dots , stage(${grc1leg2_stage})
	
	if "`debug'"~=""  {
		di _n as txt "DEBUG: Displaying -debug- output"
		di _n as txt "DEBUG: -grc1leg2- is executing -gr combine- on component graphs"
		di _n as txt `"DEBUG: Options not parsed by -syntax- and passed through to -gr combine- are:"' ///
		   _n as txt `"`options'"'	_n
		local noi noisily		   
	}

	//	When editing component panels below, panels are identified by their sequence in this combined graph
	graph combine `anything' , `holes' `name' nodraw `options'  // combine graphs
	
	if "`name'" != "" {				// get graph name
		local 0 `", `name'"'
		syntax [, name(string) ]
		local 0 `"`name'"'
		syntax [anything(name=name)] [, replace]
	}
	else	local name Graph

	//  Options for -gr combine-
	if "`ysize'`xsize'`margins'`scale'`scheme'"~="" {
		graph display `name' , `ysize' `xsize' `margins' `scale' `scheme'
		graph rename `name' `name', replace
		if "`dots'"=="dots"  make10dots , stage(${grc1leg2_stage})
	}	

	//  Determine from which type of graph the legend is borrowed
	chk_legend `name' , lfrom(`lfrom') legendfrom(`"`legendfrom'"') `trap' `debug'  // Double quotes added ver. 2.12
		if "`debug'"~="" {
			di _n as txt "DEBUG: The subroutine -chk_legend- returns:"
			return list
			matlist r(keystyledex)
		} 
		local Nkeys `r(Nkeys)'
		local Nlabels `r(Nlabels)'
		local labelwidth `r(labelwidth)'
		local legendtype `r(legendtype)'
		local legendclass `r(legendclass)'
		local NkeysEqNlabels `r(NkeysEqNlabels)'
		local NminofKeysLabels  `r(NminofKeysLabels)'
		local NmaxofKeysLabels  `r(NmaxofKeysLabels)'
		//  The matrix consisting of the value of -styledex- for all the legend keys
		tempname keystyledex
		mat define `keystyledex' = r(keystyledex)
		local SmlstEqLrgst = `r(SmlstEqLrgst)'
	
					// turn off legends & optionally titles, etc on component graphs
	forvalues i = 1/`Npnls' {
 	
					//  Suppress legend on a -combined- graph, version 1.34 
		forvalues g = 1/30 {
			cap classutil describe  .`name'.plotregion1.graph`i'.plotregion1.graph`g'.legend    
			if _rc == 0 {
				_gm_edit .`name'.plotregion1.graph`i'.plotregion1.graph`g'.legend.draw_view.set_false
				_gm_edit .`name'.plotregion1.graph`i'.plotregion1.graph`g'.legend.fill_if_undrawn.set_false
				_gm_edit .`name'.plotregion1.graph`i'.plotregion1.graph`g'.legend.draw_view.setstyle, style(no)
			}
			else {
				continue, break				
			}
		}
			
		cap classutil describe  .`name'.graphs[`i'].legend    
		if _rc == 0 {
			_gm_edit .`name'.graphs[`i'].legend.draw_view.set_false
			_gm_edit .`name'.graphs[`i'].legend.fill_if_undrawn.set_false
			//  Suppress legend on a -by- graph
			_gm_edit .`name'.graphs[`i'].legend.draw_view.setstyle, style(no)
		}

		cap classutil describe  .`name'.graphs[`i'].plotregion1.atlegend
		if _rc == 0 {
			_gm_edit .`name'.graphs[`i'].plotregion1.atlegend.draw_view.setstyle, style(no)
		}
		
		if "`xtob1title'"~="" {  // turn off xaxis1 titles  
			cap classutil describe  .`name'.graphs[`i'].xaxis1.title    
			if _rc == 0 {
				_gm_edit .`name'.graphs[`i'].xaxis1.title.draw_view.set_false
			}
		}
		if "`ytol1title'"~="" | "`y1tol1title'"~="" {  // turn off yaxis1 titles (by M. Over)
			cap classutil describe  .`name'.graphs[`i'].yaxis1.title    
			if _rc == 0 {
				_gm_edit .`name'.graphs[`i'].yaxis1.title.draw_view.set_false
			}
		}
		if "`l1tol1title'"~=""  {  // turn off l1 titles  
			cap classutil describe  .`name'.graphs[`i'].l1title    
			if _rc == 0 {
				_gm_edit .`name'.graphs[`i'].l1title.draw_view.set_false
			}
		}
		if "`y2tor1title'"~="" {  // turn off yaxis2 titles  
			cap classutil describe  .`name'.graphs[`i'].yaxis2.title    
			if _rc == 0 {
				_gm_edit .`name'.graphs[`i'].yaxis2.title.draw_view.set_false
			}
		}
		if "`r1tor1title'"~="" {  // turn off yaxis2 titles  
			cap classutil describe  .`name'.graphs[`i'].r1title    
			if _rc == 0 {
				_gm_edit .`name'.graphs[`i'].r1title.draw_view.set_false
			}
		}

			// suppress main title, subtitle and/or note in component panels
			// no provision for suppressing other titles such as caption, b1title, b2title, l1title, etc.
/*
Option:               Graph from and text size option names:
-maintotoptitle-      -maintitlefrom- or -mainfrom-, -mtsize()-
-subtosubtitle-       -subfrom-, -stsize()-
-notetonote-          -notefrom-, -ntsize()-
-subtotoptitle-       <--  Not implemented
-maintosubtitle-      <--  Not implemented
-subtonote-           <--  Not implemented
*/

		if "`debug'"~=""  {
			di as txt "DEBUG: In line 237, -options- local macro is: -" as res `"`options'"'
		}
		// suppress the main, sub or note title from -mainfrom- -subfrom- or -notefrom- 
		// if they are to be moved to the combined graph's main, sub and/or note title
		foreach ttl in main sub note {
			
			local ttlopt =  ///
				cond("`ttl'"=="main","maintotoptitle", ///
				cond("`ttl'"=="sub","subtosubtitle", ///
				cond("`ttl'"=="note","notetonote","noneofabove")))
			
			local ttltype =  ///
				cond("`ttl'"=="main","title", ///
				cond("`ttl'"=="sub","subtitle", ///
				cond("`ttl'"=="note","note","noneofabove")))
				if "`ttltype'"=="noneofabove" {
					di as err "Error in grc1leg2, Line 247:  This error should never occur."
				}

			if "``ttlopt''"~="" {  // turn off `ttl' title in component graphs
				cap classutil describe  .`name'.graphs[`i'].`ttltype'
				if _rc == 0 {
					_gm_edit .`name'.graphs[`i'].`ttltype'.draw_view.set_false
					_gm_edit .`name'.graphs[`i'].`ttltype'.fill_if_undrawn.set_false
				}
			}
		}

		if "`dots'"=="dots"  make10dots , stage(${grc1leg2_stage}) 
	}
	
	//  legend is from a single graph
	if "`legendtype'"=="legend"  {
								// insert overall legend
		.`name'.insert (legend = .`name'.graphs[`lfrom'].legend)	    ///
				`location' plotregion1 , `ring' `span' 

		_gm_log  .`name'.insert (legend = .graphs[`lfrom'].legend) 	    ///
				`location' plotregion1 , `ring' `span' 
	}
		
	//  if not and if legend is from -graph ..., by()- 
	//  (with -else- here ... , -grc1leg2- is recursive)
	else if "`legendtype'"=="atlegend"  {
								// insert overall legend
		.`name'.insert (legend = .`name'.graphs[`lfrom'].plotregion1.atlegend)	    ///
				`location' plotregion1 , `ring' `span' 

		_gm_log  .`name'.insert (legend = .graphs[`lfrom'].plotregion1.atlegend) 	    ///
				`location' plotregion1 , `ring' `span' 
	}

	//  if not and if legend is from -gr combine- 
	//  (and with -else- here, -grc1leg2- is recursive)
	else if "`legendtype'"=="combined"  {
								// insert overall legend
		.`name'.insert (legend = .`name'.plotregion1.graph`lfrom'.plotregion1.graph1.legend)	    ///
				`location' plotregion1 , `ring' `span' 

		_gm_log  .`name'.insert (legend = .plotregion1.graph`lfrom'.plotregion1.graph1.legend) 	    ///
				`location' plotregion1 , `ring' `span' 
	}

							//  Display combined legend
	_gm_edit .`name'.legend.style.box_alignment.setstyle ,		    ///
		style(`.`clockpos'.compass2style')

	_gm_edit .`name'.legend.draw_view.setstyle, style(yes)		
		
							// use -xtitlefrom- xtitle as overall b1title
	if "`xtob1title'"=="" & "`xtitlefrom'"~="" {
		local xtob1title xtob1title
	}
	if "`xtob1title'"~="" {
							// allow b1title to be from any graph
		if "`xtitlefrom'" != "" {			
			local xfrom : list posof "`xtitlefrom'" in anything
			if `xfrom' == 0 {
				di as error `"`xtitlefrom' not found in graph name list"'
				error 198
			}
		}
		else	local xfrom 1		// use graph 1 for xtitle by default

		.`name'.b1title = .`name'.graphs[`xfrom'].xaxis1.title
		_gm_log .`name'.b1title = .graphs[`xfrom'].xaxis1.title
		_gm_edit .`name'.b1title.draw_view.set_true
		
	}

							// use -ytitlefrom- ytitle as overall l1title
	if "`ytol1title'"=="" & "`ytitlefrom'"~="" {
		local ytol1title ytol1title
	}
	if "`ytol1title'"~="" {
							// allow l1title to be from any graph
		if "`ytitlefrom'" != "" {			
			local yfrom : list posof "`ytitlefrom'" in anything
			if `yfrom' == 0 {
				di as error `"`ytitlefrom' not found in graph name list"'
				error 198
			}
		}
		else	local yfrom 1		// use graph 1 for ytitle by default

		.`name'.l1title = .`name'.graphs[`yfrom'].yaxis1.title
		_gm_log .`name'.l1title = .graphs[`yfrom'].yaxis1.title
		_gm_edit .`name'.l1title.draw_view.set_true

	}

							// use -y2titlefrom-'s y2title as overall r1title
	if "`y2tor1title'"=="" & "`y2titlefrom'"~="" {
		local y2tor1title y2tor1title
	}
	if "`y2tor1title'"~="" {
							// allow r1title to be from any graph
		if "`y2titlefrom'" != "" {			
			local y2from : list posof "`y2titlefrom'" in anything
			if `y2from' == 0 {
				di as error `"`y2titlefrom' not found in graph name list"'
				error 198
			}
		}
		else	local y2from 1		// use graph 1 for ytitle by default

		.`name'.r1title = .`name'.graphs[`y2from'].yaxis2.title
		_gm_log .`name'.r1title = .graphs[`y2from'].yaxis2.title
		_gm_edit .`name'.r1title.draw_view.set_true

	}

	foreach t in l1 l2 r1 r2  {	
								// use -`t'titlefrom-'s `t'title as overall `t'title
		if "``t'to`t'title'"=="" & "``t'titlefrom'"~="" {
			local `t'to`t'title `t'to`t'title
		}
		if "``t'to`t'title'"~="" {
								// allow `t'title to be from any graph
			if "``t'titlefrom'" != "" {			
				local yfrom : list posof "``t'titlefrom'" in anything
				if `yfrom' == 0 {
					di as error `"``t'titlefrom' not found in graph name list"'
					error 198
				}
			}
			else	local yfrom 1		// use graph 1 for `t'title by default

			.`name'.`t'title = .`name'.graphs[`yfrom'].`t'title
			_gm_log .`name'.`t'title = .graphs[`yfrom'].`t'title
			_gm_edit .`name'.`t'title.draw_view.set_true

		}
	}
	
	// use the main, sub or note title from -mainfrom- -subfrom- or -notefrom- 
	// as the combined graph's main, sub and/or note title
	foreach ttl in main sub note {
		
		local ttlopt =  ///
			cond("`ttl'"=="main","maintotoptitle",  ///
			cond("`ttl'"=="sub","subtosubtitle",  ///
			cond("`ttl'"=="note","notetonote","noneofabove")))
			
		if "``ttlopt''"=="" & "``ttl'from'"~="" {
			local `ttlopt' `ttlopt'
		}
		if "``ttlopt''"~="" {
								// allow `ttl' title to be from any graph
			if "``ttl'from'" != "" {			
				local `ttl'from : list posof "``ttl'from'" in anything
				if ``ttl'from' == 0 {
					di as error `"``ttl'from' not found in graph name list"'
					error 198
				}
			}
			else	local `ttl'from 1		// use graph 1 for `ttl' title by default

			local ttltype =  ///
				cond("`ttl'"=="main","title", ///
				cond("`ttl'"=="sub","subtitle", ///
				cond("`ttl'"=="note","note","noneofabove")))
			if "`ttltype'"=="noneofabove" {
				di as err "Error in grc1leg2, Line 420:  This error should never occur."
			}
			
			.`name'.`ttltype' = .`name'.graphs[``ttl'from'].`ttltype'
			_gm_log .`name'.`ttltype' = .graphs[``ttl'from'].`ttltype'
			_gm_edit .`name'.`ttltype'.draw_view.set_true

		}

	}

	_gm_edit .`name'.legend.draw_view.set_true

			// VW's code to maintain serset reference counts
			// must pick up sersets by reference, they were 
			// -.copy-ied when the legend was created above
	if "`legendtype'"=="legend"  {
		forvalues i = 1/`.`name'.legend.keys.arrnels' {
			if "`.`name'.legend.keys[`i'].view.serset.isa'" != "" {
				_gm_edit .`name'.legend.keys[`i'].view.serset.ref_n + 99

				.`name'.legend.keys[`i'].view.serset.ref = 		   ///
					.`name'.graphs[`lfrom'].legend.keys[`i'].view.serset.ref

				_gm_log  .`name'.legend.keys[`i'].view.serset.ref = 	   ///
					.graphs[`lfrom'].legend.keys[`i'].view.serset.ref
			}
			if "`.`name'.legend.plotregion1.key[`i'].view.serset.isa'" != "" {
				_gm_edit						   ///
					.`name'.legend.plotregion1.key[`i'].view.serset.ref_n + 99

				.`name'.legend.plotregion1.key[`i'].view.serset.ref =  ///
					.`name'.graphs[`lfrom'].legend.keys[`i'].view.serset.ref

				_gm_log							   ///
					.`name'.legend.plotregion1.key[`i'].view.serset.ref =  ///
					.graphs[`lfrom'].legend.keys[`i'].view.serset.ref
			}
			if "`dots'"=="dots"  make10dots , stage(${grc1leg2_stage})
		}
	}
	
			//  Fix the serset numbering when source is a -gr, by()- graph
	if "`legendtype'"=="atlegend"  {
		forvalues i = 1/`.`name'.legend.keys.arrnels' {
			if "`.`name'.legend.keys[`i'].view.serset.isa'" != "" {
				_gm_edit .`name'.legend.keys[`i'].view.serset.ref_n + 99
		
				.`name'.legend.keys[`i'].view.serset.ref = 		   ///
					.`name'.graphs[`lfrom'].plotregion1.atlegend.keys[`i'].view.serset.ref
	
				_gm_log  .`name'.legend.keys[`i'].view.serset.ref = 	   ///
					.graphs[`lfrom'].plotregion1.atlegend.keys[`i'].view.serset.ref
			}
			if "`.`name'.legend.plotregion1.key[`i'].view.serset.isa'" != "" {
				_gm_edit						   ///
					.`name'.legend.plotregion1.key[`i'].view.serset.ref_n + 99

				.`name'.legend.plotregion1.key[`i'].view.serset.ref =  ///
					.`name'.graphs[`lfrom'].plotregion1.atlegend.keys[`i'].view.serset.ref

				_gm_log							   ///
					.`name'.legend.plotregion1.key[`i'].view.serset.ref =  ///
					.graphs[`lfrom'].plotregion1.atlegend.keys[`i'].view.serset.ref
			}
			if "`dots'"=="dots"  make10dots , stage(${grc1leg2_stage})
		}
	}

			//  Fix the serset numbering when source is a -gr_combine- graph
	if "`legendtype'"=="combined"  {
		forvalues i = 1/`.`name'.legend.keys.arrnels' {
			if "`.`name'.legend.keys[`i'].view.serset.isa'" != "" {
				_gm_edit .`name'.legend.keys[`i'].view.serset.ref_n + 99
		
				.`name'.legend.keys[`i'].view.serset.ref = 		   ///
					.`name'.plotregion1.graph`lfrom'.plotregion1.graph1.legend.keys[`i'].view.serset.ref
	
				_gm_log  .`name'.legend.keys[`i'].view.serset.ref = 	   ///
					.plotregion1.graph`lfrom'.plotregion1.graph1.legend.keys[`i'].view.serset.ref
			}
			if "`.`name'.legend.plotregion1.key[`i'].view.serset.isa'" != "" {
				_gm_edit						   ///
					.`name'.legend.plotregion1.key[`i'].view.serset.ref_n + 99

				.`name'.legend.plotregion1.key[`i'].view.serset.ref =  ///
					.`name'.plotregion1.graph`lfrom'.plotregion1.graph1.legend.keys[`i'].view.serset.ref

				_gm_log							   ///
					.`name'.legend.plotregion1.key[`i'].view.serset.ref =  ///
					.plotregion1.graph`lfrom'.plotregion1.graph1.legend.keys[`i'].view.serset.ref
			}
			if "`dots'"=="dots"  make10dots , stage(${grc1leg2_stage})
		}
	}

	if "`dots'"=="dots" {
		make10dots , stage(${grc1leg2_stage})
		di _n
	}
	
	//  Optionally suppress display of the combined legend
	if  "`loff'"~=""  {
		.`name'.legend.draw_view.setstyle, style(no)
		_gm_log .`name'.legend.draw_view.setstyle, style(no)
		
	}
	
	//  Modify the legend contents
	else {
	
		.`name'.legend.xoffset = `lxoffset'
		_gm_log .`name'.legend.xoffset = `lxoffset'

		.`name'.legend.yoffset = `lyoffset'
		_gm_log .`name'.legend.yoffset = `lyoffset'

		if "`ltitle'"~="" {
			.`name'.legend.title.text = {}
			.`name'.legend.title.text.Arrpush `ltitle'
			_gm_log .`name'.legend.title.text.Arrpush `ltitle'		
		}
		if "`lsubtitle'"~="" {
			.`name'.legend.subtitle.text = {}
			.`name'.legend.subtitle.text.Arrpush `lsubtitle'
			_gm_log .`name'.legend.subtitle.text.Arrpush `lsubtitle'
		}

		if "`lholes'"~="" {
			GetHoles , lholes(`lholes')
				local lholes `r(holes)'
			.`name'.legend.Edit , cmd(.holes = "`lholes'") keepstyles 
			_gm_log .`name'.legend.Edit , cmd(.holes = "`lholes'") keepstyles 
		}

		//  Don't draw the panel from which legend is borrowed
		//  Try to fill in the hole left by the undrawn panel.  Rarely works.
		if "`debug'"~=""  di as txt _n "DEBUG: Number of panels (including holes) is: -"as res "`Npnls'" as txt "-" 
		if  "`hidelegendfrom'"~=""  {
			opts_exclusive "`loff' `hidelegendfrom'"
			
			.`name'.plotregion1.graph`lfrom'.draw_view.setstyle, style(no)
			.`name'.plotregion1.graph`lfrom'.fill_if_undrawn.setstyle, style(no)

			_gm_log .`name'.plotregion1.graph`lfrom'.draw_view.setstyle, style(no)
			_gm_log .`name'.plotregion1.graph`lfrom'.fill_if_undrawn.setstyle, style(no)
			
			local Npnls = `Npnls' - 1
		}

		******  Resize legend elements  ******
		
		if "`debug'"~=""  {
			di _n as txt "DEBUG: User supplied legend re-sizing options:"
			di as txt _col(10) "-legscale({help size}): -" as res "`legscale'" as txt "-" 
			di as txt _col(10) "-ltsize({help textsizestyle}): -" as res "`ltsize'" as txt "-" 
			di as txt _col(10) "-lsubtsize({help size}): -" as res "`lsubtsize'" as txt "-" 
			di as txt _col(10) "-lmsize({help markersizestyle}): -" as res "`lmsize'" as txt "-" 
			di as txt _col(10) "-symxsize({help size}): -" as res "`symxsize'" as txt "-" 
			di as txt _col(10) "-symysize({help size}): -" as res "`symysize'" as txt "-" 
			di as txt _col(10) "-labsize({help textsizestyle}): -" as res "`labsize'" as txt "-" 
			di as txt _col(10) "-autosize-: -" as res "`autosize'" as txt "-" 
		}

		//  Default -legscale- to shrink all legend elements not explicitly sized
		if "`legscale'"=="" & "`autosize'"~="noautosize" {
			//	Default defined to be a multiplicative expresson such as *.7

			tempname scale_opts
			//  No. of panels             1   2   3   4   5   6   7   8   9  10  11  12
			matrix define `scale_opts' = [1, .8, .7, .7, .6, .6, .6, .6, .6, .4, .4, .4]
			
			local legscale = "*" + strofreal(cond(`Npnls'>12,.3,`scale_opts'[1,`Npnls']))
			
			if "`debug'"~="" {
				di as txt _n "DEBUG: Since user did not suppress automatic resizing of legend elements "  ///
					_n "with option -noautosize- or specify the option -legscale-, -grc1leg2- notes"  ///
					_n "that the number of panels (including any holes and excluding any hidden panel)" ///
					_n as txt "is Npnls = " as res "`Npnls'" as txt " and shrinks all legend elements accordingly by setting" ///
					_n as txt "-legscale to: -" as res "`legscale'" as txt "-." 
			}
		}

		if "`debug'"~=""  {
			di _n as txt "DEBUG: Legend attributes from suroutine -chk_legend-:"
			di as txt "Number of keys is: -" as res "`Nkeys'" as txt "-"
			di as txt "Number of labels is: -" as res "`Nlabels'" as txt "-"
			di as txt "NkeysEqNlabels is: -" as res "`NkeysEqNlabels'" as txt "-"
			di as txt "SmlstEqLrgst is: -" as res "`SmlstEqLrgst'" as txt "-"
			di as txt "labelwidth is: -" as res "`labelwidth'" as txt "-"
			di as txt "legendclass is: -" as res "`legendclass'" as txt "-"
			di as txt "NminofKeysLabels is: -" as res "`NminofKeysLabels'" as txt "-"
			di as txt "legendtype is: -" as res "`legendtype'" as txt "-"			
		}
		
		//  Start legend resizing
		
		//    The size of each legend element (labsize, symxsize, symysize, ltsize, lsubtsize, lmsize):
		//      . starts as specified in the source legend from a component graph specified by -legendfrom()-
		//      . if the legend element's size is NOT user specified and ... 
		//          a. -autosize- is not equal to -noautosize- (the default), 
		//				the element's starting size is modified by -legscale-, or
		//          b. user has set -autosize- to -noautosize-, 
		//          	the element's starting size is NOT modified
		//          	(i.e. legscale is set to "*1")
		//      . if the element's size is user specified, this specification modifies the element's size,
		//          regardless of the settings of -legscale- or -autosize- 
		
		//	The following parts of the legend accept any of the size types 
		//	listed in -help size-. (i.e. #pt, #in, #cm, #rs, #, *#) 
		//	and apply to all keys and labels in the legend.
		foreach legpart in labsize symxsize symysize ltsize lsubtsize  {
			
			//  -legpart- is NOT user-specified
			if "``legpart''"=="" {
				if "`autosize'" ~= "noautosize" {
					local `legpart' `legscale'
				}
			}

			//  -labsize- is user-specified or defaults to -legscale-
			if "`legpart'"=="labsize" {
				if "``legpart''"~="" {
					.`name'.legend.Edit, style(labelstyle(size(`labsize')))
					_gm_log .`name'.legend.Edit, style(labelstyle(size(`labsize'))) 
				}
			}
				
			//  -sym`xy'size- is user-specified or defaults to -legscale-
			foreach xy in x y {
				if "`legpart'"=="sym`xy'size" {
					if "``legpart''"~="" {
						.`name'.legend.Edit, style(key_`xy'size(`sym`xy'size')) keepstyles
						_gm_log .`name'.legend.Edit, style(key_`xy'size(`sym`xy'size')) keepstyles
					}
				}
			}
				
			//  -ltsize- is user-specified or defaults to -legscale-
			if "`legpart'"=="ltsize" {
				if "``legpart''"~="" {
					.`name'.legend.title.style.editstyle size(`ltsize') editcopy
					_gm_log .`name'.legend.title.style.editstyle size(`ltsize') editcopy
				}
			}
			
			//  -lsubtsize- is user-specified or defaults to -legscale-
			if "`legpart'"=="lsubtsize" {
				if "``legpart''"~="" {
					.`name'.legend.subtitle.style.editstyle size(`lsubtsize') editcopy
					_gm_log .`name'.legend.subtitle.style.editstyle size(`lsubtsize') editcopy
				}
			}

		}
		
		//	Size specifications for the marker symbols that appear in legends accept any of the size types 
		//	listed in -help size-. (i.e. #pt, #in, #cm, #rs, #, *#), but also the size designations:
		//  vtiny, tiny, vsmall, small, medsmall, medium, medlarge, large, vlarge, huge, vhuge, ehuge.

		//  Allow resizing of markers in the legend only if they exist (_rc~=4018) & NminofKeysLabels > 0
		//	For example, neither -graph pie- nor -graph bar- has markers.

		if `NminofKeysLabels' <= 0  {
			di as txt `"Warning: Because the legend borrowed for the combined graph is not "complete","' /// 
				_n "automatic resizing of legend markers is disabled and {cmd:grc1leg2} ignores the option -lmsize-."  ///
				_n `"See the discussion of "known issues" in {cmd:grc1leg2}'s help file {help grc1leg2##known:here}."'
		}
		
		else {  //  `NminofKeysLabels' > 0

			//  Define mata string matrices:  origkeysizes newkeysizes
			if "`debug'"~=""  {
				tempname origkeysizes newkeysizes
				mata : `origkeysizes' = J(`Nkeys',3,"")
				mata : `newkeysizes' = J(`Nkeys',3,"")
			}

			if "`lmsize'"=="" {
				local lmsize `legscale'
			}
			
			forvalues i = 1/`Nkeys' {
				
				//	If there are markers, resize them using -lmsize- or -legscale-, using relative sizes
				cap `noi' classutil describe .`name'.legend.plotregion1.key[`i'].view.style.marker.size
					if _rc==0 {
						local origlmsize = `.`name'.legend.plotregion1.key[`i'].view.style.marker.size.val'
						orig2newsize , lmsize("`lmsize'") origsize(`origlmsize') `debug'
						local newlmsize  = r(newsize)
						.`name'.legend.plotregion1.key[`i'].view.style.editstyle marker(size(`newlmsize')) editcopy
						_gm_log .`name'.legend.plotregion1.key[`i'].view.style.editstyle marker(size(`newlmsize')) editcopy
					}
					else {
						if "`debug'"~=""  di as txt "DEBUG: _rc = " as res _rc
						local origlmsize = .
						local newlmsize  = .
					}

				//	If there are keys that have a -xsize, resize them using -lmsize- or -legscale-, using relative sizes
				cap `noi' classutil describe .`name'.legend.plotregion1.key[`i'].xsz
					if _rc==0 {
						local origxsize = `.`name'.legend.plotregion1.key[`i'].xsz.val'
						orig2newsize , lmsize("`lmsize'") origsize(`origxsize') `debug'
						local newxsize  = r(newsize)
						.`name'.legend.plotregion1.key[`i'].xsz.editstyle `newxsize' editcopy
						_gm_log .`name'.legend.plotregion1.key[`i'].xsz.editstyle `newxsize' editcopy
					}
					else {
						if "`debug'"~=""  di as txt "DEBUG: _rc = " as res _rc
						local origxsize = .
						local newxsize  = .
					}
				
				//	If there are keys that have a -ysize, resize them using -lmsize- or -legscale-, using relative sizes
				cap `noi' classutil describe .`name'.legend.plotregion1.key[`i'].ysz
					if _rc==0 {
						local origysize = `.`name'.legend.plotregion1.key[`i'].ysz.val'
						orig2newsize , lmsize("`lmsize'") origsize(`origysize') `debug'
						local newysize  = r(newsize)
						.`name'.legend.plotregion1.key[`i'].ysz.editstyle `newysize' editcopy
						_gm_log .`name'.legend.plotregion1.key[`i'].ysz.editstyle `newysize' editcopy
					}
					else {
						if "`debug'"~=""  di as txt "DEBUG: _rc = " as res _rc
						local origysize = .
						local newysize  = .
					}

					if "`debug'"~=""  {
						mata : `origkeysizes'[`i', . ] =  ("`origlmsize'", "`origxsize'" , "`origysize'")
						mata : `newkeysizes'[`i', . ]  =  ("`newlmsize'", "`newxsize'", "`newysize'")
					}
				
				local rnms `rnms' key_`i'

			}

			if "`debug'"~=""  {
				di _n as txt "DEBUG: Key sizes before and after re-sizing options are applied:"
				di _n as txt "Matrix: origkeysizes"  ///
					_n as txt "  Column names: " as res "origlmsize, origxsize , origysize"  ///
					_n as txt "  Row names: " as res "`rnms'"
				mata : `origkeysizes' 
				di _n as txt "Matrix: newkeysizes"  ///
					_n as txt "  Column names: " as res "newlmsize, newxsize , newysize"  ///
					_n as txt "  Row names: " as res "`rnms'"
				mata : `newkeysizes'
			}
		}
			
		if "`debug'"~=""  {
			mata : mata drop `origkeysizes' 
			mata : mata drop `newkeysizes' 
		}
			
			
	// End of legend resizing 
		
		//  Resize the overall titles only with options, not automatically
		if  "`xtob1title'"~="" & "`xtsize'"~=""  {
			.`name'.b1title.style.editstyle size(`xtsize') editcopy
			_gm_log .`name'.b1title.style.editstyle size(`xtsize') editcopy
		}
		if  ("`ytol1title'"~="" | "`l1tol1title'"~="") & "`ytsize'"~=""  {
			.`name'.l1title.style.editstyle size(`ytsize') editcopy
			_gm_log .`name'.l1title.style.editstyle size(`ytsize') editcopy
		}
		if  ("`y2tor1title'"~="" | "`r1tor1title'"~="") & "`y2tsize'"~=""  {
			.`name'.r1title.style.editstyle size(`y2tsize') editcopy
			_gm_log .`name'.r1title.style.editstyle size(`y2tsize') editcopy
		}
		if  "`maintotoptitle'"~="" & "`mtsize'"~=""  {
			.`name'.title.style.editstyle size(`mtsize') editcopy
			_gm_log .`name'.title.style.editstyle size(`mtsize') editcopy
		}
		if  "`subtosubtitle'"~="" & "`stsize'"~=""  {
			.`name'.subtitle.style.editstyle size(`stsize') editcopy
			_gm_log .`name'.subtitle.style.editstyle size(`stsize') editcopy
		}
		if  "`notetonote'"~="" & "`ntsize'"~=""  {
			.`name'.note.style.editstyle size(`ntsize') editcopy
			_gm_log .`name'.note.style.editstyle size(`ntsize') editcopy
		}

	**** Rearranging the rows and columns of the borrowed legends using options -lcols()- and -lrows()-
	**** Logging these changes with _gm_log is not sufficient to assure they are preserved in a saved .gph file
		if "`lcols'"~="" | "`lrows'"~=""  {
			chkrowcol , lrows("`lrows'") lcols("`lcols'")
			
			if "`lcols'"~="" {
				.`name'.legend.Edit, style(cols(`r(nrowsorcols)')) style(rows(0)) keepstyles 
				_gm_log .`name'.legend.Edit, style(cols(`r(nrowsorcols)')) style(rows(0)) keepstyles 
			}
			else {

				.`name'.legend.Edit, style(cols(0)) style(rows(`r(nrowsorcols)')) keepstyles 			
				_gm_log .`name'.legend.Edit, style(cols(0)) style(rows(`r(nrowsorcols)')) keepstyles 			
			}
			graph rename `name' `name', replace  // This line does not fix the saved file
			
			if "`isasis'"==""  {
				di _n as txt "Note: To preserve in a saved graphics file the legend's row and column rearrangements"  ///
					_n "made with the options -lrows()- and/or -lcols()-, specify the suboption -asis-"  ///
					_n "for the {help saving_option:saving()} option or for the {help gr save:graph save} command." _n
			}
		}
	}
	
	if "`graphon'"=="" {
	    set graphics `usergrset'
		global grc1leg2_set_graph
	}

	cap gr draw `name'					// redraw graph
	if _rc > 0 {
		// Trap if the -gr draw- fails, presumably because of a bad legend
		local exitrc = _rc
		badlegend , lfrom(`lfrom') legendfrom(`"`legendfrom'"') exitrc(`exitrc') `trap'
	}

	if `"`saving'"' != `""' {
		
		cap noi gr save `"`name'"' `saving' 
		if _rc>0 {
			di as err "-grc1leg2-'s {help saving_option:saving()} option has failed."
			if "`isasis'"==""  {
				di as err "Try specifying {help saving_option:saving(filename,asis)}" _n
			}
			exit _rc
		}
		gettoken fname : saving , parse(", ")
		if ("`lcols'"~="" | "`lrows'"~="") & "`isasis'"==""  {
			di as txt _n `"Warning: After using -grc1leg2- options -lrows()- or -lcols()-, a "live" .gph file"'  ///
				_n "saved to disk may not retain those edits.  To retain a changed row/column configuration,"  ///
				_n "try specifying {help saving_option:saving(filename,asis)}" _n 
		}
		di as txt `"To view and verify the .gph file saved to disk, click {stata graph use `"`fname'"':here}."'
	}


end  /* End of main -grc1leg2 program */

program GetHoles, rclass
	syntax , lholes(numlist ascending >0)
	
	return local holes `lholes'
end  /* End of subroutine -GetHoles-  */


program chkrowcol, rclass
	syntax , [lcols(string) lrows(string)]
	
	if "`lcols'"~="" & "`lrows'"~="" {
		di as err "Specify only one of the two options: lcols() lrows()"
		error 198
	}
	
	if "`lcols'"~="" {
		local rc cols
	}
	else {
		local rc rows
	}
	
	local rl`rc' = real("`l`rc''")
	cap confirm integer number `rl`rc''
	if _rc > 0  {
		di as err "The option -l`rc'()- requires an integer."
		exit 7
	}
	
	return scalar nrowsorcols = `rl`rc''
	
end  /* End of subroutine -chkrowcol-  */

prog define make10dots , rclass
	syntax [, stage(integer -1)]
	
	if `stage' < 0 {
	    di as err "Error in -grc1leg2-'s subroutine -make10dots-"
		exit 199
	}
	local start = `stage' + 1
	local end = `stage' + 10
	foreach dot of numlist `start'/`end' {
		nois _dots `dot' 0
	}
	global grc1leg2_stage `end'
end  /* End of subroutine -make10dots-  */

program badlegend
*	syntax:  badlegend , lfrom(`lfrom') legendfrom(`"`legendfrom'"') exitrc(`exitrc') `trap' 
	syntax , lfrom(string) legendfrom(string)  [notrap exitrc(integer 198)]

	di as err _n "The legend in graph #`lfrom', -`legendfrom'-, has too few keys"  ///
		_n "or some keys have been suppressed by, for example, the -legend(order())- suboption."
		
	if "`trap'" == "notrap" {
		di as err _n "Captured return code _rc = {stata search r(`exitrc'),local:r(`exitrc');}" _n
	}
	else {
		graph drop _all
		serset clear
		set graph on
		di as err _n "Possible remedies include specifying the option -noautosize-"  ///
			_n as err "or using a hidden auxiliary graph for the legend as demonstrated"  ///
			_n as err "in Examples 3.10 of the help file {help grc1leg2##ex_3_10:here}." _n
			
		di as err _n "-grc1leg2- has cleared the graph space by executing"  ///
			_n "-graph drop _all- and -serset clear- before exiting."  ///
			_n `"For further information, see the discussion "'  ///
			_n `"of "known issues" in {cmd:grc1leg2}'s help file {help grc1leg2##known:here}."' _n

		error `exitrc'
	}

end  /* End of subroutine -badlegend- */

prog define chk_legend, rclass
*	Check the legend selected for use in the combined graph

*	Calling command:
*		chk_legend `name' , lfrom(`lfrom') legendfrom(`"`legendfrom'"') `trap' `debug'
	
	syntax      name  , lfrom(integer) legendfrom(string) [trap debug]

	local name `namelist'
	local exitrc 0
	
	if "`debug'"~= "" {
		local noi noisily
		di _n as txt `"DEBUG: Description of the legend's "class" using -classutil describe-:"'
	}
	
	cap `noi'  classutil describe  .`name'.graphs[`lfrom'].legend
	if _rc == 0 {
		local legend legend
		local legendclass   .graphs[`lfrom'].legend
		local Nkeys       `.`name'`legendclass'.keys.arrnels'
		local Nlabels     `.`name'`legendclass'.labels.arrnels'
		local Nmap        `.`name'`legendclass'.map.arrnels'   
		local labelwidth  `.`name'`legendclass'.labelwidth.val'
		local  NminofKeysLabels = min(`Nkeys',`Nlabels')
		local  NmaxofKeysLabels = max(`Nkeys',`Nlabels')
	}
	cap `noi'  classutil describe  .`name'.graphs[`lfrom'].plotregion1.atlegend
	if _rc == 0 {
		local atlegend atlegend
		local legend 
		local legendclass   .graphs[`lfrom'].plotregion1.atlegend
		local Nkeys       `.`name'`legendclass'.keys.arrnels'
		local Nlabels     `.`name'`legendclass'.labels.arrnels'
		local Nmap        `.`name'`legendclass'.map.arrnels'   
		local labelwidth  `.`name'`legendclass'.labelwidth.val'
		local  NminofKeysLabels = min(`Nkeys',`Nlabels')
		local  NmaxofKeysLabels = max(`Nkeys',`Nlabels')
	}
	cap `noi'  classutil describe  .`name'.plotregion1.graph`lfrom'.plotregion1.graph1.legend
	if _rc == 0 {
		local combined combined
		local legend 
		local atlegend 
		local legendclass   .plotregion1.graph`lfrom'.plotregion1.graph1.legend
		local Nkeys       `.`name'`legendclass'.keys.arrnels'
		local Nlabels     `.`name'`legendclass'.labels.arrnels'
		local Nmap        `.`name'`legendclass'.map.arrnels'   
		local labelwidth  `.`name'`legendclass'.labelwidth.val'
		local  NminofKeysLabels = min(`Nkeys',`Nlabels')
		local  NmaxofKeysLabels = max(`Nkeys',`Nlabels')
	}
	if ("`legend'"=="legend") + ("`atlegend'"=="atlegend") + ("`combined'"=="combined") > 1 {
		//  Type -combined- overrides type -atlegend-, which overrides type -legend-
		di as err "Legend types are " as res "`legend'" "`atlegend'" "`combined'"
		di as err "The legend is of more than one type.  This error should never occur."
		local exitrc 198
	}

	if ("`legend'"=="legend") + ("`atlegend'"=="atlegend") + ("`combined'"=="combined") ==0  {
		badlegend , lfrom(`lfrom') legendfrom(`"`legendfrom'"') `trap'   // Double quotes added ver. 2.12
	}
	
	//  Hypothesis: 
	//    Resizing legend elements fails when the values of -styledex- vary over the keys
	//    If so, detecting variation among the -styledex- values could be a way to trap 
	//    prior use of a legend(order())- option which prevents -gr draw- 
	//    from constructing the combined graph and thus crashes Stata and scrambles the sersets.
	tempname keystyledex
	local smallest  .
	local largest -9999999999
	foreach key of numlist 1/`Nkeys' {
		cap classutil describe .`name'`legendclass'.keys[`key'].pos.styledex
		if _rc==0 {
			local this_styledex `.`name'`legendclass'.keys[`key'].pos.styledex'
			mat define `keystyledex' = (nullmat(`keystyledex') , `this_styledex')
			local smallest = min(`this_styledex', `smallest')
			local largest  = max(`this_styledex', `largest')
			local cnms `cnms' key`key'
			mat colnames `keystyledex' = `cnms'
		}
	}
	
	return matrix keystyledex = `keystyledex'
	return scalar smallest = `smallest'
	return scalar largest = `largest'
	return local  SmlstEqLrgst = `smallest' == `largest'
		
	return local  legendtype `legend'`atlegend'`combined'
	return local  legendclass  `legendclass'
	return local  NkeysEqNlabels = (`Nkeys' == `Nlabels')
	return local  NminofKeysLabels  `NminofKeysLabels'
	return local  NmaxofKeysLabels  `NmaxofKeysLabels'
	foreach lcl in Nkeys Nlabels Nmap {
		confirm integer number ``lcl''
		return scalar `lcl' = ``lcl''
	}
	confirm number `labelwidth'
	return scalar labelwidth = `labelwidth'	
	
	if `exitrc'> 0 & "`trap'"=="" {
		error `exitrc'
	}

end	  /* End of subroutine chk_legend */

prog define orig2newsize, rclass
*	Subroutine to allow option -lmsize- to accept all size specifications in -help size-
*	Added in version 2.25, 10Oct2023

	//	Size specifications for the marker symbols that appear in legends accept any of the size types 
	//	listed in -help size-. (i.e. #pt, #in, #cm, #rs, #, *#), but also the size designations:
	//  vtiny, tiny, vsmall, small, medsmall, medium, medlarge, large, vlarge, huge, vhuge, ehuge.

	syntax , [lmsize(string) origsize(real -99) debug]
	
	local lmsize = subinstr("`lmsize'"," ","",.)
	
	if "`lmsize'"=="" {
		local lmsize *1
	}
	
	cap confirm number `lmsize'
	
	//  If user specifies a positive numeric value, it is returned
	if _rc==0 {
		local newsize = real("`lmsize'")
		if `newsize' > 0 {
			return scalar newsize = real("`lmsize'")
		}
		else {
			di as err "User has specified option -lmsize- as: -`lmsize'-" 
			exit 198
		}
		exit		
	}
	
	//  Assure that -lmsize- contains one and only one of the defined 
	//  alphanumeric character strings listed in -help size-
	local Nalpha = 0
	foreach alpha in   ///
		*   ///
		pt in cm rs  ///
		vtiny tiny vsmall small medsmall medium medlarge large vlarge huge vhuge ehuge  {
			
		if "`debug'" ~= "" di as txt "-alpha- is: " as res "`alpha'" 
	
		if strpos("`lmsize'","`alpha'") > 0  {
		
			//  Get the rest of -lmsize- after extracting -alpha-
			local notalpha = subinstr("`lmsize'","`alpha'","",.)

			cap confirm number `notalpha'

			//  sizetype is one of pt, in, cm, rs or *
			if _rc== 0 {
				local sizetype "`alpha'"
				local numpart `notalpha'
				local Nalpha = `Nalpha' + 1
			}
			//  sizetype is one of vtiny, tiny,  ... , ehuge
			else if "`notalpha'"==""  {
			
				if inlist("`alpha'", "pt", "in", "cm", "rs", "*") {
					di as err "An -lmsize- specification containing any of pt, in, cm, rs or * must include a number"
					di as err "Here, user has specified option -lmsize- as: -`lmsize'-" 
					exit 198 
				}
			
				local sizetype "`alpha'"
				local numpart
				local Nalpha = `Nalpha' + 1
			}
			
		}
		
	}
	
	if `Nalpha'~=1 {
		di as err "Invalid -lmsize- option: -`lmsize'-" 
		exit 198
	}
	
	
	if "`sizetype'"=="*" {
		cap assert "`lmsize'"== "`sizetype'`numpart'"
	}
	else {
		cap assert "`lmsize'"== "`numpart'`sizetype'"		
	}
	if _rc > 0 {
		di as err "Invalid -lmsize- option: -`lmsize'-" 
		exit 198		
	}
	
	if "`sizetype'"=="*" {
		return local newsize = `origsize' * real("`numpart'")
	}
	else {
		return local newsize = "`lmsize'"
	}
	return local  lmsize  `lmsize'
	return local  sizetype `sizetype'
	return scalar numpart  = real("`numpart'")
	return scalar origsize = real("`origsize'")
	
end  /*   End of program -orig2newsize-   */


exit

* Version 1.0.5 (21feb2015): renamed for packaging with AIDSCost (no other changes)
* Version 1.1.0 (30mar2016): add the -xtob1title- and -xtitlefrom()- options
* Version 1.1.1 (1apr2016): make -xtitlefrom()- imply the -xtob1title- option
* Version 1.1.2 (8apr2016): Attempt to add a size option to the legend
*	The value of the option -lsize- appears in the graph editor under
*	legend/properties/labels/size, but has no effect on the actual size of the labels
* Version 1.1.3 (11apr2016): Incorporates Derek Wagner's suggestion for how 
*	to add a size option to the legend (Subsequently deprecated)
* Version 1.1.4 (12apr2016): Fixes bug in xtitlefrom() option
* Version 1.2  (11nov2019): Implement options -ytol1title- and -maintotoptitle-
* Version 1.3  (23Jan2021): Fix the option -lsize- and rename it -LABSize-
*	Add the legend options: SYMXsize, SYMYsize, LCOLS, LROWS, LHoles, 
*	LTItle, LTSize, LSUBTItle, LSUBTSize, LXOffset, LYOffset, LOFF,
* Version 1.31 (29Jan2021): Add options XTSize, YTSize, MTSize
* Version 1.32 (29Jan2021): Add error trap when -legendfrom- graph has no legend (lines 61, 72)
* Version 1.33 (12Mar2021): Add options -y2tor1title- & -y2titlefrom(name)- and -y2tsize- to move and re-size titles on the right y2-axis
* Version 1.40 (24Mar2021): Enable legend relocation from a -gr combine- or a -gr,by()- graph
* Version 1.41 (26Mar2021): Fix bug in options -lcols()- and -lrows()-
* Version 1.42 (29Mar2021): Add the -hidelegendfrom- option
* Version 1.50 (4Apr2021):  Again attempt to fix the bug in options -lcols()- and -lrows()-
* Version 1.60 (15Jun2021):  Accommodate the graph display options in response to the issue raised here:
*	https://www.statalist.org/forums/forum/general-stata-discussion/general/1610172-grc1leg-ignoring-aspect-ratio

* Version 2.0  (3Mar2022): Revamp to allow scaling of all legend elements with new -legscale()-
*	Set default value of -legscale()- as a function of the number of panels in the combined graph (lines 576-594)
*	Allow user to override the default scaling of all legend elements.
*	Fix bug in -saving()- option in ver. 1.60 where saved .gph file did not incorporate -grc1leg2- edits.
*	When executing option -saving()-, display a clickable message enabling user to view and validate the saved .gph file
*	Add debug options -debug- and -notrap- (undocumented) and -noautosize-. Change dots option to optionally on.

* Version 2.01  (4Mar2022): Made program recursive by adding the modifer "else" at the beginning of lines 274, 285.
* Version 2.02  (4Mar2022): Improve error trapping with -badlegend- subroutine. Subroutine -badlegend- sets graph on before exiting. 
* Version 2.10 (12Mar2022): Add several error traps to attempt to detect and then to avoid or politely exit on encountering a "bad" legend.
*	Use subroutine -chk_legend- to check existence of a legend and pull several legend attributes from -classutil describe-.
* Version 2.11 (13Mar2022): Incomplete legend only disables marker resizing (line 557), not resizing of labels or symbols.
*	Alter warning and error messages accordingly.  Add link to "known issues" when badlegend exits with error.
* Version 2.12 (22Apr2022): Add double compound quotes around `"`legendfrom'"' wherever it appears.
*	Replace = word("`anything'",1) with -gettoken- in line 96
* Version 2.13 (30May2022): Remove error-generating -holes()- option on -gr display- command in line 139.
* Version 2.20 (15Jun2022): Fix option -mtsize()-. For backward compatibility, keep option -maintitlefrom()-.
*	Add options -mainfrom()-, -subtosubtitle-, -subfrom()-, -stsize()- & -notetonote-, -notefrom()-, -ntsize()-
* Version 2.21 (15Nov2022): Remove the trap requiring -legscale()- options to be absolute numbers 
*	or multiplicative expressions.  (Commented out lines 561-583)  Allows Stata's -graph- engine 
*	to trap errors in the -size()- option and allows the full range of sizing options specified in -help size-.
* Version 2.22 (1Dec2022): Fabio Tufano found that -grc1leg2- would crash when attempting to rescale marker symbols, 
*	if the component graphs have no markers. For example -graph pie- and -graph bar- have no markers.
*	So in line 623 , we only trap an error if _rc ~= 4018.  See help r(4018).
*	Also add column names to the debug matrix -keystyledex- in line 868.
* Version 2.23 (10Dec2022): Add code so that option -hidelegendfrom- is more likely to suppress the display of a blank panel
*	in place of the plot named by -legendfrom()-.  Rarely helps.
*	Unnecessary when the hidden panel is that for the last named graph, and user configures the panel layout with -rows()- &/or -cols()-.
*	When all panels are in one row (i.e. -rows(1)-) or the panel to be hidden is one of only three graphs,
*	the -hidelegendfrom- option successfully suppresses the display of the blank panel regardless of its sequence in the list of graphs. 
*	Best if the user always places the name of the hidden panel last in the list of graphs to be combined.
* Version 2.24 (14May2023): NminofKeysLabels substituted for Nkeys in line 675.  
*	Add _gm_edit ... fill_if_undrawn.set_false in line 254
* Version 2.25 (10Oct2023):  Revise legend re-sizing.  Add the subroutine -orig2newsize-. 
*	-grc1leg2- no longer fails when the -order()- suboption has been used 
*	in the -legendfrom()- graph to suppress one or more legend keys.
* Version 2.26 (4Nov2023):  Identical to -grc1leg2.ado- ver. 2.25.  
*	Only help and associated files have changed.
*	6Jun2024: To conform to conventions in -require-, change the "*!" in 2nd line to "*".
*	

*	TODO: 
*	  6Jun2024: Option -legscale()- is documented in -gc1leg2.sthlp-
*		but not accepted by the -syntax- statement in this ado file.
*	  Previous: Explore whether titles acceptable by -gr combine- 
*		(e.g. main, sub and note) can be combined with, replaced or appended to
*		titles transferred from a component graph???  
*		Can we exploit the -merged explicit- functionality?  See "help repeated options",
*		especially note the section on -prefix- and -suffix- options.
*		An undocumented feature that DOES work is adding suboptions 
*		to imported titles.  For example, after importing a main title 
*		to the combined graph with -maintotoptitle-, the -grc1leg2 option 
*		title(,span) works on the imported title. 


*	Options outside the legend are of two kinds (all seem to work in mem and gph graphs):
*		1.	Options passed to -gr combine- 
*				xcommon, ycommon, holes()
*				xsize(), ysize(), scheme()

*		2.  Options implemented in -grc1leg2- (but are outside the legend):
*				ring(), pos()  
*				legendfrom(), hidelegendfrom
*				lxoffset, lyoffset, xtob1title, ytol1title, y2tor1title

*	Options inside the legend are of two kinds:
*		3.	Options that work in both memory and saved files:
*				labsize() for all labels
*				lmsize() for all markers
*				symxsize() and symysize() for all symbols
*				ltsize(), lsubtsize()
*				legscale() to set default for all of above
*				lholes()                   
*
*		4.	Options that work for memory files BUT ARE LOST IN SAVED FILES
*				lrows(), lcols()
*			a.  Best work-around is to apply these options in component graph,
*				so -grc1leg2- need never rearrange the rows and columns of a legend
*			b.  Alternative is to save with the option -asis- which preserves the edits,
*				but prevents future editing.

*	Sources on Stata class programming for graphics:
*		help class
*		help classman
*		viewsource grc1leg.ado
*		viewsource legend_g.class
*		viewsource legendstyle.class
*		viewsource loggraph_g.class
*		viewsource graph_g.class
*		viewsource global_g.class
*		viewsource graphsize.class
*		viewsource gsize.class
*		viewsource numstyle.class
*		viewsource relsize.class 
*		viewsource sizetype.class 
*		viewsource symbolsize.class
*		viewsource codestyle.class
*		viewsource style.class
*		viewsource _gm_log.ado
*		viewsource _fr_legend_parse_and_log.ado 
*		viewsource gr_current.ado 
