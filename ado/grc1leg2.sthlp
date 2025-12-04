{smcl}
{* *! Help file version 2.26 (4Nov2023) written by Mead Over (mover@cgdev.org)}{...}
{* *! This help file is based on the help file for grc1leg by Vince Wiggins. See -Authors-.}{...}
{viewerdialog grc1leg2 "dialog grc1leg2"}{...}
{vieweralsosee "[G-2] graph combine" "mansection G-2 graphcombine"}{...}
{vieweralsosee "[G-3] graph ..., by()" "mansection G-3 by_option"}{...}
{vieweralsosee "[G-4] gph files and sersets" "mansection G-4 conceptgphfiles"}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "[G-2] gr combine" "help gr_combine##remarks4"}{...}
{vieweralsosee "[G-3] gr ..., by()" "help by_option"}{...}
{vieweralsosee "[G-4] gph files and sersets" "help gph_files"}{...}
{vieweralsosee "[SJ] grc1leg by Vince Wiggins" "net describe grc1leg,from(http://www.stata.com/users/vwiggins)"}{...}
{vieweralsosee "addplot by Ben Jann" "ssc describe addplot"}{...}
{viewerjumpto "Syntax" "grc1leg2##syntax"}{...}
{viewerjumpto "Description" "grc1leg2##description"}{...}
{viewerjumpto "Legend options" "grc1leg2##legopts"}{...}
{viewerjumpto "Title options" "grc1leg2##titleopts"}{...}
{viewerjumpto "Graph display and miscellaneous options" "grc1leg2##grdispopts"}{...}
{viewerjumpto "Graph combine options" "grc1leg2##grcombopts"}{...}
{viewerjumpto "Remarks" "grc1leg2##remarks"}{...}
{viewerjumpto "Examples" "grc1leg2##examples"}{...}
{viewerjumpto "  Examples using -gr ..., by()-" "grc1leg2##ex_gr_by"}{...}
{viewerjumpto "  Examples using -gr combine ...-" "grc1leg2##ex_grcomb"}{...}
{viewerjumpto "  Examples using -grc1leg2-" "grc1leg2##ex_grc1leg2"}{...}
{viewerjumpto "Known issues" "grc1leg2##known"}{...}
{viewerjumpto "Reference" "grc1leg2##references"}{...}
{viewerjumpto "Authors" "grc1leg2##author"}{...}
{hline}
help for {hi:grc1leg2}{right:{hi:version 2.26 4Nov2023}}
{hline}
{title:Title}

{p2colset 5 22 26 2}{...}
{p2col :{cmd:grc1leg2} {hline 2}}Combine multiple graphs with a single common legend{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}

{p 8 23}
{cmd:grc1leg2}
{it:name}
[{it:name} ...]
[{cmd:,}
{it:{help graph_combine:combine_options}}
{it:legend_options}
{it:title_options}
{it:{help graph_display:gr_display_options}}
]

{p 4 4 2}
where {it:name} is

	{it:name}{col 40}description
	{hline 65}
	{it:simplename}{...}
{col 40}{help name_option:name} of graph in memory{help grc1leg2##old:*}
	{it:name}{cmd:.gph}{...}
{col 40}name of graph stored on disk{help grc1leg2##old:*}
	{cmd:"}{it:name}{cmd:"}{...}
{col 40}name of graph stored on disk{help grc1leg2##old:*}
	{hline 65}
{p 8 8 2}
The "wildcard" characters "*" and "?" are not permitted in the list of graph names to be combined.
	
{p 4 4 2}
and where {it:combine_options} are:

{col 9}{it:combine_options}{col 40}description
{col 9}{hline 69}{p2colset 9 40 42 2}
{p2col:{cmdab:colf:irst}}display down columns{p_end}
{p2col:{cmdab:r:ows:(}{it:#}{cmd:)} | {cmdab:c:ols:(}{it:#}{cmd:)}}display in
        {it:#} rows or {it:#} columns{p_end}
{p2col:{cmdab:hol:es:(}{it:{help numlist}}{cmd:)}}positions to leave blank
       {p_end}
{p2col:{cmd:iscale(}[{cmd:*}]{it:#}{cmd:)}}size of text and markers{p_end}
{p2col:{cmd:altshrink}}alternate scaling of text, etc.{p_end}
{p2col:{cmd:imargin(}{it:{help marginstyle}}{cmd:)}}margins for individual
         graphs{p_end}

{p2col:{cmdab:ycom:mon}}give {it:y} axes common scales{p_end}
{p2col:{cmdab:xcom:mon}}give {it:x} axes common scales{p_end}

{p2col:{it:{help title_options}}}titles to appear on combined graph{p_end}
{p2col:{it:{help region_options}}}outlining, shading, aspect ratio{p_end}

{p2col:{cmdab:com:monscheme}}put graphs on common scheme{p_end}
{p2col:{helpb scheme_option:{ul:sch}eme({it:schemename})}}overall look{p_end}
{p2col:{help nodraw_option:{bf:nodraw}}}suppress display of combined graph{p_end}
{p2col:{help name_option:{bf:name(}{it:name}{bf:, ...)}}}specify name for
        combined graph{p_end}
{p2col:{help saving_option:{bf:saving(}{it:filename}{bf:, ...)}}}save combined
        graph in file. (If using option {opt lr:ows(integer)} or {opt lc:ols(integer)}, specify the sub-option "asis".) {p_end}
{col 9}{hline 69}

{p 4 4 2}
and the {it:legend_options} are

{col 9}{it:legend_options}{col 40}description
{col 9}{hline 69}{p2colset 9 40 42 2}
{p 15}{it}Specifying from which component graph to borrow the legend{sf}{p_end}
{p2col:{opt leg:endfrom(name)}}specifies the graph from which the legend
	for the combined graphs is to be taken.
	The argument {it:name} must match one of the names from the list of graph names specified.
	(Default is the first graph listed.){help grc1leg2##old:*}{p_end}
{p2col:{opt hide:legendfrom}}Hide the graph from which legend is taken{p_end}
{p 15}{it}Changing the position of the legend{sf}{p_end}
{p2col:{opth pos:ition(clockpos)}}Where legend appears in the combined graph{help grc1leg2##old:*}{p_end}
{p2col:{opth ring(ringpos)}}where legend appears (within or outside the plot region){help grc1leg2##old:*}{p_end}
{p2col:{opth lxo:ffset(real)}}Shift the legend's horizontal position to the right or left{p_end}
{p2col:{opth lyo:ffset(real)}}Shift the legend's vertical position up or down{p_end}
{p2col:{opt span}}"Center" the legend{help grc1leg2##old:*}{p_end}
{p2col:{opt loff}}Suppress the combined legend{p_end}
{p 15}{it}Arranging the elements of the legend{sf}{p_end}
{p2col:{opth lh:oles(numlist)}}specifies which positions in the legend array are to be left empty{p_end}
{p2col:{opt lc:ols(integer)}}Reset the number of columns in the legend{help grc1leg2##known:+}{p_end}
{p2col:{opt lr:ows(integer)}}Reset the number of rows in the legend{help grc1leg2##known:+}{p_end}
{p 15}{it}Specifying the legend's title and/or subititle{sf}{p_end}
{p2col:{opth lti:tle(string)}}Legend title{p_end}
{p2col:{opth lsubti:tle(string)}}Legend subtitle{p_end}
{p 15}{it}Changing the size of the legend and its elements{sf}{p_end}
{p2col:{opth legs:cale(size)}}Override {cmd:grc1leg2}'s default size reduction of all the following legend elements.{p_end}
{p2col:{opth lts:ize(textsizestyle)}}Size of the text in the legend title (overrides default resizing or user-specified {opt legs:cale()}){p_end}
{p2col:{opth lsubts:ize(size)}}Size of the text in the legend subtitle (overrides default resizing or user-specified {opt legs:cale()}){p_end}
{p2col:{opth lms:ize(markersizestyle)}}Size of the marker keys in the legend (overrides default resizing or user-specified {opt legs:cale()}){p_end}
{p2col:{opth symx:size(size)}}Set the width of the symbol keys in the legend (overrides default resizing or user-specified {opt legs:cale()}){p_end}
{p2col:{opth symy:size(size)}}Set the height of the symbol keys in the legend (overrides default resizing or user-specified {opt legs:cale()}){p_end}
{p2col:{opth labs:ize(textsizestyle)}}Size of the text of the key labels in the legend (overrides default resizing or user-specified {opt legs:cale()}){p_end}
{p2col:{opt noauto:size}}Disable {cmd:grc1leg2}'s default automatic resizing (typically shrinking) of legend elements.{p_end}
{col 9}{hline 69}
{p 8 8 2}
Stata's many other {help legend_options:legend options}
	are not available here in {cmd:grc1leg2}. 
	To use these other legend options,
	apply them to the legend to be used in the combined graph.
	See {view grc1leg2.sthlp##ex_3_10: Examples 3.10a, b an c, Example 3.12 and Example 3.13} 
	for guidance on using legend options not available in {cmd:grc1leg2}.{p_end}
	
{p 4 4 2}
and the {it:title_options} are:

{col 9}{it:title_options}{col 40}description
{col 9}{hline 69}
{col 30}{it:x-title}
{p2col:{cmdab:xtob:1title}}Suppress the {it:xtitle} on individual panels
	and use the {it:xtitle} from one of the panels 
	as the {it:b1title} on the combined graph.{p_end}
{p2col:{cmdab:xti:tlefrom:(}{it:name}{cmd:)}}Graph from which
	to take the {it:xtitle}. Default is from the first graph.{p_end}
{p2col:{opth xts:ize(textsizestyle)}}Modify the size of the borrowed 
	x-title as displayed on the combined graph.{p_end}
	
{col 22}{it:y-title on the left y-axis}
{p2col:{cmdab:ytol:1title} or {cmd:l1tol1title}}Suppress the {it:ytitle} or {it:l1title} on the left y-axis of the individual panels
	and use the {it:ytitle} or {it:l1title} from one of the panels 
	as the {it:l1title} on the left y-axis of the combined graph.{p_end}
{p2col:{cmd:{y|l1}titlefrom(}{it:name}{cmd:)}}Graph from which to take the {it:ytitle} or {it:l1title} from the left y-axis.
	Default is from the first graph.{p_end}
{p2col:{opth yts:ize(textsizestyle)}}Modify the size of the borrowed 
	y-title as displayed on the left y-axis of the combined graph.{p_end}
	
{col 22}{it:y-title on the right y-axis}
{p2col:{cmdab:y2tor1title} or {cmd:r1tor1title}}Suppress the {it:ytitle} or {it:r1title} on the right side of individual panels
	and use the {it:ytitle} or {it:r1title} from the right y-axis of one of the panels 
	as the {it:r1title} on the combined graph.{p_end}
{p2col:{cmdab:{y2|r1}titlefrom(}{it:name}{cmd:)}}Graph from which to take the {it:ytitle} or {it:r1title} from the right y-axis.
	Default is from the first graph.{p_end}
{p2col:{opth y2tsize(textsizestyle)}}Modify the size of the borrowed 
	y-title as displayed on the right y-axis of the combined graph.{p_end}
	
{col 30}{it:main title}
{p2col:{cmdab:mainto:toptitle}}Suppress the main {it:title} on individual panels
	and use the main {it:title} from one of the panels 
	as the overall {it:title} on the combined graph.{p_end}
{p2col:{cmdab:mainf:rom:(}{it:name}{cmd:)}}Graph from which to take 
	the main {it:title}. Default is from the first graph.
	For backward compatibility, {cmdab:mainti:tlefrom:(}{it:name}{cmd:)}
	can be used instead of option {cmdab:mainf:rom:(}{it:name}{cmd:)}.{p_end}
{p2col:{opth mts:ize(textsizestyle)}}Modify the size of the borrowed 
	main {it:title} as displayed on the combined graph.{p_end}
	
{col 30}{it:subtitle}
{p2col:{cmdab:subto:subtitle}}Suppress the {it:subtitle} on individual panels
	and use the {it:subtitle} from one of the panels 
	as the overall {it:subtitle} on the combined graph.{p_end}
{p2col:{cmdab:subf:rom:(}{it:name}{cmd:)}}Graph from which to take 
	the {it:subtitle}. Default is from the first graph.{p_end}
{p2col:{opth sts:ize(textsizestyle)}}Modify the size of the borrowed 
	{it:subtitle} as displayed on the combined graph.{p_end}
	
{col 30}{it:note}
{p2col:{cmdab:noteto:note}}Suppress the {it:note} on individual panels
	and use the {it:note} from one of the panels 
	as the overall {it:note} on the combined graph.{p_end}
{p2col:{cmdab:notef:rom:(}{it:name}{cmd:)}}Graph from which to take 
	the {it:note}. Default is from the first graph.{p_end}
{p2col:{opth nts:ize(textsizestyle)}}Modify the size of the borrowed 
	{it:note} as displayed on the combined graph.{p_end}
{col 9}{hline 69}

{p 4 4 2}
and the {it:{help graph_display:gr_display_options}} and miscellaneous options are:

{col 9}	{it:gr_display_options}{col 40}description
{col 9}{hline 69}
{p2col:{cmdab:ysiz:e:(}{it:#}{cmd:)}}change height of combined graph (in inches){p_end}
{p2col:{cmdab:xsiz:e:(}{it:#}{cmd:)}}change width of combined graph (in inches){p_end}
{p2col:{cmdab:margin:s:(}{it:{help marginstyle}}{cmd:)}}change outer margins
         of combined graph{p_end}
{p2col:{help scale_option:{bf:scale(}{it:#}{bf:)}}}resize text, markers, and
        line widths of combined graph{p_end}
{p2col:{helpb scheme_option:{ul:sch}eme({it:schemename})}}change overall look
         of combined graph and legend{p_end}
{p2col:{cmdab:com:monscheme}}put graphs on common scheme{p_end}
{p2col:{opt d:ots}}Display dots to show {cmd:grc1leg2}'s progress{p_end}
{p2col:{opt graph:on}}Leave the graphics setting as set by the user.{p_end}
{...}
{col 9}{hline 69}
{p 8 8 2}
Options {opt dots} and {opt graph:on} do not appear in the 
{stata db grc1leg2:dialog}.


{marker description}{...}
{title:Description}

{p 4 4 2}
{cmd:grc1leg2} is an enhanced version of Vince Wiggins' {help grc1leg2##author:grc1leg}.  
	Like {cmd:grc1leg}, {cmd:grc1leg2} is a wrapper for Stata's {help graph combine},
	invoking its ability to array separate, previously drawn graphs as panels 
	in a single combined graph. 
	In addition to allowing all the {help graph combine} options, {cmd:grc1leg2}:{p_end}
{phang}(a) suppresses the display of the legends on the component graphs;{help grc1leg2##old:*}{p_end}
{phang}(b) creates a legend for the combined graph by borrowing the legend 
	previously defined for one of the component graphs;{help grc1leg2##old:*}{p_end}
{phang}(c) optionally changes the position of the legend on the combined graph;{help grc1leg2##old:*}{p_end}
{phang}(d) optionally suppresses the x-titles or the y-titles of the component graphs, 
	borrowing them for the combined graph;{p_end}
{phang}(e) optionally suppresses the main titles, subtitles or notes of the component graphs, 
	borrowing one of them for the corresponding text element of the combined graph;{p_end}
{phang}(f) by default, shrinks all elements of the legend on the combined graph so that
	the sizes of markers and symbols in the legend approximately match their sizes
	in the component panels;{p_end}
{phang}(g) allows user to override the default size of some or all the elements 
	of the legend;{p_end}
{phang}(h) facilitates the calibration and coding of fine adjustments 
	using the dialog {stata db grc1leg2:grc1leg2};{p_end}
{phang}(i) optionally removes all legends from the combined graph (with the option {cmd:loff});{p_end}
{phang}(j) accepts component graphs constructed by -{help graph, by()}-, -{help graph combine}- or {cmd:grc1leg2};{p_end}
{phang}(k) optionally creates a legend for the combined graph that merges the legends of 
component panels ({help grc1leg2##ex_3_10:Example 3.10});{p_end}
{phang}(l) enables application of the {help graph_display:graph display} options to the created combined graph.{p_end}
{phang}(m) can create an Excel-style "bar-of-pie- graph (see Example 3.10c).{p_end}

{p 4 4 2}
By default, the characteristics of the common legend are inherited from the original 
	version of the legend in the first component graph, or in the graph specified by the option {opt leg:endfrom(name)}.
	With {cmd:grc1leg2}'s {help grc1leg2##legopts:legend options}, 
	the user can precisely control the placement of the common legend within the combined graph.
	Other {help grc1leg2##legopts:legend options} allow the user to override 
	some of the characteristics of the legend inherited from its original creation
	as part of one of the component graphs.  For example, the size of all markers, 
	symbols or labels in the legend can be increased or decreased.{p_end}

{p 4 4 2}
When an analyst assembles multiple component graphs into a single combined graph,
	the separate component graphs often share a common x-axis, y-axis or both.  
	In these situations, the appearance and logic of the graph is improved by 
	stripping the titles of the shared axis from all the component graphs, 
	replacing them with a single title for all the shared axes. {cmd:grc1leg2}'s
	{help grc1leg2##titleopts:title options} serve this purpose, allowing 
	the user to replace the separate {it:xtitle}s or {it:ytitle}s 
	on each of the individual graphs with a single {it:b1title}, {it:l1title} or {it:r1title} 
	on the combined graph, which is "borrowed" from one of the component graphs.
	See examples {help grc1leg2##ex_3_3:3.3} and {help grc1leg2##ex_3_9:3.9}.{p_end}

{p 4 4 2}
Since multi-panel graphs (i.e. combined graphs) are inherently more difficult
	for a reader to interpret than a single graph, it is wise to simplify multi-panel graphs 
	as much as possible. One way to simplify is to assure that the same legend 
	applies to all component graphs. When all panels use the same markers, symbols and 
	labels, the legend from any of the component graphs can serve as the legend 
	for all of them. {cmd:grc1leg2}, like its predecessor {cmd:grc1leg}, 
	simply suppresses the legends on all the component graphs and repurposes 
	or "borrows" one of those legends to serve as the legend for the combined graph.{p_end}

{p 4 4 2}
But there may be situations in which a user wishes 
	to assemble a combined or multi-panel graph from component graphs which 
	contain different variables, markers, or fill patterns.
	In this situation, none of the component graphs 
	has a legend that works for all panels of the combined graph. 
	Using the option {opt leg:endfrom(name)} together with the option {opt hide:legendfrom} 
	enables the user to create a single legend which includes legend 
	{help legend_options##description:"keys"} from all the combined graphs.  
	See example {help grc1leg2##ex_3_10:3.10}.{p_end}

{marker legopts}{...}
{title:Legend options}

{p 15}{it}Specifying from which component graph to borrow the legend{sf}{p_end}

{p 4 8 2}
{cmd:legendfrom(}{it:name}{cmd:)} specifies the graph from which the legend for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.  The single legend displayed by 
	{cmd:grc1leg2} is drawn from either the first graph name 
	appearing after the the {cmd:grc1leg2} command or from the graph named
	by the {cmd:legendfrom(}{it:name}{cmd:)} option. By default, all
	the characteristics of the legend displayed by {cmd:grc1leg2} are inherited 
	from the legend in that component graph.{help grc1leg2##old:*}{p_end}

{p 4 8 2}
{opt hide:legendfrom} hides the graph from which the legend is taken.  
	This option is only useful when none of the component graphs 
	contains all the {help legend_options##description:"keys"}
	for the combined graph.  
	The graph to be hidden should appear last in the list of named graphs.
	See example {help grc1leg2##ex_3_10:3.10}
	.{p_end}

{p 15}{it}Changing the position of the legend{sf}{p_end}

{p 4 8 2}
{opth pos:ition(clockpos)} and {opth ring(ringpos)}
    override the default location of the legend, which is usually centered
    below the plot region.  {cmd:position()} specifies a direction {it:(sic)}
    according to the hours on the dial of a 12-hour clock, and {cmd:ring()}
    specifies the distance from the plot region.{help grc1leg2##old:*}

{p 8 8 2}
    {opt ring(0)} specifies that the legend be placed inside the plot, 
	rather than on its periphery.  {opt ring(k)} for {it:k}>0,
    specifies positions outside the plot region; the larger the {cmd:ring()}
    value, the farther away from the plot region is the legend.  {cmd:ring()}
    values may be integers or nonintegers and are treated ordinally. The default is 1.
	See {help legend_options##remark3:Where legends appear} for more detail.{help grc1leg2##old:*}

{p 8 8 2}
    {opt position(12)} puts the legend directly above the plot region
    (assuming {cmd:ring()}>0), {cmd:position(3)} directly to the right
    of the plot region, and so on.{help grc1leg2##old:*}

{p 8 8 2}
	See the 
	{view grc1leg2.sthlp##examples:examples} 
	below for guidance on using the 
	{opth ring(ringpos)}
	and the
	{cmdab:pos:ition:(}{it:{help clockpos}}{cmd:)}
	options and see 
    {hi:Positioning of titles} in
    {it:{help title_options}} for more information on
    the {cmd:position()} and {cmd:ring()} suboptions.

{p 4 8 2}
{cmd:span} specifies that the legend is to be placed in an area spanning the
    entire width (or height) of the graph rather than an area spanning the
    plot region.
    This affects whether the legend is centered with respect to the plot
    region or the entire graph.
    See {hi:Spanning} in
    {it:{help title_options}} for more information on {cmd:span}.{help grc1leg2##old:*}
	
{p 4 8 2}
{cmd:loff} This option "hides" the combined legend. With this option, 
	{cmd:grc1leg2} has the effect of removing from the combined graph 
	all the legends on the component graphs.  
	Since this option only hides the combined legend, but does not delete it, 
	the user can reveal the legend by opening the graph in the 
	{help graph editor}, selecting the legend and clicking "Show".

{p 15}{it}Arranging the elements of the legend{sf}{p_end}

{p 4 8 2}
{opt lxo:ffset(real)} and {opt lyo:ffset(real)}	enable fine
	adjustments to the position of the legend on the combined graph.
	The user should first roughly determine the legend's position 
	with the options {opth ring(ringpos)} and {opth pos:ition(clockpos)}
	and then make finer adjustments as necessary with the options
	{opt lxo:ffset(real)} and {opt lyo:ffset(real)}.
	
{p 4 8 2}
{opth lh:oles(numlist)} inserts one or more gaps or "holes" in designated 
	locations in the legend as described {help by_option##byopts:here}.
	Potentially useful for separating legend keys into groups.{p_end}

{p 4 8 2}
{opt lc:ols(integer)} and {opt lr:ows(integer)}
    specify how the legend symbols and keys are arrayed.  These options invoke the 
    Stata legend options {cmd:cols()} and {cmd:rows()} 
	as described {help by_option##byopts:here}.  However, 
	unlike the changes made by other {cmd:grc1leg2} legend-modifying options,
	the changes to the legend layout effected by
	{opt lc:ols(integer)} and {opt lr:ows(integer)} are not retained
	in a {help gph_files##remarks4:"live"} .gph file. To save the rearranged
	legend layout to a {help gph_files:.gph} file, specify the sub-option {opt asis} 
	in {cmd:grc1leg2}'s {help saving_options:saving} option 
	or on the {help graph save:graph save} command.
	Alternatively, avoid using {opt lc:ols(integer)} and {opt lr:ows(integer)} 
	by rearranging the legend layout in the component graph 
	before executing {cmd:grc1leg2}.{p_end}

{p 15}{it}Specifying the legend's title and/or subititle{sf}{p_end}

{p 4 8 2}
{opt lti:tle(text)} and {opt lsubti:tle(text)}
	invoke the legend options {cmd:title(}{it:text}{cmd:)} and {cmd:subtitle(}{it:text}{cmd:)}
	documented in {help title_options:help title options}, but do not permit
	the suboptions documented there. To control the size of the text of the 
	legend's title and subtitle, see the options 
	{opth lts:ize:(textsizestyle)} and {opth lsubts:ize(textsizestyle)}.
	Specifying {it:zero} suppresses this title on the combined graph.{p_end}
 
{p 15}{it}Changing the size of the legend and its elements{sf}{p_end}

{p 4 8 2}
{opth legs:cale(size)} overrides {cmd:grc1leg2}'s default size reduction 
	of all legend elements. Just as Stata's {help gr combine:gr combine}
	reduces the the size of text and markers in the graphs it combines, 
	{cmd:grc1leg2} automatically reduces the size of all elements in the combined legend 
	as the number of component graphs increases.  For example, if there are 
	two component graphs, {cmd:grc1leg2} internally sets {opth legs:cale(size)} to "*0.8", 
	which reduces the size of all elements in the borrowed legend by 20%.
	If there are eight or nine component graphs, {cmd:grc1leg2} sets 
	{opth legs:cale(size)} to "*0.6". This default behavior has the effect
	of shrinking the size of markers in the legend to approximately
	match the size of those markers in the component graphs.{p_end}
	
{p 4 8 2}
By specifying {opth legs:cale(size)}, the user can shrink or expand all elements of the legend.
	For example, by specifying {opt legscale(*1.1)}, the user instructs {cmd:grc1leg2}
	to magnify the size of all legend elements by 10%.
	{opt legscale(*0.8)} shrinks all elements by 20%.
	{p_end}
	
{p 4 8 2}
To alter the size of one type of legend element, the user specifies 
	the size multiplier for that element type.  Options 
	{opth lts:ize(textsizestyle)} and {opth lsubts:ize(textsizestyle)}
	modify the size of the title or subtitle of the borrowed legend,
	whether they were in the original legend or added by {cmd:grc1leg2}'s 
	options {opt lti:tle(text)} and {opt lsubti:tle(text)}.
	Specifying these options overrides {opth legs:cale(size)} for that element type.{p_end}

{p 4 8 2}
Four options are available to alter the size of the legend's markers, 
	symbols and labels:	{opth lms:ize(markersizestyle)}, 
	{opth symx:size(size)}, {opth symy:size(size)}
	and {opth labs:ize(textsizestyle)}.
	Specifying these options overrides {opth legs:cale(size)} for that element type.{p_end}

{p 4 8 2}
Stata's {help twoway} graph commands use graph {help marker_options:"markers"}
	each of which is associated with a set of points in the graph.   
	The option {opth lms:ize(markersizestyle)} enables {cmd:grc1leg2} 
	to control the size of these markers within the legend 
	independently of their size(s) in the component graphs. However, some graphs, 
	notably those produced by {help graph pie} and {help graph bar}, 
	contain no markers. Their legends contain symbols, but no markers. 
	When combining these graphs, {cmd:grc1leg2} 
	permits the option {opth lms:ize(markersizestyle)}, but it has no effect.
	For legends with markers but no symbols, the options {opth symx:size(size)} and 
	{opth symy:size(size)} are permitted but have no effect.{p_end}
	
{p 4 8 2}
By default, {cmd:grc1leg2} reduces the size of all legend elements to correspond 
	to their displayed sizes in the combined graph.  
	Thus the legend and all its elements get smaller with additional component graphs.
	Option {opt noauto:size} disables {cmd:grc1leg2}'s default automatic downsizing
	of legend elements.{p_end}

 
{marker titleopts}{...}
{title:Title options}

{p 4 8 2}
{cmdab:xtob:1title}
	suppresses the {it:xtitle} on individual panels
	and uses the {it:xtitle} from one of the panels 
	as the {it:b1title} on the bottom margin of the combined graph. This option
	is consistent with this program's objective of simplifying
	and de-cluttering a multi-panel combined graph.
	However it should never be used if the panels have
	different x axes.  Thus, it is not a default option.

{p 4 8 2}
{cmdab:xti:tlefrom:(}{it:name}{cmd:)}
	specifies the graph from which the {it:xtitle} for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.
	
{p 4 8 2}
{opth xts:ize(textsizestyle)}
	specifies the size of the borrowed x-title as displayed on the combined graph.
	Specifying {it:zero} suppresses this title on the combined graph.{p_end}
	
{p 4 8 2}
{cmdab:ytol:1title}
	suppresses the {it:ytitle} on the left y-axis of individual panels
	and uses the {it:ytitle} from one of the panels 
	as the {it:l1title} on the left margin of the combined graph. This option
	is consistent with this program's objective of simplifying
	and de-cluttering a multi-panel combined graph.
	However it should only be used if the left y-axes of the separate graphs 
	being combined can be described by the same y-axis title.
	Thus, it is not a default option.

{p 4 8 2}
{cmdab:yti:tlefrom:(}{it:name}{cmd:)}
	specifies the graph from which the {it:ytitle} for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.
		
{p 4 8 2}
{cmdab:l1tol1title}
	suppresses the {it:l1title} on the left y-axis of individual panels
	and uses the {it:l1title} from one of the panels 
	as the {it:l1title} on the left margin of the combined graph.

{p 4 8 2}
{cmd:l1titlefrom(}{it:name}{cmd:)}
	specifies the graph from which the {it:l1title} for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.
		
{p 4 8 2}
{opth yts:ize(textsizestyle)}
	specifies the size of the borrowed y-title as displayed on the left y-axis of the combined graph.
	Specifying {it:zero} suppresses this title on the combined graph.{p_end}

{p 4 8 2}
{cmdab:y2tor1title}
	suppresses the {it:ytitle} on the right y-axis of individual panels
	and uses the {it:ytitle} from the right y-axis of one of the panels 
	as the {it:r1title} on the right margin of the combined graph.

{p 4 8 2}
{cmd:y2titlefrom(}{it:name}{cmd:)}
	specifies the graph from which the {it:ytitle} for the right y-axis of 
	the combined graph is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.

{p 4 8 2}
{cmdab:r1tor1title}
	suppresses the {it:r1title} on the right y-axis of individual panels
	and uses the {it:r1title} from the right y-axis of one of the panels 
	as the {it:r1title} on the right margin of the combined graph.

{p 4 8 2}
{cmd:r1titlefrom(}{it:name}{cmd:)}
	specifies the graph from which the {it:r1title} for the right y-axis of
	the combined graph is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.

{p 4 8 2}
{opth y2tsize(textsizestyle)}
	specifies the size of the borrowed y-title as displayed on the right y-axis of the combined graph.
	Specifying {it:zero} suppresses this title on the combined graph.{p_end}

{p 4 8 2}
{cmdab:mainto:toptitle}
	suppresses the main {it:title} on individual panels
	and uses the main {it:title} from one of the panels 
	as the overall {it:title} on the combined graph. 

{p 4 8 2}
{cmdab:mainti:tlefrom:(}{it:name}{cmd:)}
	specifies the graph from which the overall {it:title} for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.

{p 4 8 2}
{opth mts:ize(textsizestyle)}
	specifies the size of the borrowed main title as displayed on the combined graph.
	Specifying {it:zero} suppresses this title on the combined graph.{p_end}

{p 4 8 2}
{cmdab:subto:subtitle}
	suppresses the {it:subtitle} on individual panels
	and uses the {it:subtitle} from one of the panels 
	as the overall {it:subtitle} on the combined graph. 

{p 4 8 2}
{cmdab:subf:rom:(}{it:name}{cmd:)}
	specifies the graph from which the overall {it:subtitle} for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.

{p 4 8 2}
{opth sts:ize(textsizestyle)}
	specifies the size of the borrowed main title as displayed on the combined graph.
	Specifying {it:zero} suppresses this title on the combined graph.{p_end}

{p 4 8 2}
{cmdab:noteto:note}
	suppresses the {it:note} on individual panels
	and uses the {it:note} from one of the panels 
	as the overall {it:note} on the combined graph. 

{p 4 8 2}
{cmdab:notef:rom:(}{it:name}{cmd:)}
	specifies the graph from which the overall {it:note} for
	the combined graphs is to be taken. The default is the first graph in the
	list.  The argument {it:name} must match one of the names from the list of 
	graph names specified.

{p 4 8 2}
{opth nts:ize(textsizestyle)}
	specifies the size of the borrowed note as displayed on the combined graph.
	Specifying {it:zero} suppresses this note on the combined graph.{p_end}

{marker grdispopts}{...}
{title:Graph display and miscellaneous options}

{p 4 8 2}
{it:gr_display_options} enable the user to control the aspect ratio of 
	the combined graph (by setting {opt xsize(#)} and {opt ysize(#)}) or 
	to adjust its overall margins or scale. The option 
	{helpb scheme_option:{ul:sch}eme({it:schemename})} overrules 
	the graphics schemes originally used on the individual graphs 
	to impose a common scheme on all the component graphs and on the 
	combined graph. See help for {help graph_display:graph display} 
	for further details on these options.
	See {help grc1leg2##ex_3_11:Example 3.11}.{p_end}
	
{p 4 8 2}
In order to save time and avoid some unneeded manipulation of the graphics window, 
	{cmd:grc1leg2} sets graphics to {help set graphics:set graphics off} 
	while it is working and then returns to the user's graphics setting 
	in order to display the combined graph. This procedure has the 
	disadvantage of closing any open graphics windows.  To suppress this 
	behavior and allow -grc1leg2- to keep existing graphics windows 
	open while constructing the combined graph, specify the 
	{opt graph:on} option.  When the {opt graph:on} option is not specified 
	and a syntax error of some kind causes {cmd:grc1leg2} to crash,
	the user may have to issue a {cmd:set graphics on} command
	(or exit and restart Stata) in order to display graphs.{p_end}

{p 4 8 2}
Unless suppressed by the option {opt nodots}, {cmd:grc1leg2} displays a sequence 
	of dots which advances at each stage of processing.{p_end}

{marker grcombopts}{...}
{title:Graph combine options}

{p 4 8 2}
{it:combine_options} are passed through to the {help graph combine} command.
	They specify how the component graphs are arrayed, titling of the combined
    graphs, and other common graph options.  Among the {help graph combine} 
	options most useful here in {cmd:grc21leg2} are the workhorse options 
	{help name_option:name(name, ...)} and 
	{help saving_option:saving(filename,...)} and the 
	{opth holes(numlist)} option which can be used 
	to make space for placement of the combined legend.  See 
    {help graph_combine:graph combine} for details on all the {help graph combine} options 
	and the {help grc1leg2##examples:examples} in this help file for 
	their use with {help graph combine} and {cmd:grc1leg2}.

	
{marker remarks}{...}
{title:Remarks}

{p 4 4 2}
{cmd:grc1leg2} is like Stata's {cmd:graph combine} except it 
	reduces the clutter in the combined graph.  
	The legend displayed is one of the legends from the graphs being combined.  
	Optionally {cmd:grc1leg2} can also suppress the titles 
	on each of the individual graphs, borrowing one of them to apply to the combined graph.
	{cmd:grc1leg2} can also alter characteristics of the borrowed legend and titles 
	as they are displayed in the combined graph.  
	Otherwise, {cmd:grc1leg2} behaves like {help graph_combine}.

{p 4 4 2}
When constructing a multi-panel graph with at least three panels,
	it is rarely satisfying for each panel to have its own legend.
	In most cases, it would be analytically and aesthetically preferable 
	to have a single common legend, with a common set of legend keys, 
	for all panels in the graph.  

{p 4 4 2}
Stata's graph commands offer several ways to display a single
	common legend for all the panels in a multi-panel graph.
	However, by default, Stata puts the legend of a multi-panel 
	graph outside the multi-panel layout, squeezing all the panels 
	either vertically or horizontally.  This solution works logically,
	but typically makes it harder to read the separate panels. 

{p 4 4 2}
With appropriate options, you can enhance the legibility 
	of the multi-panel graph by leaving room for the legend 
	in the multi-panel layout and using the approriate 
	legend options to place the legend in that empty spot.
	In Stata there are several ways to accomplish this objective without 
	resorting to the {help graph editor}:{p_end}

{p 8 12 2}
	{ul:1.	One-step procedure, using {help by_option:graph ..., by()}}{p_end}
{p 12 16 2}
		a.	When feasible, construct the multi-panel graph using 
			a single graph command with the -by- option.  Stata
			will automatically construct a single legend with keys
			that are common across all panels of the graph.
			Nick Cox's tip #139 
			{browse "https://journals.sagepub.com/doi/pdf/10.1177/1536867X20976341":here}
			gives many examples showing the power 
			of using {help by_option:graph ..., by()}.{p_end}
{p 12 16 2}
		b.	Optimum legend placement is typically in a "hole" 
			at the bottom right of the multi-panel layout. 
			To achieve this effect, use the legend options:
			- {opt at(#)} {opt position(5)} -.
			The {opt at(#)} option is a powerful tool for placing a legend 
			within a "hole" in a multi-panel graph.  The absence of such an 
			option for the {help graph combine} command is a major 
			reason for the existence of this program, {cmd:grc1leg2}.{p_end}
{p 12 16 2}
		c.	Unlike the {help gr combine} and the 
			{cmd:grc1leg2} commands discussed below,
			the {help by_options:graph ..., by()} command accepts the full 
			range of {help legend_options:legend options}. See this
			{help by_options##use_of_legends:help file} for details.
			{p_end}
			
{p 8 12 2}
	{ul:2.	Two-step procedure, using {help gr combine:gr combine}} {p_end}
{p 12 16 2}
		a.	Construct and {help name_option:name} or {help graph rename:rename} the graphs for each of the separate panels{p_end}
{p 16 20 2}
				i.   Assure that the assignment of marker styles, line styles
				     and, especially, colors are consistent with the legend of the last of the named graphs.{p_end}
{p 16 20 2}
				ii.  Suppress the legend in all but the last of the separate graphs.{p_end}
{p 16 20 2}
				iii. For the last graph, specify legend options
					    -{opt ring(0)} {opt pos(5)} {opt xoffset(#)}-
				     with experimentation on the value of the offset.
					 This is also the place to specify any other
					 of the full range of Stata 
					 {help legend_options:legend options}.{p_end}
{p 12 16 2}
		b.	Combine the named graphs using {cmd:gr combine}.{p_end}
{p 20 20 2}
			Since {cmd:gr combine} does not have a {cmd:legend} option, 
			use the layout options -{opt cols(#)}- and/or -{opt rows(#)}- to assure 
			that the last specifed graph will be in the 
			bottom row of the panel layout.  This result is assured 
			if the number of component graphs is odd and no "holes"
			are specified prior to the lower right position.
			See this 		
{browse "https://www.statalist.org/forums/forum/general-stata-discussion/general/1443949-filling-hole-with-legend-when-combining-uneven-number-of-graphs":discussion on the Statalist Forum}.{p_end}

{p 8 12 2}
	{ul:3.	Two-step procedure using this program, {cmd:grc1leg2}}{p_end}
{p 12 16 2}
		a.	Construct and {help name_option:name} or {help graph rename:rename} the graphs for each of the separate panels{p_end}
{p 16 20 2}
				i.  Assure that the assignment of marker styles, line styles
				     and, especially, colors are consistent with the legend of one of the named graphs.{p_end}
{p 16 20 2}
				ii. No need to suppress the legends on any of the 
					component graphs{p_end}
{p 16 20 2}
				iii. For one of the component graphs, by default the first,
					specify any desired {help legend_options:legend options}
					that are not allowed by the {cmd:grc1leg2} command as 
					documented in this help file.{p_end}
{p 12 16 2}
		b.	Combine the named graphs using {cmd:grc1leg2}{p_end}
{p 16 20 2}
			i.	Unlike {cmd:gr combine}, {cmd:grc1leg2} has
				legend options to control legend placement.
				For example, to achieve a similar legend placement to
				method -2- above, one can specify 
				{opt ring(0)} and {opt pos(5)} as options on 
				the {cmd:grc1leg2} command instead of on 
				the command to construct one of the component graphs.{p_end}
{p 16 20 2}
			ii. Optionally use the {cmd:grc1leg2} options 
			{opt lxo:ffset(real)} and {opt lxyo:ffset(real)}
			to refine the legend placement.{p_end}
{p 16 20 2}
			iii. Optionally use the {cmd:grc1leg2} options {opth legs:ize(size)}
			to shrink or enlarge the entire legend.  The user may also choose
			to shrink or enlarge all the legend elements of a type
			by experimenting with the options {opth lms:ize(size)},
			{opth labs:ize(textsizestyle)}, {opth symy:size(size)} and {opth symx:size(size)}.
			These options override the key marker, label and symbol sizes
			specified in step 3.a.iii above.{p_end}
{p 16 20 2}
			iv. Optionally use the {opt xtob:1title(text)} option to suppress the 
			{it:xtitle} on each of the individual graphs and insert an overall
			{it:b1title} for the combined multi-panel graph.  Of course,
			this option is only approriate if all panels should have the same 
			{it:xtitle}{p_end}
{p 16 20 2}
			v. Optionally use the {opt ytol:1title(text)} option to suppress the 
			{it:ytitle} on each of the individual graphs and insert an overall
			{it:l1title} for the combined multi-panel graph.  
			Analagously with option {opt xtob:1title(text)},
			this option is only approriate if all panels should have the same 
			{it:ytitle}{p_end}
{p 16 20 2}
			vi. Optionally use the {opt y2tor1title(text)} option to suppress the 
			{it:ytitle} on the second y-axis (on the right margin) 
			of each of the individual graphs and insert an overall
			{it:r1title} for the combined multi-panel graph.  
			As for option {opt ytol:1title(text)},
			this option is only approriate if all panels should have the same 
			{it:ytitle} on the right y-axis{p_end}

{p 4 4 2}
	The above strategies for placing a legend in a "hole" in the multi-panel 
	combined graph apply when the multi-panel graph has at least 3 component graphs
	or panels.  
	The strategies for placing a common legend are more restricted when there are only
	two panels, because in that case 
	Stata's {help by_option:graph ..., by()} 
	and {help gr combine} commands do not respond to the 
	{cmd:holes(}{it:{help numlist}}) option.
	When there are only two panels, {cmd:grc1leg2} is useful for controlling the 
	placement of the legend on the combined graph and for reducing graph clutter 
	by suppressing the x- and y-titles on the component graphs, 
	moving them to the b1- and l1- titles of the combined graph.
	The do file entitled {cmd:grc1test.do}, available with this
	{stata `"view net describe grc1leg2, from("http://digital.cgdev.org/doc/stata/MO/Misc")"':package}
	as an "ancillary file, includes applications of {cmd:grc1leg2} when there are only two panels.{p_end}

{marker examples}{...}
{title:Examples}

{p 4 4 2}
The following examples demonstrate all three above approaches
to appending a single legend to a graph combining three or more panels.{p_end}

{p 4 4 2}
To skip the examples using {help by_option:gr ..., by()} or {help gr combine},
going directly to the examples using {cmd:grc1leg2}:{p_end}
{p 8 8 2}
o First set up the data by clicking
	{it:{stata "grc1leg2_examples setup":here}}{p_end}
{p 8 8 2}
o Then generate the four component graphs by clicking 
	{it:{stata "grc1leg2_examples make4panels":here}}.{p_end}
{p 8 8 2}
o Then scroll down to the {cmd:grc1leg2} Examples 3.0 {it}ff.{sf} {help grc1leg2##ex_grc1leg2:here}.{p_end}

{p 4 4 2}
To try all examples, start here:{p_end}

{p 8 8 2}sysuse auto2 {p_end}
{p 8 8 2}gen byte qual = 1*(rep78<3)+2*(rep78==3)+3*(rep78>=4){p_end}
{p 12 12 2}lab def qual 1 "Low Quality"  2  "Medium Quality"  3  "High Quality"{p_end}
{p 12 12 2}lab value qual qual{p_end}
{p 12 12 2}lab var qual "Quality - Mapping of rep78 into trichotomy"{p_end}
{p 8 8 2}tab rep78 qual, mi{p_end}
{p 8 8 2}*	(For simplicity, we assign missing values of rep78 to qual==3.){p_end}

		{it:({stata "grc1leg2_examples setup":click to set up the data, prior to running the examples below})}

{p 8 12 2}
{marker ex_gr_by}
	{bf}1.	Examples of the one-step procedure, using  {help by_option:graph ..., by()}{p_end}
	==============================================================={sf}

{p 4 4 2}
{it:Example 1.0:} Using a single graph command with the -by- option
	automatically assigns a single legend to a combined graph.  
	Here are examples with three panels and with five panels.
	Example 1.0 shows the result of {help by_option:graph ..., by()} 
	without options to control the placement of the legend.
	
	{title:Example 1.0: A three-panel graph using {help by_option:graph ..., by()} with default legend placement}	
	twoway  ///
		(scatter mpg weight)  ///
		(lfit mpg weight ),  ///
			by(qual,  ///
				title("Ex. 1.0: Three panels, with legend at 6 o'clock")  ///
				subtitle("Use -twoway ..., by()- without options") ///
			)  ///
		name(grby3dflt, replace)
	
		{it:({stata "grc1leg2_examples grby3dflt":click to run, after clicking above to set up the data})}

{p 4 4 2}
{it:Example 1.1:} For better control over legend placement, both the following examples use
	the options {cmd:at(#)} and {cmd:pos(#)} to insert the legend 
	into the multi-panel array. Note that two {cmd:legend}
	options are specified in each command. The legend option 
	within the {cmd:by()} definition controls the organization 
	and appearance of the legend; the one outside the {cmd:by()} definition
	determines the placement of the legend.  See 
	{help by_options##use_of_legends:help by_options} 
	for details. {p_end}

	{title:Example 1.1: A three-panel graph using {help by_option:graph ..., by()}}	
	twoway  ///
		(scatter mpg weight)  ///
		(lfit mpg weight ),  ///
			legend(col(1)) ///
			by(qual,  ///
				legend(pos(0) at(4))  ///
				title("Three panels, with legend in a hole")  ///
				subtitle("Use -twoway ..., by()- with -at(4) pos(5)-") ///
			)  ///
		name(grby3, replace)

		{it:({stata "grc1leg2_examples grby3":click to run Example 1.1})}

	{title:Example 1.2:  A five-panel graph using {help by_option:graph ..., by()}}	
	twoway  ///
		(scatter mpg weight)  ///
		(lfit mpg weight ),  ///
			legend(col(1)) ///
			by(rep78,  ///
				legend(pos(0) at(6))  ///
				title("Five panels, with legend in a hole")  ///
				subtitle("Use -twoway ..., by()- with -at(6) pos(0)-") ///
			)   ///
		name(grby5, replace)

		{it:({stata "grc1leg2_examples grby5":click to run Example 1.2})}

{p 8 12 2}
{marker ex_grcomb}
	{bf}2.	Examples of a two-step procedure, using {help gr combine}:{p_end}
	======================================================={sf}

{p 4 4 2}
The Stata command {help gr combine} does not have a {cmd:legend} option.
	In order for the combined graph to have only one legend,
	we must assure that only 1 of the component graphs has a legend.  
	Using {help gr combine}, the two steps to the combined graph with legend are:

{p 4 4 2}
	First step: Create the component graphs and name them panel1, 2, 3 and 0.  
	In order for {help gr combine} to display a single common legend, 
	the legends must be suppressed on all but one of the individual component 
	graphs at the time they are originally created. 
	The legend to be displayed can be on the last of the component graphs 
	and can be moved to a "hole" using the xoffset and yoffset options
	at the time this component graph is originally created.  
	Executing the following code creates the named memory graphs {it:panel1}, 
	{it:panel2}, {it:panel3} and {it:panel0} but displays no graphic output.{p_end}

	set graph off
	
	twoway  ///
		(scatter mpg weight if qual==1)  ///
		(lfit mpg weight if qual==1),  ///
			ytitle(Miles per gallon)  ///
			subtitle("Low Quality")  ///
			legend(col(1) off) ///  Hidden legend has one column
			name(panel1, replace)

	twoway  ///
		(scatter mpg weight if qual==2)  ///
		(lfit mpg weight if qual==2),  ///
			ytitle(Miles per gallon)  ///
			subtitle("Medium Quality")  ///
			legend(row(1) off) ///  Hidden legend has one row
			name(panel2, replace)

	twoway  ///
		(scatter mpg weight if qual==3)  ///
		(lfit mpg weight if qual==3),  ///
			ytitle(Miles per gallon)  ///
			subtitle("High Quality")  ///
			legend(col(1) ring(0) pos(3) xoffset(55) )  ///
			name(panel3, replace)

	twoway  ///
		(scatter mpg weight)  ///
		(lfit mpg weight),  ///
			ytitle(Miles per gallon)  ///
			subtitle("Entire sample")  ///
			legend(col(1) ring(0) pos(3) xoffset(40) )  ///
			name(panel0, replace)
	set graph on

	*	List the named graphs now in memory
	graph dir, memory 
{marker b4ex_2_1}
{p 10 10 10}
		{it:({stata "grc1leg2_examples make4panels":click to run the above code, before running Examples 2.x or Examples 3.x.}}{p_end}

{p 4 4 2}
{it:Example 2.1:} Second step: By turning off the legends on 
	all but the last of several component graphs 
	and specifying the legend on the last graph 
	with options -ring(0) pos(5) xoffset(#)-,
	{help gr combine} does a passable job of 
	creating a multi-panel graph with a single legend. 
	For this approach to work, properly calibrate the offset
	and arrange the placement of the panels to allow
	for a "hole" which can accommodate the legend.{p_end}
	{marker ex_2_1}
	{title:Example 2.1: Assemble three one-panel graphs using {help gr_combine}}
	gr combine panel1 panel2 panel3,  ///
		xcommon ycommon  ///
		title("Three panels, with legend in a hole")  ///
		subtitle("Use -gr combine ...  , having specified"  ///
				"-col(1) ring(0) pos(3) xoffset(55)- on -panel3-")  ///
		name(grcomb3, replace) 

		{it:({stata "grc1leg2_examples grcomb3":click to run, after creating panels 0, 1, 2 & 3 above})}
	
{p 4 4 2}
{it:Example 2.2:} A disadvantage of using {help gr combine} as in Example 2.1 
	compared with using {help by_option: graph twoway ..., by()} 
	or {cmd:grc1leg2} as demonstrated in examples 1.x and 3.x 
	respectively is that {help gr combine} uses superfluous ink 
	to repeat the axis titles for each panel.  
	(With extra work, the user could suppress the axis titles 
	on each of the component graphs before combining them using {help gr combine}.)
	
{p 4 4 2}
{help gr combine} can equally well display a single legend 
	when there are five panels. In Example 2.2 the legend from the 
	graph in the fifth panel is placed in the space that 
	would otherwise be occupied by a sixth panel.{p_end}

	{title:Example 2.2: Using {help gr_combine} with five panels}	
	gr combine panel1 panel2 panel1 panel2 panel3,  ///
		xcommon ycommon  ///
		title("Five panels, with legend in a hole")  ///
		subtitle("Use -gr combine ...  , having specified"  ///
			"-ring(0) pos(5) xoffset(40)- on the last panel")  ///
		name(grcomb5, replace) 

		{it:({stata "grc1leg2_examples grcomb5":click to run Example 2.2})}
		
{p 4 4 2}
{it:Example 2.3:} Example 2.3 demonstrates using {help gr combine} to construct 
	a graph with eight panels and the legend in the middle. 
	In the above two examples there was no need to specify the
	holes option, since the number of panels and the default panel layout assured there
	would be a "hole" in the lower right hand corner of the array.  
	In this example, because we want the hole in the middle, 
	after the fourth panel, we must specify 
	{opt holes(5)}.{p_end}
	
	{title:Example 2.3: Using {help gr_combine} with eight panels}	
	gr combine panel1 panel2 panel1 panel0 panel1 panel2 panel1 panel2,  ///
		xcommon ycommon holes(5)    ///
		title("Ex. 2.3: Eight panels, with legend in the middle")  ///
		subtitle("Use -gr combine ... , having specified"  ///
			"-ring(0) pos(3) xoffset(40)- on graph for the fourth panel")  ///
		name(grcomb8, replace) 

		{it:({stata "grc1leg2_examples grcomb8":click to run Example 2.3})}

		
{p 4 4 2}
	{marker ex_grc1leg2}
	{bf}3.	Examples of a two-step procedure using {cmd:grc1leg2}{p_end}
	==============================================={sf}

{p 4 4 2}
In addition to allowing all the {help graph combine} options, {cmd:grc1leg2}
	allows the user to modify the combined graph in all the ways (a) through (m) listed 
	{help grc1leg2##description:above}. 
	The following examples demonstrate many of these capabilities, 
	without attention to the aesthetic or analytical quality of the result.
	Hopefully users can use the tools described here to produce 
	visualizations which attain these qualities. 
	(For other examples, see the
	do file entitled {cmd:grc1test.do}, available with this
	{stata `"view net describe grc1leg2, from("http://digital.cgdev.org/doc/stata/MO/Misc")"':package}.)

{p 4 4 2}
{marker ex_3_0}
{it:Example 3.0:} If starting here (without running Examples {help grc1leg2##examples:1.1 and 1.2} 
	and {help grc1leg2##b4ex_2_1:2.1 - 2.3} above),
	read and prepare the data by clicking 
	{stata "grc1leg2_examples setup":here},
	and then create the component panel graphs ({it}panel0{sf}, {it}panel1{sf}, {it}panel2{sf} and {it}panel3{sf}) by clicking
	{stata "grc1leg2_examples make4panels":here}.{p_end}

	{title:Example 3.0: Using {cmd:grc1leg2} without options}	
	grc1leg2 panel0 panel1 panel2 panel3 ,  ///
		title("Ex. 3.0: Four panels")  ///
		subtitle("Use -grc1leg2- to borrow the legend from panel0")  ///
		name(grc14dflt, replace)

		{it:({stata "grc1leg2_examples grc4dflt":click to run Example 3.0})}
	
{p 4 4 2}
{marker ex_3_1}
{it:Example 3.1:} To display the single legend clearly,
	leaving a "hole" somewhere in the layout
	makes a place for the legend in that hole.
	Example 3.1 demonstrates leaving the hole in the lower 
	right corner of the layout (in clock position 5).{p_end}

	{title:Example 3.1: Using {cmd:grc1leg2} to display three panels with their legend in a "hole"}	
	grc1leg2 panel1 panel2 panel3,  ///
		ring(0) pos(5)  ///
		title("Ex. 3.1: Three panels, with legend in a hole")  ///
		subtitle("Use -grc1leg2- with options -ring(0) pos(5)- and" /// 
			"without the option -xtob1title-")  ///
		name(grc13woxtob1, replace)

		{it:({stata "grc1leg2_examples grc3woxtob1":click to run Example 3.1})}
	
{p 4 4 2}
In the above example, note that we can use the legend from 
	the named graph {it:panel1} despite the legend's display having been suppressed 
	at the time of {it:panel1}'s creation by the option {opt legend(off)}.
	
{p 4 4 2}
{it:Example 3.2:} In the combined graph named {it:grc3woxtob1} produced above by {cmd:grc1leg2}, 
	the options {opt ring(0)} and {opt pos(5)} provide insufficient
	control of the legend's placement, 
	pushing the legend too far to the southeast.
	By adding appropriately selected legend offsets, we can center 
	the legend in the middle of the "hole" in the southeast quadrant.{p_end}
	{marker ex_3_2}
	{title:Example 3.2: Using {cmd:grc1leg2} to center the combined legend in the "hole"}	
	grc1leg2 panel1 panel2 panel3,  ///
		ring(0) pos(5)  ///
		lxoffset(-20) lyoffset(17)  ///
		title("Ex. 3.2: Three panels, with legend in a hole")  ///
		subtitle("Use -grc1leg2- with options -ring(0) pos(5)- and" /// 
			"fine-tuned with legend offset options")  ///
		name(grc13_offset, replace)

		{it:({stata "grc1leg2_examples grc3_offset":click to run Example 3.2})}
		
{p 4 4 2}
{it:Example 3.3:} Use of {cmd:grc1leg2}'s {cmdab:xtob:1title} option in Example {help grc1leg2##ex_3_3:3.3} 
	saves additional space in the y-dimension
	and reduces the excess "ink" in the graph by suppressing {it:xtitle}s
	on the individual panels and instead using a single overall {it:b1title}
	for the entire combined graph. 
	The option {cmdab:ytol:1title} borrows the y-title from 
	one of the component graphs, saving space in the x-dimension.
	Optionally use {cmd:xcommon} and/or {cmd:ycommon} to facilitate 
	comparison of adjacent panels.
	(Example {help grc1leg2##ex_3_9:3.9} shows how to replace redundant 
	titles on the right-hand y-axis with a single {it:r1title} on the combined graph.){p_end}
	{marker ex_3_3}
	{title:Example 3.3: Using {cmd:grc1leg2} to suppress redundant axis titles}	
	grc1leg2 panel1 panel2 panel3,  ///
		xcommon ycommon ring(0) pos(5)  ///
		lxoffset(-10) lyoffset(17)  ///
		title("Ex. 3.3: Three panels, with legend in a hole")  ///
		subtitle("Use -grc1leg2- with options -ring(0) pos(5)- " /// 
			"with the offset options plus -xtob1title- and -ytol1title-")  ///
		xtob1title ytol1title  ///
		name(grc13, replace)

		{it:({stata "grc1leg2_examples grc3":click to run Example 3.3})}

{p 4 4 2}
{it:Example 3.3_bis:} Options are also available to suppress redundant main titles, subtitles or notes
	from component panel graphs, so that they instead appear as 
	the main {it:title}, {it:subtitle} or {it:note} of the combined graph.
	Example 3.3_bis shows how to move the main title and note 
	from the the graph -panel1_bis- to the combined graph.{p_end}

	{title:Example 3.3_bis: Moving the main title and a note to the combined graph}	
	twoway  ///
		(scatter mpg weight if qual==1, yaxis(1))  ///
		(lfit mpg weight if qual==1),  ///
			title("Ex. 3.3_bis: Move main title and note to combined graph")  ///
			subtitle("Low Quality")  ///
			note("Source: Gibbon, Edward (1890) The Decline and Fall of the Roman Empire."   ///
				"              London: F. Warne and Co.")  ///
			ytitle(Miles per gallon)  ///
			legend(col(1) off) ///
			name(panel1_bis, replace)

	grc1leg2 panel2 panel3 panel1_bis,  ///
		xcommon ycommon ring(0) pos(5)  ///
		lxoffset(-10) lyoffset(17) legendfrom(panel1_bis)  ///
		xtob1title ytol1title notetonote mainfrom(panel1_bis) notefrom(panel1_bis) ///
		name(grc13_bis, replace) 

		{it:({stata "grc1leg2_examples grc3_bis":click to run Example 3.3_bis})}
	
{p 4 4 2}
{it:Example 3.4a:} Use of the {opth legs:cale(size)}
	option allows the user to increase the size of all elements 
	inside the legend box. However, in this case doing so 
	enlarges the scatter plot markers in the legend 
	to the extent that they no longer resemble
	the markers in the component panels. To override the enlargement
	of the markers, we add the option {opt lms:ize(*0.8)}
	to shrink the legend markers back until their size matches 
	their appearance in component panels.{p_end}
	{marker ex_3_4}
	{title:Example 3.4a: Using {cmd:grc1leg2} option {opth legs:cale(size)} to alter the size of all legend elements}	
	grc1leg2 panel1 panel2 panel3,  ///
		xcommon ycommon ring(0) pos(5)  ///
		lxoffset(-6) lyoffset(14)  ///
		title("Ex. 3.4a: Three panels, with legend in a hole")  ///
		subtitle("Use -grc1leg2- with options -ring(0) pos(5)- " /// 
			"and with -legscale(*1.2) lmsize(*.8)-")  ///
		xtob1title ytol1title legscale(*1.2) lmsize(*.8)  ///
		name(grc13legscale, replace)

		{it:({stata "grc1leg2_examples grc3legscale":click to run Example 3.4a})}

{p 4 4 2}
{it:Example 3.4b:} An alternative approach to creating a similar legend is to 
 	use the {opth labs:ize(textsizestyle)} and {opth symx:size(size)} options
	to instead change the sizes of the labels and symbols inside the legend box, 
	overriding the default sizes.
	By experimenting with various combinations of the legend and legend element 
	sizing options, it should be possible to achieve the desired effect 
	without resorting to the {help graph editor}.{p_end}
	
	{title:Example 3.4b: Using {cmd:grc1leg2} to edit specific legend elements}	
	grc1leg2 panel1 panel2 panel3,  ///
		xcommon ycommon ring(0) pos(5)  ///
		lxoffset(-6) lyoffset(14)  ///
		title("Ex. 3.4b: Three panels, with legend in a hole")  ///
		subtitle("Use -grc1leg2- with options -ring(0) pos(5)- and with the" /// 
			"offset, borrowed title, -labsize(*1.2)- and -symxsize(*1.2)- options")  ///
		xtob1title ytol1title labsize(*1.2) symxsize(*1.2) ///
		name(grc13labsize, replace)

		{it:({stata "grc1leg2_examples grc3labsize":click to run Example 3.4b})}

{p 4 4 2}
{it:Example 3.5:} To position the legend in the middle of an eight-panel array,
	we use the options -ring(0) pos(0) holes(5)-{p_end}
	{marker ex_3_5}
	{title:Example 3.5: Using {cmd:grc1leg2} to put a legend in the middle of eight panels}	
	grc1leg2 panel1 panel2 panel3 panel2 panel3 panel2 panel3 panel2,  ///
		xcommon ycommon ring(0) pos(0) holes(5)  ///
		title("Eight panels: with legend in middle")  ///
		subtitle("Use -grc1leg2- with options -ring(0) pos(0) holes(5)- and"  ///
			"with the options -xtob1title- and -ytol1title-")  ///
		xtob1title ytol1title  ///
		name(grc18pnl, replace)
		
		{it:({stata "grc1leg2_examples grc8pnl":click to run Example 3.5})}

{p 4 4 2}
{it:Example 3.6:} When the graph array is rectangular, with four or six panels, 
	it may not be feasible or desirable to locate the common legend 
	inside the graph. In this case, when the legend must be located
	on one of the margins of the multi-panel graph, the most 
	obvious alternatives are to leave the legend in its default location at 
	{opt pos:ition(6)} or to move it to the right margin with {opt pos:ition(3)}. 
	However, {opt pos:ition(6)} conflicts with the option {opt xtob:1title}
	by putting the x-title below the legend.  
	The solution to using the option {opt xtob:1title} with the legend at the bottom 
	of the combined graph is to use the option {opt ring(2)} instead 
	of the default {opt ring(1)}.  Furthermore, by using the {opt legendfrom} option
	to select the legend from -panel2- that was created with one row and two columns,
	the legend lies flat at the bottom of the combined graph.{p_end}
	{marker ex_3_6}
	{title:Example 3.6: Using {cmd:grc1leg2} to put the legend below the {it:b1title}}	
	grc1leg2 panel0 panel1 panel2 panel3 ,  ///
		xcommon ycommon ring(2) legendfrom(panel2) ///
		title("Four panels: with legend at bottom")  ///
		subtitle("Use -grc1leg2- with options -ring(2) pos(6)- and"  ///
			"with the options -xtob1title- and -ytol1title-")  ///
		xtob1title ytol1title  ///
		name(grc14pnl, replace)
		
		{it:({stata "grc1leg2_examples grc4pnl":click to run Example 3.6})}

{p 4 4 2}
{it:Example 3.7:} Like its progenitor {help grc1leg}, {cmd:grc1leg2} is conceived as a utility 
	to add a legend to a composite graph made up of several panels.
	Because a multi-panel graph is already complex, a user would rarely 
	want to further complicate its interpretation by including a panel which 
	itself consists of multiple panels. However, if the user does need to embed a previously  
	created multi-panel graph as one of several panels in a new multi-panel graph, 
	{cmd:grc1leg2} can accommodate.{p_end}

{p 4 4 2}
Example 3.7 shows that {cmd:grc1leg2} can successfully find and relocate
	the legend on a multi-panel graph created by {help graph, by()}.
	Using the {help gr combine} option {opt altshrink} helps the original titles
	fit into the combined graph.{p_end}
	{marker ex_3_7}
	{title:Example 3.7: Borrowing the legend from a graph created by -graph ..., by()-}	
	grc1leg2 grby3 grcomb3,  ///
		title("Ex. 3.7: Relocating the legend from a by() graph")  ///
		altshrink  ///
		name(grc1fromby, replace)
		
		{it:({stata "grc1leg2_examples grcfromby":click to run Example 3.7})}

{p 4 4 2}
{it:Example 3.8:} Similarly Example 3.8 shows that {cmd:grc1leg2} can successfully find and relocate 
	the legend on a multi-panel graph created by {help gr_combine:graph combine}.{p_end}
	{marker ex_3_8}
	{title:Example 3.8: Borrowing the legend from a graph created by -graph combine-}	
	grc1leg2 grcomb3 grby3,  ///
		title("Ex. 3.8: Relocating the legend from a combined graph")  ///
		altshrink  ///
		name(grc1fromcomb, replace)
		
		{it:({stata "grc1leg2_examples grcfromcomb":click to run Example 3.8})}

{p 4 4 2}
To a limited degree {cmd:grc1leg2}, like {cmd: gr combine}, is recursive, accomodating graphs as input
	that it has itself previously constructed.  One could, for example, 
	combine the results of Examples 3.4b and 3.6 with a single legend borrowed from 3.4b like this:{p_end}

	{stata "grc1leg2 grc14pnl grc13labsize , altshrink ring(0) pos(5) lyoffset(20)"}
	
{p 4 4 2}
Or even like this:{p_end}
	
	{stata "grc1leg2 grc1fromby grc1fromcomb, altshrink"}

{p 4 4 2}
{it:Example 3.9:} When component panels of a combined graph have a y-axis on both the left and 
	the right side of the plot region, each of these two y-axes may have its own title. 
	The option {opt y2tor1title} directs {cmd:grc1leg2} to suppress the redundant
	titles on {cmd:yaxis(2)} of the component panels and relocate one of them to the
	{help title_options:r1title} of the combined graph. 
	Example 3.9 demonstrates this capability. 
	Also see example {help grc1leg2##ex_3_3:3.3}{p_end}
	{marker ex_3_9}
	{title:Example 3.9: Relocating the titles on a second y-axis}
	set graph off
	twoway  ///
		(scatter mpg weight if qual==1, yaxis(1))  ///
		(scatter length weight if qual==1, yaxis(2)),  ///
			subtitle("Low Quality")  ///
			ylabel(,angle(hor) axis(2))  ///
			name(panel4, replace)
	
	twoway  ///
		(scatter mpg weight if qual==2, yaxis(1))  ///
		(scatter length weight if qual==2, yaxis(2)),  ///
			subtitle("Medium")  ///
			ylabel(,angle(hor) axis(2))  ///
			name(panel5, replace)
	
	twoway  ///
		(scatter mpg weight if qual==3, yaxis(1))  ///
		(scatter length weight if qual==3, yaxis(2)),  ///
			subtitle("High Quality")  ///
			ylabel(,angle(hor) axis(2))  ///
			name(panel6, replace)
	set graph on
	
	grc1leg2 panel4 panel5 panel6,  ///
		title("Ex. 3.9: Relocating the title on yaxis(2)")  ///
		xtob1title ytol1title y2tor1title  ///
		pos(4) ring(0) lxoffset(-20) lyoffset(15)  ///
		name(grc1y2tor1, replace)

		{it:({stata "grc1leg2_examples grcy2tor1":click to run Example 3.9})}

{p 4 4 2}
{it:Examples 3.10a, 3.10b and 3.10c:} {cmd:grc1leg} and {cmd:grc1leg2} transfer the legend from one of several component graphs 
	to become the legend for the combined graph.  But what if none of the legends of K component graphs includes 
	the keys from all of them?  A solution to this problem is to create one 
	extra graph, the K+1st graph, with a legend containing the keys from all K of the component graphs. 
	Then we borrow the legend from this K+1st graph, but hide the actual K+1st graph.{p_end}

{p 4 4 2}
Examples 3.10a, 3.10b and 3.10c show how this works.  In Example 3.10a the three desired component graphs 
	are {it:panel7}, {it:panel8}, and {it:panel9}, each with a legend containing different keys.
	So we create a fourth graph, called {it:panel10}, 
	which graphs all three y-axis variables against the common x-axis variable, {it:weight}.
	By specifying the colors of the markers, symbols, labels 
	and fitted lines for all the component graphs, including the auxiliary graph ({it:panel10}), 
	we assure that the elements of the legend displayed by {cmd:grc1eg2} 
	match those in the displayed panels. 
	(For the purposes of this example, we ignore other possible approaches 
	such as measuring price in thousands of dollars or using a 
	{stata "tw (scatter mpg length weight) (scatter price weight, yaxis(2))":second y-axis}.) 

{p 4 4 2}
Using {cmd:grc1leg2}'s option {opt leg:endfrom(graph_name)}, we specify that the combined legend 
	be borrowed from the component graph {it:panel10}, the last of the listed graph names.  
	Also specifying the option {opt hide:legendfrom} 
	instructs {cmd:grc1leg2} to hide the unreadable graph from which the legend has been borrowed.
	The space left by the hidden graph serves as the "hole" for the combined legend.
	The option {opt legend(colfirst cols(2) order(1 2 3 5 4) holes(3))} used to create {it:panel10}
	organizes the legend elements so that all three {it:panel9} markers are grouped 
	together in the second column, improving the legend's readability in the combined graph.
	By default, {cmd:grc1leg2} shrinks all elements of the legend by 30% using the option
	{opt legs:cale(*.7)}, allowing the two-column layout to fit within the available "hole". {p_end}
	{marker ex_3_10}
	{title:Examples 3.10a, b and c: Assembling a composite legend using the {opt hide:legendfrom} option}
	
	set graph off
	
	twoway  ///
		(scatter mpg weight, mcolor(blue)),  ///
			name(panel7, replace)
	
	twoway  ///
		(scatter length weight, mcolor(red)),  ///
			name(panel8, replace)
		
	twoway  ///
		(scatter price weight, mcolor(green))  ///
		(lfitci  price weight, lcolor(green)),  ///
			ytitle(Price)  ///
			name(panel9, replace)
	
	twoway  ///  This is the component graph from which we take the legend
		(scatter mpg weight, mcolor(blue))  ///
		(scatter length weight, mcolor(red))  ///
		(scatter price weight, mcolor(green))  ///
		(lfitci  price weight, lcolor(green)),  ///
			legend(colfirst cols(2) order(1 2 3 5 4) holes(3))  ///  <- Avoids using the option -lcols()- of -grc1leg2-
			name(panel10, replace)
	set graph on
	
	grc1leg2 panel7 panel8 panel9 panel10,  ///
		title("Ex. 3.10a: Assemble the legend keys from different panels"  ///
			"to construct the combined legend")  ///
		subtitle("Combining twoway graphs with different markers")  ///
		xtob1title legendfrom(panel10) hidelegendfrom  ///
		pos(4) ring(0) lyoffset(15)  ///
		name(grc1hide, replace)

		{it:({stata "grc1leg2_examples grchide":click to run Example 3.10a})}

{p 4 4 2}
{it:Example 3.10b:} The same trick can be used to combine a graph without markers, such as by {help graph pie},
	with a graph containing markers, such as {help scatter}. 
	We cannot make an auxiliary graph with a legend containing both pie chart and scatter plot markers
	by simply combining a pie chart with a scatter plot as we did with scatter plots above,
	because {help graph twoway} cannot make a pie chart.  
	Instead we use {help twoway rarea} to mimic the pie chart legend's keys in an auxiliary graph named {it:sym_and_mark}. 
	To assure that the combined scatter plots in {it:sctr_markers} use the same colors 
	as the pie chart, we specify the {help pstyle} options {it:p1pie}, {it:p2pie} and 
	{it:p3pie} for the scatter plots in {it:sctr_markers} and also in {it:sym_and_mark}.
	To control the layout of the panels in the combined graph, list 
	the hidden graph, {it:sym_and_mark}, last among the graphs to be combined by {cmd:grc1leg2}. 

	set graph off

	//  The pie graph's legend has symbols.
	graph pie, over(qual) plabel(_all percent, format(%5.1f) size(vlarge))  ///
		name(pie_symbols, replace)

	//  The scatter plot's legend has markers.
	tw  (scatter price weight if qual==1, pstyle(p1pie) msym(Oh))  ///
		(scatter price weight if qual==2, pstyle(p2pie))  ///
		(scatter price weight if qual==3, pstyle(p3pie)),  ///
		name(sctr_markers, replace)
		
	//  The auxiliary graph needed for its legend has both symbols and markers 
	tw  (rarea price price weight if qual==1)  ///
		(rarea price price weight if qual==2)  ///
		(rarea price price weight if qual==3)   ///
		(scatter price weight if qual==1, pstyle(p1pie) msym(Oh))  ///
		(scatter price weight if qual==2, pstyle(p2pie))  ///
		(scatter price weight if qual==3, pstyle(p3pie)),  ///
		legend(  ///
			label(1 "Low Quality") ///
			label(2 "Medium Quality") ///
			label(3 "High Quality") ///
			label(4 "Low Quality") ///
			label(5 "Medium Quality") ///
			label(6 "High Quality") ///
			rows(2)  ///
		)  ///
		name(sym_and_mark, replace)

	set graph on
	
	grc1leg2 pie_symbols sctr_markers sym_and_mark,  ///
		title("Ex. 3.10b: Assemble the legend keys from different panels"  ///
			"to construct the combined legend")  ///
		subtitle("Combining a graph with markers with a graph with color swatches")  ///
		legendfrom(sym_and_mark) hidelegendfrom  ///
		name(grc1hide2, replace)

		{it:({stata "grc1leg2_examples grchide2":click to run Example 3.10b})}

{p 4 4 2}
{it:Example 3.10c:} Excel includes a graph called a 
	{browse "https://www.exceldemy.com/bar-of-pie-chart-excel/":"bar_of_pie" chart},
	which is a combined graph   
	consisting of a pie chart with a highlighted or "exploded" slice and a bar chart 
	showing subcategories of the the exploded slice. 
	Example 3.10c demonstrates the construction of a bar_of_pie chart using -grc1leg2.
	Hat tip to Fabio Tufano for suggesting this application of {cmd:grc1leg2}.{p_end}

	//  Generate a discrete variable with three categories of foreign cars by country of manfacture
	gen byte forcntry = 1 if foreign
		replace forcntry = 2 if inlist(word(make,1),"VW", "BMW", "Audi")
		replace forcntry = 3 if inlist(word(make,1),"Datsun", "Honda", "Mazda", "Toyota")
	
		label define forcntry 1 "Other foreign" 2 "Germany" 3 "Japan" 
		label values forcntry forcntry
	
	//  The new categorical variable is defined only for foreign cars
	tab forcntry foreign, mi
	
	set graph off

	//  The big picture: Foreign as a share of all models in 1978
	gr pie , over(foreign) angle(305) plabel(_all percent, size(large) format(%6.1f)) ///
		subtitle(Shares of domestic and foreign models)  ///
		pie(1) pie(2, explode)   ///
		graphregion(margin(l -25) ) ///  <- to prepare for landscape orientation
		name(pie_dom_for, replace)

	//  Zoom in on the distribution of foreign models by country of origin
	gr bar (percent), over(forcntry) asyvars stack yalternate  ///
		bar(1, bstyle(p3bar)) bar(2, bstyle(p4bar)) bar(3, bstyle(p5bar) ) ///
		blabel(bar ,pos(center) size(large) format(%6.1f))  ///
		graphregion(margin(r +25) ) fxsize(35)  ///  <- to prepare for landscape orientation  
		subtitle("Breakdown of foreign models")  ///
		name(bar_for, replace) 

{p 4 4 2}
For the sole purpose of using its legend in the combined graph, we create a "dummy graph" with five legend keys.
Since we are only using the legend from this graph, we can use any five-category variable to construct the dummy graph.
-auto.dta- concidentally contains a five-category variable, -rep78-.  
So we construct a pie chart over that variable to make a legend with five color swatches.  
In order to spread the legend horizontally, 
we use the option -holes(3/15)- to put three empty columns 
in the middle of the legend.{p_end}

	//  Construct the "dummy graph" 
	gr pie , over(rep78) ///
		legend(colfirst cols(6) holes(3/15)  ///
			title("Location of manufacturer")  ///
			order(  ///
				1 "Domestic" ///
				2 "Foreign"  ///
				5 "Japan"    ///
				4 "Germany"  /// 
				3 "Other foreign"  ///
			) )  ///
		name(pie_legend, replace)

	set graph on
	
	//  -grc1leg2- allows us to apply the legend from the dummy graph to the combined graph
	grc1leg2 pie_dom_for bar_for pie_legend, legendfrom(pie_legend) hidelegendfrom  ///
		title("Ex. 3.10c: Distribution of automobile models by domestic and foreign"  ///
		"and within foreign, by country of origin")  ///
		xsize(9) ysize(5)  ///  <- landscape orientation
		name(bar_of_pie, replace)
		
		{it:({stata "grc1leg2_examples bar_of_pie":click to run Example 3.10c})}
		
{p 4 4 2}
To more closely replicate the Excel "bar-of-pie" look, you may want to add arrows 
	from the exploded slice to the bar.  {cmd:grc1leg2} will not do this for you,
	but it is easy to do with the {help graph editor}.  To see the result of such an edit,
	as applied to the graph produced by Example 3.10c above, click on the next link.{p_end}

		{it:({stata "grc1leg2_examples play_grec_on_Ex_3_10c":click to add arrows to the "bar of pie" graph})}

{p 4 4 2}
{it:Example 3.11:} One way to change the overall "look" of a combined graph would be to carefully control 
	the look of each of the component graphs. Often, however, it is easier
	or more effective to apply options to change the look of the combined graph without bothering 
	to change the look of the component graphs.  Example 3.11 shows that 
	{cmd:grc1leg2}'s {help grc1leg2##grdispopts:{it:gr_display_options}} borrowed from 
	{help graph_display:graph display} allow the user to override 
	some features of the individual graphs in favor of a common look for all the panels 
	of a combined graph.{p_end}
	{marker ex_3_11}
	{title:Example 3.11: Changing the overall look of a combined graph}  
	grc1leg2 panel1 panel2 panel3,  ///
		xcommon ycommon ring(0) pos(5)  ///
		lxoffset(-5) lyoffset(15)  ///
		xtob1title ytol1title  ///
		xsize(8) ysize(8) scheme(s1rcolor)  ///  {bf}<-- set the dimensions & scheme for this graph only{sf}
		title("Example 3.11 is 3.3 with a different overall look")  ///
		subtitle("From the same component panels,"  ///
			"alter the overall look of the combined graph" /// 
			"using the xsize(), ysize() and scheme() options")  ///
		name(grc13_dispopts, replace)

		{it:({stata "grc1leg2_examples grc3_dispopts":click to run Example 3.11})}

		
{p 4 4 2}
{it:Example 3.12:} {cmd:grc1leg2}'s options {opt lr:ows(integer)} and {opt lc:ols(integer)} enable 
	the user to alter the row and column arrangement of the original borrowed legend.  
	However, when saving the combined file to disk, the user must specify the 
	saving sub-option {opt asis} to preserve the row and column rearrangement.  
	The changes made by all other {cmd:grc1leg2} options save to disk without problem in a "live" {help gph_files:gph} file.
	Example 3.12 shows that a saved "live" {help gph_files:gph} file 
	preserves all edits except the changed arrangement.
	(See Issue #2 under {help grc1leg2##known:"Known issues"}.){p_end}
	{marker ex_3_12}
	{title:Example 3.12: Change the legend's row and column arrangement and save to disk}  
	grc1leg2 panel7 panel8 panel9 panel10,  ///
		title("Ex. 3.12: Change the legend's row and column arrangement"  ///
		`"with lcols(1) and save to disk as a "live" gph file"')  ///
		xtob1title legendfrom(panel10) hidelegendfrom  ///
		pos(4) ring(0) lxoffset(-5) lyoffset(15) lcols(1) ///
		name(grc1lcols, replace) saving(grclcols, replace)

		{it:({stata "grc1leg2_examples grclcols":click to run Example 3.12})}
		{it:({stata "graph use grclcols":click here to view the saved gph file})}
	
{p 4 4 2}
{it:Example 3.13:} Example 3.13 demonstrates how to save the effects of options 
	{opt lr:ows(integer)} and {opt lc:ols(integer)} by using the sub-option {opt asis}
	within {cmd:grc1leg2}'s {opth saving(saving_options)}.
	However, a {help gph_files:gph} file saved "{opt asis}" is "frozen" and can no longer be edited
	using Stata's graph editor.  For this reason, if one wants to save a "live" 
	{help gph_files:gph} file, the preferred strategy is to avoid using {cmd:grc1leg2}'s
	{opt lr:ows(integer)} or its {opt lc:ols(integer)} option by arranging the rows and columns 
	in the component graph as in {help grc1leg2##ex_3_10:Example 3.10}.{p_end}
	{marker ex_3_13}
	{title:Example 3.13: Save the changed row and column arrangement with the option -asis-}  
	grc1leg2 panel7 panel8 panel9 panel10,  ///
		title("Ex. 3.13: Change the legend's row and column arrangement"  ///
		`"with lcols(1) and save to disk as an "asis" gph file"')  ///
		xtob1title legendfrom(panel10) hidelegendfrom  ///
		pos(4) ring(0) lxoffset(-5) lyoffset(15) lcols(1) ///
		name(grc1lcolsasis, replace) saving(grc1lcolsasis, replace asis)	// <--  Only change is to include the sub-option -asis-
		
		{it:({stata "grc1leg2_examples grclcolsasis":click to run Example 3.13})}
		{it:({stata "graph use grc1lcolsasis":click here to view the saved gph file})}

		
{marker known}{...}
{help grc1leg2##known:+}{title:KNOWN ISSUES}

{p 4 4 2}
{ul:Issue 1.}  {cmd:grc1leg2} sometimes fails when the legend designated
	for the combined graph is not "complete" or is otherwise unexpected or degenerate.
	Starting with version 2.10, {cmd:grc1leg2} attempts to detect these conditions and either 
	prevent them or exit politely with an informative error message. If you encounter 
	an error such as 
	{err:series # not found},
	{err: class type not found} or
	{err: class member function not found}	
	({search r(111), local:r(111)},
	{search r(4018), local:r(4018)} or
	{search r(4023), local:r(4023)}), 
	please contact the
	{browse "http://www.cgdev.org/expert/mead-over/":author}.{p_end}
	
{p 4 4 2}
{ul:Issue 2.}  The changes effected by using {cmd:grc1leg2}'s {opt lr:ows(integer)} or {opt lc:ols(integer)} option 
	are not preserved when the displayed memory graph is saved to disk as a "live" {help gph_files:gph} file.
	To preserve these changes in a saved file, the save must be performed with the 
	sub-option {opt asis}. See Examples {help grc1leg2##ex_3_12:3.12} and {help grc1leg2##ex_3_13:3.13}{p_end}


{marker references}{...}
{title:Reference}
{phang}
Cox, Nicholas J. (2020) "Stata tip 139: The by() option of graph can work better
	than graph combine", The Stata Journal, Vol 20, Number 4, pp. 1016–1027.
	{browse "https://journals.sagepub.com/doi/pdf/10.1177/1536867X20976341"}
	

{marker author}{...}
{title:Authors and acknowledgements}

{phang}{browse "https://www.statalist.org/forums/member/11-vince-wiggins-statacorp":Vince Wiggins} coded {help grc1leg} and distributed it on Statalist 16 June 2003.
	His version 1.0.5, dated 02jun2010, is available from
	{net "describe grc1leg, from(http://www.stata.com/users/vwiggins)":Stata}.
	{cmd:grc1leg2} is a hack of his version 1.0.5.  This help file builds on his 
	original help file for {cmd:grc1leg}, adding documentation
	of {cmd:grc1leg2}'s options and examples of Stata's alternative
	approaches to control the placement of a single legend in a multi-panel 
	graph.  In April 2016, Derek Wagner of StataCorps suggested code 
	to implement the -labsize(textsizestyle)- option.{p_end}
	
{phang}Thanks to Statalist members Florian Schneider and Andrew Musau, 
	who found a bug I introduced in version 1.41 and led me 
	to identify the problem with some legend options which cause {cmd:grc1leg2}
	to fail and to generate the error {err:series # not found}.
	See the discussion of the problem as a {help grc1leg2##known:known issue}.
	{p_end}

{phang}The addition of the {help grc1leg2##grdispopts:{it:gr_display_options}} in version 1.60 is in response 
	to a discussion on Statlist 
	{browse "https://www.statalist.org/forums/forum/general-stata-discussion/general/1610172-grc1leg-ignoring-aspect-ratio":here}
	and especially to 
	{browse "https://www.statalist.org/forums/member/10-jeff-pitblado-statacorp":Jeff Pitlabo}'s 
	{browse "https://www.statalist.org/forums/forum/general-stata-discussion/general/1610172-grc1leg-ignoring-aspect-ratio?p=1611539#post1611539":suggestion}
	that the aspect ratio of a combined graph could be changed in "post-production" by using {help graph display}.

{phang}Version 2.0 responds to the suggestion of a Statalist member that {cmd:grc1leg2} 
	should allow the user to alter the size of the legend keys 
	and, by default, should shrink those keys for the combined 
	graph in order to preserve their resemblance to the keys in the component graphs.
	To address these issues, this version adds the options {opth legs:cale(size)} and {opth lms:ize(size)}.
	In implementing these changes, I discovered that the edits effected by many 
	previously introduced {cmd:grc1leg2} options were not preserved 
	when the graph was saved using the option {opth saving(saving_option)}.
	This problem is now largely fixed.  The exceptions are {cmd:grc1leg2}'s 
	options {opt lrow()} and {opt lcol()}, the results of which are displayed 
	but not saved to a live {help gph} file.  
	See Examples {help grc1leg2##ex_3_12:3.12 and 3.13} for a work-around.
	
{phang}{marker old}{help grc1leg2##old:*} Options marked with this asterisk 
	were part of {help grc1leg}.{p_end}

{phang}Contact {browse "http://www.cgdev.org/expert/mead-over/":Mead Over} at MOver@CGDev.org or on Statalist 
if you observe any problems and especially if you encounter difficulties with, or can suggest a solution for, 
the {help grc1leg2##known:known issues}. {p_end}

{* Version History}{...}
{* Version 1.0.5 of grc1leg	02jun2010 by Mead Over}{...}
{* Version 1.1.0  1Apr2016: Added xtob1title and xtitlefrom}{...}
{* Version 1.2   11Apr2016: Added lsize(textsizestyle) option.}{...}
{* Version 1.2.1 15Apr2016: Edit.}{...}
{* Version 1.2.2 11Nov2019: Add options ytol1title, maintotoptitle}{...}
{* Version 1.3   23Jan2021: Add more legend and borrowed title options}{...}
{* Version 1.3.1 29Jan2021: Add options xtsize, ytsize, mtsize and example 3.6}{...}
{* Version 1.3.2 9Feb2021:  Add references to Cox's Tip #139}{...}
{* Version 1.3.3 13Mar2021: Document options -y2tor1title-, -y2titlefrom- and -y2tsize-}{...}
{* Version 1.3.4 20Mar2021: Document options -y2tor1title-, -y2titlefrom- and -y2tsize-}{...}
{* Version 1.40 24Mar2021: Add examples 3.7 & 3.8 to demonstrate relocation of legends from -gr combine- and -gr,by()- graphs}{...}
{* Version 1.41 26Mar2021: Add examples 3.7 & 3.8 to demonstrate relocation of legends from -gr combine- and -gr,by()- graphs}{...}
{* Version 1.42 29Mar2021: Clean up some typos. Add -hidelegendfrom- option, example.}{...}
{* Version 1.50 4Apr2021: Add discussion of "known issue"}{...}
{* Version 1.60 15Jun2021: Add documentation for the graph display options and -graphon- and -nodots-}{...}
{* Version 2.00 3Mar2022: Document legscale, lmsize and issues with lcols() and lrows() requiring -asis- option.}{...}
{* Version 2.01 4Mar2022: Add remark that grc1leg2 can be applied recursively. Lines 1159-63}{...}
{* Version 2.02 6Mar2022: Expand the discussion of known issue #1, (b).}{...}
{* Version 2.10 12Mar2022: Change panel9 to use -lfitci- and change Examples 3.10, 3.12 and 3.13.}{...}
		{* Update discussion of Known Issue #1 with definition of a "complete" legend.}{...}
		{* Add Example 3.10_bis to demonstrate Known Issue #1.}{...}
{* Version 2.11 13Mar2022: Change Example 3.10_bis to specify that only *marker* resizing is disabled by an incomplete legend.  In later versions, 3.10_bis is deprecated}{...}
{*		Update discussion of Known Issue #1 accordingly.}{...} 
{* Version 2.13 30May2022: Edit Description section of this help file, change header from 2.11->2.13}{...} 
{* Version 2.20 21Jun2022: Document options -maintotoptitle-, -maintitlefrom()- or -mainfrom()-, -mtsize()-}{...} 
{* 		-subtosubtitle-, -subfrom()-, -stsize()- & -notetonote-, -notefrom()-, -ntsize()-}{...} 
{* 		For various uses of the option textsizestyle, remind user that  {it:zero} suppresses the text}{...} 
{* Version 2.21 15Nov2022: Note that the option -noautosize- disables legscale(size) and resizing of all legend elements}{...} 
{* Version 2.22 1Dec2022: Note that for graphs having no "markers", option -lmsize()- is permitted but has no effect}{...} 
{* Version 2.23 10Dec2022: Suggest that the graph with a hidden legend, if any, should always be the last graph in -gphlist-}{...} 
{* Version 2.24 14May2023: Expand the description of the default behavior of -autosize-}{...} 
{* Version 2.25 10Oct2023: Add Example 3.10c which demonstrates production of a bar_of_pie graph.}{...}
{* 		Now that marker sizing works better, remove cautionary language about resizing failures.}{...}
{* Version 2.26 4Nov2023: Fix the subroutine that adds arrows to the memory graph -bar_of_pie- and add expository text.}{...}
{* 		Prepend italicized exercise numbers to the text preceding the bolded underlined exercise title.}{...}
{* }{...}
