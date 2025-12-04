{smcl}
{* *! version 1.2.7 25sep2023}{...}
{vieweralsosee "[R] lincom" "mansection R lincom"}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "[R] nlcom" "help nlcom"}{...}
{vieweralsosee "[R] test" "help test"}{...}
{vieweralsosee "[R] testnl" "help testnl"}{...}
{viewerjumpto "Syntax" "xlincom##syntax"}{...}
{viewerjumpto "Description" "xlincom##description"}{...}
{viewerjumpto "Options" "xlincom##options"}{...}
{viewerjumpto "Examples" "xlincom##examples"}{...}
{viewerjumpto "Stored results" "xlincom##results"}{...}
{viewerjumpto "Acknowledgments" "xlincom##acknowledgments"}{...}
{viewerjumpto "Author" "xlincom##author"}{...}
{viewerjumpto "Also see" "xlincom##also_see"}{...}


{title:Title}

{phang}
{cmd:xlincom} {hline 2} Multiple linear combinations of parameters


{marker syntax}{...}
{title:Syntax}

{phang}
Single combination

{p 8 16 2}
{cmd:xlincom} [{it:name}=]{it:{helpb xlincom##exp:exp}} [{cmd:,} {it:options}]

{phang}
Multiple combinations

{p 8 16 2}
{cmd:xlincom} {cmd:(}[{it:name}=]{it:{helpb xlincom##exp:exp}}{cmd:)} [{cmd:(}[{it:name=}]{it:{helpb xlincom##exp:exp}}{cmd:)} ...] [{cmd:,} {it:options}]

{synoptset 18}{...}
{synopthdr}
{synoptline}
{synopt :{opt post}}post estimation results{p_end}
{synopt :{opt repost}}add results to estimates in memory{p_end}
{synopt :{opt covzero}}set all covariances to zero{p_end}
{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}{p_end}
{synopt :{opt df(#)}}use t distribution with {it:#} degrees of freedom for
       computing p-values and confidence intervals{p_end}
{synopt :{opt nohead:er}}suppress output header{p_end}
{synopt :{opt ef:orm}}exponentiated coefficients; exp(b){p_end}
{synopt :{opt or}}odds ratio{p_end}
{synopt :{opt hr}}hazard ratio{p_end}
{synopt :{opt shr}}subhazard ratio{p_end}
{synopt :{opt ir:r}}incidence-rate ratio{p_end}
{synopt :{opt rr:r}}relative-risk ratio{p_end}
{synopt :{it:{help estimation_options##display_options:display_options}}}control column formats{p_end}
{synoptline}
{p2colreset}{...}

{marker exp}{...}
    {it:exp} is a linear expression containing
        {it:coef}
        {it:eqno:coef}
        {cmd:_b[}{it:coef}{cmd:]}
        {cmd:_b[}{it:eqno}{cmd::}{it:coef}{cmd:]}
        {cmd:[}{it:eqno}{cmd:]}{it:coef}
        {cmd:[}{it:eqno}{cmd:]_b[}{it:coef}{cmd:]}

    {it:eqno} is
        {cmd:#}{it:#}
        {it:name}

{pstd}
{it:exp} is any linear combination of coefficients that is valid
syntax for {helpb lincom:lincom}. An optional {it:name} may be specified to label 
the transformation; {it:name} can be any {help reswords:valid Stata name}. 


{marker description}{...}
{title:Description}

{pstd}
{cmd:xlincom} computes point estimates, standard errors, t or z statistics,
p-values, and confidence intervals for single or multiple linear combinations of coefficients 
as well as covariances in the case of multiple combinations. {helpb nlcom:nlcom} can also do this, 
but {cmd:xlincom} is much faster and offers the same syntax as {helpb lincom:lincom}. {cmd:xlincom} 
internally calls {helpb lincom:lincom} for each linear combination and extracts coefficient 
estimates and variances from its output. Results can be posted or reposted for exporting 
with pretty table commands or testing.


{marker options}{...}
{title:Options} 

{phang}
{opt post} posts estimation results in {cmd:e()}.

{phang}
{opt repost} adds results to the estimates in memory. If the estimates in memory are from a 
single equation model, the model estimates will be prefixed with equation name "Main" and the linear combinations 
will be prefixed with equation name "xlincom". In the case of a multiple equation model, the linear combinations will 
be added with equation name "xlincom". This option is intended to make it easy to create tables combining
model coefficients and linear combinations of these coefficients.

{phang}
{opt covzero} sets covariances to zero for speed improvements. The 
transformations should not be tested against other coefficients 
if this option is specified as this will yield invalid results. 

{phang}
{opt level(#)} specifies the confidence level. The default is {cmd:level(95)} 
or as set by {helpb set level}.

{phang}
{opt df(#)} specifies that the t distribution with {it:#} degrees of
freedom be used for computing p-values and confidence intervals.
The default is to use {cmd:e(df_r)} degrees of freedom or the standard normal
distribution if {cmd:e(df_r)} is missing.

{phang}
{opt noheader} suppresses the output header.

{phang}
{opt eform}, {opt or}, {opt hr}, {opt shr},  {opt irr}, and {opt rrr} all report
coefficient estimates as exp(b) rather than b. Only one of these options may be 
specified. {opt or} is the default after {cmd:logistic}. See {helpb lincom:help lincom} 
for more information about these options.


{marker examples}{...}
{title:Examples}

{pstd}Example taken from {helpb lincom:help lincom}

    {hline}
{pstd}Setup{p_end}
{phang2}{cmd:. webuse regress}{p_end}
{phang2}{cmd:. regress y x1 x2 x3}{p_end}

{pstd}Estimate single linear combination{p_end}
{phang2}{cmd:. xlincom x2-x1}{p_end}

{pstd}Estimate single linear combination, label transformation{p_end}
{phang2}{cmd:. xlincom myname = 3*x1 + 500*x3}{p_end}

{pstd}Estimate single linear combination, label transformation and post results{p_end}
{phang2}{cmd:. xlincom myname =  3*x1 + 500*x3 - 12, post}{p_end}

{pstd}Estimate multiple linear combinations of coefficients, label transformations and post results{p_end}
{phang2}{cmd:. qui regress y x1 x2 x3}{p_end}
{phang2}{cmd:. xlincom (name1 = x2-x1) (name2 = 3*x1 + 500*x3) (name3 = 3*x1 + 500*x3 - 12), post}{p_end}

{pstd}Add sum of coefficients to a formatted table using {cmd:esttab}{p_end}
{phang2}{cmd:. webuse gxmpl1}{p_end}
{phang2}{cmd:. reg gnp L(0/2).cpi}{p_end}
{phang2}{cmd:. xlincom (t = cpi) (t1 = cpi + l1.cpi) (t2 = cpi + l1.cpi + l2.cpi), repost}{p_end}
{phang2}{cmd:. esttab, eqlabels("Main" "Sum of coefficients", span)}{p_end}


{marker results}{...}
{title:Stored results}

{pstd}
{cmd:xlincom} stores the following in {cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(level)}}confidence level{p_end}

{p2col 5 15 19 2: Matrices}{p_end}
{synopt:{cmd:r(table)}}coefficient table{p_end}
{p2colreset}{...}

{pstd}
If option {opt post} is specified, {cmd:xlincom} stores the following in {cmd:e()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(df_r)}}degrees of freedom{p_end}

{p2col 5 15 19 2: Macros}{p_end}
{synopt:{cmd:e(predict)}}xlincom_p{p_end}
{synopt:{cmd:e(cmd)}}xlincom{p_end}
{synopt:{cmd:e(properties)}}{cmd:b V}{p_end}
{synopt:{cmd:e(depvar)}}dependent variable{p_end}

{p2col 5 15 19 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}coefficient vector{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of the estimators{p_end}

{p2col 5 15 19 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}estimation sample{p_end}
{p2colreset}{...}

{pstd}
If option {opt repost} is specified, {cmd:xlincom} keeps the estimates in memory
but changes {cmd:e(b)} and {cmd:e(V)} to include coefficients and variance/covariances
of the linear combinations. 


{marker acknowledgments}{...}
{title:Acknowledgments}

{pstd} 
I would like to thank Roger Newson, as some of the code of {cmd:xlincom} 
is based on the code of his command {cmd:lincomest}. 

{pstd} 
I would also like to thank Ben Jann, who pointed me towards the approach to add
results to the estimates in memory which led to the {opt repost} option. 

{pstd}
Since much of {cmd:xlincom}'s options are the same as {cmd:lincom}'s I have 
used information from the help file of {helpb lincom:lincom} while making 
this help file for consistency and clarity, especially for shared options.


{marker author}{...}
{title:Author}

{pstd}
Wouter Wakker, wouter.wakker@outlook.com


{marker also_see}{...}
{title:Also see}

{pstd}
{helpb lincom:lincom}, {helpb nlcom:nlcom}, {helpb test:test}, {helpb testnl:testnl}
