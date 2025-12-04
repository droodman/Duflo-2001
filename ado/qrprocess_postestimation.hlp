{smcl}
{* *! version 1.0.1  14april2020}{...}

{title:Title}

{p2colset 5 32 34 2}{...}
{p2col :{hi: qrprocess postestimation} {hline 2} Postestimation tools for qrprocess}{p_end}
{p2colreset}{...}

{marker description}{...}
{title:Description}

{pstd}
All postestimation commands available after {cmd:qreg} are also available after {cmd:qrprocess}.
 In addition, qrprocess predict provides two additional options.

{pstd}  
For more detailed information on postestimation options offered by {cmd:qrprocess}, please refer to {helpb qrprocess_postestimation##CFM_Stata:Chernozhukov, Fernández-Val and Melly (2020)}. 

{synoptset 25 notes}{...}
{p2coldent :Command}Description{p_end}
{synoptline}
INCLUDE help post_estatsum
INCLUDE help post_estatvce
INCLUDE help post_estimates
INCLUDE help post_lincom
INCLUDE help post_linktest
INCLUDE help post_margins
INCLUDE help post_marginsplot
INCLUDE help post_nlcom
{synopt :{helpb qrprocess_postestimation##predict:predict}}predictions, residuals, influence statistics, and other diagnostic measures{p_end}
INCLUDE help post_predictnl
INCLUDE help post_test
INCLUDE help post_testnl
{synoptline}
{p2colreset}{...}
{phang}


{marker syntax_predict}{...}
{marker predict}{...}
{title:Syntax for predict}

{phang}

{p 8 20 2}
{cmd:predict} {dtype} {newvar} {ifin}
	[{cmd:,} {it:statistic} {cmd:equation(}{it:eqno}{cmd:)}]

{synoptset 25 tabbed}{...}
{synopthdr :statistic}
{synoptline}

{syntab :{help qrprocess_postestimation##usual_options_predict:Usual options}}
{synopt :{opt xb}}linear prediction; the default{p_end}
{synopt :{opt stdp}}standard error of the linear prediction{p_end}
{synopt :{opt stddp}}standard error of the difference in linear predictions{p_end}
{synopt :{opt r:esiduals}}residuals{p_end}

{syntab :{help qrprocess_postestimation##qrprocess_options_predict:Specific options}}
{synopt :{opth rearranged(numlist)}}calculates the conditional quantile(s) obtain by rearrangement{p_end}
{synopt :{opth cdf(numlist)}}calculates the conditional cumulative distribution function{p_end}
{synoptline}
{p2colreset}{...}
INCLUDE help esample


{marker options_predict}{...}
{title:Options for predict}

{marker usual_options_predict}
{dlgtab:Usual options}

{phang}{opt xb}, the default, calculates the linear prediction.

{phang}{opt stdp} calculates the standard error of the linear prediction.

{phang}{opt stddp} calculates the standard error of the difference in linear predictions between equations 1 and 2.

{phang}{opt r:esiduals} calculates the residuals.

{marker qrprocess_options_predict} 
{dlgtab:Specific options}

{phang}{opth rearranged(numlist)} calculates the conditional quantile(s) obtain by rearrangement as defined in {help qrprocess_postestimation##Chernozhukov_et_al_2010:Chernozhukov et al. (2010)}.
This monotone quantile function can be seen as a sorting or monotone rearrangement of the original function.
The estimated monotone quantile function is numerically equal to the original one if the original curve is increasing in the quantile, but differs from the original function otherwise.
{help qrprocess_postestimation##Chernozhukov_et_al_2010:Chernozhukov et al. (2010)}
have shown that the rearranged function is asymptotically equivalent with the original one but is closer to the true functions in finite samples.
The {it:numlist} specifies the quantile(s) to be calculated. Note that all estimated quantile regressions are used to calculate each conditional quantile. 
For instance, the calculated conditional median may not be the same after the estimation of 3 quantile regressions or after 100 quantile regressions. 
It is recommended to estimate a large number of quantile regressions to better approximate the conditional distribution. 

{phang}{opth cdf(numlist)}calculates the conditional CDF as defined in {help qrprocess_postestimation##Chernozhukov_et_al_2010:Chernozhukov et al. (2010)}.
The {it:numlist} specifies the values at which the CDF will be calculated. 
The remarks made in the description of {cmd:rearranged(}{it:numlist}{cmd:)} about the number of quantile regressions also apply to this statistic.

{dlgtab:Multiple equations}

{phang}{opt eq:uation(eqno[,eqno])} specifies the equation to which the calculation should be made.
This option matters only for {cmd:xb}, {cmd:stdp}, {cmd:stddp}, and {cmd:residuals}.
 For {cmd:rearranged(}{it:numlist}{cmd:)} and {cmd:cdf(}{it:numlist}{cmd:)} the user must supply the quantiles and y-values directly in parentheses after the option and not through {cmd:equation}.


{marker examples}{...}
{title:Examples}

{pstd}Setup - median regression{p_end}
{phang2}{cmd:. sysuse auto}{p_end}
{phang2}{cmd:. qrprocess price weight length foreign}{p_end}

{pstd}Obtain predicted values{p_end}
{phang2}{cmd:. predict hat}

{pstd}Obtain residuals{p_end}
{phang2}{cmd:. predict r, resid}{p_end}

{pstd}Setup - regression of a large number of quantiles{p_end}
{phang2}{cmd:. qrprocess price weight length foreign, q(0.01(0.01)0.99) noprint}{p_end}

{pstd}Obtain conditional CDF{p_end}
{phang2}{cmd:. predict cdf, cdf(4000(1000)8000)}


{marker references}{...}
{title:References}

{phang}
{marker Chernozhukov_et_al_2010}
Chernozhukov, V., I. Fernández-Val, and A. Galichon. 2010. Quantile and probability curves without crossing. {it:Econometrica} 78(3): 1093-1125.
{p_end}

{phang}
{marker CFM_Stata}
Chernozhukov, V., I. Fernández-Val, and  B. Melly. 2020b. Quantile and distribution regression in Stata: algorithms, pointwise and functional inference. {it:Working paper}.
{p_end}
