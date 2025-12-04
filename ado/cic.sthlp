{smcl}
{* $Id: cic.sthlp $}{...}
{vieweralsosee "" "--"}{...}
{viewerjumpto "Title" "cic##title"}{...}
{viewerjumpto "Syntax" "cic##syntax"}{...}
{viewerjumpto "Description" "cic##description"}{...}
{viewerjumpto "Options" "cic##options"}{...}
{viewerjumpto "Remarks" "cic##remarks"}{...}
{viewerjumpto "Author" "cic##author"}{...}
{viewerjumpto "Examples" "cic##examples"}{...}
{viewerjumpto "References" "cic##references"}{...}
{marker title}{...}
{title:Title}

{p2colset 1 8 10 2}{...}
{p2col:{bf:cic} {hline 2}} Stata implementation of the Athey and Imbens (2006) Changes-in-Changes model{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}

{p 8 12 2}
. {cmd:cic} {it:{help cic##estimator:estimator}}
                      {it:{help varname:depvar}}
                      {it:{help varname:tvar}}
                      {it:{help varname:pvar}}
                      [{it:{help varlist:varlist}}]
                      {ifin}
                      [{it:{help cic##weight:weight}}]
                      [{cmd:,}
                      {it:{help cic##options_table:options}}]

{phang}{it:depvar} is the dependent variable{p_end}

{phang}{it:tvar} must contain values 0 or 1, representing the treatment (1) and comparison (0) groups.{p_end}

{phang}{it:pvar} must contain values 0 or 1, representing the pre-intervention (0) and post-intervention (1) periods.{p_end}

{phang}{it:varlist} is a (optional) list of covariates variables (see the {help cic##remarks:remarks}) {p_end}


{marker estimator}{...}
{synoptset 16}{...}
{synopthdr:estimator}
{synoptline}
{synopt :{opt continuous}}CIC estimator with continuous outcomes (Athey and Imbens 2006, equation 9){p_end}
{synopt :{opt dci}}CIC model with discrete outcomes, under the conditional independence assumption
(Athey and Imbens 2006, equation 29){p_end}
{synopt :{opt bounds}}lower and upper bound estimates of discrete CIC model, without conditional independence
(Athey and Imbens 2006, equation 25){p_end}
{synopt :{opt all}}all of the above{p_end}
{synoptline}


{marker options}{...}
{marker options_table}{...}
{synoptset 16}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt :{opth at(numlist)}}a list of percentiles for CIC results. default is at(10(10)90){p_end}
{synopt :{opth v:ce(vcetype)}}{opt vce(none)}, {opt vce(delta)}, or {opt vce(bootstrap [, bsopts])} are allowed;
default is {opt vce(none)}{p_end}
{synopt :{opt did}}calculates traditional DID and quantile DID estimates;
by default this option is off, unless {it:varlist} exists in which case it is turned on{p_end}
{synopt :{opt unt:reated}}estimates the counterfactual effect of the policy for the untreated group
(Athey and Imbens 2006, Setion 3.2){p_end}
{synopt :{opt rou:nd(#)}} rounds {it:depvar} to # units (=0 for no rounding, the default);
any rounding is performed after adjusting for covariates, if applicable{p_end}

{syntab:Reporting}
{synopt :{opt level(#)}}set confidence level; default is level(95){p_end}
{synopt :{opt notab:le}}suppresses the table of results{p_end}
{synopt :{opt noh:eader}}suppresses the table header{p_end}
{synopt :{opt nol:egend}}suppresses the table legend{p_end}
{synopt :{it:{help cic##display_options:display_options}}}control columns and
column formats, row spacing, line width, display of omitted variables and base and empty
cells, and factor-variable labeling{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{it:varlist} may contain factor variables; see {help fvvarlists}.{p_end}
{p 4 6 2}
{opt bootstrap}:, {opt by}:, {opt jackknife}:, and {opt statsby}: are allowed;{p_end}
{p 4 6 2}
Weights are not allowed with the {help bootstrap}: prefix.{p_end}
{marker weight}{...}
{p 4 6 2}
{opt fweight}s, {opt iweight}s, and {opt aweight}s are allowed; see {help weight}.{p_end}
{p 4 6 2}
When the {opt by:} prefix is used, only the last group is saved in ereturn.{p_end}
{p 4 6 2}
{marker display_options}{...}
{opt display_options} include: noomitted, vsquish, noemptycells, baselevels, and allbaselevels;
see {help estimation options##display_options:[R] estimation options}.{p_end}


{synoptset 25}{...}
{synopthdr:bsopts}
{synoptline}
{synopt :{opt reps(#)}}                      perform # bootstrap replications; default is 1000{p_end}
{synopt :{opt saving(filename[,replace])}}   save bootstrap results to filename (optionally,
replace specifies that filename be overwritten, if it exists.){p_end}
{synopt :{opt sepercentile}}                 obtain bootstrap standard errors from percentiles of bootstrap
estimates instead of using Stata's default method. standard error = (p(97.5) - p(2.5)) / (2*1.96), where p(N)
is Nth percentile of bootstrap iterations (this is the method used in Athey and Imbens' MATLAB code){p_end}
{synopt :{opt accel(vector)}}                acceleration values for each statistic{p_end}
{synopt :{opt mse}}                          use MSE formula for variance estimation{p_end}
{synopt :{opt nodots}}                       suppress the replication dots{p_end}
{synopt :{opt size(#)}}                      draw samples of size #{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}
{opt vce(bootstrap[, bsopts])} stratifies the sample by {it:tvar} and {it:pvar}{p_end}
{p 4 6 2}
See {help bootstrap postestimation} for features available after estimation.{p_end}
{p 4 6 2}
The following two lines are equivalent:{p_end}
{phang2}{cmd: . cic y treat post, vce(bootstrap, [bsopts])}{p_end}
{phang2}{cmd: . bootstrap _b, strata(treat post) [bsopts]: cic y treat post, vce(none}){p_end}
{p 4 8 0}However, the second line is slower because vce(bootstrap) is implemented in Mata and runs with less overhead.
However, the bootstrap prefix is more flexible due the availability of size(), strata(), cluster(), idcluster() and other options.{p_end}
{p 4 6 2}
CIC also appears to works with the {help svy bootstrap}: prefix, but you will need to use {help svyset} to set
up the bsrweight() variables, PSUs, weights and strata before calling CIC.{p_end}
{p 4 6 2}
The sample size for {opt size(#)} is defined as follows:
without weights, the sample in each group is calculated as the number of observations in the group;
with fweights, the sample in each group is calculated as the sum of the fweights for observations in the group;
with iweights, the sample in each group is calculated as the sum of the iweights for observations in the group,
divided by the sum of the weights for all observations, and multiplied by the value specified in size()
(rounded to the nearest integer -- by default, size()==the sum of the iweights);
with aweights, the weights are normalized to mean 1. then, the sample in each group is calculated as the sum
of the weights for observations in the group (rounded to the nearest integer).
This sub-option is allowed only with pweights, aweights, and iweights. With unweighted samples,
you could generate a variable equal to one and use it as an iweight.{p_end}


{marker description}{...}
{title:Description}

{pstd}
This Stata command, {cmd:cic}, implements the changes-in-changes (CIC) model proposed by Athey and Imbens (2006).
The command estimates the average and quantile treatment effects of a treatment in settings where
repeated cross sections of individuals are observed in a treatment group and a control group, before and after the treatment.
The CIC model relaxes several assumptions of the standard linear difference-in-differences model.
Both the continuous CIC model and discrete CIC model (with and without a conditional independence assumption)
are included in the cic command, as are treatment effects from
standard linear difference-in-differences model and a quantile difference-in-differences model.
By implementing the CIC estimator alongside these other two pre-existing estimators,
the {cmd:cic} command can illustrate how the effect the treatment varies across a variety of assumptions.


{marker remarks}{...}
{title:Remarks}

{pstd}
I wrote the code for this Stata implementation for one of my dissertation projects (Kranker 2011, 2016).
The code started as a simple "port" of {browse "https://athey.people.stanford.edu/research":Athey and Imbens' Matlab code}.
Then I made some changes to the code to speed it up in various ways and extend the methods.

{pstd}
While was rummaging through some old files in June 2019,
I ran across this code, thought it might be helpful to others, and decided to post the package online.
I see Blaise Melley also has {browse "https://sites.google.com/site/blaisemelly/home/computer-programs/cic_stata":his own version of the model for Stata}.
Let's hope this release prevents yet another person from doing this work!

{pstd}
The use of difference-in-differences (DID) methods is widespread in program evaluation and empirical
economics (Imbens and Wooldridge 2009). DID methods involve comparing outcomes between two groups across two time periods,
where only one of the two groups are exposed to the intervention in one of the periods.
The DID estimator calculates the difference in outcomes between the treatment and comparison groups after the intervention began,
minus the difference in outcomes between the treatment and comparison groups before the intervention began.
Or, equivalently, the DID estimator can be seen as the change in outcomes for the treatment group
before and after the intervention, minus the change in outcomes for the comparison group over the same time period.
It is straightforward to generalize this basic two group, two period DID model has been generalized in various ways—for example,
to adjust for observed covariates, include more than two groups, or include more than two time periods.

{pstd}
Athey and Imbens (2006) proposed a changes-in-changes (CIC) model which generalizes the DID model by relaxing relaxes several assumptions.
(Thus the standard DID model is nested in the CIC model as a special case.)
The CIC model estimates the entire distribution of outcomes under the counterfactual,
allowing one to calculate average treatment effects or estimate effects at specific quantiles.

{pstd}
This Stata command, {cmd:cic},  implements the CIC estimator from Athey and Imbens (2006).
{cmd:cic} is written in Mata with an effort to maximize parallel computing; in tests (not shown),
I found {cmd:cic} estimated the model more quickly than the Matlab code previously distributed by Athey and Imbens.
The {cmd:cic} command also offers several previously unavailable features (e.g., to allow for covariates).
In addition, you can use Stata's {help bootstrap}: prefix, which offers more flexibility for
computing bootstrapped standard errors (e.g., strata, blocks).

{pstd}
Imbens and Wooldridge (2009) provide a nice, short overview of the method.
Athey and Imbens (2006) explain their methods (with proofs) in a fairly long and complicated article.
The appendix is also quite helpful.

{pstd}
Covaraiates are implemented according to the parametric approach outlined by Athey and Imbens (2006):
"... apply the CIC estimator to the residuals from an ordinary least squares regression
with the effects of the dummy variables added back in." (p. 466)


{marker examples}{...}
{title:Examples}

{pstd}Setup{p_end}
{phang2}{cmd:. sysuse nlsw88, clear}{p_end}
{phang2}{cmd:. set seed 1}{p_end}
{phang2}{cmd:. gen TREAT = uniform() < .5}{p_end}
{phang2}{cmd:. replace wage = wage + TREAT}{p_end}
{phang2}{cmd:. gen POST = uniform() < .5}{p_end}
{phang2}{cmd:. replace wage = wage - POST}{p_end}

{pstd}Estimate{p_end}
{phang2}{cmd:. cic continuous wage TREAT POST, vce(bootstrap, reps(50))}{p_end}
{phang2}{cmd:. bootstrap, reps(50): cic all wage TREAT POST, at(50 90) did vce(none)}{p_end}
{phang2}{cmd:. cic all wage TREAT POST, vce(delta) at(50)}{p_end}
{phang2}{cmd:. cic dci wage TREAT POST i.occupation, at(50) vce(bootstrap, reps(50))}{p_end}


{marker author}{...}
{title:Author}

{pstd}
Stata code by Keith Kranker

{pstd}
Based on Matlab code by Susan Athey & Guido W. Imbens,
available at {browse "https://athey.people.stanford.edu/research"}


{marker references}{...}
{title:References}

{psee}
Athey, Susan and Guido W. Imbens. "Identification and Inference in Nonlinear Difference-in-Differences Models."
*Econometrica*, vol. 74, no. 2, March 2006, pp. 431-497. ({browse "http://dx.doi.org/10.1111/j.1468-0262.2006.00668.x"})

{psee}
Imbens, Guido W. and Jeffery M. Wooldridge. "Recent Developments in the Econometrics of Program Evaluation."
*Journal of Economic Literature*, vol. 47, no. 1, 2009, pp. 5–86. ({browse "http://dx.doi.org/10.1257/jel.47.1.5"})

{psee}
Kranker, Keith. "The Effect of Disease Management Programs on Medicaid Expenditures."
Doctoral dissertation. College Park, MD: University of Maryland, 2011. ({browse "http://hdl.handle.net/1903/12101"})

{psee}
Kranker, Keith. "Effects of Medicaid Disease Management Programs on Medical Expenditures: Evidence from a Natural Experiment in Georgia."
*Journal of Health Economics*, vol. 46, March 2016, pp. 52-69.
({browse "http://dx.doi.org/10.1016/j.jhealeco.2016.01.008"}){p_end}
