{smcl}
{* 27apr2021}{...}
{cmd:help mata mm_qr()}
{hline}

{title:Title}

{p 4 17 2}
{bf:mm_qr() -- Quantile regression}


{title:Syntax}

{pstd}
Simple syntax

{p 8 24 2}
{it:b} =
{cmd:mm_qrfit(}{it:y} [{cmd:,} {it:X}{cmd:,} {it:w}{cmd:,} {it:p}{cmd:,} {it:cons}{cmd:,} {it:b_init}{cmd:,} {it:relax}]{cmd:)}

{p 11 24 2}{bind:       }{it:y}:  {it:real colvector} containing dependent variable{p_end}
{p 11 24 2}{bind:       }{it:X}:  {it:real matrix} containing predictors{p_end}
{p 11 24 2}{bind:       }{it:w}:  {it:real colvector} containing weights; specify {cmd:1} for unweighted results{p_end}
{p 11 24 2}{bind:       }{it:p}:  evaluation probability; default is {bf:0.5} (median regression); may type {cmd:.} to select the default{p_end}
{p 11 24 2}{bind:    }{it:cons}:  whether to include a constant; default is {cmd:1}; specify {cmd:0} to omit the constant{p_end}
{p 11 24 2}{bind:  }{it:b_init}:  {it:real colvector} containing starting values; may type {cmd:.} to select default starting values{p_end}
{p 11 24 2}{bind:   }{it:relax}:  whether to return error in case of non-convergence; default is {cmd:0}; specify {cmd:1} to bypass error{p_end}


{pstd}
Advanced syntax

{pmore}
Initialize a new {cmd:mm_qr()} object

            {cmd:class mm_qr scalar} {it:S}
            or
            {it:S} = {cmd:mm_qr()}

{pmore}
Provide data

{p 12 24 2}
{it:S}{cmd:.data(}{it:y} [{cmd:,} {it:X}{cmd:,} {it:w}{cmd:,} {it:cons}]{cmd:)}

{p 11 24 2}{bind:       }{it:y}:  {it:real colvector} containing dependent variable{p_end}
{p 11 24 2}{bind:       }{it:X}:  {it:real matrix} containing predictors; may specify {cmd:.} or {cmd:J(0,0,.)} to omit {it:X}{p_end}
{p 11 24 2}{bind:       }{it:w}:  {it:real colvector} containing weights; specify {cmd:1} for unweighted results{p_end}
{p 11 24 2}{bind:    }{it:cons}:  whether to include a constant; default is {cmd:1}; specify {cmd:0} to omit the constant{p_end}

{p 12 12 2}
Specifying {it:S}{cmd:.data()} clears any results and starting values that may already exist in {it:S}.

{pmore}
Retrieve results

{p2colset 13 37 39 2}{...}
{p2col:{it:b}{bind:     } = {it:S}{cmd:.b()}}fitted coefficients (column vector){p_end}
{p2col:{it:xb}{bind:    } = {it:S}{cmd:.xb(}[{it:X}]{cmd:)}}linear predictions; may specify custom {it:X}; default
    is to use {it:X} from {it:S}{cmd:.data()}{p_end}
{p2col:{it:sdev}{bind:  } = {it:S}{cmd:.sdev()}}sum of weighted absolute deviations{p_end}
{p2col:{it:conv}{bind:  } = {it:S}{cmd:.converged()}}1 if converged, 0 else{p_end}
{p2col:{it:iter}{bind:  } = {it:S}{cmd:.iter()}}number of iterations{p_end}
{p2col:{it:gap}{bind:   } = {it:S}{cmd:.gap()}}duality gap{p_end}
{p2col:{it:n}{bind:     } = {it:S}{cmd:.n()}}number of observations (rows in {it:y}){p_end}
{p2col:{it:N}{bind:     } = {it:S}{cmd:.N()}}number of observations (sum of weights){p_end}
{p2col:{it:cons}{bind:  } = {it:S}{cmd:.cons()}}1 if model has a constant, 0 else{p_end}
{p2col:{it:k}{bind:     } = {it:S}{cmd:.k()}}number of predictors{p_end}
{p2col:{it:K}{bind:     } = {it:S}{cmd:.K()}}number of parameters ({it:S}{cmd:.k()}+{it:S}{cmd:.cons()}){p_end}
{p2col:{it:k_omit}{bind:} = {it:S}{cmd:.k_omit()}}number of omitted terms{p_end}
{p2col:{it:omit}{bind:  } = {it:S}{cmd:.omit()}}column vector flagging omitted terms (0/1){p_end}
{p2col:{it:ymean}{bind: } = {it:S}{cmd:.ymean()}}mean of {it:y}{p_end}
{p2col:{it:means}{bind: } = {it:S}{cmd:.means()}}means of {it:X} (row vector){p_end}

{p 12 12 2}
Specifying {it:S}{cmd:.b()}, {it:S}{cmd:.converged()}, {it:S}{cmd:.iter()}, {it:S}{cmd:.xb()}, {it:S}{cmd:.sdev()},
or {it:S}{cmd:.gap()} will trigger estimation, if not already carried out.

{pmore}
Settings

{p2col:{it:S}{cmd:.p(}{it:p}{cmd:)}}evaluation probability; default is {bf:0.5} (median regression){p_end}
{p2col:{it:S}{cmd:.b_init(}{it:b_init}{cmd:)}}{it:real colvector} containing starting values for the coefficients; the
    default is to use a least-squares fit; may specify {cmd:.z} to clear starting values{p_end}
{p2col:{it:S}{cmd:.tol(}{it:tol}{cmd:)}}convergence tolerance; default is {bf:1e-8}{p_end}
{p2col:{it:S}{cmd:.maxiter(}{it:maxiter}{cmd:)}}maximum number of iterations; default is as set by {helpb set maxiter}{p_end}
{p2col:{it:S}{cmd:.beta(}{it:beta}{cmd:)}}step length of the Frisch-Newton algorithm; default is {cmd:0.99995}{p_end}
{p2col:{it:S}{cmd:.qd(}{it:qd}{cmd:)}}whether to use quad precision; default is {cmd:1}; specify {cmd:0} for double precision{p_end}
{p2col:{it:S}{cmd:.demean(}{it:demean}{cmd:)}}whether to use demeaning; default is {cmd:1}; specify {cmd:0} to skip demeaning{p_end}
{p2col:{it:S}{cmd:.collin(}{it:collin}{cmd:)}}whether to identify collinear terms; default is {cmd:1}; specify {cmd:0} to skip{p_end}
{p2col:{it:S}{cmd:.log(}{it:log}{cmd:)}}whether to display information on estimation progress; default is {cmd:0} (no display); specify
    {cmd:1} for an iteration log, {cmd:2} for progress dots (including line break at end), or {cmd:3} for progress dots 
    (without line break){p_end}

{p 12 12 2}
Settings can be changed at any time, but note that changing a setting will clear the
estimated results (but not the data). An exception is {it:S}{cmd:.log()} which does not
clear anything. Furthermore, {it:S}{cmd:.b_init()} is only allowed if {it:S}{cmd:.data()} has
already been applied.

{p 12 12 2}
All functions for settings can be used without argument to retrieve a setting. For
example, type {it:b_init} = {it:S}{cmd:.b_init()} to retrieve the vector of starting
values. The functions do not clear results if used in this form.


{title:Description}

{pstd}
{cmd:mm_qr()} fits a quantile regression model using an interior point (Frisch-Newton)
algorithm (Portnoy and Koenker 1997). It solves the same estimation
problem as official Stata's {helpb qreg} (which is based on the linear
programming technique), although results may be slightly different
due to the optimization problem being a step-function (meaning that multiple acceptable
solutions may exist). See {helpb qrprocess} (Chernozhukov, Fernandez-Val, and
Melly 2020) for a Stata implementation of quantile regression based on the
interior point algorithm. Note that the interior point algorithm tends to be
faster than the linear programming technique in large datasets.

{pstd}
{cmd:mm_qr()} uses quad precision and demeaning (unless the
constant is excluded) when computing cross products. Specifying {it:quad}=0 and/or
{it:demean}=0 will make {cmd:mm_qr()} faster, but possibly less precise. {it:demean}=0 is
potentially more harmful than {it:quad}=0 (and typically less effective in terms of
speed gains). Use {it:quad}=0 and {it:demean}=0 only if your data is well-behaved (reasonable
means, not much collinearity). For models without constant ({it:cons}=0), argument {it:demean} has no
effect. This is because mean-deviation formulas are not applicable in this case
(meaning that models without constant will generally be affected by precision
issues if the data is not well-behaved).

{pstd}
{cmd:mm_qr()} takes special action to identify collinear predictors and excludes
them during estimation. The coefficients for these terms will be set to 0 in the
resulting coefficient vector. If collinear predictors have already been removed
from {it:X} you can specify {it:collin}=0 to skip these extra computations (the
additional cost for these computations is only small; it is essentially zero if no
starting values are provided).


{title:Examples}

{pstd}
If you are only interested in the coefficients, you can use
{cmd:mm_qrfit()} (simple syntax) to obtain a quick quantile regression fit without much typing:

        . {stata sysuse auto}
        . {stata qreg price weight length foreign}
        . {stata "mata:"}
        : {stata y = st_data(., "price")}
        : {stata X = st_data(., "weight length foreign")}
        : {stata mm_qrfit(y, X)}
        : {stata end}

{pstd}
In this example, the solution by {cmd:mm_qrfit()} is somewhat different from
the solution by {helpb qreg}. Both solutions, however, are equally valid as they
both minimize the sum of absolute deviations. This can be seen in the next
example

{pstd}
For more sophisticated applications, use the advanced syntax. Example:

        . {stata "mata:"}
        : {stata S = mm_qr()}    // initialize object
        : {stata S.data(y, X)}   // declare data
        : {stata S.p(0.5)}       // request median regression
        : {stata S.b()}          // compute result
        : {stata S.iter()}       // number of iterations?
        : {stata S.sdev()}       // sum of weighted absolute deviations?
        : {stata S.p(0.25)}      // request 0.25-quantile regression using same data
        : {stata S.log(1)}       // turn iteration log on
        : {stata S.b()}          // compute result
        : {stata end}


{title:Diagnostics}

{pstd}
Missing values in {it:y}, {it:X}, or {it:w} will lead to error or invalid
results.

{pstd}
To preserve memory, {cmd:mm_qr()} does not store a copy of the data in {it:S}
(it only stores pointers). Modifying {it:y}, {it:X} or
{it:w} after {it:S}{cmd:.data()} has been applied will change the data used
by {cmd:mm_qr()}, but it will not clear existing results in {it:S}. This may lead to
inconsistent results. It is best not to modify {it:y}, {it:X} and {it:w} as
long as {it:S} exists.


{title:Reference}

{phang}
Chernozhukov, V., I. Fernandez-Val, B. Melly. 2020. QRPROCESS: Stata module
for quantile regression: fast algorithm, pointwise and uniform inference.
Statistical Software Components S458763. Available from
{browse "http://ideas.repec.org/c/boc/bocode/s458763.html"}.

{phang}
Portnoy, S., R. Koenker. 1997. The Gaussian hare and the Laplacian
tortoise: computability of squared-error versus absolute-error
estimators. Statistical Science 12(4):279-300.


{title:Source code}

{pstd}
{help moremata11_source##mm_qr:mm_qr.mata}


{title:Author}

{pstd}
Ben Jann, University of Bern, ben.jann@soz.unibe.ch
{p_end}
{pstd}
Blaise Melly, University of Bern, blaise.melly@vwi.unibe.ch


{title:Also see}

{p 4 13 2}
Online:  help for
{helpb moremata}, {helpb mf_mm_ls:mm_ls()}, {helpb qreg}, {helpb qrprocess} (from SSC)
