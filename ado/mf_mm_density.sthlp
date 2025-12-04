{smcl}
{* 19nov2022}{...}
{cmd:help mata mm_density()}
{hline}

{title:Title}

{pstd}
    {bf:mm_density() -- Density estimation}


{title:Syntax}

{dlgtab:Initialize density estimation object}

        {cmd:class mm_density scalar} {it:D}
        or
        {it:D} {cmd:= mm_density()}

{dlgtab:Setup}

{pstd}Provide data

        {it:D}{cmd:.data(}{it:X} [{cmd:,} {it:w}{cmd:,} {it:pw}{cmd:,} {it:sorted}]{cmd:)}

{p 12 14 2}{it:X} is a {it:real colvector} containing data (missing values not allowed)

{p 12 14 2}{it:w} is a {it:real colvector} containing weights (missing or negative values not allowed); default is {it:w} = 1 (no weights)

{p 12 14 2}{it:pw}!=0 indicates that weights are sampling weights; default is {it:pw} = 0

{p 12 14 2}{it:sorted}!=0 indicates that {it:X} is sorted and non-missing, that {it:w} is non-missing and
non-negative, and that {it:X} does not contain values that are outside of the support defined by
{it:D}{cmd:.support()}; default is {it:sorted} = 0; specify {it:sorted}!=0 to save some computer time
if you know that these conditions are true

{pstd}Choose kernel

        {it:D}{cmd:.kernel(}{it:kernel} [{cmd:,} {it:adapt}]{cmd:)}

{p 12 14 2}{it:kernel} is a {it:string scalar} specifying the kernel name; available
kernels are {cmd:"epanechnikov"},
{cmd:"epan2"}, {cmd:"biweight"}, {cmd:"triweight"},
{cmd:"cosine"}, {cmd:"gaussian"}, {cmd:"parzen"},
{cmd:"rectangle"}, and {cmd:"triangle"}
(abbreviations allowed); default is {cmd:"gaussian"}; specifying {cmd:""} 
(empty string) selects the default

{p 12 14 2}{it:adapt} is a {it:real scalar} specifying the number of stages of the adaptive
kernel density estimator; {it:adapt} may not be negative; default is {it:adapt} = 0 
(non-adaptive kernel); specifying {cmd:.} (missing) selects the default

{pstd}Set bandwidth or choose bandwidth selection method

        {it:D}{cmd:.bw(}{it:bw} [{cmd:,} {it:adjust}{cmd:,} {it:dpi}{cmd:,} {it:qui}]{cmd:)}

{p 12 14 2}{it:bw} is a (strictly positive) {it:real scalar} specifying the value 
of the bandwidth or a {it:string scalar} specifying the bandwidth selection 
rule; available rules are {cmd:"silverman"} (optimal of Silverman),
{cmd:"normalscale"} (normal scale rule), {cmd:"oversmoothed"} (oversmoothed rule),
{cmd:"sjpi"} (Sheather-Jones solve-the-equation plug-in), {cmd:"dpi"}
(Sheather-Jones direct plug-in), and {bf:"isj"} (bandwidth based on
diffusion estimator); default is {cmd:"sjpi"}; specifying {cmd:""} (empty string)
or {cmd:.} (missing) selects the default

{p 12 14 2}{it:adjust} is a {it:real scalar} specifying a multiplication factor
for the bandwidth; {it:adjust} must be strictly positive; default is 
{it:adjust} = 1; specifying {cmd:.} (missing) selects the default

{p 12 14 2}{it:dpi} is a {it:real scalar} specifying the number of stages of functional
estimation for the {cmd:"dpi"} rule; {it:dpi} may not be negative; default is 
{it:dpi} = 2; specifying {cmd:.} (missing) selects the default

{p 12 14 2}{it:qui}!=0 suppresses the warning message that is displayed if
SJPI or ISJ fails; default is {it:qui} = 0

{pstd}Define support of data and choose boundary correction method

        {it:D}{cmd:.support(}{it:minmax} [{cmd:,} {it:method}{cmd:,} {it:rd}]{cmd:)}

{p 12 14 2}{it:minmax} is a {it:real vector} specifying the lower and upper bounds
of the support of {it:X}; specify one value (lower boundary only) or
two values (lower and upper boundary); value {cmd:.} (missing) is interpreted as
(minus) infinity (i.e. unbounded); default is {it:minmax} = {cmd:(.,.)} 
(unbounded support)

{p 12 14 2}{it:method} is a {it:string scalar} specifying the boundary-correction
method to be used; available methods are {cmd:"renormalization"},
{cmd:"reflection"}, and {bind:{cmd:"linear correction"}} (abbreviations allowed); default
is {cmd:"renormalization"}; specifying {cmd:""} 
(empty string) selects the default

{p 12 14 2}{it:rd}!=0 indicates that the data is to be interpreted as relative data (relative ranks); {it:minmax} defaults to
{cmd:(0,1)} in this case and automatic bandwidth selection is modified; values outside [0,1] are not allowed
in {it:minmax} if {it:rd}!=0; default is {it:rd} = 0

{pstd}Set grid size of approximation estimator

        {it:D}{cmd:.n(}{it:n} [{cmd:,} {it:pad}]{cmd:)}

{p 12 14 2}{it:n} is a {it:real scalar} specifying the grid size used by the fast approximation
estimator (and the bandwidth selectors); default 
is {it:n} = 2^10 = 1024; specifying {cmd:.} (missing) selects the default

{p 12 14 2}{it:pad} is a {it:real scalar} specifying the padding proportion of the approximation
grid; {it:pad} may not be negative; the default is {it:pad} = 0.1, which means that the grid will be padded
by 10 percent of the observed data range on each side of the data (unless the support of the
data has been restricted by {it:D}{cmd:.support()}, in which case the approximation grid
will span the defined support); specifying {cmd:.} (missing) selects the default

{pstd}Retrieve settings

{p2colset 9 30 32 2}{...}
{p2col:{it:X}{space 5} = {it:D}{cmd:.X()}}{it:real colvector} containing data
    {p_end}
{p2col:{it:w}{space 5} =  {it:D}{cmd:.w()}}{it:real colvector} containing weights
    {p_end}
{p2col:{it:nobs}{space 2} =  {it:D}{cmd:.nobs()}}{it:real scalar} containing number of observations (sum of weights)
    {p_end}
{p2col:{it:pw}{space 4} =  {it:D}{cmd:.pw()}}{it:real scalar} containing {it:pw} flag
    {p_end}
{p2col:{it:sorted} =  {it:D}{cmd:.sorted()}}{it:real scalar} containing {it:sorted} flag
    {p_end}
{p2col:{it:kernel} =  {it:D}{cmd:.kernel()}}{it:string scalar} containing kernel name
    {p_end}
{p2col:{it:adapt}{space 1} =  {it:D}{cmd:.adapt()}}{it:real scalar} containing number of stages of adaptive estimator
    {p_end}
{p2col:{it:kh}{space 4} =  {it:D}{cmd:.kh()}}{it:real scalar} containing canonical bandwidth of kernel
    {p_end}
{p2col:{it:bw}{space 4} =  {it:D}{cmd:.bw()}}{it:string scalar} containing name of bandwidth selection rule or {it:real scalar} containing user-provided bandwidth
    {p_end}
{p2col:{it:adjust} =  {it:D}{cmd:.adjust()}}{it:real scalar} containing bandwidth adjustment factor
    {p_end}
{p2col:{it:dpi}{space 3} =  {it:D}{cmd:.dpi()}}{it:real scalar} containing number of DPI levels
    {p_end}
{p2col:{it:minmax} =  {it:D}{cmd:.support()}}{it:real rowvector} containing lower and upper bound of support
    {p_end}
{p2col:{it:lb}{space 4} =  {it:D}{cmd:.lb()}}{it:real scalar} containing lower bound of support
    {p_end}
{p2col:{it:ub}{space 4} =  {it:D}{cmd:.ub()}}{it:real scalar} containing upper bound of support
    {p_end}
{p2col:{it:bc}{space 4} =  {it:D}{cmd:.bc()}}{it:string scalar} containing name of boundary-correction method
    {p_end}
{p2col:{it:rd}{space 4} =  {it:D}{cmd:.rd()}}{it:real scalar} containing {it:rd} flag
    {p_end}
{p2col:{it:n}{space 5} =  {it:D}{cmd:.n()}}{it:real scalar} containing grid size of approximation estimator
    {p_end}
{p2col:{it:pad}{space 3} =  {it:D}{cmd:.pad()}}{it:real scalar} containing padding proportion of approximation grid
    {p_end}

{pstd}
    Technical remarks

{phang}
{bind:  o }Changing any of the above settings will clear all results that may already exist in {it:D}.

{phang}
{bind:  o }To preserve memory, {it:D}{cmd:.data()} does not store a copy of the
    data in {it:D}; it only stores pointers to {it:X} and {it:w}. Modifying {it:X}
    or {it:w} after {it:D}{cmd:.data()} has been applied will change the data used
    by {cmd:mm_density()}, but it will not clear existing results in
    {it:D}. This may lead to inconsistent results. It is best not to modify
    {it:X} and {it:w} as long as {it:D} is active.

{dlgtab:Estimation}

{pstd}Compute density estimate

        {it:d} = {it:D}{cmd:.d(}{it:at} [{cmd:,} {it:exact}]{cmd:)}
        or
        {it:d} = {it:D}{cmd:.d(}k{cmd:,} {it:from}{cmd:,} {it:to} [{cmd:,} {it:exact}]{cmd:)}

{p 12 14 2}{it:at} is a {it:real vector} specifying custom values at which the density is to be estimated

{p 12 14 2}{it:k} is a {it:real scalar} specifying the size of the grid over which the density is to be estimated
(regular grid with {it:k} points)

{p 12 14 2}{it:from} is a {it:real scalar} specifying the lowest value of the
grid; specify {it:from} >= {cmd:.} to determine the value automatically. Let {it:xmin}
be the minimum of the data, let {it:tau} be a padding value depending on
bandwidth and kernel, and let {it:lb} be the lower bound of the support as set
by {it:D}{cmd:.support()} (or minus infinity if unbounded). The lowest value of
the grid is then determined as follows:

{p2colset 17 37 39 2}{...}
{p2col:if {it:from} < {cmd:.}:}{it:from}
    {p_end}
{p2col:else if {it:from} = {cmd:.z}:}{it:xmin}
    {p_end}
{p2col:else if {it:from} = {cmd:.y}:}max({it:lb}, {it:xmin}-{it:tau})
    {p_end}
{p2col:else if bounded:}{it:lb}
    {p_end}
{p2col:else:}{it:xmin}-{it:tau}
    {p_end}

{p 12 14 2}{it:to} is a {it:real scalar} specifying the largest value of the
grid; specify {it:to} >= {cmd:.} to determine the value automatically (analogously to
{it:from})

{p 12 14 2}{it:exact}!=0 requests the exact density estimator to be used; default is
{it:exact} = 0 (approximation estimator)

{pstd}
    Retrieve results after estimation

{p2colset 9 22 24 2}{...}
{p2col:{it:d}{space 1} = {it:D}{cmd:.d()}}{it:real colvector} containing density estimate fitted last
    {p_end}
{p2col:{it:at} = {it:D}{cmd:.at()}}{it:real colvector} containing evaluation points of density estimate
    {p_end}

{pstd}
    Self-threading functions

{p2colset 9 22 24 2}{...}
{p2col:{it:h}{space 1} = {it:D}{cmd:.h()}}{it:real scalar} containing bandwidth
    {p_end}
{p2col:{it:D}{space 1} = {it:D}{cmd:.D()}}{it:real colvector} containing full-grid approximation estimator
    {p_end}
{p2col:{it:AT} = {it:D}{cmd:.AT()}}{it:real colvector} containing evaluation grid of approximation estimator
    {p_end}
{p2col:{it:W}{space 1} = {it:D}{cmd:.W()}}{it:real colvector} containing grid counts of approximation estimator
    {p_end}
{p2col:{it:L}{space 1} = {it:D}{cmd:.L()}}{it:real colvector} containing local bandwidth factors of approximation estimator
    {p_end}
{p2col:{it:l}{space 1} = {it:D}{cmd:.l()}}{it:real colvector} containing local bandwidth factors of exact estimator
    {p_end}

{pmore}
    The self-threading functions can be used irrespective of whether {it:D}{cmd:.d()} has been applied or
    not; results will be the same.

{dlgtab:Other functions}

{pstd}Obtain levels or first derivatives of kernel function using current settings
(including boundary correction)

        {it:levels}      {cmd:=} {it:D}{cmd:.K(}{it:X}{cmd:,} {it:at}{cmd:,} {it:h}{cmd:)}
        {it:derivatives} {cmd:=} {it:D}{cmd:.Kd(}{it:X}{cmd:,} {it:at}{cmd:,} {it:h}{cmd:)}

{p 12 14 2}{it:X} is a {it:real colvector} containing the points at which the levels or derivatives will be evaluated

{p 12 14 2}{it:at} is a {it:real colvector} containing kernel locations (midpoints)

{p 12 14 2}{it:h} is a {it:real colvector} containing kernel bandwidths

{p 12 14 2}{it:X}, {it:at}, and {it:h} are required to be r-conformable 
(see {helpb m6_glossary##r-conformability:{bind:[M-6] glossary}}).


{title:Description}

{pstd}
    {cmd:mm_density()} is a class-based system for univariate kernel density
    estimation.


{title:Examples}

{pstd}
    Density estimate with default settings:

        . {stata "mata:"}
        : {stata x = rnormal(1000, 1, 0, 1)}
        : {stata S = mm_density()}
        : {stata S.data(x)}
        : {stata S.d(13, -3, 3), S.at()}
        : {stata end}

{pstd}
    Display full grid approximation estimator:

        . {stata "mata:"}
        : {stata x = rnormal(50000,1, 0, 1)  \ rnormal(50000,1, 4, 1.5)}
        : {stata S = mm_density()}
        : {stata S.data(x)}
        : {stata mm_plot((S.D(), S.AT()), "line")}
        : {stata end}

{pstd}
    Compute bandwidth (without estimating the density):

        . {stata "mata:"}
        : {stata x = rnormal(1000,1, 0, 1)  \ rnormal(1000,1, 4, 1.5)}
        : {stata S = mm_density()}
        : {stata S.data(x)}
        : {stata S.bw("sj");     S.h()}  // Sheather-Jones solve-the-equation
        : {stata S.bw("dpi");    S.h()}  // Sheather-Jones direct plug-in
        : {stata S.bw("isj");    S.h()}  // diffusion estimator bandwidth
        : {stata S.bw("silver"); S.h()}  // optimal of Silverman
        : {stata S.bw("over");   S.h()}  // oversmooothed rule
        : {stata S.bw("normal"); S.h()}  // normal scale rule
        : {stata end}


{title:Source code}

{pstd}
    {help moremata11_source##mm_density:mm_density.mata}


{title:Methods and Formulas}

{pstd}
For a brief overview of the methods used by {cmd:mm_density()} see
{browse "http://boris.unibe.ch/69421/2/kdens.pdf":Jann (2007)}. A book-length
treatment can be found in Wand and Jones (1995). Not covered in these
references is the improved Sheather-Jones bandwidth selector based on diffusion
methods; see {browse "http://doi.org/10.1214/10-AOS799":Botev et al. (2010)}.


{title:References}

{phang}
    Botev, Z.I., J.F. Grotowski, and D.P. Kroese (2010). Kernel density
    estimation via diffusion. Annals of Statistics
    38(5): 2916-2957. DOI: {browse "http://doi.org/10.1214/10-AOS799":10.1214/10-AOS799}.
    {p_end}
{phang}
    Jann, B. (2007). Univariate kernel density
    estimation. DOI: {browse "http://boris.unibe.ch/69421/2/kdens.pdf":10.7892/boris.69421}.
    {p_end}
{phang}
    Wand, M.P., M.C. Jones (1995). Kernel Smoothing. London: Chapman and Hall.
    {p_end}


{title:Author}

{pstd} Ben Jann, University of Bern, ben.jann@soz.unibe.ch


{title:Also see}

{psee}
Online:  help for
{helpb mf_mm_ddens:mm_ddens()},
{helpb mf_mm_ecdf:mm_ecdf()},
{helpb mf_mm_histogram:mm_histogram()},
{helpb kdensity},
{helpb histogram},
{helpb moremata}
{p_end}

