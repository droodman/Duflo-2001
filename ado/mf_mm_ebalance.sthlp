{smcl}
{* 10dec2021}{...}
{cmd:help mata mm_ebalance()}
{hline}

{title:Title}

{pstd}
    {bf:mm_ebalance() -- Entropy balancing}


{title:Syntax}

{pstd}
Initialize a new {cmd:mm_ebalance()} object

        {cmd:class mm_ebalance scalar} {it:S}
        or
        {it:S} = {cmd:mm_ebalance()}

{pstd}
Settings

{p2colset 9 30 32 2}{...}
{p2col:{it:S}{cmd:.tau(}{it:tau}{cmd:)}}target sum of balancing weights; {it:tau}
    can be a (strictly positive) real number or, alternatively, {cmd:"Wref"}
    (sum of weights in reference sample), {cmd:"W"} (sum of weight in main
    sample), {cmd:"Nref"} (number of rows in {it:Xref}), or {cmd:"N"} (number of
    rows in {it:X}); default is {cmd:"Wref"}
    {p_end}
{p2col:{it:S}{cmd:.scale(}{it:scale}{cmd:)}}scales to be used for standardization; 
    {it:scale} can be a (strictly positive) real vector providing custom values 
    (same length as columns in {it:X}) or, alternatively, 
    {cmd:"main"} (standard deviations from main sample), {cmd:"ref"}
    (standard deviations from reference sample), {cmd:"avg"} 
    (average between {cmd:main} and {cmd:ref}), {cmd:"wavg"} (weighted average 
    between {cmd:main} and {cmd:ref}), or {cmd:pooled} (standard deviations from
    pooled sample); standard deviations are computed using population formulas (division
    by N rather than N-1); scales equal to 0 will be reset to 1; default is {cmd:"main"}
    {p_end}
{p2col:{it:S}{cmd:.}[{cmd:no}]{cmd:adj(}{it:p}{cmd:)}}selects the means to be
    adjusted where {it:p} is a vector of indices referring to columns of {it:X};
    {it:S}{cmd:.adj(}{it:p}{cmd:)} causes all selected means to be adjusted and
    all non-selected means to be held fixed; alternatively, 
    {it:S}{cmd:.noadj(}{it:p}{cmd:)} causes all selected means to be held fixed
    and all non-selected means to be adjusted; the elements in {it:p} must be
    >= 1; duplicates and elements pointing to columns that do not exist
    will be ignored; specify {it:S}{cmd:.adj(.)} to adjust all means (the default)
    {p_end}
{p2col:{it:S}{cmd:.btol(}{it:btol}{cmd:)}}balancing tolerance; default is {bf:1e-6};
    balancing is achieved if the balancing loss is smaller than {it:btol}
    {p_end}
{p2col:{it:S}{cmd:.ltype(}{it:ltype}{cmd:)}}type of balancing loss function, can be {cmd:"reldif"}
    (maximum relative difference; the default), {cmd:"absdif"} 
    (maximum absolute difference), or {cmd:"norm"} (Euclidean norm of differences)
    {p_end}
{p2col:{it:S}{cmd:.etype(}{it:etype}{cmd:)}}evaluator type, can be {cmd:"bl"}
    (evaluator based on balancing loss; the default), {cmd:"wl"} 
    (evaluator based on distribution of weights), {cmd:"mm"} (method of moments
    evaluator), or {cmd:"mma"} (method of moments evaluator including intercept);
    when using an evaluator other than {cmd:"bl"} you might want to reduce 
    {it:vtol} a bit (e.g. to {bf:1e-10})
    {p_end}
{p2col:{it:S}{cmd:.trace(}{it:trace}{cmd:)}}trace level; default is {cmd:"value"};
    specify {cmd:"none"} to suppress iteration log; see
    {helpb mf_optimize##i_tracelevel:optimize()} for details
    {p_end}
{p2col:{it:S}{cmd:.maxiter(}{it:maxiter}{cmd:)}}maximum number of iterations;
    default is as set by {helpb set maxiter}
    {p_end}
{p2col:{it:S}{cmd:.ptol(}{it:ptol}{cmd:)}}convergence tolerance for coefficient
    vector; default is {bf:1e-6}; convergence is reached if {it:ptol} or {it:vtol} is
    satisfied; also see {helpb mf_optimize##i_ptol:optimize()}
    {p_end}
{p2col:{it:S}{cmd:.vtol(}{it:vtol}{cmd:)}}convergence tolerance for the minimization
    criterion; default is {bf:1e-7}; convergence is reached if {it:ptol} or {it:vtol} is
    satisfied; also see {helpb mf_optimize##i_ptol:optimize()}
    {p_end}
{p2col:{it:S}{cmd:.difficult(}{it:diff}{cmd:)}}stepping
    algorithm to be used in nonconcave regions; {it:diff}=0 (the default) uses
    the standard algorithm; {it:diff}!=0 uses an alternative algorithm; see the
    singular H methods in {helpb mf_optimize##i_singularH:optimize()} and
    the description of the {cmd:difficult} option in {helpb maximize}
    {p_end}
{p2col:{it:S}{cmd:.nostd(}{it:nostd}{cmd:)}}whether standardization should be applied
    during optimization to improve robustness of the algorithm; {it:nostd}=0 (the default)
    fits the coefficients using standardized data and then back-transforms the
    results; {it:nostd}=1 omits such standardization (not recommended); in any case,
    the balancing loss of the final fit will be obtained from the original data 
    {p_end}
{p2col:{it:S}{cmd:.nowarn(}{it:nowarn}{cmd:)}}whether "convergence not achieved"
    and "balance not achieved" messages should be displayed; {it:nowarn}=0 (the default)
    displays the messages; {it:nowarn}!=0 suppresses the messages{p_end}

{p 8 8 2}
Settings can be changed at any time, but changing a setting will clear
any results that may already exist in {it:S} (but not the data).

{p 8 8 2}
All functions for settings can be used without argument to retrieve a setting. For
example, type {it:ltype} = {it:S}{cmd:.ltype()} to retrieve the setting for the
type of loss function. The functions do not clear results if used in this form.

{pstd}
Provide data

{p 8 20 2}
{it:S}{cmd:.data(}{it:X}{cmd:,} {it:w}{cmd:,} {it:Xref}{cmd:,} {it:wref} [, {it:fast}]{cmd:)}

{p 7 20 2}{bind:       }{it:X}:  {it:real matrix} containing the main data (the data to be
    reweighted); rows are observations, columns are variables
    {p_end}
{p 7 20 2}{bind:       }{it:w}:  {it:real colvector} containing base weights;
    use {it:w} = {cmd:1} for unweighted data; if not scalar, {it:w} must have
    the same number of rows as {it:X}
    {p_end}
{p 7 20 2}{bind:    }{it:Xref}:  {it:real matrix} containing the reference data;
    rows are observations, columns are variables; {it:Xref} must have the same
    number of columns as {it:X}; if {it:Xref} only has a single row, it is
    assumed to contain population averages
    {p_end}
{p 7 20 2}{bind:    }{it:wref}:  {it:real colvector} containing base weights
    for the reference data; use {it:wref} = {cmd:1} for unweighted data; if not
    scalar, {it:wref} must have the same number of rows as {it:Xref}; if
    {it:Xref} only has a single row, {it:wref} is assumed to contain the
    population size
    {p_end}
{p 7 20 2}{bind:    }{it:fast}:  {it:real scalar} indicating whether checks for
    missing values and negative base weights should be skipped (to save computer
    time); specify {it:fast}!=0 to skip the checks; omitted {it:fast} is
    equivalent to {it:fast}=0; specify {it:fast}!=0 only if you are certain that
    there are no missing values and that there are no negative base weights;
    {cmd:mm_ebalance()} may break or return invalid results if either condition is violated
    {p_end}

{p 8 8 2}
Specifying {it:S}{cmd:.data()} clears any results that may already exist in {it:S}.

{pstd}
Retrieve results

{p2colset 9 34 36 2}{...}
{p2col:{it:b}{bind:      } = {it:S}{cmd:.b()}}fitted coefficients (column vector){p_end}
{p2col:{it:a}{bind:      } = {it:S}{cmd:.a()}}normalizing intercept (scalar){p_end}
{p2col:{it:wbal}{bind:   } = {it:S}{cmd:.wbal()}}balancing weights: {it:w} * exp({it:Xb} + {it:a}){p_end}
{p2col:{it:xb}{bind:     } = {it:S}{cmd:.xb()}}linear predictions: {it:Xb} + {it:a}{p_end}
{p2col:{it:pr}{bind:     } = {it:S}{cmd:.pr()}}propensity scores: invlogit({it:Xb} + {it:a} + ln({it:Wref}/{it:tau})){p_end}
{p2col:{it:madj}{bind:   } = {it:S}{cmd:.madj()}}means of {it:X} after reweighting{p_end}
{p2col:{it:wsum}{bind:   } = {it:S}{cmd:.wsum()}}total of balancing weights: sum({it:wbal}){p_end}
{p2col:{it:loss}{bind:   } = {it:S}{cmd:.loss()}}balancing loss at final fit{p_end}
{p2col:{it:bal}{bind:    } = {it:S}{cmd:.balanced()}}1 if balance is achieved ({it:loss} < {it:btol}), 0 else{p_end}
{p2col:{it:value}{bind:  } = {it:S}{cmd:.value()}}value of the optimization criterion at final fit{p_end}
{p2col:{it:iter}{bind:   } = {it:S}{cmd:.iter()}}number of iterations of the optimization algorithm{p_end}
{p2col:{it:conv}{bind:   } = {it:S}{cmd:.converged()}}1 if the optimization algorithm converged, 0 else{p_end}
{p2col:{it:IF_b}{bind:   } = {it:S}{cmd:.IF_b()}}influence functions of coefficients (main sample){p_end}
{p2col:{it:IFref_b}{bind:} = {it:S}{cmd:.IFref_b()}}influence functions of coefficients (reference sample){p_end}
{p2col:{it:IF_a}{bind:   } = {it:S}{cmd:.IF_a()}}influence function of intercept (main sample){p_end}
{p2col:{it:IFref_a}{bind:} = {it:S}{cmd:.IFref_a()}}influence function of intercept (reference sample){p_end}

{p 8 8 2}
Specifying any of the above functions will trigger estimation, if not already carried out.

{pstd}
Retrieve information on data

{p2colset 9 34 36 2}{...}
{p2col:{it:k}{bind:      } = {it:S}{cmd:.k()}}number of variables (columns) in {it:X} (number of coefficients){p_end}
{p2col:{it:omit}{bind:   } = {it:S}{cmd:.omit()}}column vector flagging collinear variables (omitted coefficients){p_end}
{p2col:{it:k_omit}{bind: } = {it:S}{cmd:.k_omit()}}number of collinear variables (omitted coefficients){p_end}
{p2col:{it:X}{bind:      } = {it:S}{cmd:.X()}}data of main sample{p_end}
{p2col:{it:w}{bind:      } = {it:S}{cmd:.w()}}base weights of main sample{p_end}
{p2col:{it:N}{bind:      } = {it:S}{cmd:.N()}}number of rows in main sample{p_end}
{p2col:{it:W}{bind:      } = {it:S}{cmd:.W()}}size (sum of weights) of main sample{p_end}
{p2col:{it:m}{bind:      } = {it:S}{cmd:.m()}}means of {it:X} (before reweighting){p_end}
{p2col:{it:s}{bind:      } = {it:S}{cmd:.s()}}standard deviations of {it:X}{p_end}
{p2col:{it:Xref}{bind:   } = {it:S}{cmd:.Xref()}}data of reference sample{p_end}
{p2col:{it:wref}{bind:   } = {it:S}{cmd:.wref()}}base weights of reference sample{p_end}
{p2col:{it:Nref}{bind:   } = {it:S}{cmd:.Nref()}}number of rows in reference sample{p_end}
{p2col:{it:Wref}{bind:   } = {it:S}{cmd:.Wref()}}size (sum of weights) of reference sample{p_end}
{p2col:{it:mref}{bind:   } = {it:S}{cmd:.mref()}}means of {it:Xref}{p_end}
{p2col:{it:sref}{bind:   } = {it:S}{cmd:.sref()}}standard deviations of {it:Xref}{p_end}
{p2col:{it:scale}{bind:  } = {it:S}{cmd:.scale()}}scales used for standardization{p_end}
{p2col:{it:mu}{bind:     } = {it:S}{cmd:.mu()}}target values for means of reweighted {it:X}{p_end}
{p2col:{it:tau}{bind:    } = {it:S}{cmd:.tau()}}target sum of balancing weight{p_end}


{title:Description}

{pstd}
    {cmd:mm_ebalance()} performs entropy balancing (Hainmueller 2012). The
    goal of {cmd:mm_ebalance()} is to find a vector of weights such that the
    means in the reweighted data of a given sample match the means in the data
    of a reference sample (i.e., to find weights that balance the means between
    the two samples) or, equivalently, to find weights that adjust the sample
    means to known population values.

{pstd}
    Estimation of the required coefficients is performed by calling Mata's
    {helpb mf_optimize:optimize()}. Collinear terms will be ignored during
    optimization. However, these terms will be taken into account when computing
    the final balancing loss after optimization has completed.

{pstd}
    In addition to the balancing weights and the coefficients,
    {cmd:mm_ebalance()} also provides influence functions that are useful for
    the computation of standard errors of the coefficients and for the correction
    of standard errors of statistics that have been estimated from reweighted
    data; see {browse "http://ideas.repec.org/p/bss/wpaper/39.html":Jann (2021b)},
    which also provides details on the methods and formulas implemented in
    {cmd:mm_ebalance()}.

{pstd}
    A wrapper for {cmd:mm_ebalance()} that obtains balancing weights in a single
    line of code is provided as {helpb mf_mm_wbal:mm_wbal()}. Furthermore, an
    older function for entropy balancing with somewhat different
    features is available as {helpb mf_mm_ebal:mm_ebal()}.

{pstd}
    See {helpb ebalfit} (Jann 2021a) for an easy to use Stata implementation
    of entropy balancing that is based on {cmd:mm_ebalance()}. For an alternative
    Stata implementation see {helpb ebalance} by Hainmueller and Xu (2011, 2013).


{title:Examples}

{dlgtab:Balance subsamples}

        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(union, hours, ttl_exp, tenure)}
        . {stata generate byte nonunion = 1 - union}
        . {stata "mata:"}
        : {stata X    = st_data(., "hours ttl_exp tenure", "union")}
        : {stata Xref = st_data(., "hours ttl_exp tenure", "nonunion")}
        : {stata S = mm_ebalance()}
        : {stata S.data(X, 1, Xref, 1)}
        : {stata mean(Xref)', mean(X)', mean(X, S.wbal())'}
        : {stata rows(Xref), rows(X), sum(S.wbal())}
        : {stata S.b()}        {it:(obtain coefficients)}
        : {stata S.a()}        {it:(obtain normalizing constant)}
        : {stata S.loss()}     {it:(obtain balancing loss)}
        : {stata end}

{pstd}
    The balancing weights adjust the sample of unionized workers in a
    way such that the means of working hours, work experience, and tenure are
    the same as in the reference sample (non-unionized workers).

{pstd}
    By default, the sum of the balancing weights will be equal to the size
    of the reference sample.

{pstd}
    Balance only a subset of the means and hold the other means fixed at their
    original values:

        . {stata "mata:"}
        : {stata S.adj(2)}      {it:(balance work experience only)}
        : {stata mean(Xref)', mean(X)', mean(X, S.wbal())'}
        : {stata S.noadj(2)}    {it:(do not balance work experience)}
        : {stata mean(Xref)', mean(X)', mean(X, S.wbal())'}
        : {stata end}

{dlgtab:Adjust sample to population}

{pstd}
    From census data you know that the population averages of working hours,
    work experience, and tenure are 35, 10, and 5, respectively. The size of the
    population is 10 million. You want to reweight your sample such that
    it is in line with the population:

        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(hours, ttl_exp, tenure)}
        . {stata "mata:"}
        : {stata X = st_data(., "hours ttl_exp tenure")}
        : {stata mu = (35, 10, 5)}
        : {stata N  = 1e7}
        : {stata S = mm_ebalance()}
        : {stata S.data(X, 1, mu, N)}
        : {stata mean(X)', mean(X,S.wbal())'}
        : {stata S.wsum()}
        : {stata end}


{title:Source code}

{pstd}
    {help moremata11_source##mm_ebalance:mm_ebalance.mata}


{title:References}

{phang}
    Hainmueller, J. (2012). Entropy Balancing for Causal Effects: A
    Multivariate Reweighting Method to Produce Balanced Samples in
    Observational Studies. Political Analysis
    20(1): 25-46. DOI: {browse "http://doi.org/10.1093/pan/mpr025":10.1093/pan/mpr025}
    {p_end}
{phang}
    Hainmueller, J., Y. Xu (2011). EBALANCE: Stata module to perform Entropy
    reweighting to create balanced samples. Statistical Software Components
    S457326, Boston College Department of Economics. {browse "http://ideas.repec.org/c/boc/bocode/s457326.html"}.
    {p_end}
{phang}
    Hainmueller, J., Y. Xu (2013). ebalance: A Stata Package for Entropy Balancing.
    Journal of Statistical Software
    54(7):1-18. DOI: {browse "http://doi.org/10.18637/jss.v054.i07":10.18637/jss.v054.i07}
    {p_end}
{phang}
    Jann, B. (2021a). ebalfit: Stata module to perform entropy balancing. Available
    from http://github.com/benjann/ebalfit/.
    {p_end}
{phang}
    Jann, B. (2021b). Entropy balancing as an estimation command. University of 
    Bern Social Sciences Working Papers 39. Available from
    {browse "http://ideas.repec.org/p/bss/wpaper/39.html"}.
    {p_end}


{title:Author}

{pstd} Ben Jann, University of Bern, ben.jann@unibe.ch


{title:Also see}

{psee}
Online:  help for
{helpb mf_optimize:[M-5] optimize()}, {helpb moremata}, {helpb mf_mm_wbal:mm_wbal()}, {helpb mf_mm_ebal:mm_ebal()},
{helpb ebalfit} (if installed)
{p_end}

