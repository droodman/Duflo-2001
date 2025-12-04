{smcl}
{* 10dec2021}{...}
{cmd:help mata mm_ebal()}
{hline}

{title:Title}

{pstd}
    {bf:mm_ebal() -- Entropy balancing}


{title:Syntax}

{pstd}
    Initialize the balancing problem:

{p 12 27 2}
    {it:S} = {cmd:mm_ebal_init(}{it:X1}{cmd:,} {it:w1}{cmd:,} {it:X0}{cmd:,} {it:w0} [{cmd:,} {it:target}{cmd:,} {it:cov}{cmd:,} {it:nc}{cmd:,} {it:dfc}{cmd:,} {it:nostd}]{cmd:)}

{p 8 8 2}
    where

{p 12 16 2}
    {it:X1} is the data matrix of the treatment group; rows are observations, columns
    are variables; {it:X1} and {it:X0} must have the same number of columns

{p 12 16 2}
    {it:w1} is a column vector containing base weights of the treatment group; use
    {it:w1} = {cmd:1} for unweighted data; if not scalar, {it:w1} must have
    the same number of rows as {it:X1}

{p 12 16 2}
    {it:X0} is the data matrix of the control group; rows are observations, columns
    are variables; {it:X1} and {it:X0} must have the same number of columns

{p 12 16 2}
    {it:w0} is a column vector containing base weights of the control group; use
    {it:w0} = {cmd:1} for unweighted data; if not scalar, {it:w0} must have
    the same number of rows as {it:X0}

{p 12 16 2}
    {it:target} specifies the target moments to be balanced; {it:target} is a vector containing
    elements equal to {cmd:1} (balance mean), {cmd:2} (balance mean and variance), or
    {cmd:3} (balance mean, variance, and skewness); the elements of {it:target} will
    be applied to the variables one after the other; if {it:target} is shorter
    then the number of variables, the elements will be recycled; for example,
    {it:target} = {cmd:2} will balance all means and all variances; the default is
    {it:target} = {cmd:1} (balance all means)

{p 12 16 2}
    {it:cov}!=0 balances the covariances
    (in addition to the moments requested by {it:target})

{p 12 16 2}
    {it:nc}!=0 includes the normalization constraint (target sum of weights)
    in the optimization problem, rather than rescaling the weights ex
    ante; in this case, if perfect balance is not possible, the sum of the
    balancing weights is no longer guaranteed to be equal to the size of the
    treatment group

{p 12 16 2}
    {it:dfc}!=0 applies degrees-of-freedom correction to balancing constraints
    for variances and covariances; by default, {cmd:mm_ebal()} uses constraints
    that are consistent with computing variances and covariances in the
    reweighted control group based on weights that are normalized to sum to the
    size of the treatment group (or with variance formulas that ignore
    degrees-of-freedom adjustment in the denominator); if {it:dfc}!=0 is
    specified, the results are consistent with
    weights normalized to the size of the control group; {it:dfc} only
    affects variances and covariances that are not collinear with lower
    moments

{p 12 16 2}
    {it:nostd}!=0 suppresses standardization of the constraint matrix; by default,
    {cmd:mm_ebal()} divides the columns of the constraint matrix by the
    standard deviations of the corresponding terms in the treatment group (if the
    standard deviations exist); this should make the optimization more stable as all
    constraints have a similar scaling and the balancing loss is expressed in terms of
    standardized differences; specify {it:nostd}!=0 to omit such standardization


{pstd}
    Run the balancing algorithm:

{p 12 27 2}
    {it:balanced} = {cmd:mm_ebal(}{it:S}{cmd:)}

{p 8 8 2}
    where
    {it:balanced} will be {cmd:1} if balance is achieved (balancing loss < {it:btol}) and
    {cmd:0} else


{pstd}
    Retrieve the balancing weights:

{p 12 27 2}
    {it:W} = {cmd:mm_ebal_W(}{it:S}{cmd:)}

{p 8 8 2}
    where
    {it:W} will be a column vector containing the weights that can be used to adjust
    the control group to the treatment group; the sum of {it:W} will equal the
    size (sum of weights) of the treatment group


{pstd}
    Define optimization settings before running {cmd:mm_ebal()}:

{p 8 12 2}
    {cmd:mm_ebal_btol(}{it:S}{cmd:,} {it:btol}{cmd:)}{break}set the balancing
    tolerance; default is {it:btol} = {cmd:1e-5}; balancing is achieved if the
    balancing loss is smaller than {it:btol}

{p 8 12 2}
    {cmd:mm_ebal_trace(}{it:S}{cmd:,} {help mf_optimize##i_tracelevel:{it:trace}}{cmd:)}{break}set
    what is displayed in the iteration log; default is {it:trace} =
    {cmd:"value"}; {it:trace} = {cmd:"none"} suppresses the iteration log;
    see {helpb mf_optimize##i_tracelevel:optimize()} for details

{p 8 12 2}
    {cmd:mm_ebal_maxiter(}{it:S}{cmd:,} {it:maxiter}{cmd:)}{break}set the maximum
    number of iterations; the default is {cmd:c(maxiter)} as set by
    {helpb set maxiter} (16,000 by default)

{p 8 12 2}
    {cmd:mm_ebal_ptol(}{it:S}{cmd:,} {it:ptol}{cmd:)}{break}set the convergence
    tolerance for the parameter vector (lambda coefficients); default is
    {it:ptol} = {cmd:1e-6}; convergence is reached if {it:ptol} or {it:vtol} is
    satisfied; also see {helpb mf_optimize##i_ptol:optimize()}

{p 8 12 2}
    {cmd:mm_ebal_vtol(}{it:S}{cmd:,} {it:vtol}{cmd:)}{break}set the convergence
    tolerance for the balancing loss; default is {it:vtol} = {cmd:1e-7};
    convergence is reached if {it:ptol} or {it:vtol} is satisfied; also see
    {helpb mf_optimize##i_ptol:optimize()}

{p 8 12 2}
    {cmd:mm_ebal_difficult(}{it:S}{cmd:,} {it:flag}{cmd:)}{break}set the stepping
    algorithm to be used in nonconcave regions; {it:flag}=0 (the default) uses
    the standard algorithm; {it:flag}!=0 uses an alternative algorithm; see the
    singular H methods in {helpb mf_optimize##i_singularH:optimize()} and
    the description of the {cmd:difficult} option in {helpb maximize}

{p 8 12 2}
    {cmd:mm_ebal_nowarn(}{it:S}{cmd:,} {it:flag}{cmd:)}{break}set whether
    the message "convergence not achieved" is to be displayed if convergence is
    not reached within the maximum number of iterations; {it:flag}=0 (the default)
    displays the message; {it:flag}!=0 suppresses the message

{p 8 12 2}
    {cmd:mm_ebal_Z(}{it:S}{cmd:,} {it:Z}{cmd:)}{break}set the starting value
    for the parameter vector (lambda coefficients); the default is
    {it:Z} =  J(1, {it:c}, 0) where {it:c} is the number of balancing constraints; if
    {it:nc}!=0 the default is {it:Z} =  (ln(N1/N0), J(1, {it:c}, 0)) where N1
    and N0 are the group sizes (sum of weights)

{p 8 8 2}
    If applied without 2nd argument, the above functions return the current value of the
    setting. For example, type {it:btol} = {cmd:mm_ebal_btol(}{it:S}{cmd:)}
    to obtain the current balancing tolerance setting.


{pstd}
    Retrieve auxiliary results after running {cmd:mm_ebal()}:

{p 8 12 2}
    {it:balanced} = {cmd:mm_ebal_balanced(}{it:S}{cmd:)}{break}{cmd:1} if balance is achieved (balancing loss < {it:btol}), {cmd:0} else

{p 8 12 2}
    {it:conv} = {cmd:mm_ebal_conv(}{it:S}{cmd:)}{break}{cmd:1} if
    the optimization algorithm converged, {cmd:0} else

{p 8 12 2}
    {it:rc} = {cmd:mm_ebal_rc(}{it:S}{cmd:)}{break}
    scalar containing the return code issued by {helpb mf_optimize##r_error:optimize()} in case of error

{p 8 12 2}
    {it:Z} = {cmd:mm_ebal_Z(}{it:S}{cmd:)}{break}
    row vector containing the fitted parameter vector (lambda coefficients)

{p 8 12 2}
    {it:g} = {cmd:mm_ebal_g(}{it:S}{cmd:)}{break}row vector containing
    the gradient at {it:Z}

{p 8 12 2}
    {it:v} = {cmd:mm_ebal_v(}{it:S}{cmd:)}{break}scalar containing the
    balancing loss at {it:Z}

{p 8 12 2}
    {it:i} = {cmd:mm_ebal_i(}{it:S}{cmd:)}{break}scalar containing the number
    of iterations

{p 8 12 2}
    {it:N} = {cmd:mm_ebal_N(}{it:S}{cmd:)}{break}scalar containing the
    target sum of weights (size of the treatment group)

{p 8 12 2}
    {it:C} = {cmd:mm_ebal_C(}{it:S}{cmd:)}{break}constraint matrix (excluding
    collinear columns); rows are observations

{p 8 12 2}
    {it:CC} = {cmd:mm_ebal_CC(}{it:S}{cmd:)}{break}(non-redundant) collinear
    columns from constraint matrix; rows are observations

{p 8 12 2}
    {it:Q} = {cmd:mm_ebal_Q(}{it:S}{cmd:)}{break}column vector containing the
    (rescaled) base weights of the control group



{title:Description}

{pstd}
    {cmd:mm_ebal()} performs entropy balancing proposed by Hainmueller (2012).
    Given data from a treatment group and a control group, the goal of
    {cmd:mm_ebal()} is to find a vector of weights such that selected moments
    (e.g. means and variances) in the reweighted control group match the
    corresponding moments in the treatment group (i.e., to find weights that
    balance the treatment and control groups with respect to the moments).

{pstd}
    The code of {cmd:mm_ebal()} is loosely based on the Stata package 
    {helpb ebalance} (version 1.5.4, 2015-01-29) by Hainmueller and Xu (2011, 
    2013) and on R package {cmd:ebal} (version 0.1-6, 2014-01-27) by Hainmueller
    (2014). Instead of using a custom algorithm, however, {cmd:mm_ebal()} is
    implemented in terms of Mata's {helpb mf_optimize:optimize()}. Results will 
    be highly accurate as long as a balancing solution exists.

{pstd}
    A newer function for entropy balancing with somewhat different
    features is available as {helpb mf_mm_ebalance:mm_ebalance()}. A wrapper
    that obtains balancing weights in a single
    line of code is available as {helpb mf_mm_wbal:mm_wbal()}. A Stata command
    based on {helpb mf_mm_ebalance:mm_ebalance()} is available as 
    {helpb ebalfit} (Jann 2021).

{pstd}
    Remark on how {cmd:mm_ebal()} handles collinearity: (1) Terms that are
    collinear across both groups will be ignored because the
    corresponding balancing constraints are redundant (example: variance of a
    binary variable). (2) Terms that are collinear only in the control group
    will be ignored during optimization, however, since the corresponding 
    balancing constraints are not redundant, these terms will be taken into
    account when computing the final balancing loss after optimization has
    completed (example: empty factor-variable level). 


{title:Examples}

{dlgtab:Basic procedure}

{pstd}
    The basic procedure is to first define the balancing problem using
    {cmd:mm_ebal_init()}, then apply {cmd:mm_ebal()} to find the balancing
    weights, and then to retrieve the fitted weights using {cmd:mm_ebal_W()}. Here
    is an example:

        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(wage, union, age, grade, hours, ttl_exp, tenure)}
        . {stata generate byte nonunion = 1 - union}
        . {stata "mata:"}
        : {stata X1 = st_data(., "age grade hours ttl_exp tenure", "union")}
        : {stata X0 = st_data(., "age grade hours ttl_exp tenure", "nonunion")}
        : {stata S = mm_ebal_init(X1, 1, X0, 1)}
        : {stata (void) mm_ebal(S)}
        : {stata w = mm_ebal_W(S)}
        : {stata end}

{pstd}
    We can now confirm that the weights do balance the data. In the original sample,
    the group means and the mean differences are:

        . {stata "mata: mean(X1)', mean(X0)', mean(X1)' - mean(X0)'"}

{pstd}
    Applying the balancing weights to the control group removes the mean differences
    (apart from roundoff error):

        . {stata "mata: mean(X1)', mean(X0, w)', mean(X1)' - mean(X0, w)'"}

{dlgtab:Balancing higher order moments}

{pstd}
    By default, {cmd:mm_ebal()} computes weights that balance the means of the
    provided variables. Specify {it:target} and {it:cov} to
    balance additional moments. In the following example, {it:tar} = {cmd:2}
    and {it:cov} = {cmd:1} is used to balance all means, variances, and
    covariances:

        . {stata "mata:"}
        : {stata S = mm_ebal_init(X1, 1, X0, 1, 2, 1)}
        : {stata (void) mm_ebal(S)}
        : {stata w = mm_ebal_W(S)}
        : {stata mean(X1) - mean(X0, w)}
        : {stata variance(X1) - variance(X0, w)}
        : {stata end}

{dlgtab:Estimating an ATT based on the balanced sample}

{pstd}
    We can now use the balancing weights to estimate an average treatment
    effect on the treated (ATT). Using unbalanced data, the estimate of the effect
    of union status on wages is 1.46 USD:

        . {stata regress wage union}

{pstd}
    Adjusting covariate imbalance using the weights computed by {cmd:mm_ebal()}
    we get an ATT of .82 USD:

        . {stata generate double w = 1}
        . {stata `"mata: st_store(., "w", "nonunion", w)"'}
        . {stata regress wage union [pweight = w]}

{dlgtab:Adjusting a sample to known population moments}

{pstd}
    Assume that the mean age in the relevant population is known to be 35 with a standard deviation of 2.5
    and that the mean grade is 12 with a standard deviation of 1.9. The population
    size is 1.2 million. You could adjust the sample to these values as follows:

        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(age, grade)}
        . {stata "mata:"}
        : {stata age = st_data(., "age")}
        : {stata grade = st_data(., "grade")}
        : {stata N = 1.2e6; dfc = N / (N - 1)}
        : {stata pop = (35, 2.5^2, 12, 1.9^2)}
        : {stata "sample = (age, (age:-35):^2 * dfc, grade, (grade:-12):^2 * dfc)"}
        : {stata S = mm_ebal_init(pop, N, sample, 1)}
        : {stata (void) mm_ebal(S)}
        : {stata w = mm_ebal_W(S)}
        : {stata sum(w)}
        : {stata mean(age, w), sqrt(variance(age, w))}
        : {stata mean(grade, w), sqrt(variance(grade, w))}
        : {stata end}

{dlgtab:Orthogonalization of variables}

{pstd}
    Entropy balancing can also be used, for example, to find weights that 
    orthogonalize the data (such that correlations between variables are
    zero). Here is an example:

        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(age, hours, tenure)}
        . {stata "mata:"}
        : {stata X = st_data(., "age hours tenure")}
        : {stata "C = X :- mean(X)"}
        : {stata "C = C, C[,1]:*C[,2], C[,1]:*C[,3], C[,2]:*C[,3]"}
        : {stata S = mm_ebal_init(J(1, cols(C), 0), rows(C), C, 1)}
        : {stata (void) mm_ebal(S)}
        : {stata w = mm_ebal_W(S)}
        : {stata mean(X)}           // results from original data
        : {stata variance(X)}
        : {stata mean(X, w)}        // reweighted results
        : {stata variance(X, w)}
        : {stata end}

{pstd}
    The trick is to first center the data and then fit weights such that
    the means of the pairwise products between the centered variables become zero.

{pstd}
    In the above example, the means of the variables are preserved, but the 
    variances change. To preserve the variances in addition to the means, 
    standardize the data (excluding degrees of freedom correction) and add the
    squares of the standardized variables to the optimization problem:

        . {stata "mata:"}
        : {stata N = rows(X)}
        : {stata "C = (X:-mean(X)) :/ sqrt(diagonal(variance(X))*(N-1)/N)'"}
        : {stata "C = C, C:^2:-1, C[,1]:*C[,2], C[,1]:*C[,3], C[,2]:*C[,3]"}
        : {stata S = mm_ebal_init(J(1, cols(C), 0), rows(C), C, 1)}
        : {stata (void) mm_ebal(S)}
        : {stata w = mm_ebal_W(S)}
        : {stata mean(X)}           // results from original data
        : {stata variance(X)}
        : {stata mean(X, w)}        // reweighted results
        : {stata variance(X, w)}
        : {stata end}


{title:Source code}

{pstd}
    {help moremata11_source##mm_ebal:mm_ebal.mata}


{title:References}

{phang}
    Hainmueller, J. (2012). Entropy Balancing for Causal Effects: A
    Multivariate Reweighting Method to Produce Balanced Samples in
    Observational Studies. Political Analysis
    20(1): 25-46. DOI: {browse "http://doi.org/10.1093/pan/mpr025":10.1093/pan/mpr025}
    {p_end}
{phang}
    Hainmueller, J. (2014). ebal: Entropy reweighting to create balanced
    samples. {browse "http://CRAN.R-project.org/package=ebal"}.
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
    Jann, B. (2021). ebalfit: Stata module to perform entropy balancing. Available
    from http://github.com/benjann/ebalfit/.
    {p_end}

{title:Author}

{pstd} Ben Jann, University of Bern, ben.jann@unibe.ch


{title:Also see}

{psee}
Online:  help for
{helpb mf_optimize:[M-5] optimize()},
{helpb moremata}, {helpb mf_mm_ebalance:mm_ebalance()}, {helpb mf_mm_wbal:mm_wbal()}, 
{helpb ebalance} (if installed), {helpb ebalfit} (if installed),
{p_end}

