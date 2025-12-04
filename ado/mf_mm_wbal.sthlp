{smcl}
{* 10dec2021}{...}
{cmd:help mata mm_wbal()}
{hline}

{title:Title}

{pstd}
    {bf:mm_wbal() -- Generate balancing weights using entropy balancing}


{title:Syntax}

{p 8 23 2}
{it:real rowvector}{bind: }
{cmd:mm_wbal(}{it:X}{cmd:,} {it:w}{cmd:,} {it:Xref}{cmd:,} {it:wref} [{cmd:,} {it:nowarn}]{cmd:)}

{p 8 8 2}
    where

{p 12 16 2}
    {it:X} is the data to be reweighted; rows are observations, columns
    are variables

{p 12 16 2}
    {it:w} is a column vector containing base weights; use
    {it:w} = {cmd:1} for unweighted data; if not scalar, {it:w} must have
    the same number of rows as {it:X}

{p 12 16 2}
    {it:Xref} is the reference data; rows are observations, columns
    are variables; {it:Xref} must have the same number of columns as {it:X};
    if {it:Xref} only has a single row, it is assumed to contain population
    averages

{p 12 16 2}
    {it:wref} is a column vector containing base weights for the reference data; use
    {it:wref} = {cmd:1} for unweighted data; if not scalar, {it:wref} must have
    the same number of rows as {it:Xref}; if {it:Xref} only has a single row,
    {it:wref} is assumed to contain the population size

{p 12 16 2}
    {it:nowarn}!=0 suppresses the warning messages that are displayed
    if convergence or balancing is not achieved. Omitted {it:nowarn} is
    equivalent to {it:nowarn}=0.


{title:Description}

{pstd}
    {cmd:mm_wbal()} returns a vector of balancing weights obtained by 
    entropy balancing. {cmd:mm_wbal()} is implemented
    as a wrapper for {helpb mf_mm_ebalance:mm_ebalance()}. See the help file
    of {helpb mf_mm_ebalance:mm_ebalance()} for technical details and 
    references.


{title:Examples}

{dlgtab:Balance subsamples}

        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(union, hours, ttl_exp, tenure)}
        . {stata generate byte nonunion = 1 - union}
        . {stata "mata:"}
        : {stata X    = st_data(., "hours ttl_exp tenure", "union")}
        : {stata Xref = st_data(., "hours ttl_exp tenure", "nonunion")}
        : {stata wbal = mm_wbal(X, 1, Xref, 1)}
        : {stata mean(Xref)', mean(X)', mean(X, wbal)'}
        : {stata rows(Xref), rows(X), sum(wbal)}
        : {stata end}

{pstd}
    The balancing weights adjust the sample of unionized workers in a
    way such that the means of working hours, work experience, and tenure are
    the same as in the reference sample (non-unionized workers).

{pstd}
    The sum of the balancing weights is equal to the size of the reference sample.

{dlgtab:Adjust sample to population}

{pstd}
    From census data you know that the population averages of working hours, 
    work experience, and tenure are 35, 10, and 5, respectively. The size of the
    population is 1 million. You want to reweight your sample such that
    it is in line with the population:
    
        . {stata sysuse nlsw88, clear}
        . {stata drop if missing(hours, ttl_exp, tenure)}
        . {stata "mata:"}
        : {stata X = st_data(., "hours ttl_exp tenure")}
        : {stata mu = (35, 10, 5)}
        : {stata N  = 1e6}
        : {stata wbal = mm_wbal(X, 1, mu, N)}
        : {stata mean(X)', mean(X, wbal)'}
        : {stata sum(wbal)}
        : {stata end}


{title:Source code}

{pstd}
    {help moremata11_source##mm_wbal:mm_wbal.mata}


{title:Author}

{pstd} Ben Jann, University of Bern, ben.jann@unibe.ch


{title:Also see}

{psee}
Online:  help for
{bf:{help moremata}}, {helpb mf_mm_ebalance:mm_ebalance()}
{p_end}

