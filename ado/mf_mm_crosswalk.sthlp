{smcl}
{* 23aug2021}{...}
{cmd:help mata mm_crosswalk()}
{hline}

{title:Title}

{p 4 10 2}
{bf:mm_crosswalk() -- Translate values between classifications (bulk recoding)}


{title:Syntax}

{p 8 23 2}
{it:transmorphic vector}
{cmd:mm_crosswalk(}{it:x}{cmd:,} {it:from}{cmd:,} {it:to} [{cmd:,} {it:d}{cmd:,}
    {it:n}]{cmd:)}

{p 8 23 2}
{it:transmorphic vector}
{cmd:mm_crosswalk_hash(}{it:x}{cmd:,} {it:from}{cmd:,} {it:to} [{cmd:,} {it:d}]{cmd:)}

{p 4 8 2}
where

{p 12 16 2}
{it:x}:  {it:transmorphic vector} containing values to be translated

{p 9 16 2}
{it:from}:  {it:transmorphic vector} containing the origin values of the
crosswalk dictionary; {it:from} must be of the same type as {it:x}; the values in
{it:from} are assumed to be unique (this is not checked)

{p 11 16 2}
{it:to}:  {it:transmorphic vector} containing the destination values of the
crosswalk dictionary; {it:to} must have the same length as {it:from}

{p 12 16 2}
{it:d}:  {it:transmorphic scalar} specifying a default destination value for
elements of {it:x} that do not have a match in {it:from}; alternatively, {it:d}
may be a {it:transmorphic vector} providing individual default values (must
have same length as {it:x}); in any case, {it:d} must be of the
same type as {it:to}; the default for {it:d} is {cmd:missingof(}{it:to}{cmd:)}

{p 12 16 2}
{it:n}:  {it:real scalar} specifying the maximum length of the
index-based crosswalk vector; this is only relevant if {it:x} and {it:from}
are integer and non-missing such that fast index-based translation is
possible; if the index-based crosswalk vector would be longer than
{it:n}, {cmd:mm_crosswalk()} automatically switches to the (slower but more
memory-efficient) hash-based translation algorithm; the default for
{it:n} is {cmd:1e6}

{p 16 16 2}
specify {it:n}<1 to enforce
the hash-based algorithm; specify {it:n}=. to enforce the index-based
algorithm, provided {it:x} and {it:from} are integer and non-missing; specify
{it:n}={cmd:.z} to enforce the index-based algorithm and skip any checks for
noninteger or missing values in {it:x} and {it:from}; use {it:n}={cmd:.z}
to save computer time if you know that {it:x} and {it:from} are
integer and nonmissing (the function may break or return invalid results
if these assumptions are not met)

{p 16 16 2}
in any case, usage of the index-based algorithm is only considered if {it:x}
and {it:from} have storage type {cmd:real}


{title:Description}

{pstd}
{cmd:mm_crosswalk()} translates {it:x} based on the dictionary provided
by {it:from} and {it:to}. That is, for each element in {it:x}, {cmd:mm_crosswalk()}
looks for a match in {it:from} and then returns the element from {it:to} that
has the same index as the matched element in {it:from}. Think of
{cmd:mm_crosswalk()} as a way to bulk-recode {it:x} where the element-by-element
pairs of {it:from} and {it:to} provide the recoding rules. Value
{it:d} is returned for elements in  {it:x} that have no match in {it:from}.

{pstd}
If feasible, {cmd:mm_crosswalk()} uses a fast translation technique based on
indexing. This requires all elements in {it:x} and {it:from} to be integer and
nonmissing (also see the description of argument {it:n} above). In all other
cases a hash-based algorithm is employed (implemented in terms of
{helpb mf_asarray:asarray()}). The hash-based algorithm is slower than the
index-based algorithm, but it works with any type of input.

{pstd}
Function {cmd:mm_crosswalk_hash()} directly calls the hash-based algorithm.


{title:Examples}

{pstd}
    Input and output may be of different type:

        . {stata "mata:"}
        : {stata x = (1,2,3,4,5)'}
        : {stata from = (2,3)}
        : {stata to = ("two","three")}
        : {stata mm_crosswalk(x, from, to, "--")}
        : {stata end}

        . {stata "mata:"}
        : {stata x = ("one","two","three","four","five")}
        : {stata from = ("two","three")}
        : {stata to = (2,3)}
        : {stata mm_crosswalk(x, from, to, .a)}
        : {stata end}

{pstd}
    Partial recoding:

        . {stata "mata:"}
        : {stata x = (1,2,3,4,5)}
        : {stata mm_crosswalk(x, (2,3), (3,2), x)}
        : {stata end}


{title:Conformability}

    {cmd:mm_crosswalk(}{it:x}{cmd:,} {it:from}{cmd:,} {it:to}{cmd:,} {it:d}{cmd:,} {it:n}{cmd:)}
            {it:x}:  {it:n x} 1  or  1 {it:x n}
         {it:from}:  {it:l x} 1  or  1 {it:x l}
           {it:to}:  {it:l x} 1  or  1 {it:x l}
            {it:d}:  1 {it:x} 1  or  {it:n x} 1  or  1 {it:x n}
            {it:n}:  1 {it:x} 1
       {it:result}:  {it:n x} 1  or  1 {it:x n}  (same orientation as {it:x})

    {cmd:mm_crosswalk_hash(}{it:x}{cmd:,} {it:from}{cmd:,} {it:to}{cmd:,} {it:d}{cmd:)}
            {it:x}:  {it:n x} 1  or  1 {it:x n}
         {it:from}:  {it:l x} 1  or  1 {it:x l}
           {it:to}:  {it:l x} 1  or  1 {it:x l}
            {it:d}:  1 {it:x} 1  or  {it:n x} 1  or  1 {it:x n}
       {it:result}:  {it:n x} 1  or  1 {it:x n}  (same orientation as {it:x})

{pstd}
    Orientation of vectors does not matter for conformability, only length is relevant.


{title:Diagnostics}

{pstd}
    The values in {it:from} are assumed to be unique such that the dictionary
    defined by {it:from} and {it:to} is non-ambiguous (although not necessarily
    bijective). Returned results will be arbitrary if this assumption is
    not met.

{pstd}
    The functions return void if {it:x} is void.

{pstd}
    The functions return defaults as specified by {it:d} if {it:from} and
    {it:to} are void.

{pstd}
    Missing values are treated like any other values.


{title:Source code}

{pstd}
{help moremata11_source##mm_crosswalk:mm_crosswalk.mata}


{title:Author}

{pstd} Ben Jann, University of Bern, ben.jann@unibe.ch


{title:Also see}

{psee}
Online:  help for
{helpb moremata}, {helpb mf_editvalue:editvalue()}, {helpb mf_asarray:asarray()}
{p_end}
