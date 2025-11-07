# Duflo-2001
This archive contains the shareable data and code used in the [reanalysis](https://arxiv.org/abs/2207.09036) of Duflo (2001), "[Schooling and Labor Market Consequences of School Construction in Indonesia: Evidence from an Unusual Policy Experiment](https://doi.org/10.1257/aer.91.4.795)."

The main data file, from the 1995 Intercensal survey (SUPAS), is not contained in this repository. It can however be found [here](https://github.com/droodman/Duflo2001/blob/main/inpresdata.dta).

The reanalysis also uses the 2005 SUPAS and 2011-12 SUSENAS survey data sets. The [2005 SUPAS data at IPUMS](https://international.ipums.org/international-action/sample_details/country/id#tab_id2005a) is used for the first. The second was obtained through the Harvard library system.

The "Regency-level vars" files contain figures on population, school attendance,  planned school construction, and water and sanitation spending. The Duflo (2001) versions of the variables, which have been used in many studies, are here copied from the [public data archive](https://www.dropbox.com/s/ayq0e2esty5hduw/Ashraf_Bau_Nunn_Voena_JPE_2020_Replication_Files.zip) of [Ashraf et al. (2020)](https://doi.org/10.1086/704572). The new versions carry the suffix "new". Images of the government documents they were reconstructed from are in the "Printed sources" folder.

Regencies and municipality boundaries in Indonesia have changed over time, mostly through subdivision, occasionally through merger. This complicates linking regency-level data from the 1971 census and mid-1970s presidential directives to the follow-ups in 1995, 2005, 2011-14, and 2017-19. IPUMS helpfully provides [shapefiles](https://international.ipums.org/international/gis_yrspecific_2nd.shtml) that modern database and GIS software can use to make the linkages. The concordances folder contains concordances linking the 1995 coding to the 2005, 2011-14, and 2017-19 codings. The 1970s data are manually coded with respect to 1995. Notes in "Baseline variable reconstruction.xlsx" in the "Regency-level vars" folder document complications in this coding, including a few cases where the original and new differ.

"Duflo 2001.do" generates nearly all the results. Exception: "de Chaisemartin and d'Haultfoeuille 2017 simulation.do" produces the table in appendix G.
