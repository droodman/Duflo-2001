# Duflo-2001
This archive contains the shareable code and data used in the reanalysis of Duflo (2001), "[Schooling and Labor Market Consequences of School Construction in Indonesia: Evidence from an Unusual Policy Experiment](https://doi.org/10.1257/aer.91.4.795)."

Most of the data used in the original study and the reanalysis cannot be shared because of restrictions in the licenses from Badan Pusat Statistik (BPS). The NBER has licensed 1995 intercensal (SUPAS) data and makes it available to NBER affiliates. At this writing, BPS does not make the 1995 SUPAS and other older data sets available to researchers. IPUMS makes freely available a [large subset of the 1995 SUPAS data](https://international.ipums.org/international-action/sample_details/country/id#tab_id1995a), which is missing many smaller regencies, or perhaps the most heavily weights observations in general. The posted code is written to work with the IPUMS subset as well, but produces different results from it.

The [2005 SUPAS data at IPUMS](https://international.ipums.org/international-action/sample_details/country/id#tab_id2005a) is used. The newer data sets--2010 SAKERNAS and 2013-14 SUSENAS--must be [bought from the BPS](https://silastik.bps.go.id/v3/index.php/site/login/): the [district-level 2010 SAKERNAS](https://silastik.bps.go.id/v3/index.php/mikrodata/view/MDdwMHQ1VjNwRk5WTGEwejBJQlA4UT09) and the [2013](https://silastik.bps.go.id/v3/index.php/mikrodata/view/TjdmRnZKczRiajErODAyUFRYWXNFdz09) and [2014](https://silastik.bps.go.id/v3/index.php/mikrodata/view/NTdBQklET3pTZU1XaGw5R0p6R1RPZz09) district-level SUSENAS. To save money, buy only variables UMUR, JK, WEIGHT, B1P01, B1P02, B5P1A, B5P6B, B5P12A, and B5P12B from SAKERNAS; and UMUR, JK, FWT_TAHUN, B5_TL1, B5_TL2, B5R15, B5R29, and B5R28B from SUSENAS (and HB if you want to copy [Hsiao](https://allanhsiao.github.io/files/Hsiao_schools.pdf) in restricting to heads of household).

The "Regency-level vars" files contain figures on population, school attendance,  planned school construction, and water and sanitation spending. The Duflo (2001) versions of the variables, which have been used in many studies, are here copied from the [public data archive](https://www.dropbox.com/s/ayq0e2esty5hduw/Ashraf_Bau_Nunn_Voena_JPE_2020_Replication_Files.zip) of [Ashraf et al. (2020)](https://doi.org/10.1086/704572). The new versions carry the suffix "new". Images of the government documents they were reconstructed from are in the "Printed sources" folder.

Regencies and municipality boundaries in Indonesia have changed over time, mostly through subdivision, occasionally through merger. This complicates linking regency-level data from the 1971 census and mid-1970s presidential directives to the follow-ups in 1995, 2005, 2010, and 2013-14. IPUMS helpfully provides [shapefiles](https://international.ipums.org/international/gis_yrspecific_2nd.shtml) that modern database and GIS software can use to make the linkages. The concordances folder contains concordances linking the 1995 coding to the 2005 and 2010-14 codings. The 1970s data are manually coded with respect to 1995. Notes in "Baseline variable reconstruction.xlsx" in the "Regency-level vars" folder document complications in this coding, including a few cases where the original and new diverge.
