### rspiro - Spirometry equations for R

Package `rspiro` implements multiple spirometry equations: currently
the GLI-2012 (Quanjer), race-neutral GLI global (2022), JRS-2014, NHANES III (Hankinson)
as well as the GLI-2017 diffusing capacity equations (Stanojevic et al). 
More may be added later. 

It offers user-friendly functions to calculate predicted or LLN 
(Lower Limit of Normal) values given demographic data, or to convert 
absolute spirometry values to percent (%) predicted or z-scores.

**Installation**

Open R, and give:

      devtools::install_github("thlytras/rspiro")

If you do not have the package "devtools", first install it from CRAN with:

      install.packages("devtools")


**Acknowledgement**

This package uses the lookup tables provided by the [GLI 
initiative](https://www.ers-education.org/guidelines/global-lung-function-initiative/spirometry-tools/it-engineers-and-manufacturers/), 
to whom I'm grateful for their work.

**Disclaimer**

Although every effort has been made to verify correct operation of this 
software, I cannot accept any liability arising from its use. The 
software is provided "as is", without any express or implied warranty,
and no implication of fitness for a particular use.

If you find any bugs, please open an issue or, alternatively, send me 
e-mail to thlytras@gmail.com
