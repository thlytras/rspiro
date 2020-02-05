### rspiro - Spirometry equations for R

Package `rspiro` implements multiple spirometry equations: currently
the GLI-2012 (Quanjer) and NHANES III (Hankinson), with potentially 
more to be added later. 

It offers user-friendly functions to calculate predicted or LLN 
(Lower Limit of Normal) values given demographic data, or to convert 
absolute spirometry values to percent (%) predicted or z-scores.

**Installation**

Open R, and give:

      devtools::install_github("thlytras/rspiro")

If you do not have the package "devtools", first install it from CRAN with:

      install.packages("devtools")


**Acknowledgement**

This package uses the lookup table included with the [R code provided by
the GLI initiative](http://www.ers-education.org/guidelines/global-lung-function-initiative/tools/r-macro.aspx), 
to whom I'm grateful for their work.

**Disclaimer**

Although every effort has been made to verify correct operation of this 
software, I cannot accept any liability arising from its use. The 
software is provided "as is", without any express or implied warranty,
and no implication of fitness for a particular use.

If you find any bugs, please open an issue or, alternatively, send me 
e-mail to thlytras@gmail.com
