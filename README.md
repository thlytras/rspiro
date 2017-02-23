### rspiro - Spirometry equations for R

This package implements the GLI-2012 (Quanjer) spirometry equations
in R. It allows user-friendly calculation of predicted (mean normal) 
and LLN (Lower Limit of Normal) values, as well as converting absolute
measurements to percent (%) predicted, for the whole range of spirometric
parameters (FEV1, FVC, FEV1FVC, etc).

More equations (Hankinson, etc) will be implemented later.

**Installation**

Open R, and give:

      devtools::install_git("https://github.com/thlytras/rspiro.git")

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

If you find any bugs, feel free to send me an e-mail at thlytras@gmail.com
