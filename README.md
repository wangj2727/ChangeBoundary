# ChangeBoundary
This repository stored R codes used to create an R shiny app, which explores boundary and power after adding interm looks.
 
# View app
https://jwang24.shinyapps.io/ChangeBoundary/

If you could not open the above link, copy the following codes into your R console and run.
`library(shiny)`
`runGitHub( "ChangeBoundary", "wangj2727")`

# R version
R version 4.1.3 (2022-03-10)

# R package information
    + sessioninfo::package_info()
     package         * version date (UTC) lib source
     askpass           1.1     2019-01-13 [1] CRAN (R 4.1.3)
     bslib           * 0.3.1   2021-10-06 [1] CRAN (R 4.1.3)
     cachem            1.0.6   2021-08-19 [1] CRAN (R 4.1.3)
     cli               3.4.0   2022-09-08 [1] CRAN (R 4.1.3)
     curl              4.3.2   2021-06-23 [1] CRAN (R 4.1.3)
     digest            0.6.29  2021-12-01 [1] CRAN (R 4.1.3)
     dplyr           * 1.1.0   2023-01-29 [1] CRAN (R 4.1.3)
     DT              * 0.23    2022-05-10 [1] CRAN (R 4.1.3)
     ellipsis          0.3.2   2021-04-29 [1] CRAN (R 4.1.2)
     fansi             1.0.3   2022-03-24 [1] CRAN (R 4.1.3)
     fastmap           1.1.0   2021-01-25 [1] CRAN (R 4.1.3)
     fontawesome       0.2.2   2021-07-02 [1] CRAN (R 4.1.3)
     fs                1.5.2   2021-12-08 [1] CRAN (R 4.1.3)
     generics          0.1.3   2022-07-05 [1] CRAN (R 4.1.3)
     glue              1.6.2   2022-02-24 [1] CRAN (R 4.1.2)
     htmltools         0.5.2   2021-08-25 [1] CRAN (R 4.1.3)
     htmlwidgets       1.5.4   2021-09-08 [1] CRAN (R 4.1.3)
     httpuv            1.6.5   2022-01-05 [1] CRAN (R 4.1.3)
     jquerylib         0.1.4   2021-04-26 [1] CRAN (R 4.1.3)
     jsonlite          1.8.4   2022-12-06 [1] CRAN (R 4.1.3)
     later             1.3.0   2021-08-18 [1] CRAN (R 4.1.3)
     ldbounds        * 2.0.0   2022-02-24 [1] CRAN (R 4.1.3)
     lifecycle         1.0.3   2022-10-07 [1] CRAN (R 4.1.3)
     magrittr          2.0.3   2022-03-30 [1] CRAN (R 4.1.3)
     mime              0.12    2021-09-28 [1] CRAN (R 4.1.1)
     openssl           2.0.1   2022-05-14 [1] CRAN (R 4.1.3)
     pillar            1.8.1   2022-08-19 [1] CRAN (R 4.1.3)
     pkgconfig         2.0.3   2019-09-22 [1] CRAN (R 4.1.2)
     promises          1.2.0.1 2021-02-11 [1] CRAN (R 4.1.3)
     purrr             1.0.1   2023-01-10 [1] CRAN (R 4.1.3)
     R6                2.5.1   2021-08-19 [1] CRAN (R 4.1.2)
     RColorBrewer      1.1-3   2022-04-03 [1] CRAN (R 4.1.3)
     Rcpp              1.0.8.3 2022-03-17 [1] CRAN (R 4.1.3)
     rlang             1.0.6   2022-09-24 [1] CRAN (R 4.1.3)
     rsconnect         0.8.26  2022-05-31 [1] CRAN (R 4.1.3)
     rstudioapi        0.14    2022-08-22 [1] CRAN (R 4.1.3)
     sass              0.4.1   2022-03-23 [1] CRAN (R 4.1.3)
     sessioninfo       1.2.2   2021-12-06 [1] CRAN (R 4.1.3)
     shiny           * 1.7.1   2021-10-02 [1] CRAN (R 4.1.3)
     shinycssloaders * 1.0.0   2020-07-28 [1] CRAN (R 4.1.3)
     shinydashboard  * 0.7.2   2021-09-30 [1] CRAN (R 4.1.3)
     shinyjs         * 2.1.0   2021-12-23 [1] CRAN (R 4.1.3)
     shinythemes     * 1.2.0   2021-01-25 [1] CRAN (R 4.1.3)
     stringi           1.7.6   2021-11-29 [1] CRAN (R 4.1.2)
     stringr         * 1.5.0   2022-12-02 [1] CRAN (R 4.1.3)
     tibble            3.1.8   2022-07-22 [1] CRAN (R 4.1.3)
     tidyr           * 1.3.0   2023-01-24 [1] CRAN (R 4.1.3)
     tidyselect        1.2.0   2022-10-10 [1] CRAN (R 4.1.3)
     utf8              1.2.2   2021-07-24 [1] CRAN (R 4.1.2)
     vctrs             0.5.2   2023-01-23 [1] CRAN (R 4.1.3)
     withr             2.5.0   2022-03-03 [1] CRAN (R 4.1.3)
     xtable            1.8-4   2019-04-21 [1] CRAN (R 4.1.3)
