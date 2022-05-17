# ChangeBoundary
This repository stored R codes used to create an R shiny app, which explores boundary and power after adding interm looks.
 
# View app
https://rstudio-connectdev.niaid.nih.gov/connect/#/apps/b84d625d-f736-4085-a09a-dfa8d6e64fb7/access

If you could not open the above link, copy the following codes into your R console and run.
`library(shiny)`
`runGitHub( "ChangeBoundary", "wangj2727")`

# R version
R version 4.1.1 (2021-08-10)

# R package information
    + sessioninfo::package_info()
    package         * version date (UTC) lib source
    assertthat        0.2.1   2019-03-21 [2] CRAN (R 4.1.1)
    bslib           * 0.3.1   2021-10-06 [2] CRAN (R 4.1.1)
    cachem            1.0.6   2021-08-19 [2] CRAN (R 4.1.1)
    cli               3.2.0   2022-02-14 [1] CRAN (R 4.1.1)
    crayon            1.4.2   2021-10-29 [2] CRAN (R 4.1.1)
    DBI               1.1.1   2021-01-15 [2] CRAN (R 4.1.1)
    digest            0.6.28  2021-09-23 [2] CRAN (R 4.1.1)
    dplyr           * 1.0.7   2021-06-18 [2] CRAN (R 4.1.1)
    DT              * 0.22    2022-03-28 [1] CRAN (R 4.1.1)
    ellipsis          0.3.2   2021-04-29 [2] CRAN (R 4.1.1)
    fansi             0.5.0   2021-05-25 [2] CRAN (R 4.1.1)
    fastmap           1.1.0   2021-01-25 [2] CRAN (R 4.1.1)
    fontawesome       0.2.2   2021-07-02 [2] CRAN (R 4.1.1)
    fs                1.5.0   2020-07-31 [2] CRAN (R 4.1.1)
    generics          0.1.1   2021-10-25 [2] CRAN (R 4.1.1)
    glue              1.4.2   2022-02-24 [2] CRAN (R 4.1.1) 
    htmltools         0.5.2   2021-08-25 [2] CRAN (R 4.1.1)
    htmlwidgets       1.5.4   2021-09-08 [1] CRAN (R 4.1.1)
    httpuv            1.6.3   2021-09-09 [2] CRAN (R 4.1.1)
    jquerylib         0.1.4   2021-04-26 [2] CRAN (R 4.1.1)
    jsonlite          1.7.2   2020-12-09 [2] CRAN (R 4.1.1)
    later             1.3.0   2021-08-18 [2] CRAN (R 4.1.1)
    ldbounds        * 2.0.0   2022-02-24 [1] CRAN (R 4.1.1)
    lifecycle         1.0.1   2021-09-24 [2] CRAN (R 4.1.1)
    magrittr          2.0.1   2020-11-17 [2] CRAN (R 4.1.1)
    mime              0.12    2021-09-28 [2] CRAN (R 4.1.1)
    pillar            1.6.4   2021-10-18 [2] CRAN (R 4.1.1)
    pkgconfig         2.0.3   2019-09-22 [2] CRAN (R 4.1.1)
    promises          1.2.0.1 2021-02-11 [2] CRAN (R 4.1.1)
    purrr             0.3.4   2020-04-17 [2] CRAN (R 4.1.1)
    R6                2.5.1   2021-08-19 [2] CRAN (R 4.1.1)
    RColorBrewer      1.1-2   2014-12-07 [2] CRAN (R 4.1.1)
    Rcpp              1.0.7   2021-07-07 [2] CRAN (R 4.1.1)
    rlang             0.4.12  2022-03-04 [2] CRAN (R 4.1.1) 
    rsconnect         0.8.24  2021-08-05 [2] CRAN (R 4.1.1)
    rstudioapi        0.13    2020-11-12 [2] CRAN (R 4.1.1)
    sass              0.4.0   2021-05-12 [2] CRAN (R 4.1.1)
    sessioninfo       1.2.2   2021-12-06 [1] CRAN (R 4.1.1)
    shiny           * 1.7.1   2021-10-02 [2] CRAN (R 4.1.1)
    shinycssloaders * 1.0.0   2020-07-28 [1] CRAN (R 4.1.1)
    shinydashboard  * 0.7.2   2021-09-30 [1] CRAN (R 4.1.1)
    shinyjs         * 2.1.0   2021-12-23 [1] CRAN (R 4.1.1)
    shinythemes     * 1.2.0   2021-01-25 [1] CRAN (R 4.1.1)
    sourcetools       0.1.7   2018-04-25 [2] CRAN (R 4.1.1)
    stringi           1.7.5   2021-10-04 [2] CRAN (R 4.1.1)
    stringr         * 1.4.0   2019-02-10 [2] CRAN (R 4.1.1)
    tibble            3.1.5   2021-09-30 [2] CRAN (R 4.1.1)
    tidyr           * 1.1.4   2021-09-27 [2] CRAN (R 4.1.1)
    tidyselect        1.1.1   2021-04-30 [2] CRAN (R 4.1.1)
    utf8              1.2.2   2021-07-24 [2] CRAN (R 4.1.1)
    vctrs             0.3.8   2021-04-29 [2] CRAN (R 4.1.1)
    withr             2.5.0   2022-03-03 [1] CRAN (R 4.1.1)
    xtable            1.8-4   2019-04-21 [2] CRAN (R 4.1.1)
