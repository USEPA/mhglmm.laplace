library(mhglmm.laplace)
library(parallel)
library(purrr)
nsims <- 2000
ntime <- 30

################################################################################
##################### INLA vs spmodel
################################################################################

######## poisson n = 225 - 100
n_cores <- detectCores()
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mhglmm.laplace)
})
autor_compare_poisson <- parLapply(cl, seq_len(nsims), safely(autor_trial),
                                   family = "poisson", sqrt_n_fixed = 15,
                                   range = 0.999)
stopCluster(cl)
get_autor_output_from_list(autor_compare_poisson, only_time = FALSE)
rm(autor_compare_poisson)
autor_compare_poisson <- lapply(seq_len(ntime), safely(autor_trial),
                                family = "poisson", sqrt_n_fixed = 15,
                                range = 0.999)
get_autor_output_from_list(autor_compare_poisson, only_time = TRUE)
rm(autor_compare_poisson)

######## poisson n = 400 - 100
n_cores <- detectCores()
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mhglmm.laplace)
})
autor_compare_poisson <- parLapply(cl, seq_len(nsims), safely(autor_trial),
                                   family = "poisson", sqrt_n_fixed = 20,
                                   range = 0.999)
stopCluster(cl)
get_autor_output_from_list(autor_compare_poisson, only_time = FALSE)
rm(autor_compare_poisson)
autor_compare_poisson <- lapply(seq_len(ntime), safely(autor_trial),
                                family = "poisson", sqrt_n_fixed = 20,
                                range = 0.999)
get_autor_output_from_list(autor_compare_poisson, only_time = TRUE)
rm(autor_compare_poisson)

####### poisson n = 625 - 100
n_cores <- detectCores()
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mhglmm.laplace)
})
autor_compare_poisson <- parLapply(cl, seq_len(nsims), safely(autor_trial),
                                   family = "poisson", sqrt_n_fixed = 25,
                                   range = 0.999)
stopCluster(cl)
get_autor_output_from_list(autor_compare_poisson, only_time = FALSE)
rm(autor_compare_poisson)
autor_compare_poisson <- lapply(seq_len(ntime), safely(autor_trial),
                                family = "poisson", sqrt_n_fixed = 25,
                                range = 0.999)
get_autor_output_from_list(autor_compare_poisson, only_time = TRUE)
rm(autor_compare_poisson)

################################################################################
##################### glmmTMB vs spmodel
################################################################################

######## poisson n = 225 - 100
n_cores <- detectCores()
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mhglmm.laplace)
})
geostat_compare_poisson <- parLapply(cl, seq_len(nsims), safely(geostat_trial),
                                     family = "poisson", n_fixed = 125, sp_bayes = TRUE)
stopCluster(cl)
get_geostat_output_from_list(geostat_compare_poisson, only_time = FALSE)
rm(geostat_compare_poisson)
# for time
geostat_compare_poisson <- lapply(seq_len(ntime), safely(geostat_trial),
                                     family = "poisson", n_fixed = 125, sp_bayes = TRUE)
get_geostat_output_from_list(geostat_compare_poisson, only_time = TRUE)
rm(geostat_compare_poisson)

######## poisson n = 400 - 100
n_cores <- detectCores()
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mhglmm.laplace)
})
geostat_compare_poisson <- parLapply(cl, seq_len(nsims), safely(geostat_trial),
                                     family = "poisson", n_fixed = 300)
stopCluster(cl)
get_geostat_output_from_list(geostat_compare_poisson, only_time = FALSE)
rm(geostat_compare_poisson)
# for time
geostat_compare_poisson <- lapply(seq_len(ntime), safely(geostat_trial),
                                  family = "poisson", n_fixed = 300)
get_geostat_output_from_list(geostat_compare_poisson, only_time = TRUE)
rm(geostat_compare_poisson)

######## poisson n = 625 - 100
n_cores <- detectCores()
cl <- makeCluster(n_cores)
clusterEvalQ(cl, {
  library(mhglmm.laplace)
})
geostat_compare_poisson <- parLapply(cl, seq_len(nsims), safely(geostat_trial),
                                     family = "poisson", n_fixed = 525)
stopCluster(cl)
get_geostat_output_from_list(geostat_compare_poisson, only_time = FALSE)
rm(geostat_compare_poisson)
# for time
geostat_compare_poisson <- lapply(seq_len(ntime), safely(geostat_trial),
                                  family = "poisson", n_fixed = 525)
get_geostat_output_from_list(geostat_compare_poisson, only_time = TRUE)
rm(geostat_compare_poisson)

# > sessionInfo()
# R version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 11 x64 (build 22621)
#
# Matrix products: default
#
#
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C
# [5] LC_TIME=English_United States.utf8
#
# time zone: America/Los_Angeles
# tzcode source: internal
#
# attached base packages:
#   [1] parallel  stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
#   [1] purrr_1.0.2               mhglmm.laplace_0.0.0.9000
#
# loaded via a namespace (and not attached):
#   [1] htmlwidgets_1.6.4  devtools_2.4.5     remotes_2.4.2.1    processx_3.8.3     lattice_0.21-9
# [6] callr_3.7.3        tzdb_0.4.0         vctrs_0.6.5        tools_4.3.2        ps_1.7.6
# [11] generics_0.1.3     tibble_3.2.1       proxy_0.4-27       fansi_1.0.6        pkgconfig_2.0.3
# [16] Matrix_1.6-1.1     KernSmooth_2.23-22 desc_1.4.3         lifecycle_1.0.4    compiler_4.3.2
# [21] stringr_1.5.1      statmod_1.5.0      httpuv_1.6.14      htmltools_0.5.7    usethis_2.2.2
# [26] class_7.3-22       crayon_1.5.2       later_1.3.2        pillar_1.9.0       urlchecker_1.0.1
# [31] ellipsis_0.3.2     classInt_0.4-10    cachem_1.0.8       sessioninfo_1.2.2  mime_0.12
# [36] tidyselect_1.2.0   digest_0.6.34      stringi_1.8.3      sf_1.0-15          dplyr_1.1.4
# [41] rprojroot_2.0.4    fastmap_1.1.1      grid_4.3.2         here_1.0.1         cli_3.6.2
# [46] magrittr_2.0.3     pkgbuild_1.4.3     utf8_1.2.4         e1071_1.7-14       withr_3.0.0
# [51] readr_2.1.5        promises_1.2.1     bit64_4.0.5        bit_4.0.5          hms_1.1.3
# [56] memoise_2.0.1      shiny_1.8.0        miniUI_0.1.1.1     profvis_0.3.8      rlang_1.1.3
# [61] Rcpp_1.0.12        spmodel_0.5.1      xtable_1.8-4       glue_1.7.0         DBI_1.2.1
# [66] pkgload_1.3.4      rstudioapi_0.15.0  vroom_1.6.5        R6_2.5.1           fs_1.6.3
# [71] units_0.8-5

# > sessioninfo::session_info()
# ─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.2 (2023-10-31 ucrt)
# os       Windows 11 x64 (build 22621)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United States.utf8
# ctype    English_United States.utf8
# tz       America/Los_Angeles
# date     2024-03-20
# rstudio  2023.09.1+494 Desert Sunflower (desktop)
# pandoc   NA
#
# ─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────
# package        * version    date (UTC) lib source
# bit              4.0.5      2022-11-15 [1] CRAN (R 4.3.2)
# bit64            4.0.5      2020-08-30 [1] CRAN (R 4.3.2)
# cachem           1.0.8      2023-05-01 [1] CRAN (R 4.3.2)
# callr            3.7.3      2022-11-02 [1] CRAN (R 4.3.2)
# class            7.3-22     2023-05-03 [1] CRAN (R 4.3.2)
# classInt         0.4-10     2023-09-05 [1] CRAN (R 4.3.2)
# cli              3.6.2      2023-12-11 [1] CRAN (R 4.3.2)
# crayon           1.5.2      2022-09-29 [1] CRAN (R 4.3.2)
# DBI              1.2.1      2024-01-12 [1] CRAN (R 4.3.2)
# desc             1.4.3      2023-12-10 [1] CRAN (R 4.3.2)
# devtools         2.4.5      2022-10-11 [1] CRAN (R 4.3.2)
# digest           0.6.34     2024-01-11 [1] CRAN (R 4.3.2)
# dplyr            1.1.4      2023-11-17 [1] CRAN (R 4.3.2)
# e1071            1.7-14     2023-12-06 [1] CRAN (R 4.3.2)
# ellipsis         0.3.2      2021-04-29 [1] CRAN (R 4.3.2)
# fansi            1.0.6      2023-12-08 [1] CRAN (R 4.3.2)
# fastmap          1.1.1      2023-02-24 [1] CRAN (R 4.3.2)
# fs               1.6.3      2023-07-20 [1] CRAN (R 4.3.2)
# generics         0.1.3      2022-07-05 [1] CRAN (R 4.3.2)
# glue             1.7.0      2024-01-09 [1] CRAN (R 4.3.2)
# here             1.0.1      2020-12-13 [1] CRAN (R 4.3.2)
# hms              1.1.3      2023-03-21 [1] CRAN (R 4.3.2)
# htmltools        0.5.7      2023-11-03 [1] CRAN (R 4.3.2)
# htmlwidgets      1.6.4      2023-12-06 [1] CRAN (R 4.3.2)
# httpuv           1.6.14     2024-01-26 [1] CRAN (R 4.3.2)
# KernSmooth       2.23-22    2023-07-10 [1] CRAN (R 4.3.2)
# later            1.3.2      2023-12-06 [1] CRAN (R 4.3.2)
# lattice          0.21-9     2023-10-01 [1] CRAN (R 4.3.2)
# lifecycle        1.0.4      2023-11-07 [1] CRAN (R 4.3.2)
# magrittr         2.0.3      2022-03-30 [1] CRAN (R 4.3.2)
# Matrix           1.6-1.1    2023-09-18 [1] CRAN (R 4.3.2)
# memoise          2.0.1      2021-11-26 [1] CRAN (R 4.3.2)
# mhglmm.laplace * 0.0.0.9000 2024-03-20 [1] local
# mime             0.12       2021-09-28 [1] CRAN (R 4.3.1)
# miniUI           0.1.1.1    2018-05-18 [1] CRAN (R 4.3.2)
# pillar           1.9.0      2023-03-22 [1] CRAN (R 4.3.2)
# pkgbuild         1.4.3      2023-12-10 [1] CRAN (R 4.3.2)
# pkgconfig        2.0.3      2019-09-22 [1] CRAN (R 4.3.2)
# pkgload          1.3.4      2024-01-16 [1] CRAN (R 4.3.2)
# processx         3.8.3      2023-12-10 [1] CRAN (R 4.3.2)
# profvis          0.3.8      2023-05-02 [1] CRAN (R 4.3.2)
# promises         1.2.1      2023-08-10 [1] CRAN (R 4.3.2)
# proxy            0.4-27     2022-06-09 [1] CRAN (R 4.3.2)
# ps               1.7.6      2024-01-18 [1] CRAN (R 4.3.2)
# purrr          * 1.0.2      2023-08-10 [1] CRAN (R 4.3.2)
# R6               2.5.1      2021-08-19 [1] CRAN (R 4.3.2)
# Rcpp             1.0.12     2024-01-09 [1] CRAN (R 4.3.2)
# readr            2.1.5      2024-01-10 [1] CRAN (R 4.3.2)
# remotes          2.4.2.1    2023-07-18 [1] CRAN (R 4.3.2)
# rlang            1.1.3      2024-01-10 [1] CRAN (R 4.3.2)
# rprojroot        2.0.4      2023-11-05 [1] CRAN (R 4.3.2)
# rstudioapi       0.15.0     2023-07-07 [1] CRAN (R 4.3.2)
# sessioninfo      1.2.2      2021-12-06 [1] CRAN (R 4.3.2)
# sf               1.0-15     2023-12-18 [1] CRAN (R 4.3.2)
# shiny            1.8.0      2023-11-17 [1] CRAN (R 4.3.2)
# spmodel          0.5.1      2024-01-09 [1] CRAN (R 4.3.2)
# statmod          1.5.0      2023-01-06 [1] CRAN (R 4.3.3)
# stringi          1.8.3      2023-12-11 [1] CRAN (R 4.3.2)
# stringr          1.5.1      2023-11-14 [1] CRAN (R 4.3.2)
# tibble           3.2.1      2023-03-20 [1] CRAN (R 4.3.2)
# tidyselect       1.2.0      2022-10-10 [1] CRAN (R 4.3.2)
# tzdb             0.4.0      2023-05-12 [1] CRAN (R 4.3.2)
# units            0.8-5      2023-11-28 [1] CRAN (R 4.3.2)
# urlchecker       1.0.1      2021-11-30 [1] CRAN (R 4.3.2)
# usethis          2.2.2      2023-07-06 [1] CRAN (R 4.3.2)
# utf8             1.2.4      2023-10-22 [1] CRAN (R 4.3.2)
# vctrs            0.6.5      2023-12-01 [1] CRAN (R 4.3.2)
# vroom            1.6.5      2023-12-05 [1] CRAN (R 4.3.2)
# withr            3.0.0      2024-01-16 [1] CRAN (R 4.3.2)
# xtable           1.8-4      2019-04-21 [1] CRAN (R 4.3.2)
