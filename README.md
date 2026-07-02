# Biblioscape

Research compendium for visualizing the scientific landscape through bibliographic network analysis


So far, this is a case study/proof of concept using [FRB-MTE-OFB projects](https://www.fondationbiodiversite.fr/la-frb-en-action/programmes-et-projets/impacts-sur-la-biodiversite-terrestre-dans-lanthropocene/):  

- clean references and get their DOI when missing

``` r
source("analysis/01_clean_references.R")
```

- fetch records from [openalex database](https://openalex.org/)

``` r
source("analysis/02_fetch_openalex.R")
```

- explore bibliometrix R package
```r
quarto::quarto_render("analysis/03_explore_bibliometrix.qmd")
```

## References:   

- Aria, M., Le, T., Cuccurullo, C., Belfiore, A., & Choe, J. (2024). openalexR: An R-Tool for Collecting Bibliometric Data from OpenAlex. R J., 15(4), 167-180.  

- Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), 959-975

- Aria, M. & Cuccurullo, C. (2026). Science Mapping Analysis: A Primer with Biblioshiny, McGraw-Hill Education. ISBN: 978-88-386-2297-7.