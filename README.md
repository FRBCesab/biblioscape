# Biblioscape

Research compendium for visualizing the scientific landscape through bibliographic network analysis


The first case study uses [FRB-MTE-OFB projects](https://www.fondationbiodiversite.fr/la-frb-en-action/programmes-et-projets/impacts-sur-la-biodiversite-terrestre-dans-lanthropocene/):  

- clean references and get their DOI when missing

``` r
source("analysis/01_clean_references.R")
```

- fetch records from [openalex database](https://openalex.org/)

``` r
source("analysis/02_fetch_openalex.R")
```

- explore [bibliometrix R package](https://frbcesab.github.io/biblioscape/analysis/03_explore_bibliometrix.html)
```r
quarto::quarto_render("analysis/03_explore_bibliometrix.qmd")
```

- explore [FRB-MTE-OFB membership network](https://frbcesab.github.io/biblioscape/analysis/04a_explore_mte_members.html)
```r
quarto::quarto_render("analysis/04a_explore_mte_members.qmd")
```

- explore [FRB-MTE-OFB citation  network](https://frbcesab.github.io/biblioscape/analysis/04b_explore_mte_citations.html)  
```r
quarto::quarto_render("analysis/04b_explore_mte_citations.qmd")
```

- explore [FRB-MTE-OFB bibliographic coupling](https://frbcesab.github.io/biblioscape/analysis/04c_explore_mte_coupling.html)  
```r
quarto::quarto_render("analysis/04c_explore_mte_coupling.qmd")
```


The second case study uses [FRB CESAB projects](https://www.fondationbiodiversite.fr/en/about-the-foundation/le-cesab/):   

- explore [CESAB networks](https://frbcesab.github.io/biblioscape/analysis/05a_explore_cesab.html)    
```r
quarto::quarto_render("analysis/05a_explore_cesab.qmd")
```



Many documents are available online. 

- render the online index
```r
quarto::quarto_render("index.qmd")
```




## References:   

- Aria, M., Le, T., Cuccurullo, C., Belfiore, A., & Choe, J. (2024). openalexR: An R-Tool for Collecting Bibliometric Data from OpenAlex. R J., 15(4), 167-180.  

- Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), 959-975

- Aria, M. & Cuccurullo, C. (2026). Science Mapping Analysis: A Primer with Biblioshiny, McGraw-Hill Education. ISBN: 978-88-386-2297-7.

- Csardi G, Nepusz T (2006). “The igraph software package for complex network research.” InterJournal, Complex Systems, 1695. https://igraph.org.  

## Tutorial on network analysis

- Hugues Pecout H, Beauguitte L, Fernandez M (2023) Analyse de réseau avec igraph, <https://elementr.gitpages.huma-num.fr/session_reseau/intro_reseau_igraph/igraph.html>