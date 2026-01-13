# Template Repo for building an R package

- [usethis](https://usethis.r-lib.org/)
- [devtools](https://devtools.r-lib.org/)
- [pkgdown](https://pkgdown.r-lib.org/) (requires [pandoc to be installed](https://github.com/jgm/pandoc/releases/tag/3.8.3))


## R Package Structure

Below are the folders and files that are often found in an R package.

| Description | Source | Bundle | Binary |
| :---------- | :----- | :----- | :--- |
| **Important Metadata** | DESCRIPTION, NAMESPACE, LICENSE, NEWS.md | DESCRIPTION, NAMESPACE, LICENSE, NEWS.md | DESCRIPTION, NAMESPACE, LICENSE, NEWS.md |
| **Documentation** | README.Rmd $\rightarrow$ README.md | README.md | *(Not included)* |
| **Data Directory** | `data/` (aa.rda, bb.rda) | `data/` (aaa.rda, bbb.rda) | `data/` (Rdata.rdb, Rdata.rds, Rdata.rdx) |
| **Help Files** | `man/` | `man/` | INDEX, Meta/, help/, html/ |
| **R Code** | `R/` (ccc.R, ddd.R, sysdata.rda) | `R/` (ccc.R, ddd.R, sysdata.rda) | `R/` (zzzpackage, zzzpackage.rdb, zzzpackage.rdx, sysdata.rdb, sysdata.rdx) |
| **Compiled Code** | `src/` | `src/` | `libs/` |
| **Tests** | `tests/` | `tests/` | *(Not included)* |
| **Installed Files** | `inst/` (eee-file, fff-folder/, CITATION) | `inst/` (eee-file, fff-folder/, CITATION) | eee-file, fff-folder/, CITATION (moved to top-level) |
| **Vignettes & Articles** | `vignettes/`, `articles/` (ggg-article.Rmd, hhh-vignette.Rmd) | `build/vignette.rds`, `inst/doc/`, `vignettes/` (hhh-vignette.Rmd) | `doc/` (hhh-vignette.R, hhh-vignette.Rmd, hhh-vignette.html, index.html) |
| **Dev/Config Files** | .github/, .gitignore, .Rbuildignore, _pkgdown.yml, etc. | *(Not included)* | *(Not included)* |

* [R Packages (2e): Package structure and state](https://r-pkgs.org/structure.html)

## Building a package website

1. `devtools::document()` will create the documentation for your exported functions in your `/R` folder files.
2. `pkgdown::build_site_github_pages()` will create your `/docs` folder that can be used in Github for your Github pages website.

