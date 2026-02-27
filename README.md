# RMQS v2.0 – Shiny Application  
**Revised Methodological Quality Scale**

## Overview

RMQS v2.0 is an interactive Shiny application designed to operationalize and apply the *Revised Methodological Quality Scale (RMQS)* for the evaluation of methodological rigor in educational and psychological research.

The application implements the full RMQS v2.0 framework, including bilingual item definitions, structured coding options, and exportable outputs to facilitate transparent and reproducible quality assessment workflows.

This repository contains the full source code, configuration files, and dependency lockfile required to reproduce the application.

---

## The RMQS v2.0 Framework

RMQS v2.0 is a metascientific refinement of earlier methodological quality instruments, designed to:

- Improve operational clarity of criteria
- Reduce ambiguity in coding decisions
- Facilitate reproducible quality assessment
- Enable structured auditing of research designs

The application separates:

- **Core app logic** (`app.R`)
- **Configuration file** (`items_extended_bilingual.xlsx`)
- **Export-ready item structure** (`items.csv`)
- **Dependency lockfile** (`renv.lock`)

The configuration file functions as a compact operational representation of the extended RMQS codebook.

---

## Reproducibility

This repository includes an `renv.lock` file to ensure computational reproducibility.

To reproduce the environment:

```r
install.packages("renv")
renv::restore()
