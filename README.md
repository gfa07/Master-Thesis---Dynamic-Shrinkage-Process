# Dynamic Shrinkage Process in Stan

![Status](https://img.shields.io/badge/status-in%20progress-yellow)
![Language](https://img.shields.io/badge/language-R%20%7C%20Stan-blue)

---

Implementation of Dynamic Shrinkage Process (DSP) priors from Kowal (2019) in Stan.

## Overview

This repository investigates the feasibility and implementation of DSP priors within the Stan probabilistic programming framework. The focus is on modeling time-varying parameters under global-local shrinkage, with attention to computational performance and inference quality.

---

## Repository Structure

- `stan/` — Stan model implementations of DSP priors, plus the R scripts specific to running them
- `R/` — R scripts for data generation, model fitting, and analysis
- `data/` — datasets (simulated or empirical)
- `docs/` — supplementary materials and notes, mainly plots
- `DSP/` — the modified `dspPerfected` package (forked from the original `dsp`), plus the R scripts used to build and fit the DSP models for the synthetic data
- `CompStanDSP/` — comparison scripts between the Stan and DSP implementations

---

## Requirements

* R (≥ 4.x)
* Stan (via `rstan` or `cmdstanr`)

---

## Reference

Daniel R. Kowal, David S. Matteson, & David Ruppert (2019). Dynamic shrinkage processes. Journal of the Royal Statistical Society: Series B, 81(4), 781–804. https://doi.org/10.1111/rssb.12325

---
