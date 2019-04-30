---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
```


virtualPollen
============

This package is part of an upcoming paper. It provides a set of functions to:


+   Generate virtual drivers with given temporal autocorrelation.
+   Run population models of virtual taxa with different life and niche traits (maximum age, reproductive age, fecundity, growth rate, niche position, and niche breadth) that generate synthetic pollen curves based on these virtual drivers.
+   Simulate sediment accumulation rate curves and apply them to virtual pollen curves to represent the effect of taphonomic processes.
+   Represent pollen sampling at different depth intervals.
+   Interpolate virtual pollen curves sampled at different depth intervals into regular time to allow direct comparison between pollen curves generated at different resolutions.



