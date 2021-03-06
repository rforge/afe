#' Analysis of Factorial Experiments.
#'
#' \tabular{ll}{
#' Package: \tab afex\cr
#' Type: \tab Package\cr
#' Version: \tab 0.14-180\cr
#' Date: \tab 2015-05-18\cr
#' Depends: \tab R (>= 3.0.0), lme4 (>= 1.0.5), reshape2, lsmeans (>= 2.17)\cr
#' Encoding: \tab latin1\cr
#' License: \tab GPL (>=3)\cr
#' URL: \tab http://www.psychologie.uni-freiburg.de/Members/singmann/R/afex\cr
#' }
#'
#' Provides convenience functions for analyzing factorial experiments using ANOVA or mixed models. aov_ez(), aov_car(), and aov_4() allow convenient specification of between, within (i.e., repeated-measures), or mixed between-within (i.e., split-plot) ANOVAs for data in long format (i.e., one observation per row), potentially aggregating multiple observations per individual and cell of the design. mixed() fits mixed models using lme4::lmer() and computes p-values for all effects using either Kenward-Roger approximation for degrees of freedom (LMM only), parametric bootstrap (LMMs and GLMMs), or likelihood ratio tests (LMMs and GLMMs). afex uses type 3 sums of squares as default (imitating commercial statistical software).
#'
#' @aliases afex-package
#' @name afex-package
#' @docType package
#' @title The afex Package
#' @author Henrik Singmann, Ben Bolker, Jake Westfall, with contributions from S�ren H�jsgaard, John Fox, Michael A. Lawrence, Ulf Mertens
#' @keywords package
NULL
