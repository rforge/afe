% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/allFit.R
\name{allFit}
\alias{allFit}
\title{Refit \code{lmer} model using multiple optimizers}
\usage{
allFit(m, meth.tab = cbind(optimizer = rep(c("bobyqa", "Nelder_Mead",
  "optimx", "nloptwrap"), c(1, 1, 2, 2)), method = c("", "", "nlminb",
  "L-BFGS-B", "NLOPT_LN_NELDERMEAD", "NLOPT_LN_BOBYQA")), verbose = TRUE,
  maxfun = 1e+05, ...)
}
\arguments{
\item{m}{a fitted model with \code{lmer}}

\item{meth.tab}{a matrix (or data.frame) with columns
- method the name of a specific optimization method to pass to the optimizer
(leave blank for built-in optimizers)
- optimizer the \code{optimizer} function to use}

\item{verbose}{print progress messages?}

\item{maxfun}{number of iterations to allow for the optimization rountine.}

\item{...}{further arguments passed to \code{\link{update.merMod}} such as data.}
}
\value{
a list of fitted \code{merMod} objects
}
\description{
Attempt to re-fit a [g]lmer model with a range of optimizers.
The default is to use all known optimizers for R that satisfy the
requirements (do not require explicit gradients, allow
box constraints), in three categories; (i) built-in
(\code{minqa::bobyqa}, \code{lme4::Nelder_Mead}), (ii) wrapped via optimx
(most of optimx's optimizers that allow box constraints require
an explicit gradient function to be specified; the two provided
here are really base R functions that can be accessed via optimx,
(iii) wrapped via nloptr.
}
\details{
Needs packages \pkg{nloptr} and \pkg{optimx} to try out all optimizers. \pkg{optimx} needs to be loaded explicitly using \code{library} or \code{require}.
}
\examples{
\dontrun{
require(optimx)
gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
data = cbpp, family = binomial)
gm_all <- allFit(gm1)
t(sapply(gm_all,fixef)) ## extract fixed effects
sapply(gm_all,logLik) ## log-likelihoods
sapply(gm_all,getME,"theta") ## theta parameters
!sapply(gm_all,inherits,"try-error") ## was fit OK?
}
}
\author{
Ben Bolker
}
\seealso{
slice, slice2D in the bbmle package
}

