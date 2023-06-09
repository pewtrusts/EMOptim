% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EMsample.r
\name{EMsample}
\alias{EMsample}
\title{EM utility function}
\usage{
EMsample(
  EM,
  objective.label,
  strata.label,
  N = NULL,
  sampling.rate = NULL,
  nsims = NULL,
  quiet = TRUE,
  specification.only = FALSE,
  keep.samples = FALSE,
  sampling.fractions = NULL
)
}
\arguments{
\item{objective.label}{The label of the objective function to simulate and sample from}

\item{strata.label}{The label of the stratification definition to use}

\item{N}{The number of samples. If NULL, then defaults to the sampling rate to calculate the number of samples}

\item{sampling.rate}{The sampling rate (a proportion between 0 and 1). See N}

\item{nsims}{The number of simulations to run. Defaults to the number defined in the EM object}

\item{quiet}{If false, then print information messages during the sampling}

\item{specification.only}{if true, then only return the objective specification parameters and the cell specific mu's and sd's}

\item{keep.samples}{if true, then returns the individual simulation sample means and sd's}

\item{sampling.fractions}{Provides the sampling fractions for each strata. If NULL (the default), then determined using Neyman allocation}
}
\description{
This function does most of the work .. samples for a specified objective
}
\author{
Alistair Dunn & Johanna Pierre
}
