% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_feature.R
\name{generate_feature}
\alias{generate_feature}
\title{Feature Generation for Contamination Detection Model}
\usage{
generate_feature(file, hom_p = 0.999, het_p = 0.5, hom_rho = 0.005,
  het_rho = 0.1, mixture, homcut = 0.99, highcut = 0.7, hetcut = 0.3)
}
\arguments{
\item{file}{VCF input object}

\item{hom_p}{The initial value for p in Homozygous Beta-Binomial model, default is 0.999}

\item{het_p}{The initial value for p in Heterozygous Beta-Binomial model, default is 0.5}

\item{hom_rho}{The initial value for rho in Homozygous Beta-Binomial model, default is 0.005}

\item{het_rho}{The initial value for rho in Heterozygous Beta-Binomial model, default is 0.1}

\item{mixture}{A vector of whether the sample is contaminated: 0 for pure; 1 for contaminated}

\item{homcut}{Cutoff allele frequency value between hom and high, default is 0.99}

\item{highcut}{Cutoff allele frequency value between high and het, default is 0.7}

\item{hetcut}{Cutoff allele frequency value between het and low, default is 0.3}
}
\value{
A data frame with all features for training model of contamination detection
}
\description{
Generates features from each pair of input VCF objects for training contamination detection model.
}
