% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_vcf.R
\name{update_vcf}
\alias{update_vcf}
\title{Remove CNV regions within VCF files}
\usage{
update_vcf(rmCNV = FALSE, vcf, cnvobj = NULL, threshold = 0.1,
  skew = 0.5, lower = 0.45, upper = 0.55)
}
\arguments{
\item{rmCNV}{Remove CNV regions, default is FALSE}

\item{vcf}{Input VCF files}

\item{cnvobj}{cnv object, default is NULL}

\item{threshold}{Threshold for allele frequency, default is 0.1}

\item{skew}{Skewness for allele frequency, default is 0.5}

\item{lower}{Lower bound for allele frequency region, default is 0.45}

\item{upper}{Upper bound for allele frequency region, default is 0.55}
}
\value{
VCF file without CNV region
}
\description{
Remove CNV regions within VCF files
}
