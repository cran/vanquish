% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_ct.R
\name{train_ct}
\alias{train_ct}
\title{Train Contamination Detection Model}
\usage{
train_ct(feature)
}
\arguments{
\item{feature}{Feature list objects from generate_feature()}
}
\value{
A list contains two trained svm models: regression & classification
}
\description{
Trains two SVM models (classification and regression) to detects whether a sample is contaminated another sample of its same species.
}
