% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sortie_carbon.R
\name{sortie_tree_carbon}
\alias{sortie_tree_carbon}
\title{Title}
\usage{
sortie_tree_carbon(sortie_outputs, dead = FALSE, BEC, ht_from_diam = TRUE)
}
\arguments{
\item{sortie_outputs}{sortie output saved in a data.table}

\item{dead}{include snags in the calculation}

\item{BEC}{Biogeoclimatic ecosystem classification zone}

\item{Ht_from_diam}{calculate the height based on the diameter (allometry) == TRUE
or use Height from Sortie == FALSE}
}
\description{
Title
}
\details{
Currently only supports common species in the SBS and ICH
To do: add argument for seedling to call the seedling carbon function
}
