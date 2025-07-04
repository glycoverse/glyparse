% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse-glycoct.R
\name{parse_glycoct}
\alias{parse_glycoct}
\title{Parse GlycoCT Structures}
\usage{
parse_glycoct(x)
}
\arguments{
\item{x}{A character vector of GlycoCT strings.}
}
\value{
A glycan_structure object if \code{x} is a single character,
or a glycan_structure vector if \code{x} is a character vector.
}
\description{
This function parses GlycoCT strings into glycan graphs.
GlycoCT is a format used by databases like GlyTouCan and GlyGen.
}
\details{
GlycoCT format consists of two parts:
\itemize{
\item RES: Contains monosaccharides (lines starting with 'b:') and substituents (lines starting with 's:')
\item LIN: Contains linkage information between residues
}

For more information about GlycoCT format, see the glycoct.md documentation.
}
\examples{
glycoct <- paste0(
  "RES\n",
  "1b:a-dgal-HEX-1:5\n",
  "2s:n-acetyl\n",
  "3b:b-dgal-HEX-1:5\n",
  "LIN\n",
  "1:1d(2+1)2n\n",
  "2:1o(3+1)3d"
)
parse_glycoct(glycoct)

}
