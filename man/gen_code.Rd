% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_code.R
\name{gen_code}
\alias{gen_code}
\alias{aminoacid}
\title{Tools for the genetic code}
\usage{
aminoacid(codon, transl_table = 1, type = "olc")

gen_code(transl_table = 1)
}
\arguments{
\item{codon}{A character vector (upper- or lower-case letters) with the 5'-3'
codon (e.g., "AUG").}

\item{transl_table}{The translation table used. Defaults to 1 (standard
genetic code). See more in \code{Details} section.}

\item{type}{The type of output. One of
\itemize{
\item \code{"olc"} for one-letter code, e.g., "V".
\item \code{"tlc"} for three-letter code, e.g., "Val".
\item \code{"name"} for the complete name, e.g., "Valine".
}}
}
\value{
\itemize{
\item \code{aminoacid()} returns a character vector in the format \code{code_letter_name} of the given aminoacid(s)
\item \code{gen_code()} returns a data.frame object with the following columns.
\item \code{fb} The first base of the codon.
\item \code{sb} The second base of the codon.
\item \code{tb} The third base of the codon.
\item \code{codon} The codon (5'-3').
\item \code{olc} The one letter code.
\item \code{tlc} The three letter code.
\item \code{name} The name of the aminoacid.
}

The stop codons are depicted by \code{"*"}
}
\description{
\itemize{
\item \code{aminoacid()} returns the aminoacid generated by the informed codon(s).
\item \code{gen_code()} returns all the amino acids considering the translation table
used.
}
}
\details{
The translation is performed according to the desired translation
table. See more at
\url{https://www.ncbi.nlm.nih.gov/Taxonomy/taxonomyhome.html/index.cgi?chapter=cgencodes}
\enumerate{
\item The Standard Code (default)
\item The Vertebrate Mitochondrial Code
\item The Yeast Mitochondrial Code
\item The Mold, Protozoan, and Coelenterate Mitochondrial Code and the
Mycoplasma/Spiroplasma Code
\item The Invertebrate Mitochondrial Code
\item The Ciliate, Dasycladacean and Hexamita Nuclear Code
\item The Echinoderm and Flatworm Mitochondrial Code
\item The Euplotid Nuclear Code
\item The Bacterial, Archaeal and Plant Plastid Code
\item The Alternative Yeast Nuclear Code
\item The Ascidian Mitochondrial Code
\item The Alternative Flatworm Mitochondrial Code
\item Chlorophycean Mitochondrial Code
\item Trematode Mitochondrial Code
\item Scenedesmus obliquus Mitochondrial Code
\item Thraustochytrium Mitochondrial Code
\item Rhabdopleuridae Mitochondrial Code
\item Candidate Division SR1 and Gracilibacteria Code
\item Pachysolen tannophilus Nuclear Code
\item Karyorelict Nuclear Code
\item Condylostoma Nuclear Code
\item Mesodinium Nuclear Code
\item Peritrich Nuclear Code
\item Blastocrithidia Nuclear Code
\item Cephalodiscidae Mitochondrial UAA-Tyr Code
}
}
\examples{
aac <- c("AUG", "CGA", "UGA")
aminoacid(aac)
aminoacid(aac, transl_table = 2)

# complete genetic code
gen_code()

}
