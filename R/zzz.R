.onAttach <- function(libname, pkgname) {
  vers <-  "0.1.0"
  packageStartupMessage("|=============================================|")
  packageStartupMessage("| Tools for Genomic Teaching (genomics ", vers,  ") |")
  packageStartupMessage("| Author: Tiago Olivoto                       |")
  # packageStartupMessage("| Visit 'xxxxxx' for a short tutorial         |")
  packageStartupMessage("|=============================================|")
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".", "code", "codon", "fb", "sb", "tb"))
  }
