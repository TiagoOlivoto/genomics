
<!-- README.md is generated from README.Rmd. Please edit that file -->

# genomics

`genomics` is specially designed for genomic teaching. The package will
help with the generation of random nucleotides sequences with start and
stop codons (`rand_nuc_seq()`), interpretation of genetic code in
different translation tables (`gen_code()` and `aminoacid()`),
antiparallel sequences (`antiparallel()`), and RNA translation to
polypeptide chains (`translate()`, `dna_to_rna()`, `rna_to_dna()`)

# Installation

`genomics` is only on
[GitHub](https://github.com/TiagoOlivoto/genomics), and can be installed
with

``` r
devtools::install_github("TiagoOlivoto/genomics")
```

*Note*: If you are a Windows user, you should also first download and
install the latest version of
[Rtools](https://cran.r-project.org/bin/windows/Rtools/).

# Genetic code

``` r
library(genomics)
# |===========================================|
# | Tools for genomics (genomics 0.1.0)   |
# | Author: Tiago Olivoto                     |
# |===========================================|
aminoacid("AUG")
# [1] "M"
aminoacid(c("AUG", "CGA", "UGA"), type = "name")
# [1] "Methionine" "Arginine"   "*"

# standard genetic code
gen_code()
#    fb sb tb codon olc tlc          name
# 1   A  A  A   AAA   K Lys        Lysine
# 2   A  A  U   AAU   N Asn    Asparagine
# 3   A  A  G   AAG   K Lys        Lysine
# 4   A  A  C   AAC   N Asn    Asparagine
# 5   A  U  A   AUA   I Ile    Isoleucine
# 6   A  U  U   AUU   I Ile    Isoleucine
# 7   A  U  G   AUG   M Met    Methionine
# 8   A  U  C   AUC   I Ile    Isoleucine
# 9   A  G  A   AGA   R Arg      Arginine
# 10  A  G  U   AGU   S Ser        Serine
# 11  A  G  G   AGG   R Arg      Arginine
# 12  A  G  C   AGC   S Ser        Serine
# 13  A  C  A   ACA   T Thr     Threonine
# 14  A  C  U   ACU   T Thr     Threonine
# 15  A  C  G   ACG   T Thr     Threonine
# 16  A  C  C   ACC   T Thr     Threonine
# 17  U  A  A   UAA   *   *             *
# 18  U  A  U   UAU   Y Tyr     Tyrosisne
# 19  U  A  G   UAG   *   *             *
# 20  U  A  C   UAC   Y Tyr     Tyrosisne
# 21  U  U  A   UUA   L Leu       Leucine
# 22  U  U  U   UUU   F Phe Phenylalanine
# 23  U  U  G   UUG   L Leu       Leucine
# 24  U  U  C   UUC   F Phe Phenylalanine
# 25  U  G  A   UGA   *   *             *
# 26  U  G  U   UGU   C Cys      Cysteine
# 27  U  G  G   UGG   W Trp   Tryptophane
# 28  U  G  C   UGC   C Cys      Cysteine
# 29  U  C  A   UCA   S Ser        Serine
# 30  U  C  U   UCU   S Ser        Serine
# 31  U  C  G   UCG   S Ser        Serine
# 32  U  C  C   UCC   S Ser        Serine
# 33  G  A  A   GAA   E Glu  GlutamicAcid
# 34  G  A  U   GAU   D Asp  AsparticAcid
# 35  G  A  G   GAG   E Glu  GlutamicAcid
# 36  G  A  C   GAC   D Asp  AsparticAcid
# 37  G  U  A   GUA   V Val        Valine
# 38  G  U  U   GUU   V Val        Valine
# 39  G  U  G   GUG   V Val        Valine
# 40  G  U  C   GUC   V Val        Valine
# 41  G  G  A   GGA   G Gly       Glycine
# 42  G  G  U   GGU   G Gly       Glycine
# 43  G  G  G   GGG   G Gly       Glycine
# 44  G  G  C   GGC   G Gly       Glycine
# 45  G  C  A   GCA   A Ala       Alanine
# 46  G  C  U   GCU   A Ala       Alanine
# 47  G  C  G   GCG   A Ala       Alanine
# 48  G  C  C   GCC   A Ala       Alanine
# 49  C  A  A   CAA   Q Gln     Glutamine
# 50  C  A  U   CAU   H His     Histidine
# 51  C  A  G   CAG   Q Gln     Glutamine
# 52  C  A  C   CAC   H His     Histidine
# 53  C  U  A   CUA   L Leu       Leucine
# 54  C  U  U   CUU   L Leu       Leucine
# 55  C  U  G   CUG   L Leu       Leucine
# 56  C  U  C   CUC   L Leu       Leucine
# 57  C  G  A   CGA   R Arg      Arginine
# 58  C  G  U   CGU   R Arg      Arginine
# 59  C  G  G   CGG   R Arg      Arginine
# 60  C  G  C   CGC   R Arg      Arginine
# 61  C  C  A   CCA   P Pro       Proline
# 62  C  C  U   CCU   P Pro       Proline
# 63  C  C  G   CCG   P Pro       Proline
# 64  C  C  C   CCC   P Pro       Proline
```

# Generate a DNA sequence

``` r
# 'code' DNA
dna <- rand_nuc_seq(n = 40)
# Start codon  : 3-5
# Stop codon   : TAG
# Position stop: 36-38
dna
# [1] "ATATGGGCGGAGAGGGGAGCGCGAGCCGATGGCTCTAGAC"

# transcription to mRNA
mrna <- dna_to_rna(dna)
mrna
# $c_dna
# [1] "ATATGGGCGGAGAGGGGAGCGCGAGCCGATGGCTCTAGAC"
# 
# $m_dna
# [1] "TATACCCGCCTCTCCCCTCGCGCTCGGCTACCGAGATCTG"
# 
# $m_rna
# [1] "AUAUGGGCGGAGAGGGGAGCGCGAGCCGAUGGCUCUAGAC"
```

# Translation

In the following example, the mRNA (`m_rna`) in `mrna` is translated to
a polypeptide chain.

``` r
# one letter code
# stop codon as *
translate(mrna$m_rna)
# [1] "MGGEGSASRWL*"

# Three letter code
# translation continues after stop codon
translate(mrna$m_rna,
          type = "tlc",
          no_stop = TRUE,
          sep = ".")
# [1] "Met.Gly.Gly.Glu.Gly.Ser.Ala.Ser.Arg.Trp.Leu.*"
```
