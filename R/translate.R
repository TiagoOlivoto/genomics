#' Translate a RNA sequence to a protein sequence
#'
#' Translate a nucleotide (RNA) sequence to a protein chain according to the
#' standard genetic code
#'
#' @param mrna The messenger RNA.
#' @param transl_table The translation table used. Defaults to 1 (standard
#'   genetic code). See more at [aminoacid()]
#' @param start_codon The start codon. Defaults to `"AUG"`.
#' @param start_codon_first Start the translation with the `start_codon`?
#'   defaults to `TRUE`. In this case, all bases before the `start_codon` will
#'   be ignored.
#' @param no_stop A logical indicating if the translation shouldn't stop when a
#'   stop codon is encountered. Defaults to FALSE. If `TRUE`, translation will
#'   continue up to the end of the chain, and stop codons will be represented by
#'   `"*"`.
#' @param type The type of output. One of
#' * `"olc"` for one-letter code, e.g., "V" (default).
#' * `"tlc"` for three-letter code, e.g., "Val".
#' * `"name"` for the complete name, e.g., "Valine".
#' @param sep The separator between amino acids. Defaults to `"-"`.
#' @return A character
#' @export
#'
#' @examples
#' code <- rand_nuc_seq(25, type = "rna")
#' translate(code)
translate <- function(mrna,
                      transl_table = 1,
                      start_codon = "AUG",
                      start_codon_first = TRUE,
                      no_stop = FALSE,
                      type = "olc",
                      sep = ""){
  if (length(mrna) > 1){
    as.character(do.call(rbind, lapply(mrna, translate, transl_table, start_codon, start_codon_first, no_stop, type, sep)))
  } else{
    if(!is_rna(mrna, verbose = FALSE)){
      stop("A Thimine (T) was found in the sequence. Translation interrupted.",  call. = FALSE)
    }
    mrna <- tidy_sequence(mrna)
    if (start_codon_first == TRUE){
      scod <- str_locate(mrna, start_codon)
      subs <- str_sub(mrna, min(scod), nchar(mrna))
      subs <-  split_each(subs, n = 3)
    } else{
      subs <-  split_each(mrna, n = 3)
    }
    seq <- aminoacid(subs, type = type, transl_table = transl_table)
    if (no_stop == FALSE & "*" %in% seq){
      seq <- seq[1:min(which(seq == "*"))]
    }
    seq <- paste0(seq, collapse = sep)
    return(seq)
  }
}
