#' Tools for transcription DNA to RNA
#'
#' @param dna,rna A character vector containing the code sequence (e.g., "ACG")
#' @param type The type of sequence. It must be either `"code"` (the code helix)
#'   or `"mold"` the helix read by RNA polymerase.
#' @return
#' * `dna_to_rna()` returns a list with the following elements:
#'   * `c_dna` The 'code' DNA.
#'   * `m_dna` The molder DNA (antiparallel of `c_dna`)
#'   * `m_rna` The pré- messenger RNA, i.e., the `c_dna` with `U` instead `T`.
#' * `rna_to_dna()` returns a list with the following elements:
#'   * `m_rna` The messenger RNA declared.
#'   * `m_dna` The molder DNA (antiparallel of `m_rna`)
#'   * `c_rna` The 'code' DNA., i.e., the `m_rna` with `T` instead `U`.
#' @export
#' @name dna_rna
#'
#' @examples
#' # convert a dna (code or mold) to a messenger RNA
#' dna_to_rna(c("ATGCGC", "AGC"))
#' dna_to_rna(c("ATGCGC", "AGC"), type = "mold")
#'
#' # convert a messenger RNA to dna code and mold
#' rna_to_dna(c("AUGCGC", "CGACGGA"))
dna_to_rna <- function(dna, type = "code"){
  if(length(dna) > 1){
    do.call(rbind, lapply(dna, dna_to_rna, type))
  } else{
    dnac_to_rna <- function(dna_c){
      dna_c <- toupper(dna_c)
      lapply(strsplit(dna_c, "")[[1]], function(x){
        ifelse(x == "T", "U", x)
      }
      ) %>%
        as.character() %>%
        paste0(collapse = "")
    }
    if(!is_dna(dna, verbose = FALSE)){
      stop("An Uracil (U) was found in the sequence. Nothing made.", call. = FALSE)
    }
    if(type == "code"){
      dna <- dna %>% toupper()
    } else{
      dna <- antiparallel(dna) %>% toupper()
    }

    m_dna <- antiparallel(dna)
    m_rna <- dnac_to_rna(dna)
    return(list(c_dna = dna,
                m_dna = m_dna,
                m_rna = m_rna))
  }
}

#' @export
#' @name dna_rna
rna_to_dna <- function(rna){
  if(length(rna) > 1){
    do.call(rbind, lapply(rna, rna_to_dna))
  } else{
    rna <- toupper(rna)
    rna_to_cdna <- function(mrna){
      mrna <- toupper(mrna)
      lapply(strsplit(mrna, "")[[1]], function(x){
        ifelse(x == "U", "T", x)
      }
      ) %>%
        as.character() %>%
        paste0(collapse = "")
    }
    if(!is_rna(rna, verbose = FALSE)){
      stop("A Thimine (T) was found in the sequence. Nothing made.", call. = FALSE)
    }
    c_dna <- rna_to_cdna(rna)
    m_dna <- antiparallel(c_dna)
    return(list(m_rna = rna,
                c_dna = c_dna,
                m_dna = m_dna))
  }
}
