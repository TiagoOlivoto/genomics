#' Generate random nucleotide sequences
#'
#'Generate random sequences of nucleotides (i.e., A, T, C, and G) for DNA and A,
#'U, C, and G for RNA. (See details for more information).
#'
#'The random sequences are generated so that a start codon (AUG for RNA or ATG
#'for DNA) is inserted between the first and third bases in the chain. A stop
#'codon (UAA, UAG, or UGA) is randomly inserted at the end of the chain. A
#'message is returned with the position and stop codon used.
#'
#'
#' @param n The number of nucleotides.
#' @param type The type of sequence. It must be either `"dna"` to generate a DNA
#'   sequence, or `"rna"` to generate a RNA sequence
#' @param seed The seed for the random number generator . Set to ensure
#'   reproducibility
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' # generate a DNA sequenc
#' rand_nuc_seq(n = 15, seed = 10)
#'
#' # generate a RNA sequenc
#' rand_nuc_seq(n = 15, type = "rna", seed = 10)
#'
rand_nuc_seq <- function(n = 30, type = "dna", seed = NULL){
  if(!type %in% c("dna", "rna")){
    warning("`type` must be one of 'dna' or 'rna'. Defaulting to 'dna'.", call. = FALSE)
    type <- "dna"
  }
  if(n < 10){
    stop("The number of bases must be greater than 10", call. = FALSE)
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  if(type == "dna"){
    start <- c("A", "T", "G")
  } else{
    start <- c("A", "U", "G")
  }
  if(type == "dna"){
    stops <- list(
      stop1 = c("T", "A", "A"),
      stop2 = c("T", "A", "G"),
      stop3 = c("T", "G", "A")
    )
  } else{
    stops <- list(
      stop1 = c("U", "A", "A"),
      stop2 = c("U", "A", "G"),
      stop3 = c("U", "G", "A")
    )
  }
  if(type == "dna"){
    samp <- sample(c("A", "T", "C", "G"), n, replace = TRUE)
  } else{
    samp <- sample(c("A", "U", "C", "G"), n, replace = TRUE)
  }
  # insert start codon in a random position
  # between first and fourth bases
  nin <- sample(1:3, 1)
  nfin <- nin + 2
  samp[nin:nfin] <- start
  # inserts a stop codon on a random position
  # From 4 bases after start codon up to two of the end
  free_seq <- nth_element((nfin + 4):(n-2), 3)
  if(length(free_seq) == 1){
    nin2 <- free_seq
  } else{
    nin2 <- sample(free_seq, 1)
  }
  trink <- (nin2-1 - nfin)%%3
  while(trink != 0 ){
    nin2 <- nin2 + 1
    trink <- (nin2 - nfin)%%3
  }
  nfin2 <- nin2 + 2
  # selects a stop codon at random
  nstop <- sample(1:3, 1)
  samp[nin2:nfin2] <- stops[[nstop]]

  samp <- paste0(samp, collapse = "")
  pre_met <- str_sub(samp, 1, nin - 1)
  met_stop <-
    str_sub(samp, nin, nin2 - 1) %>%
    split_each(n = 3)
    # replace possible stop codons within the chain with random codons
  met_stop <-
    met_stop %>%
    str_replace_all(stops$stop1 %>% paste0(collapse = ""), rand_acg(length(met_stop))) %>%
    str_replace_all(stops$stop2 %>% paste0(collapse = ""), rand_acg(length(met_stop))) %>%
    str_replace_all(stops$stop3 %>% paste0(collapse = ""), rand_acg(length(met_stop))) %>%
    paste0(collapse = "")
  pos_stop <- str_sub(samp, nin2, nchar(samp))
  samp <- str_c(pre_met, met_stop, pos_stop)
  message("Start codon  : ", paste0(nin, "-", nfin))
  message("Stop codon   : ", paste0(stops[[nstop]], collapse = ""))
  message("Position stop: ", paste0(nin2, "-", nfin2))
  return(samp)
}
