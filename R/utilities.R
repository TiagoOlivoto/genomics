#' Separate a character column into multiple columns with a regular expression
#'
#' @param .data A data frame.
#' @param col Column name to separate in.
#' @param into Names of new variables to create as character vector.
#' @param sep Separator between columns.
#' @return A data frame with the separated columns
#' @export
#'
#' @examples
#' df <- data.frame(a = c("first_second", "my_vec"))
#' df
#' separate_col(df, a, into = c("v1", "v2"))
separate_col <- function(.data, col, into, sep = "_"){
  var <- deparse(substitute(col))
  df <-
    .data %>%
    select(var) %>%
    pull() %>%
    strsplit(split = sep)
  df <-
    do.call(rbind,
            lapply(df, function(x)x)) %>%
    as.data.frame()
  .data[[var]] <- NULL
  names(df) <- into
  return(cbind(.data, df))
}


#' Find the antiparallel sequence
#'
#' @param x A character sequence of bases (A, T, C, G)
#'
#' @return A character sequence with the antiparalell sequence of `x`
#' @export
#'
#' @examples
#' seq <- "ATCG"
#' antiparallel(seq)
#'
#' seq2 <- c(seq, "AUGDF")
#' antiparallel(seq2)
antiparallel <- function(x){
  if(length(x) > 1){
    lapply(x, antiparallel)
  } else{
    x <- tidy_sequence(x)
    lapply(strsplit(x, "")[[1]], function(x){
      case_when(x == "A" ~ "T",
                x == "T" ~ "A",
                x == "C" ~ "G",
                x == "G" ~ "C",
                x == "U" ~ "A")
    }
    ) %>%
      as.character() %>%
      paste0(collapse = "")
  }
}

# Find the nth element/sequence
#' Extract the nth element of a numeric or character sequence
#'
#' @param x A numeric or character sequence
#' @param interval The nth  interval to select from the sequence
#'
#' @return A character or numeric vector depending on the `x` input
#' @export
#'
#' @examples
#' num <- 1:20
#' nth_element(num, interval = 5)
#'
#' let <- letters[1:20]
#' nth_element(let, interval = 2)
nth_element <- function(x, interval){
  x[seq(1, length(x), interval)]
}

#' Generate random codons with A, C, and G only
#'
#' @param n The number of random codons to generate.
#'
#' @return A character vector with the same length of `n`
#' @export
#'
#' @examples
#' rand_acg(n = 5)
rand_acg <- function(n){
  sapply(1:n, function(x){
    sample(c("A", "C", "G"), 3) %>% paste0(collapse = "")
  })
}

#' Split a character sequence systematically each `n` pieces
#'
#' @param x A character sequence/vector
#' @param n The number of pieces for each split
#'
#' @return A character vector
#' @export
#'
#' @examples
#' split_each("TESTSPLITEACH2", n = 2)
#'
#' # split a character vector each 3 letter
#' split_each(c("AAAAAABBBBBBB", "BAFASFDEASDFS"), n = 3)
#'
split_each <- function(x, n = 3){
  if(length(x) > 1){
    lapply(x, split_each, n)
  } else{
    regmatches(x, gregexpr(paste0(".{", n, "}"), x))[[1]]
  }
}

#' Tidy up a character vector
#'
#' The function removes all spaces, punctuation signals, and numbers from a
#' character vector, returning a 'letter only' upper-case sequence
#'
#' @param x A character vector.
#'
#' @return A character vector of the same length as `x`
#' @export
#'
#' @examples
#' messy_seq <- c("a 1, -C B", "AS.DF2D F33")
#' tidy_sequence(messy_seq)
tidy_sequence <- function(x){
  if(length(x) > 1){
    as.character(lapply(x, tidy_sequence))
  } else{
    str <- toupper(gsub("(?<=[a-z])(?=[A-Z])|[[:space:][:punct:]]+", "", x, perl = TRUE))
    return(gsub("[0-9]", "", str, perl = TRUE))
  }
}



#' @keywords internal
#' @importFrom poorman %>%
#' @export
#' @usage lhs \%>\% rhs
NULL


# check for bases
check_base <- function(x){
  if(length(x) > 1){
    lapply(x, check_base)
  } else{
    code <- str_split(x, "")[[1]]
    bases <- c("A", "U", "G", "C")
    if(all(code %in% bases) == FALSE){
      stop("'x' must contains only 'A', 'U', 'G', or 'C'.", call. = FALSE)
    }
  }
}

#' Check if a sequence is DNA or RNA
#'
#' `is_dna()` checks the vector `x` and returns `FALSE` if a Uracil (U) is found
#' in the sequence.
#' `is_rna()` checks the vector `x` and returns `FALSE` if a Thymine (T) is
#' found in the sequence.
#'
#' @param x A character vector
#' @param verbose Logical value indicating if a warning message should be
#'   returned.
#' @return A logical value indicating if a warning should be returned
#' @name util_is
#' @export
#'
#' @examples
#' is_dna("ATG")
#' is_rna(c("AUG", "TGA"))
is_dna <- function(x, verbose = TRUE){
  if (length(x) > 1) {
    as.logical(lapply(x, is_dna, verbose))
  } else{
    test <- str_detect(x, "U")
    if (test == TRUE) {
      if (isTRUE(verbose)) {
        warning("It seems that a RNA sequence was informed.", call. = FALSE)
      }
    }
    return(!test)
  }
}
#' @name util_is
#' @export
is_rna <- function(x, verbose = TRUE){
  if (length(x) > 1) {
    as.logical(lapply(x, is_rna, verbose))
  } else{
    test <- str_detect(x, "T")
    if (test == TRUE) {
      if (isTRUE(verbose)) {
        warning("It seems that a DNA sequence was informed.", call. = FALSE)
      }
    }
    return(!test)
  }
}



#' Highlight a pattern sequence in a vector
#'
#' The function highlights a pattern (e.g., "UAG") in a vector by putting a
#' replacement  (e.g., an underscore _) in all characters that don't match the
#' sequence
#'
#' @param x A character vector.
#' @param pattern A character vector indicating the pattern to highlight
#' @param replacement A replacement to replace all the characters that do not
#'   match the `pattern` with.
#'
#' @return A list with the location and highlighted pattern.
#' @export
#'
#' @examples
#' x <- rand_nuc_seq(n = 50, seed = 1)
#' x
#' highlight_pattern(x, pattern = "CCA")
#'
#' # or a vector
#' highlight_pattern(x, pattern = c("CCA", "GTA"))
#'
#' # vectorized
#'
#' x2 <-  rand_nuc_seq(n = 50, seed = 2)
#' x2
#' highlight_pattern(c(x, x2), pattern = "ATG")
highlight_pattern <- function(x,
                              pattern,
                              replacement = "_",
                              verbose = TRUE){
  if(length(x) > 1){
    a <- lapply(x, highlight_pattern, pattern, replacement, verbose)
  } else{
    loc <- str_locate_all(x, pattern)
    names(loc) <- pattern
    dots <- paste0(rep(replacement, nchar(x)), collapse = "")
    for(i in 1:length(loc)){
      start <- loc[[i]][,1]
      end <- loc[[i]][,2]
      nsub <- seq_along(start)
      for(j in nsub){
        str_sub(dots, start[j], end[j]) <- pattern[i]
      }
    }
    loc <- lapply(loc, function(x){
      as.data.frame(x)
    }) %>%
      bind_rows(.id = "pattern")
    if(isTRUE(verbose)){
      cat("============ Location ============\n")
      print(loc)
      cat("----------------------------------\n")
      message(dots)
    }
    invisible(list(location = loc, sequence = dots))
  }
}
