#' @title get_channel_idx
#'
#' @description Get the indices of channel names in a flowFrame. It tries to
#'   match them against names, desc of the flowFrame and guess_antigen(desc).
#'
#' @param channel_names Character vector of channel names to look for.
#' @param ff The flowFrame.
#' @param verbose Logical.
#'
#' @examples
#' \dontrun{
#' fcs.loc <- system.file("extdata",package="flowCore")
#' samp <- read.flowSet(dir = fcs.loc, pattern = "0+")
#' }
#'
#' @importFrom flowCore colnames
#' @export

get_channel_idx <- function(
  channel_names,
  ff,
  verbose = getOption("verbose")
) {
  stopifnot(!missing(channel_names))
  stopifnot(inherits(ff, "flowFrame"))
  pd <- pData(parameters(ff))
  df <- data.frame(
    coln = match(channel_names, pd$name),
    chnn = match(channel_names, pd$desc),
    antn = match(channel_names, guess_antigen(pd$desc))
  )
  apply(df, 1, function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    x <- unique(x)
    if (length(x) == 1) return(x) else return(0)
  })
}


#' @title guess_ok
#'
#' @description Guess elements of x that do not include any forbidden pattern. A
#'   grep search of each forbidden pattern in x is carried out. Position of
#'   elements of x are returned if none of the patterns matched the
#'   corresponding element.
#'
#' @param x character vector (channel descriptions for example).
#' @param forbidden character vector of patterns to exclude.
#'
#' @return A vector of the positions of its first argument not found in its
#'   second argument.
#'
#' @examples
#' guess_ok(c(LETTERS[1:5], "TUYDJJ"), c("A", "D"))
#' guess_ok(c(LETTERS[1:5], "CD8_Time"), "Time")
#'
#' @importFrom checkmate assertCharacter
#' @export
guess_ok <- function(
    x,
    forbidden
) {
  assertCharacter(x)
  assertCharacter(forbidden)
  excluded <- NULL
  for (patt in forbidden)
    excluded <- c(excluded, grep(patt, x, ignore.case = TRUE))
  setdiff(seq_along(x), excluded)
}

#' @title guess_antigen
#'
#' @description Guess antigens name using channels description. It mainly
#'   ignores Time, Center... channels and removes numerical values at the
#'   beginning of the strings (e.g. "144Nd_" is removed).
#'
#' @param fcs_desc character vector with the description column of a FCS file.
#'
#' @examples
#' \dontrun{
#' fcs.loc <- system.file("extdata",package="flowCore")
#' samp <- read.flowSet(path = fcs.loc, pattern = "0+")
#' pd <- pData(parameters(samp[[1]]))
#' guess_antigen(pd$desc)
#' }
#'
#' @export
guess_antigen <- function(
    fcs_desc
) {
  ok <- guess_ok(
    fcs_desc,
    c( "Time", "Event_length", "Center", "Offset", "Width", "Residual"))
  fcs_desc[ok] <- gsub("^\\d+.+?_", "", fcs_desc[ok])
  fcs_desc
}

#' @title which_is_marker
#'
#' @description Guess which channels are linked to a biological marker. It
#'   mainly ignore channels including Time, Center... in their name.
#'
#' @param desc character vector with the description column of a FCS file or the
#'   antigens.
#'
#' @return a vector of positions of antigens matching none of the unwanted
#'   patterns.
#'
#' @examples
#' which_is_marker(c("CD4", "CD8", "190BCKG"))
#'
#' @importFrom checkmate assertCharacter
#' @export
which_is_marker <- function(
    desc
) {
  assertCharacter(desc)
  guess_ok(
    desc,
    c("time", "length", "barcode", "BCKG", "Live", "Dead",
      "DNA[12]", "Center", "Offset", "Width", "Residual"))
}

#' @title guess_match
#'
#' @description Guess the position of each x (typically an antigen name) in the
#'   look up table of targets (typically channel description or FCS column
#'   names). It uses the match function that requires an exact and complete
#'   equality of each character of the query and the target.
#'
#' @param x character vector of queries to match the look up table.
#' @param lut character vector constituting the look up table (aka targets).
#' @param without_duplicates Logical flag to consider duplicates in look up
#'   table as improper matches.
#'
#' @return a vector of length of the query. Each value is the matched position
#'   of the query in the look up table. The incomparable attribute holds the
#'   duplicated target names that were excluded using the incomparable argument
#'   of the match function. The duplicated attribute is logical whether each
#'   target is duplicated or not.
#'
#' @examples
#' guess_match(c("CD4", "CD8"), c("CD19", "CD4", "CD8", "CD45"))
#'
#' @importFrom checkmate assertCharacter
#' @export
guess_match <- function(
    x,
    lut,
    without_duplicates = TRUE
) {
  assertCharacter(x)
  assertCharacter(lut)
  assertLogical(without_duplicates)
  dupli <- duplicated(lut) | duplicated(lut, fromLast = TRUE)
  incomp <- if (without_duplicates) lut[dupli] else NULL
  found <- match(x, lut, incomparables = incomp)
  structure(found, incomparables = incomp, duplicated = dupli)
}


#' @title guess_match_channels
#'
#' @description Guess the position of each channel (typically an antigen name)
#'   in the look up tables of channel names and channel descriptions.
#'
#' @param fb a flowBunch.
#' @param channels character vector of channels to look for.
#'
#' @return a vector of length of the query. Each value is the matched position
#'   of the query in one of the look up tables.
#'
#' @importFrom checkmate assertClass assertCharacter
#' @export
guess_match_channels <- function(
    fb,
    channels
) {
  assertClass(fb, "flowBunch")
  assertCharacter(channels, min.len = 1, min.chars = 3)
  for (lut in list(fb@panel$fcs_colname,
                   fb@panel$antigen,
                   gsub("^\\d+.+?_", "", fb@panel$fcs_colname),
                   gsub("^\\d+.+?_", "", fb@panel$antigen))) {
    found <- guess_match(channels, lut)
    if (!any(is.na(found))) break
  }
  return(found)
}
