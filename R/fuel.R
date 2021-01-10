#' Determine the form of fossil fuels in accordance with GOST 25543
#'
#' Using rules prescribed by
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{Table 2})
#' determine the form of fossil fuels that could be classified as \emph{brown} or
#' \emph{hard} coal, or \emph{anthracite}.
#'
#' @param r
#'   reflectance of vitrinite, [\emph{\%}], measured in accordance with
#'   \strong{ISO 7404-5}.
#'   Type: [\code{double}].
#'
#' @param qsaf
#'   gross calorific value, [\emph{MJ/kg}] or [\emph{J/g}], measured in
#'   accordance with \strong{ISO 1928}
#'   and recalculated on moisture ash-free basis. Type: [\code{double}].
#'
#' @param vdaf
#'   volatile matter yield, [\emph{\%}], measured in accordance with
#'   \strong{ISO 562} or \strong{ISO 5071-1} and recalculated
#'   on dry ash-free basis. Type: [\code{double}].
#'
#' @return
#'   Identifier of fossil fuel form:
#'   \itemize{
#'     \item \code{brown}
#'     \item \code{hard}
#'     \item \code{anthracite}
#'   }
#'  Type: [\code{character}].
#'
#' @export
#'
#' @examples
#' fuel()
#' # [1] "hard"
#'
#' fuel(c(0.3, 1.3), c(20, NA), c(8, NA))
#' # [1] "brown" "hard"
#'
#' # building test:
#' with(
#'   read.csv(text =
#'    "R0, Qsdaf,Vdaf,Form,Comment
#'     0.2,     ,    ,brown,Unambiguously brown coal
#'     1.3,     ,    ,hard,Unambiguously hard coal
#'     2.8,     ,    ,anthracite,Unambiguously anthracite
#'     0.3,  20.,  8.,brown,Unambiguously brown coal with redundant parameters
#'     1.9,   24,28.3,hard,Unambiguously hard coal with redundant parameters
#'     3.0,   28,   6,anthracite,Unambiguously anthracite with redundant parameters
#'     0.5,   20,    ,brown,Brown coal in ambiguous brown-hard zone
#'     0.5,   24,    ,hard,Hard coal in ambiguous brown-hard zone
#'     0.5,   28,    ,hard,Hard coal in ambiguous brown-hard zone
#'     2.3,     ,   8,hard,Hard coal in ambiguous hard-anthracite zone
#'     2.3,     ,  28,hard,Hard coal in ambiguous hard-anthracite zone
#'     2.3,     ,   6,anthracite,Anthracite coal in ambiguous hard-anthracite zone"
#'   ),
#'   stopifnot(all(fuel(R0, Qsdaf, Vdaf) == Form))
#' )

fuel <- function(r = 1.3, qsaf = NA, vdaf = NA){
  checkmate::assert_double(r, 0, 7, any.missing = FALSE, min.len = 1)
  n <- length(r)
  checkmate::assert_double(qsaf, 0, 40, len = n)
  checkmate::assert_double(vdaf, 0, 100, len = n)
  r <- round(r, 2)
  qsaf <- round(qsaf)
  vdaf <- round(vdaf)

  BROWN <- "brown"
  HARD <- "hard"
  ANTR <- "anthracite"

  form <- vector(mode = "character", n)
  form[r < .4] <- BROWN
  form[r >= .6 & r < 2.20] <- HARD
  form[r >= 2.6] <- ANTR

  bh_zone <- r >= .4 & r < .6
  form[bh_zone & qsaf >= 24] <- HARD
  form[bh_zone & qsaf < 24] <- BROWN

  ha_zone <- r >= 2.2 & r <= 2.59
  form[ha_zone & vdaf >= 8] <- HARD
  form[ha_zone & vdaf < 8] <- ANTR
  form[form == ""] <- NA_character_
  form
}


