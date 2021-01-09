#' Determine the form of fossil fuels: brown or hard coal, or anthracite
#'
#' Using rules prescribed by table 2 in
#' [GOST 25543](http://docs.cntd.ru/document/1200107843)
#' determine the form of fossil fuels that could be classified as brown or hard
#' coal, or anthracite.
#'
#' @param r
#'   reflectance of vitrinite, [*%*], measured in accordance with
#'   [ISO 7404-5](https://www.iso.org/standard/42832.html). Type: [\code{double}].
#'
#' @param qsaf
#'   gross calorific value, [*MJ/kg*] or [*J/g*], measured in accordance with
#'   [ISO 1928](https://www.iso.org/standard/41592.html) and recalculated on
#'   moisture ash-free basis. Type: [\code{double}].
#'
#' @param vdaf
#'   volatile matter yield, [***], measured in accordance with
#'   [ISO 562](https://www.iso.org/standard/55943.html) or
#'   [ISO 5071-1](https://www.iso.org/standard/63045.html) and recalculated on
#'   dry ash-free basis. Type: [\code{double}].
#'
#' @return
#'   Identifier of form:
#'     * brown
#'     * hard
#'     * anthracite
#'  Type: [\code{character}].
#'
#' @export
#'
#' @examples
#'
#' coalform(0.3, 20, 8)
#' # [1] "brown"
#'
#' # dummy test:
#' with(
#'   read.csv(text =
#'              "R0, Qsdaf,Vdaf,Form,Comment
#'     0.2,     ,    ,brown,Unambiguously brown coal
#'     1.3,     ,    ,hard,Unambiguously hard coal
#'     2.8,     ,    ,anthracite,Unambiguously anthracite
#'     0.3,  20.,  8.,brown,Unambiguously brown coal with redundant parameters
#'     1.9,   24,28.3,hard,Unambiguously hard coal with redundant parameters
#'     3.0,   28,   6,anthracite,Unambiguously anthracite with redundant parameters
#'     0.5,   20,    ,brown,Brown coal in ambiguous brown-hard zone
#'     0.5,   24,    ,hard,Hard coal in ambiguous brown-hard zone
#'     0.5,   28,    ,hard,Hard coal in ambiguous brown-hard zone
#'     2.3,     ,8   ,hard,Hard coal in ambiguous hard-anthracite zone
#'     2.3,     ,28  ,hard,Hard coal  in ambiguous hard-anthracite zone
#'     2.3,     ,6   ,anthracite,Anthracite coal in ambiguous hard-anthracite zone"
#'   ),
#'   stopifnot(all(coalform(R0, Qsdaf, Vdaf) == Form))
#' )

coalform <- function(r, qsaf, vdaf){
  checkmate::assert_double(r, 0, 5, any.missing = FALSE, min.len = 1)
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


