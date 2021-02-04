#' Form the digital code of fossil fuels in accordance with GOST 25543
#'
#' In accordance with
#' \href{http://docs.cntd.ru/document/1200107843}{GOST 25543} (\emph{part 7})
#' form the digital code of fossil fuel (brown, or hard coal, or anthracite) on
#' the basis of its class, category, type, and subtype.
#'
#' @param classid
#'   identifier of fossil fuel class according to \emph{Table 3} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @param catid
#'   identifier of fossil fuel category according to \emph{Table 4} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @param typeid
#'   identifier of fossil fuel category according to \emph{Table 5} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @param subtypeid
#'   identifier of fossil fuel subtype according to \emph{Table 8} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @return
#'   Digital code according to \emph{Part 7} of
#'   \href{http://docs.cntd.ru/document/1200107843}{GOST 25543}.
#'   Type: [\code{character}].
#'
#' @export
#'
#' @examples
#'  r0   <- c(1.1,  1.3)
#'  sok  <- c( 43, 76.0)
#'  vdaf <- c(8.4, 28.3)
#'  y    <- c(  6,  9.0)
#'
#'  x <- code(fuel_class(r0), fuel_cat(sok), hard_type(vdaf), hard_subtype(y))
#'  print(x)
#'
#'  # Unit test:
#'  stopifnot(
#'    x %in% coal.state:::g25543db()$hard$S1KCFx$code
#'  )
code <- function(classid, catid, typeid, subtypeid){
  checkmate::assert_character(
    classid,
    min.chars = 2, pattern = "^[0-9]{2,2}$", any.missing = FALSE, min.len = 1,
    null.ok = FALSE
  )
  n <- length(classid)
  checkmate::assert_character(
    catid,
    min.chars = 1, pattern = "^[0-9]$", any.missing = FALSE, len = n,
    null.ok = FALSE
  )
  checkmate::assert_character(
    typeid,
    min.chars = 1, pattern = "^[0-9]{2,2}$", any.missing = FALSE, len = n
  )
  checkmate::assert_character(
    subtypeid,
    min.chars = 1, pattern = "^[0-9]{2,2}$", any.missing = FALSE, len = n
  )
  sprintf(
    "%s%s%s%s",
    classid, catid, typeid, subtypeid
  )
}
