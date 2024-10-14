#' Expectation for comparing shades
#' 
#' This function provides an expectation for use with the `tinytest` package,
#' which compares two colour vectors via [all.equal.shade(hexonly=TRUE)]. It
#' evaluates `TRUE` if the vectors have the same length and resolve to the same
#' hex colour strings.
#' 
#' @param current The colour vector to test.
#' @param target The target values to compare against, in any form valid for
#'   casting to [shade()].
#' @param ... Further arguments to [all.equal()].
#' @param info An optional information string shown in case of failure.
#' @return A \code{"tinytest"} object.
#' 
#' @rdname tinytest
#' @export
expect_equal_shades <- function (current, target, ..., info = NA_character_)
{
    result <- all.equal(shade(target), shade(current), hexonly=TRUE, ...)
    diff <- (if (isTRUE(result)) NA_character_ else result)
    return (tinytest::tinytest(isTRUE(result), call=sys.call(sys.parent(1)), diff=diff, short="data", info=info))
}
