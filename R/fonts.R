#' Activate `showtext` and check for presence of Public Sans font family
#'
#' [theme_doe()] uses `showtext` to allow R graphics devices to render the
#' Public Sans font family. Use `check_font_families()` to check that Public
#' Sans is available and to activate `showtext`.
#'
#' @return `check_font_families()` does not have a return value and is called
#'   for its side effects.
#'
#' @seealso [showtext::showtext_auto()] and [Public
#'   Sans](https://digitalnsw.github.io/public-sans/).
#' @export
check_font_families <- function() {
  if ("Public Sans" %in% sysfonts::font_families()) {
    # Ensure any newly opened graphics device uses `showtext` to draw text
    showtext::showtext_auto()
  } else {
    # Do the required files and faces exist?
    fonts <- sysfonts::font_files()
    public_sans <- fonts[fonts$family == "Public Sans",]
    faces <- list(regular = "PublicSans-Regular.ttf",
                  bold = "PublicSans-Bold.ttf",
                  italic = "PublicSans-Italic.ttf",
                  bolditalic = "PublicSans-BoldItalic.ttf")

    if (setequal(intersect(public_sans$file, faces), faces)) {
      sysfonts::font_add("Public Sans",
                         regular = faces$regular,
                         bold = faces$bold,
                         italic = faces$italic,
                         bolditalic = faces$bolditalic)
    } else {
       cli::cli_warn(
        message = c(
          "!" = "The font family {.strong Public Sans} is not installed in any of
              your operating system's font search paths. {.fn theme_doe}
              requires `PublicSans-Regular.ttf`, `PublicSans-Bold.ttf`,
              `PublicSans-Italic.ttf`, and `PublicSans-BoldItalic.ttf`.",
          "i" = "Please download and install Public Sans from
              {.url https://digitalnsw.github.io/public-sans/download/}."
        )
      )
    }
    showtext::showtext.auto()
  }
}


#' Is Public Sans available?
#'
#' Test whether the Public Sans font family is available by searching the
#' system, user and local font registries.
#'
#' @returns `TRUE` if Public Sans available; otherwise `FALSE`.
#' @export
public_sans_available <- function() {
  systemfonts::font_info("Public Sans")$family == "Public Sans"
}

#' Register Public Sans in the local font registry
#'
#' Register `doestyle`'s embedded copy of the Public Sans font family for use.
#' This doesn't install the font, but does make it available within this R
#' session.
#'
#' @returns `register_public_sans()` does not have a return value and is called
#'   for its side effects.
#' @export
register_public_sans <- function() {
  ps <- system.file("fonts/public_sans", package = "doestyle")
  systemfonts::register_font(
    "Public Sans",
    plain = paste0(ps, "/PublicSans-Regular.otf"),
    bold = paste0(ps, "/PublicSans-Bold.otf"),
    italic = paste0(ps, "/PublicSans-Italic.otf"),
    bolditalic = paste0(ps, "/PublicSans-BoldItalic.otf")
  )
}

#' Ensure Public Sans is available for use
#'
#' Checks that the Public Sans font family is available for use. If it isn't
#' installed in the system or user font registries, registers `doestyle`'s
#' embedded copy in the local font registry.
#'
#' @returns `require_public_sans()` does not have a return value and is called
#'   for its side effects.
#' @export
require_public_sans <- function() {
  if (!public_sans_available()) {
    register_public_sans()
  }
}
