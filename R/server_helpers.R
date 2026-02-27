#' Regroup files into slides maintaining current slide sizes
#'
#' After reordering files, redistributes them across slides keeping the same
#' number of images per slide. Positions are reset to sequential (1, 2, 3...).
#'
#' @param files Character vector of reordered file paths.
#' @param current_groups List of character vectors (current slide groupings).
#'
#' @return A list with `groups` (list of character vectors) and
#'   `positions` (list of integer vectors).
#' @keywords internal
#' @noRd
regroup_files <- function(files, current_groups) {
  sizes <- lengths(current_groups)
  new_groups <- vector("list", length(sizes))
  new_positions <- vector("list", length(sizes))
  file_idx <- 1
  for (i in seq_along(sizes)) {
    new_groups[[i]] <- files[file_idx:(file_idx + sizes[i] - 1)]
    new_positions[[i]] <- seq_len(sizes[i])
    file_idx <- file_idx + sizes[i]
  }
  list(groups = new_groups, positions = new_positions)
}

#' Split a slide before a global file index
#'
#' "Split before" semantics: the image at `global_idx` and everything after it
#' in the same slide moves to a new slide. Images before it stay on the original
#' slide. Positions are reset to sequential for both new slides.
#'
#' @param groups List of character vectors (current slide groupings).
#' @param positions List of integer vectors (current slide positions).
#' @param global_idx Integer, the 1-based global file index to split before.
#'
#' @return A list with `groups` and `positions`.
#' @keywords internal
#' @noRd
split_before_index <- function(groups, positions, global_idx) {
  cumulative <- cumsum(lengths(groups))

  slide_idx <- which(cumulative >= global_idx)[1]
  prior <- if (slide_idx == 1) 0 else cumulative[slide_idx - 1]
  local_idx <- global_idx - prior

  slide_files <- groups[[slide_idx]]
  before_files <- slide_files[seq_len(local_idx - 1)]
  after_files <- slide_files[local_idx:length(slide_files)]

  new_groups <- list()
  new_positions <- list()

  if (slide_idx > 1) {
    new_groups <- groups[seq_len(slide_idx - 1)]
    new_positions <- positions[seq_len(slide_idx - 1)]
  }

  new_groups <- c(new_groups, list(before_files))
  new_positions <- c(new_positions, list(seq_len(length(before_files))))

  new_groups <- c(new_groups, list(after_files))
  new_positions <- c(new_positions, list(seq_len(length(after_files))))

  if (slide_idx < length(groups)) {
    new_groups <- c(new_groups, groups[(slide_idx + 1):length(groups)])
    new_positions <- c(new_positions, positions[(slide_idx + 1):length(groups)])
  }

  list(groups = new_groups, positions = new_positions)
}

#' Merge a slide with the next slide
#'
#' Combines the images from `slide_idx` and `slide_idx + 1` into a single
#' slide. Positions are reset to sequential for the merged slide.
#'
#' @param groups List of character vectors (current slide groupings).
#' @param positions List of integer vectors (current slide positions).
#' @param slide_idx Integer, the 1-based index of the slide to merge with next.
#'
#' @return A list with `groups` and `positions`.
#' @keywords internal
#' @noRd
#' Distribute files across slides based on placeholder count
#'
#' Creates the initial slide grouping by distributing files evenly across
#' slides, each holding up to `placeholder_count` images. Positions are
#' assigned sequentially (1, 2, ...) within each slide.
#'
#' @param files Character vector of file paths.
#' @param placeholder_count Integer, max images per slide.
#'
#' @return A list with `groups` (list of character vectors) and
#'   `positions` (list of integer vectors).
#' @keywords internal
#' @noRd
distribute_files_to_slides <- function(files, placeholder_count) {
  n_files <- length(files)
  n_slides <- ceiling(n_files / placeholder_count)

  slide_groups <- vector("list", n_slides)
  slide_positions <- vector("list", n_slides)

  for (i in seq_along(files)) {
    slide_idx <- ceiling(i / placeholder_count)
    if (is.null(slide_groups[[slide_idx]])) {
      slide_groups[[slide_idx]] <- character(0)
      slide_positions[[slide_idx]] <- integer(0)
    }
    slide_groups[[slide_idx]] <- c(slide_groups[[slide_idx]], files[i])
    pos_in_slide <- length(slide_groups[[slide_idx]])
    slide_positions[[slide_idx]] <- c(slide_positions[[slide_idx]], pos_in_slide)
  }

  list(groups = slide_groups, positions = slide_positions)
}

#' Sanitize a user-supplied filename for PowerPoint output
#'
#' Strips illegal characters, appends `.pptx` if missing, and returns a default
#' when the input is empty or `NULL`.
#'
#' @param name Character scalar (may be `NULL` or empty).
#'
#' @return A sanitized filename string ending in `.pptx`.
#' @keywords internal
#' @noRd
sanitize_filename <- function(name) {
  if (is.null(name) || name == "") {
    return("report.pptx")
  }
  has_ext <- grepl("\\.pptx$", name, ignore.case = TRUE)
  stem <- if (has_ext) sub("\\.[Pp][Pp][Tt][Xx]$", "", name) else name
  stem <- gsub("[^[:alnum:]_ -]", "_", stem)
  paste0(stem, ".pptx")
}

#' Merge a slide with the next slide
#'
#' Combines the images from `slide_idx` and `slide_idx + 1` into a single
#' slide. Positions are reset to sequential for the merged slide.
#'
#' @param groups List of character vectors (current slide groupings).
#' @param positions List of integer vectors (current slide positions).
#' @param slide_idx Integer, the 1-based index of the slide to merge with next.
#'
#' @return A list with `groups` and `positions`.
#' @keywords internal
#' @noRd
merge_slides <- function(groups, positions, slide_idx) {
  if (slide_idx >= length(groups)) {
    return(list(groups = groups, positions = positions))
  }

  merged_files <- c(groups[[slide_idx]], groups[[slide_idx + 1]])
  merged_positions <- seq_len(length(merged_files))

  new_groups <- list()
  new_positions <- list()

  if (slide_idx > 1) {
    new_groups <- groups[seq_len(slide_idx - 1)]
    new_positions <- positions[seq_len(slide_idx - 1)]
  }

  new_groups <- c(new_groups, list(merged_files))
  new_positions <- c(new_positions, list(merged_positions))

  if (slide_idx + 1 < length(groups)) {
    new_groups <- c(new_groups, groups[(slide_idx + 2):length(groups)])
    new_positions <- c(new_positions, positions[(slide_idx + 2):length(groups)])
  }

  list(groups = new_groups, positions = new_positions)
}
