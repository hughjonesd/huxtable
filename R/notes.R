#' Resolve cell notes and their reference marks
#'
#' Distinct notes are ordered by their first visible cell. Marks are generated
#' afresh for the current huxtable without modifying it.
#'
#' @param ht A huxtable.
#' @return A list with a cell marker matrix, note markers and note text.
#' @noRd
resolve_cell_notes <- function(ht) {
  cell_markers <- matrix(
    NA_character_, nrow(ht), ncol(ht),
    dimnames = dimnames(cell_note(ht))
  )
  visible <- display_cells(ht, all = FALSE)
  if (nrow(visible) == 0L) {
    return(list(cell_markers = cell_markers, markers = character(), notes = character()))
  }
  visible <- visible[order(visible$display_row, visible$display_col), ]

  positions <- as.matrix(visible[, c("display_row", "display_col")])
  visible_notes <- cell_note(ht)[positions]
  has_note <- !is.na(visible_notes) & nzchar(visible_notes)
  notes <- unique(visible_notes[has_note])
  note_count <- length(notes)
  if (note_count == 0L) {
    return(list(cell_markers = cell_markers, markers = character(), notes = character()))
  }

  symbol <- note_symbol(ht)
  if (identical(symbol, "numeric")) {
    markers <- as.character(seq_len(note_count))
  } else if (identical(symbol, "roman")) {
    markers <- tolower(as.character(utils::as.roman(seq_len(note_count))))
  } else {
    alphabet <- if (identical(symbol, "alphabetic")) {
      letters
    } else {
      unique(strsplit(symbol, "", fixed = TRUE)[[1]])
    }
    base <- length(alphabet)
    markers <- vapply(seq_len(note_count), function(index) {
      marker <- ""
      while (index > 0L) {
        index <- index - 1L
        marker <- paste0(alphabet[index %% base + 1L], marker)
        index <- index %/% base
      }
      marker
    }, character(1))
  }

  note_indexes <- match(visible_notes[has_note], notes)
  cell_markers[positions[has_note, , drop = FALSE]] <- markers[note_indexes]
  list(cell_markers = cell_markers, markers = markers, notes = notes)
}
