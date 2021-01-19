use pest::{Position, Span};
use std::fmt::Display;

// == Location of an error in a file ==

/// Coords of a char in a file
#[derive(Debug, Clone, Copy)]
pub struct InFilePosition {
    pub line: usize,
    pub col: usize,
    /// Byte position
    ///
    /// See `pest::Position::pos()`.
    pub pos: usize,
}

impl InFilePosition {
    // # From Position
    ///
    /// Creates a new `InFilePosition` from a `pest::Position`.
    ///
    /// Useful in the parser when a lot of `AstNode` have to be created
    /// from pest output.
    pub fn from_position(pos: Position) -> InFilePosition {
        // Get position data
        let (line, col) = pos.line_col();
        // Return it
        InFilePosition {
            line,
            col,
            pos: pos.pos(),
        }
    }
}

/// Coords of an error in a file. Can be ponctual (point to a single location) or a span
#[derive(Debug, Clone, Copy)]
pub enum InFileLocation {
    Ponctual(InFilePosition),
    Span(InFilePosition, InFilePosition),
}

impl InFileLocation {
    /// # From Span
    ///
    /// Creates a new `InFileLocation` from a `pest::Span`.
    ///
    /// Useful in the parser when a lot of `AstNode` have to be created
    /// from pest output.
    pub fn from_span(span: &Span) -> InFileLocation {
        // Start == end => Ponctual
        if span.start() == span.end() {
            // Get the start pos
            let start = span.start_pos();
            // Return a ponctual location
            InFileLocation::Ponctual(InFilePosition::from_position(start))
        }
        // Else, its a span
        else {
            // Get the start and end pos
            let (start, end) = span.clone().split();
            // Return a span location
            InFileLocation::Span(
                InFilePosition::from_position(start),
                InFilePosition::from_position(end),
            )
        }
    }

    pub fn get_start(&self) -> &InFilePosition {
        match &self {
            InFileLocation::Ponctual(start) => &start,
            InFileLocation::Span(start, _) => &start,
        }
    }

    pub fn get_end(&self) -> &InFilePosition {
        match &self {
            InFileLocation::Ponctual(end) => &end,
            InFileLocation::Span(_, end) => &end,
        }
    }
}

/// Location of an error in a repository. Includes the file path and the location in that file.
#[derive(Debug)]
pub struct NodeLocation {
    pub file_path: String,
    pub location: InFileLocation,
}

impl NodeLocation {
    pub fn get_start(&self) -> &InFilePosition {
        &self.location.get_start()
    }

    pub fn get_end(&self) -> &InFilePosition {
        &self.location.get_end()
    }

    /// Creates a new poncutal ErrorLocation.
    ///
    /// Warning: pos must be valid char positions in the input string (be aware when using UTF-8 !).
    /// Normally, pest only returns valid positions, but keep this in mind when using this function manually.
    pub fn ponctual(file: &str, (line, col, pos): (usize, usize, usize)) -> NodeLocation {
        NodeLocation {
            file_path: String::from(file),
            location: InFileLocation::Ponctual(InFilePosition { line, col, pos }),
        }
    }

    /// Creates a new ErrorLocation covering a span. The start position must be before the end position.
    ///
    /// Warning: pos must be valid char positions in the input string (be aware when using UTF-8 !).
    /// Normally, pest only returns valid positions, but keep this in mind when using this function manually.
    pub fn span(
        file: String,
        (start_line, start_col, start_pos): (usize, usize, usize),
        (end_line, end_col, end_pos): (usize, usize, usize),
    ) -> NodeLocation {
        // Assert that the first position is before the second one
        debug_assert!(start_line <= end_line);
        debug_assert!(if start_line == end_line {
            start_col <= end_col
        } else {
            true
        });

        NodeLocation {
            file_path: file,
            location: InFileLocation::Span(
                InFilePosition {
                    line: start_line,
                    col: start_col,
                    pos: start_pos
                },
                InFilePosition {
                    line: end_line,
                    col: end_col,
                    pos: end_pos
                },
            ),
        }
    }
}

impl NodeLocation {
    /// # As ponctual

    /// Converts a span location to a ponctual location
    /// Useful for error messages
    pub fn as_ponctual(&self) -> NodeLocation {
        match &self.location {
            InFileLocation::Ponctual(pos) => NodeLocation {
                file_path: self.file_path.clone(),
                location: InFileLocation::Ponctual(pos.clone()),
            },
            InFileLocation::Span(start, _) => NodeLocation {
                file_path: self.file_path.clone(),
                location: InFileLocation::Ponctual(start.clone()),
            },
        }
    }
}

/// Implement display for the ErrorLocation so that it prints nicely.
impl Display for NodeLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.location {
            // Ponctual position
            InFileLocation::Ponctual(pos) => {
                write!(
                    f,
                    r#"in "{}", on line {}, column {}"#,
                    self.file_path, pos.line, pos.col
                )
            }
            // Span position
            InFileLocation::Span(start, end) => write!(
                f,
                r#"from "{}:{}:{}" to "{}:{}:{}""#,
                self.file_path, start.line, start.col, self.file_path, end.line, end.col
            ),
        }
    }
}
