use std::fmt::Display;


// == Location of an error in a file ==

/// Coords of a char in a file
#[derive(Debug, Clone, Copy)]
pub struct InFilePosition {
    pub line: usize,
    pub col: usize,
}

/// Coords of an error in a file. Can be ponctual (point to a single location) or a span
#[derive(Debug, Clone, Copy)]
pub enum InFileLocation {
    Ponctual(InFilePosition),
    Span(InFilePosition, InFilePosition),
}

/// Location of an error in a repository. Includes the file path and the location in that file.
#[derive(Debug)]
pub struct NodeLocation {
    pub file_path: String,
    pub location: InFileLocation,
}

impl NodeLocation {
    pub fn get_start(&self) -> &InFilePosition {
        match &self.location {
            InFileLocation::Ponctual(start) => &start,
            InFileLocation::Span(start, _) => &start,
        }
    }

    pub fn get_end(&self) -> &InFilePosition {
        match &self.location {
            InFileLocation::Ponctual(end) => &end,
            InFileLocation::Span(_, end) => &end,
        }
    }

    /// Creates a new poncutal ErrorLocation.
    pub fn ponctual(file: &str, (line, col): (usize, usize)) -> NodeLocation {
        NodeLocation {
            file_path: String::from(file),
            location: InFileLocation::Ponctual(InFilePosition { line, col }),
        }
    }

    /// Creates a new ErrorLocation covering a span. The start position must be before the end position.
    pub fn span(
        file: String,
        (start_line, start_col): (usize, usize),
        (end_line, end_col): (usize, usize),
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
                },
                InFilePosition {
                    line: end_line,
                    col: end_col,
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