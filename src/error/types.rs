use std::fmt::{Debug, Display};

use colored::Colorize;

#[derive(Debug)]
pub enum ErrorVariant {
    Syntax,
    Semantic,
    /// Error related to a dysfunction of the compiler.
    Compiler,
}
impl Display for ErrorVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorVariant::Syntax => write!(f, "Syntax"),
            ErrorVariant::Semantic => write!(f, "Semantic"),
            ErrorVariant::Compiler => write!(f, "Compiler"),
        }
    }
}

/// Severity of an error
#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
}
impl Display for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => write!(f, "{}", "Error".bright_red()),
            Severity::Warning => write!(f, "{}", "Warning".yellow()),
        }
    }
}

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
pub struct ErrorLocation {
    pub file_path: String,
    pub location: InFileLocation,
}

impl ErrorLocation {
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
    pub fn ponctual(file: &str, (line, col): (usize, usize)) -> ErrorLocation {
        ErrorLocation {
            file_path: String::from(file),
            location: InFileLocation::Ponctual(InFilePosition { line, col }),
        }
    }

    /// Creates a new ErrorLocation covering a span. The start position must be before the end position.
    pub fn span(
        file: String,
        (start_line, start_col): (usize, usize),
        (end_line, end_col): (usize, usize),
    ) -> ErrorLocation {
        // Assert that the first position is before the second one
        debug_assert!(start_line <= end_line);
        debug_assert!(if start_line == end_line {
            start_col <= end_col
        } else {
            true
        });

        ErrorLocation {
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

impl ErrorLocation {
    /// # As ponctual

    /// Converts a span location to a ponctual location
    /// Useful for error messages
    pub fn as_ponctual(&self) -> ErrorLocation {
        match &self.location {
            InFileLocation::Ponctual(pos) => ErrorLocation {
                file_path: self.file_path.clone(),
                location: InFileLocation::Ponctual(pos.clone()),
            },
            InFileLocation::Span(start, _) => ErrorLocation {
                file_path: self.file_path.clone(),
                location: InFileLocation::Ponctual(start.clone()),
            },
        }
    }
}

/// Implement display for the ErrorLocation so that it prints nicely.
impl Display for ErrorLocation {
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

#[derive(Debug)]
pub struct Error {
    pub variant: ErrorVariant,
    pub location: ErrorLocation,
    pub severity: Severity,
    pub message: String,
    pub hint: Option<String>,
    pub line_content: Option<String>,
}

impl Error {
    /// Returns the base part of the error message
    fn base_msg_fmt(&self) -> String {
        format!(
            "{}: {}\n{}",
            self.severity,
            self.message.bold(),
            self.location.as_ponctual(),
        )
    }

    /// Returns the hint part of the error message, when pertinent
    fn hint_fmt(&self) -> String {
        match &self.hint {
            None => String::from(""),
            Some(hint) => format!("\n{} {}", "Hint:".bright_cyan(), hint),
        }
    }

    /// Returns the preview part of the error message, when pertinent
    fn preview_fmt(&self) -> String {
        match &self.line_content {
            None => String::from(""),
            Some(line) => {
                let arrow_start_pos;
                let arrow_end_pos;

                // Remove spaces at the start
                let stripped_line = line.trim_start();
                // Compute the difference to offset the carets
                let delta = line.chars().count() - stripped_line.chars().count();

                // Compute the position of the arrow
                match self.location.location {
                    InFileLocation::Ponctual(pos) => {
                        arrow_start_pos = 2 + pos.col - delta;
                        arrow_end_pos = 3 + pos.col - delta;
                    }
                    InFileLocation::Span(start, end) => {
                        arrow_start_pos = 2 + start.col - delta;
                        arrow_end_pos = 3 + end.col - delta;
                    }
                };

                format!(
                    ":\n-> {}\n{}{}",
                    stripped_line,
                    (0..arrow_start_pos).map(|_| " ").collect::<String>(),
                    (arrow_start_pos..arrow_end_pos)
                        .map(|_| "^")
                        .collect::<String>(),
                )
            }
        }
    }
}

impl Display for Error {
    /// Returns the full and colored error message
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.base_msg_fmt(),
            self.preview_fmt(),
            self.hint_fmt(),
        )
    }
}
