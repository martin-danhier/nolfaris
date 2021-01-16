use std::fmt::{Debug, Display};
use crate::utils::locations::{NodeLocation, InFileLocation};
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


#[derive(Debug)]
pub struct Error {
    pub variant: ErrorVariant,
    pub location: NodeLocation,
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
