mod types;
pub use types::{*};


pub struct ErrorManager {
    pub errors: Vec<Error>,
}

impl ErrorManager {
    pub fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }
}
