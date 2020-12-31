pub trait Slice {
    fn slice_from<'a>(&'a self, start_index: usize) -> Option<&'a Self>;
}

impl Slice for str {
    fn slice_from<'a>(&'a self, start_index: usize) -> Option<&'a str> {
        self.char_indices()
            .nth(start_index)
            .and_then(|(i, _)| self.get(i..))
    }
}
