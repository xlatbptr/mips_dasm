#[derive(Debug,Clone,Default)]
pub struct Label {
	pub name: String,
	pub target: u64,
	pub is_data: bool,
	pub is_func: bool,
	pub is_sub: bool,
}

impl Label {
	pub fn new(name: String, target: u64) -> Self {
		Self {
			name,
			target,
			is_data: false,
			is_func: false,
			is_sub: false,
		}
	}
}