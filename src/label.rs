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

use crate::{Encoded,Type};
pub fn obtain_label(ins: &Encoded, vram: u64, vram_limit: u64, pc: u64) -> Option<Label> {
	match ins.get_type() {
		// Relative branches
		Type::Branching => {
			let abs = ins.get_absolute(pc);
			return Some(Label::new(format!("l_{:X}",abs),abs));
		}
		// Absolute jumps
		Type::Jump => {
			let abs = ins.get_offset();
			return Some(Label::new(format!("func_{:X}",abs),abs));
		}
		// Lui is also used commonly for labels and stuff
		Type::Immediate => {
			let opcode = ins.get_opcode();
			let abs = ins.get_absolute(pc);
			return match opcode {
				0x09 | 0x0F => Some(Label::new(format!("data_{:X}",abs),abs)),
				_ => None,
			}
		}
		_ => {
			let abs = ins.raw_dword as u64;
			if abs > vram && abs < vram_limit {
				return Some(Label::new(format!("ptr_{:X}",abs),abs));
			}
			None
		}
	}
}