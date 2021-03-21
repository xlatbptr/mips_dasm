use std::mem::size_of;

pub enum Type {
	Invalid,
	Register,
	Immediate,
	Jump,
	Interrupt,
}

#[derive(Default,Debug,Clone)]
pub struct Encoded {
	pub raw_dword: u32,				// Raw base dword
}

impl Encoded {
	pub fn new(raw_dword: u32) -> Self {
		Self {
			raw_dword,
		}
	}
	
	// opcodes
	pub fn get_opcode(&self) -> u8 {
		((self.raw_dword >> 26) & 0x3F) as u8
	}
	pub fn set_opcode(&mut self, opcode: u8) {
		self.raw_dword |= ((opcode & 0x3F) as u32) << 26;
	}

	pub fn get_coproc(&self) -> u8 {
		(((self.raw_dword >> 16) & 0x1F) >> 2) as u8
	}

	pub fn get_coproc_movx(&self) -> u8 {
		((self.raw_dword >> 16) & 0x3) as u8
	}

	// used for break and syscall
	pub fn get_break_code(&self) -> u32 {
		(self.raw_dword & 0xFFFFF) as u32
	}

	// r-type
	pub fn get_funct(&self) -> u8 {
		(self.raw_dword & 0x3F) as u8
	}
	pub fn get_shift(&self) -> u8 {
		((self.raw_dword >> 6) & 0x1F) as u8
	}
	pub fn get_rd(&self) -> u8 {
		((self.raw_dword >> 11) & 0x1F) as u8
	}
	pub fn get_rs(&self) -> u8 {
		((self.raw_dword >> 21) & 0x1F) as u8
	}

	// both r and i-type
	pub fn get_rt(&self) -> u8 {
		((self.raw_dword >> 16) & 0x1F) as u8
	}

	// j-type
	pub fn get_offset(&self) -> u64 {
		((self.raw_dword & 0x3FFFFFF) as u64) * size_of::<u32>() as u64
	}

	// i-type
	pub fn get_immediate(&self) -> i16 {
		(self.raw_dword & 0xFFFF) as i16
	}

	pub fn get_absolute(&self, base: u64) -> u64 {
		let imm = self.get_immediate() as i64 * 4;
		let mut offset = base;
		if imm > 0 {
			offset = offset.wrapping_add(imm as u64);
		} else {
			offset = offset.wrapping_sub(imm as u64);
		}
		offset
	}

	// obtain type of instruction
	pub fn get_type(&self) -> Type {
		let opcode = self.get_opcode();
		match opcode {
			0 => {
				let funct = self.get_funct();

				// break/syscall are interrupt type instructions since they invoke interrupts and
				// traps
				if funct == 0x0C || funct == 0x0D {
					// Syscall cannot have 0 as code
					let code = self.get_break_code();
					if funct == 0x0C && code != 0 {
						return Type::Invalid;
					}
					return Type::Interrupt;
				}
				Type::Register
			}
			0b10 | 0b11 => Type::Jump,
			0x01 => {
				match self.get_rt() {
					0x00 | 0x01 | 0x08..=0x0C | 0x0E | 0x10..=0x11 => Type::Immediate,
					_ => Type::Invalid,
				}
			}
			0x04..=0x0F | 0x20..=0x26 | 0x28..=0x2B | 0x30..=0x31 | 0x38 => Type::Immediate,
			_ => Type::Invalid,
		}
	}
}

use crate::label::Label;
pub fn obtain_label(ins: &Encoded, pc: u64) -> Option<Label> {
	let opcode = ins.get_opcode();
	match ins.get_type() {
		// Relative branches
		Type::Immediate => {
			let abs = ins.get_absolute(pc);
			match opcode {
				0x01 => {
					let rt = ins.get_rt();
					match rt {
						0x00 | 0x01 | 0x10 | 0x11 => Some(Label::new(format!("l_{:X}",abs),abs)),
						_ => None,
					}
				}
				0x04..=0x07 => Some(Label::new(format!("l_{:X}",abs),abs)),
				_ => None,
			}
		}
		// Absolute jumps
		Type::Jump => {
			let abs = ins.get_offset();
			match opcode {
				0x02 | 0x03 => Some(Label::new(format!("func_{:X}",abs),abs)),
				_ => None,
			}
		}
		_ => None,
	}
}

pub fn synthetize(enc: &Vec::<Encoded>, labels: &Vec::<Label>, vram: u64, i: *mut usize) -> String {
	let idx = unsafe { i.read_volatile() };
	let ins = &enc[idx];
	let pc = ((idx * size_of::<u32>()) as u64) + vram;

	let funct = ins.get_funct();
	let rd = ins.get_rd();
	let rt = ins.get_rt();
	let rs = ins.get_rs();
	match ins.get_type() {
		Type::Interrupt => {
			let code = ins.get_break_code();
			if funct == 0x0C {
				return format!("syscall");
			}
			return format!("break 0x{:X}",code);
		}
		Type::Register => {
			// R-Type
			let shift = ins.get_shift();
			let cc = ins.get_coproc();

			match funct {
				0x00 => {
					if rd == 0 && rt == 0 && shift == 0 {
						return format!("nop");
					} else {
						return format!("sll ${}, ${}, {}",rd,rt,shift);
					}
				}
				0x01 => {
					let t = ins.get_coproc_movx();

					if t == 0 {
						format!("movf ${}, ${}, {}",rd,rs,cc)
					} else {
						format!("movt ${}, ${}, {}",rd,rs,cc)
					}
				}

				0x02 => format!("srl ${}, ${}, {}",rd,rt,shift),
				0x03 => format!("sra ${}, ${}, {}",rd,rt,shift),
				0x04 => format!("sllv ${}, ${}, ${}",rd,rt,rs),
				0x06 => format!("srlv ${}, ${}, ${}",rd,rt,rs),
				0x07 => format!("srav ${}, ${}, ${}",rd,rt,rs),
				0x08 => format!("jr ${}",rs),
				0x09 => format!("jalr ${}, ${}",rs,rd),
				0x0A => format!("movz ${}, ${}, ${}",rd,rs,rt),
				0x0B => format!("movn ${}, ${}, ${}",rd,rs,rt),
				0x10 => format!("mfhi ${}",rs),
				0x11 => format!("mthi ${}",rs),
				0x12 => format!("mflo ${}",rd),
				0x13 => format!("mtlo ${}",rd),
				0x18 => format!("mult ${}, ${}",rs,rt),
				0x19 => format!("multu ${}, ${}",rs,rt),
				0x1A => format!("div ${}, ${}",rs,rt),
				0x1B => format!("divu ${}, ${}",rs,rt),
				0x20 => format!("add ${}, ${}, ${}",rd,rs,rt),
				0x21 => format!("addu ${}, ${}, ${}",rd,rs,rt),
				0x22 => format!("sub ${}, ${}, ${}",rd,rs,rt),
				0x23 => format!("subu ${}, ${}, ${}",rd,rs,rt),
				0x24 => format!("and ${}, ${}, ${}",rd,rs,rt),
				0x25 => format!("or ${}, ${}, ${}",rd,rs,rt),
				0x26 => format!("xor ${}, ${}, ${}",rd,rs,rt),
				0x27 => format!("nor ${}, ${}, ${}",rd,rs,rt),
				0x2A => format!("slt ${}, ${}, ${}",rd,rs,rt),
				0x2B => format!("sltu ${}, ${}, ${}",rd,rs,rt),
				0x30 => format!("tge ${}, ${}",rs,rt),
				0x31 => format!("tgeu ${}, ${}",rs,rt),
				0x32 => format!("tlt ${}, ${}",rs,rt),
				0x33 =>	format!("tltu ${}, ${}",rs,rt),
				0x34 => format!("teq ${}, ${}",rs,rt),
				0x36 => format!("tneq ${}, ${}",rs,rt),
				_ => {
					format!("dd 0x{:X}",ins.raw_dword)
				}
			}
		}
		Type::Immediate => {
			let opcode = ins.get_opcode();
			let imm = ins.get_immediate();
			let abs = ins.get_absolute(pc);

			let opt_label = labels.iter().find(|&a| a.target == abs as u64);
			let label_ref: String;
			if opt_label.is_some() {
				label_ref = opt_label.unwrap().name.clone();
			} else {
				label_ref = format!("{}",imm);
			}
			match opcode {
				0x01 => {
					match rt {
						0x00 => format!("bltz ${}, {}",rs,label_ref),
						0x01 => format!("bgez ${}, {}",rs,label_ref),
						0x08 => format!("tgei ${}, {}",rs,imm),
						0x09 => format!("tgeiu ${}, {}",rs,imm),
						0x0A => format!("tlti ${}, {}",rs,imm),
						0x0B => format!("tltiu ${}, {}",rs,imm),
						0x0C => format!("teqi ${}, {}",rs,imm),
						0x0E => format!("tneqi ${}, {}",rs,imm),
						0x10 => format!("bltzal ${}, {}",rs,label_ref),
						0x11 => format!("bgezal ${}, {}",rs,label_ref),
						_ => {
							format!("dd 0x{:X}",ins.raw_dword)
						}
					}
				}
				0x04 => format!("beq ${}, ${}, {}",rs,rt,label_ref),
				0x05 => format!("bne ${}, ${}, {}",rs,rt,label_ref),
				0x06 => format!("blez ${}, {}",rs,label_ref),
				0x07 => format!("bgtz ${}, {}",rs,label_ref),
				0x08 => format!("addi ${}, ${}, {}",rd,rs,imm),
				0x09 => format!("addiu ${}, ${}, {}",rd,rs,imm),
				0x0A => format!("slti ${}, ${}, {}",rd,rt,imm),
				0x0B => format!("sltiu ${}, ${}, {}",rd,rt,imm),
				0x0C => format!("andi ${}, ${}, {}",rd,rs,imm),
				0x0D => format!("ori ${}, ${}, {}",rd,rs,imm),
				0x0E => format!("xori ${}, ${}, {}",rd,rs,imm),
				0x0F => format!("lui ${}, {}",rt,imm),
				0x10 => {
					if rs == 0 {
						format!("mfc0 ${}, ${}",rt,rd)
					} else {
						format!("mtc0 ${}, ${}",rt,rd)
					}
				}
				0x11 => {
					// Co-processor instructions
					if funct == 0 && rs == 0 {
						format!("mfcl0 ${}, ${}",rt,rd)
					} else if funct == 0x00 && rs == 0x00 {
						format!("mfcl0 ${}, ${}",rt,rd)
					} else if funct == 0x05 && rs == 0x01 {
						format!("abs.d $f{}, $f{}",rt,rd)
					} else if funct == 0x05 && rs == 0x00 {
						format!("abs.s $f{}, $f{}",rt,rd)
					} else if funct == 0x00 && rs == 0x11 {
						format!("abs.d $f{}, $f{}",rt,rd)
					} else {
						format!("abs.d $f{}, $f{}",rt,rd)
					}
				}
				0x20 => format!("lb ${}, {}(${})",rt,imm,rs),
				0x21 => format!("lh ${}, {}(${})",rt,imm,rs),
				0x22 => format!("lwl ${}, {}(${})",rt,imm,rs),
				0x23 => format!("lw ${}, {}(${})",rt,imm,rs),
				0x24 => format!("lbu ${}, {}(${})",rt,imm,rs),
				0x25 => format!("lhu ${}, {}(${})",rt,imm,rs),
				0x26 => format!("lwr ${}, {}(${})",rt,imm,rs),
				0x28 => format!("sb ${}, {}(${})",rt,imm,rs),
				0x29 => format!("sh ${}, {}(${})",rt,imm,rs),
				0x2A => format!("swl ${}, {}(${})",rt,imm,rs),
				0x2B => format!("sw ${}, {}(${})",rt,imm,rs),
				0x2E => format!("swr ${}, {}(${})",rt,imm,rs),
				0x30 => format!("ll ${}, {}(${})",rt,imm,rs),
				0x31 => format!("l/swcl ${}, {}(${})",rt,imm,rs),
				0x38 => format!("sc ${}, {}(${})",rt,imm,rs),
				0x3D => format!("sdcl $f{}, {}(${})",rt,imm,rs),
				_ => {
					// Invalid
					format!("dd 0x{:X}",ins.raw_dword)
				}
			}
		}
		Type::Jump => {
			let opcode = ins.get_opcode();
			let abs = ins.get_offset();

			let opt_label = labels.iter().find(|&a| a.target == abs as u64);
			let label_ref: String;
			if opt_label.is_some() {
				label_ref = opt_label.unwrap().name.clone();
			} else {
				label_ref = format!("{}",abs);
			}
			match opcode {
				0x02 => format!("j {}",label_ref),
				0x03 => format!("jal {}",label_ref),
				_ => { "".to_string() }
			}
		}
		Type::Invalid => {
			// Pointing to a label, this simplifies everything
			let opt_label = labels.iter().find(|&a| a.target == ins.raw_dword as u64);
			let label_ref: String;
			if opt_label.is_some() {
				label_ref = opt_label.unwrap().name.clone();
				return format!("dd {}",label_ref);
			}
			format!("dd 0x{:X}",ins.raw_dword)
		}
	}
}