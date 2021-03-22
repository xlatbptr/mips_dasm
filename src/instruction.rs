use std::mem::size_of;

pub enum Type {
	Invalid,
	Register,
	Immediate,
	Jump,
	Interrupt,
	Branching,
	Float,
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

	pub fn get_fd(&self) -> u8 {
		((self.raw_dword >> 6) & 0x1F) as u8
	}
	pub fn get_ft(&self) -> u8 {
		((self.raw_dword >> 11) & 0x1F) as u8
	}
	pub fn get_fs(&self) -> u8 {
		((self.raw_dword >> 16) & 0x1F) as u8
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
	// both r and i-type
	pub fn get_rt(&self) -> u8 {
		((self.raw_dword >> 16) & 0x1F) as u8
	}
	pub fn get_rs(&self) -> u8 {
		((self.raw_dword >> 21) & 0x1F) as u8
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
			0x00 => {
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
			0x01 => {
				match self.get_rt() {
					0x00..=0x01 | 0x10..=0x11 => Type::Branching,
					_ => Type::Immediate,
				}
			}
			0x02 | 0x03 => Type::Jump,
			0x04..=0x07 => Type::Branching,
			0x11 => {
				let funct = self.get_funct();
				let rs = self.get_rs();
				let fd = self.get_fd();
				let subtype = rs;

				if (funct != 0 && funct != 0x05 && rs != 0x00 && rs != 0x01) || subtype != 0x10 {
					return Type::Invalid;
				}
				
				let fc = fd & 0x03;
				if fc == 0xFC {
					return match funct {
						0x02 | 0x0C | 0x0E => Type::Float,
						_ => Type::Invalid,
					}
				}
				return match funct {
					0x00..=0x04 | 0x06..=0x07 | 0x0C..=0x0F | 0x11..=0x13 | 0x20 | 0x21 | 0x24 => Type::Float,
					_ => Type::Invalid,
				}
			}
			0x10 | 0x20..=0x26 | 0x28..=0x2B | 0x30..=0x31 | 0x38 => Type::Immediate,
			_ => Type::Invalid,
		}
	}
}

use crate::label::Label;
pub fn obtain_label(ins: &Encoded, pc: u64) -> Option<Label> {
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
		_ => None,
	}
}

static R_NAME: [&str;32] = [
	"zero",
	"at",
	"v0","v1",
	"a0","a1","a2","a3",
	"t0","t1","t2","t3","t4","t5","t6","t7",
	"s0","s1","s2","s3","s4","s5","s6","s7",
	"t8","t9",
	"k0","k1",
	"gp",
	"sp",
	"fp",
	"ra"
];

static FL_SIZE: [&str;4] = [
	"s","d","?","w"
];

pub fn dword_data(ins: &Encoded, labels: &Vec::<Label>) -> String {
	// Pointing to a label, this simplifies everything
	let opt_label = labels.iter().find(|&a| a.target == ins.raw_dword as u64);
	let label_ref: String;
	if opt_label.is_some() {
		label_ref = opt_label.unwrap().name.clone();
		return format!("dd {}",label_ref);
	}
	format!("dd 0x{:X}",ins.raw_dword)
}

pub fn synthetize(enc: &Vec::<Encoded>, labels: &Vec::<Label>, vram: u64, i: *mut usize) -> String {
	let idx = unsafe { i.read_volatile() };
	let ins = &enc[idx];
	let pc = ((idx * size_of::<u32>()) as u64) + vram;

	let funct = ins.get_funct();
	let rd = ins.get_rd() as usize;
	let rt = ins.get_rt() as usize;
	let rs = ins.get_rs() as usize;
	let shift = ins.get_shift();
	let cc = ins.get_coproc();
	match ins.get_type() {
		Type::Interrupt => {
			let code = ins.get_break_code();
			if funct == 0x0C {
				return format!("syscall");
			}
			format!("break 0x{:X}",code)
		}
		Type::Register => {
			// R-Type
			match funct {
				0x00 => {
					if rd == 0 && rt == 0 && shift == 0 {
						return format!("nop");
					}
					format!("sll ${}, ${}, {}",R_NAME[rd],R_NAME[rt],shift)
				}
				0x01 => {
					let movx = ins.get_coproc_movx();
					if movx == 0 {
						return format!("movf ${}, ${}, {}",R_NAME[rd],R_NAME[rs],cc);
					}
					format!("movt ${}, ${}, {}",R_NAME[rd],R_NAME[rs],cc)
				}
				0x02 => format!("srl ${}, ${}, {}",R_NAME[rd],R_NAME[rt],shift),
				0x03 => format!("sra ${}, ${}, {}",R_NAME[rd],R_NAME[rt],shift),
				0x04 => format!("sllv ${}, ${}, ${}",R_NAME[rd],R_NAME[rt],R_NAME[rs]),
				0x06 => format!("srlv ${}, ${}, ${}",R_NAME[rd],R_NAME[rt],R_NAME[rs]),
				0x07 => format!("srav ${}, ${}, ${}",R_NAME[rd],R_NAME[rt],R_NAME[rs]),
				0x08 => format!("jr ${}",R_NAME[rs]),
				0x09 => format!("jalr ${}, ${}",R_NAME[rs],R_NAME[rd]),
				0x0A => format!("movz ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x0B => format!("movn ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x10 => format!("mfhi ${}",R_NAME[rs]),
				0x11 => format!("mthi ${}",R_NAME[rs]),
				0x12 => format!("mflo ${}",R_NAME[rd]),
				0x13 => format!("mtlo ${}",R_NAME[rd]),
				0x18 => format!("mult ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x19 => format!("multu ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x1A => format!("div ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x1B => format!("divu ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x20 => format!("add ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x21 => format!("addu ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x22 => format!("sub ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x23 => format!("subu ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x24 => format!("and ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x25 => format!("or ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x26 => format!("xor ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x27 => format!("nor ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x2A => format!("slt ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x2B => format!("sltu ${}, ${}, ${}",R_NAME[rd],R_NAME[rs],R_NAME[rt]),
				0x30 => format!("tge ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x31 => format!("tgeu ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x32 => format!("tlt ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x33 =>	format!("tltu ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x34 => format!("teq ${}, ${}",R_NAME[rs],R_NAME[rt]),
				0x36 => format!("tneq ${}, ${}",R_NAME[rs],R_NAME[rt]),
				_ => {
					dword_data(ins,labels)
				}
			}
		}
		Type::Branching => {
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
						0x00 => format!("bltz ${}, {}",R_NAME[rs],label_ref),
						0x01 => format!("bgez ${}, {}",R_NAME[rs],label_ref),
						0x10 => format!("bltzal ${}, {}",R_NAME[rs],label_ref),
						0x11 => format!("bgezal ${}, {}",R_NAME[rs],label_ref),
						_ => dword_data(ins,labels),
					}
				}
				0x04 => format!("beq ${}, ${}, {}",R_NAME[rs],R_NAME[rt],label_ref),
				0x05 => format!("bne ${}, ${}, {}",R_NAME[rs],R_NAME[rt],label_ref),
				0x06 => format!("blez ${}, {}",R_NAME[rs],label_ref),
				0x07 => format!("bgtz ${}, {}",R_NAME[rs],label_ref),
				_ => dword_data(ins,labels),
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
						0x08 => format!("tgei ${}, {}",R_NAME[rs],imm),
						0x09 => format!("tgeiu ${}, {}",R_NAME[rs],imm),
						0x0A => format!("tlti ${}, {}",R_NAME[rs],imm),
						0x0B => format!("tltiu ${}, {}",R_NAME[rs],imm),
						0x0C => format!("teqi ${}, {}",R_NAME[rs],imm),
						0x0E => format!("tneqi ${}, {}",R_NAME[rs],imm),
						_ => dword_data(ins,labels),
					}
				}
				0x08 => format!("addi ${}, ${}, {}",R_NAME[rd],R_NAME[rs],imm),
				0x09 => format!("addiu ${}, ${}, {}",R_NAME[rd],R_NAME[rs],imm),
				0x0A => format!("slti ${}, ${}, {}",R_NAME[rd],R_NAME[rt],imm),
				0x0B => format!("sltiu ${}, ${}, {}",R_NAME[rd],R_NAME[rt],imm),
				0x0C => format!("andi ${}, ${}, {}",R_NAME[rd],R_NAME[rs],imm),
				0x0D => format!("ori ${}, ${}, {}",R_NAME[rd],R_NAME[rs],imm),
				0x0E => format!("xori ${}, ${}, {}",R_NAME[rd],R_NAME[rs],imm),
				0x0F => format!("lui ${}, {}",R_NAME[rt],imm),
				0x10 => {
					// TODO: This is a horrible way of doing it
					if funct == 0x18 {
						format!("eret")
					} else {
						if rs == 0 {
							format!("mfc0 ${}, ${}",R_NAME[rt],R_NAME[rd])
						} else {
							format!("mtc0 ${}, ${}",R_NAME[rt],R_NAME[rd])
						}
					}
				}
				0x20 => format!("lb ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x21 => format!("lh ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x22 => format!("lwl ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x23 => format!("lw ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x24 => format!("lbu ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x25 => format!("lhu ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x26 => format!("lwr ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x28 => format!("sb ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x29 => format!("sh ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x2A => format!("swl ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x2B => format!("sw ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x2E => format!("swr ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x30 => format!("ll ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x31 => format!("l/swcl ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x38 => format!("sc ${}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				0x3D => format!("sdcl $f{}, {}(${})",R_NAME[rt],imm,R_NAME[rs]),
				_ => dword_data(ins,labels),
			}
		}
		Type::Float => {
			let fd = ins.get_fd() as usize;
			let fs = ins.get_fs() as usize;
			let ft = ins.get_ft() as usize;
			let subtype = rs;

			// Co-processor instructions
			if funct == 0 && rs == 0 {
				return format!("mfcl0 ${}, ${}",R_NAME[rt],R_NAME[rd]);
			} else if funct == 0x05 {
				return format!("abs.{} $f{}, $f{}",R_NAME[rs],fd,fs);
			}

			// Invalid
			if subtype != 0x10 {
				return dword_data(ins,labels);
			}

			let float_sz = subtype % 4;
			let fc = fd & 0xFF;
			if fc == 0xFC {
				match funct {
					0x02 => format!("c.eq.{} {} $f{}, $f{}",FL_SIZE[float_sz],cc,fs,ft),
					0x0C => format!("c.lt.{} {} $f{}, $f{}",FL_SIZE[float_sz],cc,fs,ft),
					0x0E => format!("c.le.{} {} $f{}, $f{}",FL_SIZE[float_sz],cc,fs,ft),
					_ => dword_data(ins,labels),
				}
			} else {
				match funct {
					0x00 => format!("add.{} $f{}, $f{}, $f{}",FL_SIZE[float_sz],fd,fs,ft),
					0x01 => format!("sub.{} $f{}, $f{}, ${}",FL_SIZE[float_sz],fd,fs,ft),
					0x02 => format!("mul.{} $f{}, $f{}, ${}",FL_SIZE[float_sz],fd,fs,ft),
					0x03 => format!("div.{} $f{}, $f{}, $f{}",FL_SIZE[float_sz],fd,fs,ft),
					0x04 => format!("sqrt.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x06 => format!("mov.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x07 => format!("neg.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x0C => format!("round.w.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x0D => format!("trunc.w.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x0E => format!("ceil.w.{} $f{}, $f{}, $f{}",FL_SIZE[float_sz],fd,fs,ft),
					0x0F => format!("floor.w.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x11 => {
						if (ins.raw_dword >> 16) & 3 == 1 {
							return format!("movt.{} $f{}, $f{}, {}",FL_SIZE[float_sz],fd,fs,cc);
						} else {
							return format!("movf.{} $f{}, $f{}, {}",FL_SIZE[float_sz],fd,fs,cc);
						}
					}
					0x12 => format!("movz.{} $f{}, $f{}, ${}",FL_SIZE[float_sz],fd,fs,R_NAME[ft]),
					0x13 => format!("movn.{} $f{}, $f{}, ${}",FL_SIZE[float_sz],fd,fs,R_NAME[ft]),
					0x20 => format!("cvt.s.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x21 => format!("cvt.d.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					0x24 => format!("cvt.w.{} $f{}, $f{}",FL_SIZE[float_sz],fd,fs),
					_ => dword_data(ins,labels),
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
				_ => dword_data(ins,labels),
			}
		}
		Type::Invalid => dword_data(ins,labels),
	}
}