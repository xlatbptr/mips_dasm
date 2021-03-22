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

static REGISTER_NAMES: [&str;32] = [
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
			return format!("break 0x{:X}",code);
		}
		Type::Register => {
			// R-Type
			match funct {
				0x00 => {
					if rd == 0 && rt == 0 && shift == 0 {
						return format!("nop");
					} else {
						return format!("sll ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],shift);
					}
				}
				0x01 => {
					let t = ins.get_coproc_movx();

					if t == 0 {
						return format!("movf ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],cc);
					} else {
						return format!("movt ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],cc);
					}
				}

				0x02 => format!("srl ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],shift),
				0x03 => format!("sra ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],shift),
				0x04 => format!("sllv ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],REGISTER_NAMES[rs]),
				0x06 => format!("srlv ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],REGISTER_NAMES[rs]),
				0x07 => format!("srav ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],REGISTER_NAMES[rs]),
				0x08 => format!("jr ${}",REGISTER_NAMES[rs]),
				0x09 => format!("jalr ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rd]),
				0x0A => format!("movz ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x0B => format!("movn ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x10 => format!("mfhi ${}",REGISTER_NAMES[rs]),
				0x11 => format!("mthi ${}",REGISTER_NAMES[rs]),
				0x12 => format!("mflo ${}",REGISTER_NAMES[rd]),
				0x13 => format!("mtlo ${}",REGISTER_NAMES[rd]),
				0x18 => format!("mult ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x19 => format!("multu ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x1A => format!("div ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x1B => format!("divu ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x20 => format!("add ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x21 => format!("addu ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x22 => format!("sub ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x23 => format!("subu ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x24 => format!("and ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x25 => format!("or ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x26 => format!("xor ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x27 => format!("nor ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x2A => format!("slt ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x2B => format!("sltu ${}, ${}, ${}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x30 => format!("tge ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x31 => format!("tgeu ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x32 => format!("tlt ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x33 =>	format!("tltu ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x34 => format!("teq ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				0x36 => format!("tneq ${}, ${}",REGISTER_NAMES[rs],REGISTER_NAMES[rt]),
				_ => {
					dword_data(ins,labels)
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
						0x00 => format!("bltz ${}, {}",REGISTER_NAMES[rs],label_ref),
						0x01 => format!("bgez ${}, {}",REGISTER_NAMES[rs],label_ref),
						0x08 => format!("tgei ${}, {}",REGISTER_NAMES[rs],imm),
						0x09 => format!("tgeiu ${}, {}",REGISTER_NAMES[rs],imm),
						0x0A => format!("tlti ${}, {}",REGISTER_NAMES[rs],imm),
						0x0B => format!("tltiu ${}, {}",REGISTER_NAMES[rs],imm),
						0x0C => format!("teqi ${}, {}",REGISTER_NAMES[rs],imm),
						0x0E => format!("tneqi ${}, {}",REGISTER_NAMES[rs],imm),
						0x10 => format!("bltzal ${}, {}",REGISTER_NAMES[rs],label_ref),
						0x11 => format!("bgezal ${}, {}",REGISTER_NAMES[rs],label_ref),
						_ => dword_data(ins,labels),
					}
				}
				0x04 => format!("beq ${}, ${}, {}",REGISTER_NAMES[rs],REGISTER_NAMES[rt],label_ref),
				0x05 => format!("bne ${}, ${}, {}",REGISTER_NAMES[rs],REGISTER_NAMES[rt],label_ref),
				0x06 => format!("blez ${}, {}",REGISTER_NAMES[rs],label_ref),
				0x07 => format!("bgtz ${}, {}",REGISTER_NAMES[rs],label_ref),
				0x08 => format!("addi ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],imm),
				0x09 => format!("addiu ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],imm),
				0x0A => format!("slti ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],imm),
				0x0B => format!("sltiu ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rt],imm),
				0x0C => format!("andi ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],imm),
				0x0D => format!("ori ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],imm),
				0x0E => format!("xori ${}, ${}, {}",REGISTER_NAMES[rd],REGISTER_NAMES[rs],imm),
				0x0F => format!("lui ${}, {}",REGISTER_NAMES[rt],imm),
				0x10 => {
					// TODO: This is a horrible way of doing it
					if funct == 0x18 {
						format!("eret")
					} else {
						if rs == 0 {
							format!("mfc0 ${}, ${}",REGISTER_NAMES[rt],REGISTER_NAMES[rd])
						} else {
							format!("mtc0 ${}, ${}",REGISTER_NAMES[rt],REGISTER_NAMES[rd])
						}
					}
				}
				0x11 => {
					let fd = shift;
					let fs = rd;
					let ft = rt;
					let subtype = rs;

					// Co-processor instructions
					if funct == 0 && rs == 0 {
						return format!("mfcl0 ${}, ${}",REGISTER_NAMES[rt],REGISTER_NAMES[rd]);
					} else if funct == 0x00 && rs == 0x00 {
						return format!("mfcl0 ${}, ${}",REGISTER_NAMES[rt],REGISTER_NAMES[rd]);
					} else if funct == 0x05 && rs == 0x00 {
						return format!("abs.s $f{}, $f{}",fd,fs);
					} else if funct == 0x05 && rs == 0x01 {
						return format!("abs.d $f{}, $f{}",fd,fs);
					}

					// Invalid
					if subtype != 0x10 {
						return format!("abs.s $f{}, $f{}",rt,rd);
					}

					let fc = fd & 0x03;
					if fc == 0xFC {
						match funct {
							0x02 => format!("c.eq.{} {} $f{}, $f{}",subtype-0x10,cc,fs,ft),
							0x0C => format!("c.lt.{} {} $f{}, $f{}",subtype-0x10,cc,fs,ft),
							0x0E => format!("c.le.{} {} $f{}, $f{}",subtype-0x10,cc,fs,ft),
							_ => dword_data(ins,labels),
						}
					} else {
						// TODO: This bugs
						match funct {
							0x00 => format!("add.{} $f{}, $f{}, $f{}",subtype-0x10,fd,fs,ft),
							0x01 => format!("sub.{} $f{}, $f{}, ${}",subtype-0x10,fd,fs,ft),
							0x02 => format!("mul.{} $f{}, $f{}, ${}",subtype-0x10,fd,fs,ft),
							0x03 => format!("div.{} $f{}, $f{}, $f{}",subtype-0x10,fd,fs,ft),
							0x04 => format!("sqrt.{} $f{}, $f{}",subtype-0x10,fd,fs),
							0x06 => format!("mov.{} $f{}, $f{}",subtype-0x10,fd,fs),
							0x07 => format!("neg.{} $f{}, $f{}",subtype-0x10,fd,fs),
							0x0C => format!("round.w.{} $f{}, $f{}",subtype-0x10,fd,fs),
							0x0D => format!("trunc.w.{} $f{}, $f{}",subtype-0x10,fd,fs),
							0x0E => format!("ceil.w.{} $f{}, $f{}, $f{}",subtype-0x10,fd,fs,ft),
							0x0F => format!("floor.w.{} $f{}, $f{}",subtype-0x10,fd,fs),
							0x11 => {
								if (ins.raw_dword >> 16) & 3 == 1 {
									return format!("movt.{} $f{}, $f{}, {}",subtype-0x10,fd,fs,cc);
								} else {
									return format!("movf.{} $f{}, $f{}, {}",subtype-0x10,fd,fs,cc);
								}
							}
							0x12 => format!("movz.{} $f{}, $f{}, ${}",subtype-0x10,fd,fs,REGISTER_NAMES[ft]),
							0x13 => format!("movn.{} $f{}, $f{}, ${}",subtype-0x10,fd,fs,REGISTER_NAMES[ft]),
							0x20 => format!("cvt.s.{} $f{}, $f{}",subtype-0x10,fd,fs), // 0x11 = d, 0x14 = w
							0x21 => format!("cvt.d.{} $f{}, $f{}",subtype-0x10,fd,fs), // 0x10 = s, 0x14 = w
							0x24 => format!("cvt.w.{} $f{}, $f{}",subtype-0x10,fd,fs),
							_ => dword_data(ins,labels),
						}
					}
				}
				0x20 => format!("lb ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x21 => format!("lh ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x22 => format!("lwl ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x23 => format!("lw ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x24 => format!("lbu ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x25 => format!("lhu ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x26 => format!("lwr ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x28 => format!("sb ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x29 => format!("sh ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x2A => format!("swl ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x2B => format!("sw ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x2E => format!("swr ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x30 => format!("ll ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x31 => format!("l/swcl ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x38 => format!("sc ${}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				0x3D => format!("sdcl $f{}, {}(${})",REGISTER_NAMES[rt],imm,REGISTER_NAMES[rs]),
				_ => dword_data(ins,labels),
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