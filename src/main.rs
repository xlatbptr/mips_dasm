pub mod instruction;
use crate::instruction::*;

pub mod label;
use crate::label::*;

use std::mem::size_of;
use std::error::Error;
use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::fs;

fn print_usage() {
	println!("MIPS v1.0 Disassembler");
	println!("Usage ./mips_dasm [file] -o [file]");
	println!("--output [file], -o [file]       Specifies the output file");
	println!("--vram [uint], -v [uint]         Start of the VRAM");
	println!("--file-offset [uint], -z [uint]  Offset from file to start reading at");
	println!("--size [uint], -s [uint]         Number of instructions to disassemble");
}

fn main() -> Result<(),Box<dyn Error>> {
	let mut input_file: String = "".to_string();
	let mut output_file: String = "output.S".to_string();
	let mut read_size = 0;
	let mut file_offset = 0;
	let mut vram = 0;

	// Parse arguments
	let args: Vec::<String> = env::args().collect();
	let mut j = 0;
	while j < args.len() {
		match args[j].as_str() {
			"--output" | "-o" | "--out" => {
				j += 1;
				output_file = args[j].clone();
			}
			"--vram" | "-v" => {
				j += 1;
				vram = args[j].parse::<usize>()?;
			}
			"--file-offset" | "-z" => {
				j += 1;
				file_offset = args[j].parse::<usize>()?;
			}
			"--size" | "-s" => {
				j += 1;
				read_size = args[j].parse::<usize>()? * size_of::<u32>();
			}
			"--help" | "-h" => {
				print_usage();
				return Ok(());
			}
			_ => {
				input_file = args[j].clone();
			}
		}
		j += 1;
	}

	if input_file.is_empty() {
		print_usage();
		return Ok(());
	}

	let mut f = File::open(input_file)?;

	// This fastens up the speed by a considerable amount of time
	println!("Placing file onto memory");
	let mut buffer = Vec::<u8>::new();
	f.read_to_end(&mut buffer)?;

	if read_size == 0 {
		read_size = buffer.len() - file_offset;
	}

	// Collect file's bytes and apck them to the encoding buffer
	println!("Recollecting encoded instructions");
	let mut enc = Vec::<Encoded>::new();
	enc.reserve(buffer.len()/size_of::<u32>());
	let mut labels = Vec::<Label>::new();
	labels.reserve((buffer.len()/size_of::<u32>())/32);

	for i in file_offset..(read_size / size_of::<u32>()) {
		// MIPS is big endian, so we have to read the value and then byte-swap it on
		// little endian architectures
		let ins = u32::from_be(
			(buffer[i * size_of::<u32>() + 0] as u32) << 24 |
			(buffer[i * size_of::<u32>() + 1] as u32) << 16 |
			(buffer[i * size_of::<u32>() + 2] as u32) << 8 |
			(buffer[i * size_of::<u32>() + 3] as u32));
		
		let ins = Encoded::new(ins);

		// Collect labels
		let pc = (i * size_of::<u32>()) + vram;
		let label = label::obtain_label(&ins,pc as u64);
		if label.is_some() {
			let label = label.unwrap();

			// Allow duplicates
			if labels.iter().find(|&a| a.target == label.target as u64).is_some() {
				continue;
			}
			//println!("label: {} @ {:#024x}",label.name,label.target);
			labels.push(label);
		}

		// Add onto the vector of encoded instructions
		enc.push(ins);
	}
	println!("Using {} bytes of memory",size_of::<Encoded>() * enc.len() + size_of::<u8>() * buffer.len() + size_of::<Label>() * labels.len());

	// Give back the OS some memory
	buffer.clear();
	enc.shrink_to_fit();

	// Write output to string
	println!("Sinthetizing into readable assembly");
	println!("Using {} bytes of memory",size_of::<Encoded>() * enc.len() + size_of::<u8>() * buffer.len() + size_of::<Label>() * labels.len());

	let mut i = 0;
	let mut output = String::new();
	while i < enc.len() {
		let pc = (i * size_of::<u32>()) + vram;
		let label = labels.iter().find(|&a| a.target == pc as u64);
		if label.is_some() {
			let label = label.unwrap();
			output.push_str(&format!("{}:\r\n",label.name));
		}
		let partial_output = instruction::synthetize(&enc,&labels,vram as u64,&mut i);
		output.push_str(&format!("/* {:#010X} {:#010X} */\t{}\r\n",pc,enc[i].raw_dword,partial_output));
		i += 1;
	}
	enc.clear();

	// Write the output to the given file
	fs::write(output_file,output)?;
	
	// Now we end!
	Ok(())
}