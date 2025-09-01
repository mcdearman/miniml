pub mod info_table;

#[derive(Debug)]
pub struct VM {
    stack: Vec<u32>,
    pc: usize,
    code: Vec<u32>,
    registers: [u32; 16],
}

impl VM {
    pub fn new(code: Vec<u32>) -> Self {
        VM {
            stack: Vec::new(),
            pc: 0,
            code,
            registers: [0; 16],
        }
    }

    pub fn run(&mut self) {
        while self.pc < self.code.len() {
            let instruction = self.code[self.pc];
            self.execute(instruction);
            self.pc += 1;
        }
    }

    fn execute(&mut self, instruction: u32) {
        // Placeholder for instruction execution logic
        // This would decode the instruction and perform the appropriate action
        println!("Executing instruction: {}", instruction);
    }
}
