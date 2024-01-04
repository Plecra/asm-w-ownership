
use ghost_cell::{GhostToken, GhostCell};
#[derive(Debug, Clone)]
#[repr(u8)]
enum Instruction {
    ADC_IMM = 0x69,
    ADC_ZP = 0x65,
    ADC_ZPX = 0x75,
    ADC_ABS = 0x6D,
    ADC_ABSX = 0x7D,
    ADC_ABSY = 0x79,
    ADC_INDX = 0x61,
    ADC_INDY = 0x71,

    AND_IMM = 0x29, 
    AND_ZP = 0x25,
    AND_ZPX = 0x35,
    AND_ABS = 0x2D,
    AND_ABSX = 0x3D,
    AND_ABSY = 0x39,
    AND_INDX = 0x21,
    AND_INDY = 0x31,
    
    ASL_ACC = 0x0A,
    ASL_ZP = 0x06,
    ASL_ZPX = 0x16,
    ASL_ABS = 0x0E,
    ASL_ABSX = 0x1E,

    BCC = 0x90,

    BCS = 0xB0,

    BEQ = 0xF0,

    BIT_ZP = 0x24,
    BIT_ABS = 0x2C,

    BMI = 0x30,

    BNE = 0xD0,

    BPL = 0x10,

    BRK = 0x00,

    BVC = 0x50,

    BVS = 0x70,

    CLC = 0x18,

    CLD = 0xD8,

    CLI = 0x58,

    CLV = 0xB8,

    CMP_IMM = 0xC9,
    CMP_ZP = 0xC5,
    CMP_ZPX = 0xD5,
    CMP_ABS = 0xCD,
    CMP_ABSX = 0xDD,
    CMP_ABSY = 0xD9,
    CMP_INDX = 0xC1,
    CMP_INDY = 0xD1,

    CPX_IMM = 0xE0,
    CPX_ZP = 0xE4,
    CPX_ABS = 0xEC,

    CPY_IMM = 0xC0,
    CPY_ZP = 0xC4,
    CPY_ABS = 0xCC,

    DEC_ZP = 0xC6,
    DEC_ZPX = 0xD6,
    DEC_ABS = 0xCE,
    DEC_ABSX = 0xDE,

    DEX = 0xCA,

    DEY = 0x88,

    EOR_IMM = 0x49,
    EOR_ZP = 0x45,
    EOR_ZPX = 0x55,
    EOR_ABS = 0x4D,
    EOR_ABSX = 0x5D,
    EOR_ABSY = 0x59,
    EOR_INDX = 0x41,
    EOR_INDY = 0x51,

    INC_ZP = 0xE6,
    INC_ZPX = 0xF6,
    INC_ABS = 0xEE,
    INC_ABSX = 0xFE,

    INX = 0xE8,

    INY = 0xC8,

    JMP_ABS = 0x4C,
    JMP_IND = 0x6C,

    JSR = 0x20,

    LDA_IMM = 0xA9,
    LDA_ZP = 0xA5,
    LDA_ZPX = 0xB5,
    LDA_ABS = 0xAD,
    LDA_ABSX = 0xBD,
    LDA_ABSY = 0xB9,
    LDA_INDX = 0xA1,
    LDA_INDY = 0xB1,

    LDX_IMM = 0xA2,
    LDX_ZP = 0xA6,
    LDX_ZPY = 0xB6,
    LDX_ABS = 0xAE,
    LDX_ABSY = 0xBE,

    LDY_IMM = 0xA0,
    LDY_ZP = 0xA4,
    LDY_ZPX = 0xB4,
    LDY_ABS = 0xAC,
    LDY_ABSX = 0xBC,

    LSR_ACC = 0x4A,
    LSR_ZP = 0x46,
    LSR_ZPX = 0x56,
    LSR_ABS = 0x4E,
    LSR_ABSX = 0x5E,

    NOP = 0xEA,

    ORA_IMM = 0x09,
    ORA_ZP = 0x05,
    ORA_ZPX = 0x15,
    ORA_ABS = 0x0D,
    ORA_ABSX = 0x1D,
    ORA_ABSY = 0x19,
    ORA_INDX = 0x01,
    ORA_INDY = 0x11,

    PHA = 0x48,

    PHP = 0x08,

    PLA = 0x68,

    PLP = 0x28,

    ROL_ACC = 0x2A,
    ROL_ZP = 0x26,
    ROL_ZPX = 0x36,
    ROL_ABS = 0x2E,
    ROL_ABSX = 0x3E,

    ROR_ACC = 0x6A,
    ROR_ZP = 0x66,
    ROR_ZPX = 0x76,
    ROR_ABS = 0x6E,
    ROR_ABSX = 0x7E,

    RTI = 0x40,

    RTS = 0x60,

    SBC_IMM = 0xE9,
    SBC_ZP = 0xE5,
    SBC_ZPX = 0xF5,
    SBC_ABS = 0xED,
    SBC_ABSX = 0xFD,
    SBC_ABSY = 0xF9,
    SBC_INDX = 0xE1,
    SBC_INDY = 0xF1,

    SEC = 0x38,

    SED = 0xF8,

    SEI = 0x78,

    STA_ZP = 0x85,
    STA_ZPX = 0x95,
    STA_ABS = 0x8D,
    STA_ABSX = 0x9D,
    STA_ABSY = 0x99,
    STA_INDX = 0x81,
    STA_INDY = 0x91,

    STX_ZP = 0x86,
    STX_ZPY = 0x96,
    STX_ABS = 0x8E,

    STY_ZP = 0x84,
    STY_ZPX = 0x94,
    STY_ABS = 0x8C,

    TAX = 0xAA,

    TAY = 0xA8,

    TSX = 0xBA,

    TXA = 0x8A,

    TXS = 0x9A,

    TYA = 0x98,
}
#[derive(Default)]
struct MOS6507<'brand, Bus> {
    bus: Bus,
    codegen: CodegenCell<'brand>,
}

struct Status<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct DecimalModeFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct NegativeFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct OverflowFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct ZeroFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct CarryFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct InterruptDisableFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand> InterruptDisableFlag<'brand, '_> {
    fn set_interrupt_disable(&mut self, code: &mut GhostToken<'brand>) {
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::SEI as u8);
    }
}
struct Math<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
#[derive(Debug, Clone)]
enum MemIshOperand {
    Implied,
    Accumulator,
    Immediate(u8),
    Deref(u16),
    XIndexedDeref(u16),
    YIndexedDeref(u16),
    Indirect(u16),
    XIndexedIndirectDeref(u8),
    IndirectYIndexedDeref(u8),
    
}
impl<'brand> Math<'brand, '_> {
    fn add_with_carry(&mut self, code: &mut GhostToken<'brand>, _: CarryFlag<'_, '_>, operand: MemIshOperand) {
        let codegen = self.codegen.codegen.borrow_mut(code);
        match operand {
            MemIshOperand::Immediate(imm) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_IMM as u8);
                codegen.basic_blocks[codegen.block].push(imm);
            }
            MemIshOperand::Deref(addr) if addr < 256 => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ZP as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
            }
            MemIshOperand::Deref(addr) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ABS as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
                codegen.basic_blocks[codegen.block].push((addr >> 8) as u8);
            }
            MemIshOperand::XIndexedDeref(addr) if addr < 256 => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ZPX as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
            }
            MemIshOperand::XIndexedDeref(addr) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ABSX as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
                codegen.basic_blocks[codegen.block].push((addr >> 8) as u8);
            }
            MemIshOperand::YIndexedDeref(addr) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ABSY as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
                codegen.basic_blocks[codegen.block].push((addr >> 8) as u8);
            }
            MemIshOperand::XIndexedIndirectDeref(addr) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_INDX as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
            }
            MemIshOperand::IndirectYIndexedDeref(addr) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_INDY as u8);
                codegen.basic_blocks[codegen.block].push(addr as u8);
            }
            _ => unimplemented!("operaned {operand:?} not supported for ADC"),

        }
        
    }
}
impl<'brand, 'a> DecimalModeFlag<'brand, 'a> {
    fn clear_decimal_flag<'c>(self, code: &mut GhostToken<'brand>) -> Math<'brand, 'c> where 'a: 'c {
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::CLD as u8);
        Math { codegen: self.codegen }
    }
}
impl<'brand> Status<'brand, '_> {
    fn as_mut_parts(&mut self) -> (DecimalModeFlag<'brand, '_>, NegativeFlag<'brand, '_>, OverflowFlag<'brand, '_>, ZeroFlag<'brand, '_>, CarryFlag<'brand, '_>, InterruptDisableFlag<'brand, '_>) {
        let decimal_mode = DecimalModeFlag { codegen: &self.codegen };
        let negative = NegativeFlag { codegen: &self.codegen };
        let overflow = OverflowFlag { codegen: &self.codegen };
        let zero = ZeroFlag { codegen: &self.codegen };
        let carry = CarryFlag { codegen: &self.codegen };
        let interrupt_disable = InterruptDisableFlag { codegen: &self.codegen };

        (decimal_mode, negative, overflow, zero, carry, interrupt_disable)
    }
}
impl<'brand, Bus> MOS6507<'brand, Bus> {
    fn as_mut_parts(&mut self) -> ((Status<'brand, '_>, ProgramCounter<'brand, '_>, Stack<'brand, '_>, Values<'brand, '_>), &mut Bus) {
        let status = Status { codegen: &self.codegen };
        let pc = ProgramCounter { codegen: &self.codegen };
        let stack = Stack { codegen: &self.codegen };
        let values = Values { codegen: &self.codegen };
        let registers = (status, pc, stack, values);
        (registers, &mut self.bus)
    }
}
struct ProgramCounter<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct Stack<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct Values<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}

#[derive(Default)]
struct CodegenCell<'brand> {
    codegen: GhostCell<'brand, Codegen>,
}
struct Codegen {
    basic_blocks: Vec<Vec<u8>>,
    block: usize,
}
#[derive(Default)]
struct R6532;
impl Default for Codegen {
    fn default() -> Self {
        Self::new()
    }

}
impl Codegen {
    fn new() -> Self {
        let root = vec![];
        Self {
            basic_blocks: vec![root],
            block: 0,
        }
    }
}

// pub fn reset(code: Codegen, cpu: MOS6507<R6532>) {
//     code.set_interrupt_disable();
//     let math = cpu.clear_decimal_flag();

//     let riot = cpu.bus(R6532);
//     cpu.store(riot.DDRA, 0x00);
//     cpu.store(riot.DDRB, 0xFF);

//     let heap = riot.mem;
//     heap.fill(0);
//     cpu.set_stack_pointer(heap.get(end-1));

//     cpu.forever(|cpu| {
//         cpu.inc(riot.DRB);
//         cpu.bit(riot.DRA); // sets cpu.negative to bit 7
//         eights_exponent = (!cpu.negative).select(3, 2);
//         riot.timer(.interrupt_off, eights_exponent) := 244;

//         while riot.timer(.interrupt_off).* != 0 {}  
//     });
// }


fn reset<'a>(code: &mut GhostToken<'a>, cpu: &mut MOS6507<'a, R6532>) {
    let (registers, bus) = cpu.as_mut_parts();
    let (mut status, pc, stack, values) = registers;
    let (mut decimal_mode, negative, overflow, zero, carry, mut interrupt_disable) = status.as_mut_parts();
    interrupt_disable.set_interrupt_disable(code);
    let mut math = decimal_mode.clear_decimal_flag(code);
    math.add_with_carry(code, carry, MemIshOperand::Immediate(0x00));
    
    // riot = cpu.bus(R6532)
    // riot.DDRA := 0x00
    // riot.DDRB := 0xFF
    
    // heap = riot.mem
    // heap.fill(0)
    // cpu.set_stack_pointer(heap.(end-1))

    // loop {
    //     cpu.inc(riot.DRB)
    //     cpu.bit(riot.DRA) // sets cpu.negative to bit 7
    //     eights_exponent = if !cpu.negative: 3 else : 2
    //     riot.timer(.interrupt_off, eights_exponent) := 244
        
    //     cpu.while_(|| riot.timer(.interrupt_off).* != 0, || {})
    // }
    // }
}
fn decode_block(insrs: &[u8]) -> impl Iterator<Item = (Instruction, MemIshOperand)> + '_ {
    let mut insrs = insrs.iter();
    core::iter::from_fn(move || insrs.next().map(|ins| {
        match ins {
            0x78 => (Instruction::SEI, MemIshOperand::Implied),
            0xD8 => (Instruction::CLD, MemIshOperand::Implied),
            0x69 => (Instruction::ADC_IMM, MemIshOperand::Immediate(*insrs.next().unwrap())),

            unknown => todo!("unknown instruction {:#x}", unknown),
        }
    }))
}
fn main() {
    let out = GhostToken::new(|mut token| {
        let mut target = MOS6507::default();

        reset(&mut token, &mut target);
        target.codegen.codegen.into_inner()
    });
    for block in &out.basic_blocks {
        for (insr, op) in decode_block(block) {
            println!("  {insr:?} {op:?}");
        }
        println!("");
    }
}