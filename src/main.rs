
use ghost_cell::{GhostToken, GhostCell};
impl Instruction {
    fn mnemonic(&self) -> &'static str {
        use Instruction::*;
        match self {
            ADC_IMM | ADC_ZP | ADC_ZPX | ADC_ABS | ADC_ABSX | ADC_ABSY | ADC_INDX | ADC_INDY => "ADC",
            AND_IMM | AND_ZP | AND_ZPX | AND_ABS | AND_ABSX | AND_ABSY | AND_INDX | AND_INDY => "AND",
            ASL_ACC | ASL_ZP | ASL_ZPX | ASL_ABS | ASL_ABSX => "ASL",
            BCC => "BCC",
            BCS => "BCS",
            BEQ => "BEQ",
            BIT_ZP | BIT_ABS => "BIT",
            BMI => "BMI",
            BNE => "BNE",
            BPL => "BPL",
            BRK => "BRK",
            BVC => "BVC",
            BVS => "BVS",
            CLC => "CLC",
            CLD => "CLD",
            CLI => "CLI",
            CLV => "CLV",
            CMP_IMM | CMP_ZP | CMP_ZPX | CMP_ABS | CMP_ABSX | CMP_ABSY | CMP_INDX | CMP_INDY => "CMP",
            CPX_IMM | CPX_ZP | CPX_ABS => "CPX",
            CPY_IMM | CPY_ZP | CPY_ABS => "CPY",
            DEC_ZP | DEC_ZPX | DEC_ABS | DEC_ABSX => "DEC",
            DEX => "DEX",
            DEY => "DEY",
            EOR_IMM | EOR_ZP | EOR_ZPX | EOR_ABS | EOR_ABSX | EOR_ABSY | EOR_INDX | EOR_INDY => "EOR",
            INC_ZP | INC_ZPX | INC_ABS | INC_ABSX => "INC",
            INX => "INX",
            INY => "INY",
            JMP_ABS | JMP_IND => "JMP",
            JSR => "JSR",
            LDA_IMM | LDA_ZP | LDA_ZPX | LDA_ABS | LDA_ABSX | LDA_ABSY | LDA_INDX | LDA_INDY => "LDA",
            LDX_IMM | LDX_ZP | LDX_ZPY | LDX_ABS | LDX_ABSY => "LDX",
            LDY_IMM | LDY_ZP | LDY_ZPX | LDY_ABS | LDY_ABSX => "LDY",
            LSR_ACC | LSR_ZP | LSR_ZPX | LSR_ABS | LSR_ABSX => "LSR",
            NOP => "NOP",
            ORA_IMM | ORA_ZP | ORA_ZPX | ORA_ABS | ORA_ABSX | ORA_ABSY | ORA_INDX | ORA_INDY => "ORA",
            PHA => "PHA",
            PHP => "PHP",
            PLA => "PLA",
            PLP => "PLP",
            ROL_ACC | ROL_ZP | ROL_ZPX | ROL_ABS | ROL_ABSX => "ROL",
            ROR_ACC | ROR_ZP | ROR_ZPX | ROR_ABS | ROR_ABSX => "ROR",
            RTI => "RTI",
            RTS => "RTS",
            SBC_IMM | SBC_ZP | SBC_ZPX | SBC_ABS | SBC_ABSX | SBC_ABSY | SBC_INDX | SBC_INDY => "SBC",
            SEC => "SEC",
            SED => "SED",
            SEI => "SEI",
            STA_ZP | STA_ZPX | STA_ABS | STA_ABSX | STA_ABSY | STA_INDX | STA_INDY => "STA",
            STX_ZP | STX_ZPY | STX_ABS => "STX",
            STY_ZP | STY_ZPX | STY_ABS => "STY",
            TAX => "TAX",
            TAY => "TAY",
            TSX => "TSX",
            TXA => "TXA",
            TXS => "TXS",
            TYA => "TYA",
        }
    }
}
#[derive(Debug, Clone, Copy)]
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
struct MOS6507<'brand> {
    // TODO: maybe we should be generic over the devices on the bus? 
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
impl<'brand, 'a> NegativeFlag<'brand, 'a> {
    fn reborrow(&mut self) -> NegativeFlag<'brand, '_> {
        NegativeFlag { codegen: self.codegen }
    }
}
struct OverflowFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct ZeroFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
type JumpTarget = (usize, usize);
impl<'brand, 'a> ZeroFlag<'brand, 'a> {
    fn reborrow(&mut self) -> ZeroFlag<'brand, '_> {
        ZeroFlag { codegen: self.codegen }
    }
    fn else_jump(&self, code: &mut GhostToken<'brand>, jump_target: JumpTarget) {
        let codegen = self.codegen.codegen.borrow_mut(code);
        assert_eq!(codegen.block, jump_target.0);
        let target = jump_target.1;
        let delta = target as i64 - codegen.basic_blocks[codegen.block].len() as i64 - 2;
        codegen.basic_blocks[codegen.block].push(Instruction::BNE as u8);
        codegen.basic_blocks[codegen.block].push(i8::try_from(delta).unwrap() as u8);
    }
}
struct CarryFlag<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand> CarryFlag<'brand, '_> {
    fn set_carry(&mut self, code: &mut GhostToken<'brand>) {
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::SEC as u8);
    }
    fn reborrow(&mut self) -> CarryFlag<'brand, '_> {
        CarryFlag { codegen: self.codegen }
    }
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
struct BinaryMathEnabled<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand, 'a> BinaryMathEnabled<'brand, 'a> {
    fn reborrow(&mut self) -> BinaryMathEnabled<'brand, '_> {
        BinaryMathEnabled { codegen: self.codegen }
    }
}
struct Math<'brand, 'a> {
    binary_math: BinaryMathEnabled<'brand, 'a>,
    negative: NegativeFlag<'brand, 'a>,
    zero: ZeroFlag<'brand, 'a>,
}
#[derive(Debug, Clone)]
enum MemIshOperand<'brand, 'a> {
    Implied,
    Accumulator(&'a register::A<'brand, 'a>),
    Immediate(u8),
    Deref(ArrayRef<'brand, 'a, 1>),
    XIndexedDeref(SliceRef<'brand, 'a>, &'a register::X<'brand, 'a>),
    YIndexedDeref(SliceRef<'brand, 'a>, &'a register::Y<'brand, 'a>),
    Indirect(ArrayRef<'brand, 'a, 1>),
    XIndexedIndirectDeref(SliceRef<'brand, 'a>, &'a register::X<'brand, 'a>),
    IndirectYIndexedDeref(SliceRef<'brand, 'a>, &'a register::Y<'brand, 'a>),
    
}
#[derive(Debug)]
enum MemIshOperandMut<'brand, 'a> {
    Implied,
    Accumulator(register::A<'brand, 'a>),
    Immediate(u8),
    Deref(ArrayMut<'brand, 'a, 1>),
    XIndexedDeref(SliceMut<'brand, 'a>, &'a register::X<'brand, 'a>),
    YIndexedDeref(SliceMut<'brand, 'a>, &'a register::Y<'brand, 'a>),
    Indirect(ArrayMut<'brand, 'a, 1>),
    XIndexedIndirectDeref(SliceMut<'brand, 'a>, &'a register::X<'brand, 'a>),
    IndirectYIndexedDeref(SliceMut<'brand, 'a>, &'a register::Y<'brand, 'a>),
    
}
#[derive(Debug, Clone)]
struct ArrayRef<'brand, 'a, const LEN: usize> {
    range_start: u16,
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand, const LEN: usize> ArrayRef<'brand, '_, LEN> {
    fn addr(&self) -> u16 {
        self.range_start
    }
}
#[derive(Debug, Clone)]
struct SliceRef<'brand, 'a> {
    range_start: u16,
    range_end: u16,
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand> SliceRef<'brand, '_> {
    fn addr(&self) -> u16 {
        self.range_start
    }
}
impl<'brand, 'a> Math<'brand, 'a> {
    fn init(code: CodegenCursor<'brand, '_>, decimal_mode: DecimalModeFlag<'brand, 'a>, negative: NegativeFlag<'brand, 'a>, zero: ZeroFlag<'brand, 'a>) -> Self {
        Self {
            binary_math: decimal_mode.clear_decimal_flag(code),
            negative,
            zero
        }
    }
    fn reborrow(&mut self) -> Math<'brand, '_> {
        Math { binary_math: self.binary_math.reborrow(), negative: self.negative.reborrow(), zero: self.zero.reborrow() }
    }
    fn add_with_carry(&mut self, code: &mut GhostToken<'brand>, _: CarryFlag<'brand, '_>, operand: MemIshOperand<'brand, '_>) {
        let codegen = self.binary_math.codegen.codegen.borrow_mut(code);
        match operand {
            MemIshOperand::Immediate(imm) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_IMM as u8);
                codegen.basic_blocks[codegen.block].push(imm);
            }
            MemIshOperand::Deref(var) if var.addr() < 256 => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ZP as u8);
                codegen.basic_blocks[codegen.block].push(var.addr() as u8);
            }
            MemIshOperand::Deref(var) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ABS as u8);
                codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
            }
            // FIXME: This is unchecked bounds at runtime.
            MemIshOperand::XIndexedDeref(var, _) if var.addr() < 256 => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ZPX as u8);
                codegen.basic_blocks[codegen.block].push(var.addr() as u8);
            }
            MemIshOperand::XIndexedDeref(var, _) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ABSX as u8);
                codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
            }
            MemIshOperand::YIndexedDeref(var, _) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_ABSY as u8);
                codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
            }
            MemIshOperand::XIndexedIndirectDeref(var, _) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_INDX as u8);
                codegen.basic_blocks[codegen.block].push(var.addr().try_into().unwrap());
            }
            MemIshOperand::IndirectYIndexedDeref(var, _) => {
                codegen.basic_blocks[codegen.block].push(Instruction::ADC_INDY as u8);
                codegen.basic_blocks[codegen.block].push(var.addr().try_into().unwrap());
            }
            _ => unimplemented!("operaned {operand:?} not supported for ADC"),

        }
        
    }
}
impl<'brand, 'a> DecimalModeFlag<'brand, 'a> {
    fn set_decimal_flag<'c>(self, code: &mut GhostToken<'brand>) {
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::SEC as u8);
    }
    fn clear_decimal_flag<'c>(self, code: &mut GhostToken<'brand>) -> BinaryMathEnabled<'brand, 'c> where 'a: 'c {
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::CLD as u8);
        BinaryMathEnabled { codegen: self.codegen }
    }
    fn reborrow(&mut self) -> DecimalModeFlag<'brand, '_> {
        DecimalModeFlag { codegen: self.codegen }
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
#[derive(Debug)]
struct SliceMut<'brand, 'a> {
    range_start: u16,
    range_end: u16,

    codegen: &'a CodegenCell<'brand>,
}
impl<'brand, 'a> SliceMut<'brand, 'a> {
    fn as_ref(&self) -> SliceRef<'brand, '_> {
        SliceRef { range_start: self.range_start, range_end: self.range_end, codegen: self.codegen }
    }
    fn addr(&self) -> u16 {
        self.as_ref().addr()
    }
    fn len(&self) -> u16 {
        (self.range_end + 1).saturating_sub(self.range_start)
    }
    fn to_array<const LEN: usize>(self) -> ArrayMut<'brand, 'a, LEN> {
        assert!(self.range_start.checked_add(LEN.try_into().unwrap()).unwrap() <= self.range_end);
        ArrayMut { range_start: self.range_start, codegen: self.codegen }
    }
    fn split_at_mut(self, at: u16) -> (SliceMut<'brand, 'a>, SliceMut<'brand, 'a>) {
        assert!(at >= self.range_start);
        assert!(at <= self.range_end);
        (SliceMut { range_start: self.range_start, range_end: at - 1, codegen: self.codegen }, SliceMut { range_start: at, range_end: self.range_end, codegen: self.codegen })
    }
    fn suffix(self, start: u16) -> SliceMut<'brand, 'a> {
        let end = self.range_end;
        self.reslice(start, end)
    }
    fn prefix(self, end: u16) -> SliceMut<'brand, 'a> {
        let start = self.range_start;
        self.reslice(start, end)
    }
    fn reslice(self, start: u16, end: u16) -> SliceMut<'brand, 'a> {
        assert!(start >= self.range_start);
        assert!(end <= self.range_end);
        SliceMut { range_start: start, range_end: end, codegen: self.codegen }
    }
    fn subslice(self, subrange: impl core::ops::RangeBounds<u16>) -> SliceMut<'brand, 'a> {
        let range_start = match subrange.start_bound() {
            core::ops::Bound::Included(start) => *start + self.range_start,
            core::ops::Bound::Excluded(start) => start + 1 + self.range_start,
            core::ops::Bound::Unbounded => self.range_start,
        };
        let range_end = match subrange.end_bound() {
            core::ops::Bound::Included(end) => self.range_start + *end,
            core::ops::Bound::Excluded(end) => self.range_start + *end - 1,
            core::ops::Bound::Unbounded => self.range_end,
        };
        self.reslice(range_start, range_end)
    }
    fn reborrow(&mut self) -> SliceMut<'brand, '_> {
        SliceMut { range_start: self.range_start, range_end: self.range_end, codegen: self.codegen }
    }
}
impl<'brand> MOS6507<'brand> {
    fn as_mut_parts(&mut self) -> ((Status<'brand, '_>, ProgramCounter<'brand, '_>, StackPointer<'brand, '_>, Values<'brand, '_>), SliceMut<'brand, '_>) {
        let status = Status { codegen: &self.codegen };
        let pc = ProgramCounter { codegen: &self.codegen };
        let stack = StackPointer { codegen: &self.codegen };
        let values = Values { codegen: &self.codegen };
        let registers = (status, pc, stack, values);
        (registers, SliceMut { range_start: 0, range_end: u16::MAX, codegen: &self.codegen })
    }
}
struct ProgramCounter<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct StackPointer<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
struct Stack<'brand, 'a> {
    stack: SliceMut<'brand, 'a>,
}
impl<'brand, 'a> StackPointer<'brand, 'a> {
    fn init_stack<'b>(self, code: &mut GhostToken<'brand>, stack: SliceMut<'brand, 'b>, mut x: register::X<'brand, '_>) -> Stack<'brand, 'b> {
        assert!(stack.range_start <= stack.range_end);
        assert!(stack.range_start <= 256);
        x.load(code, stack.range_end.min(256) as u8);
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::TXS as u8);
        Stack { stack }
    }
    fn reborrow(&mut self) -> StackPointer<'brand, '_> {
        StackPointer { codegen: self.codegen }
    }
}
struct Values<'brand, 'a> {
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand, 'a> Values<'brand, 'a> {
    fn reborrow(&mut self) -> Values<'brand, '_> {
        Values { codegen: self.codegen }
    }
}
trait AsCodegen<'cpu> {
    fn as_codegen(&self) -> &CodegenCell<'cpu>;
}
mod register {
    impl<'brand, 'a> Values<'brand, 'a> {
        pub fn split(self) -> (register::A<'brand, 'a>, register::X<'brand, 'a>, register::Y<'brand, 'a>) {
            (register::A { codegen: self.codegen }, register::X { codegen: self.codegen }, register::Y { codegen: self.codegen })
        }
    }
    use super::*;
    impl core::fmt::Debug for A<'_, '_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("A").finish()
        }
    }
    impl core::fmt::Debug for X<'_, '_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("X").finish()
        }
    }
    impl core::fmt::Debug for Y<'_, '_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_struct("Y").finish()
        }
    }
    pub struct A<'cpu, 'a> {
        codegen: &'a CodegenCell<'cpu>,
    }
    impl<'cpu> AsRef<CodegenCell<'cpu>> for A<'cpu, '_> {
        fn as_ref(&self) -> &CodegenCell<'cpu> {
            self.codegen
        }
    }
    impl<'cpu, 'a> A<'cpu, 'a> {
        pub fn load(&mut self, code: &mut GhostToken<'cpu>, operand: MemIshOperand<'cpu, '_>) {
            let codegen = self.codegen.codegen.borrow_mut(code);
            match operand {
                MemIshOperand::Immediate(imm) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_IMM as u8);
                    codegen.basic_blocks[codegen.block].push(imm);
                }
                MemIshOperand::Deref(var) if var.addr() < 256 => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_ZP as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                }
                MemIshOperand::Deref(var) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_ABS as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperand::XIndexedDeref(var, _) if var.addr() < 256 => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_ZPX as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                }
                MemIshOperand::XIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_ABSX as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperand::YIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_ABSY as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperand::XIndexedIndirectDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_INDX as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr().try_into().unwrap());
                }
                MemIshOperand::IndirectYIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDA_INDY as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr().try_into().unwrap());
                }

                unknown => unimplemented!("operand {unknown:?} not supported for LDA"),
            }
        }
        pub fn store(&self, code: &mut GhostToken<'cpu>, operand: MemIshOperandMut<'cpu, '_>) {
            let codegen = self.codegen.codegen.borrow_mut(code);
            match operand {
                MemIshOperandMut::Deref(var) if var.addr() < 256 => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_ZP as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                }
                MemIshOperandMut::Deref(var) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_ABS as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperandMut::XIndexedDeref(var, _) if var.addr() < 256 => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_ZPX as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                }
                MemIshOperandMut::XIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_ABSX as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperandMut::YIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_ABSY as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperandMut::XIndexedIndirectDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_INDX as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr().try_into().unwrap());
                }
                MemIshOperandMut::IndirectYIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::STA_INDY as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr().try_into().unwrap());
                }
                _ => unimplemented!("operand {operand:?} not supported for LDA"),
    
            
            }
        }
    }
    pub struct X<'cpu, 'a> {
        codegen: &'a CodegenCell<'cpu>,
    }
    impl<'cpu, 'a> X<'cpu, 'a> {
        pub fn decrement(&mut self, code: &mut GhostToken<'cpu>, math: Math<'cpu, '_>) {
            let codegen = self.codegen.codegen.borrow_mut(code);
            codegen.basic_blocks[codegen.block].push(Instruction::DEX as u8);
        }
        pub fn load(&mut self, code: &mut GhostToken<'cpu>, value: u8) {
            let codegen = self.codegen.codegen.borrow_mut(code);
            codegen.basic_blocks[codegen.block].push(Instruction::LDX_IMM as u8);
            codegen.basic_blocks[codegen.block].push(value);
        }
        pub fn store(&self, code: &mut GhostToken<'cpu>, operand: MemIshOperand<'cpu, '_>) {
            let codegen = self.codegen.codegen.borrow_mut(code);
            match operand {
                MemIshOperand::Immediate(imm) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDX_IMM as u8);
                    codegen.basic_blocks[codegen.block].push(imm);
                }
                MemIshOperand::Deref(var) if var.addr() < 256 => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDX_ZP as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                }
                MemIshOperand::Deref(var) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDX_ABS as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                MemIshOperand::YIndexedDeref(var, _) => {
                    codegen.basic_blocks[codegen.block].push(Instruction::LDX_ABSY as u8);
                    codegen.basic_blocks[codegen.block].push(var.addr() as u8);
                    codegen.basic_blocks[codegen.block].push((var.addr() >> 8) as u8);
                }
                _ => unimplemented!("operand {operand:?} not supported for LDX"),
    
            
            }
        }
    }
    pub struct Y<'cpu, 'a> {
        codegen: &'a CodegenCell<'cpu>,
    
    }
}
impl<'brand, 'a> Values<'brand, 'a> {
    fn store_constant(&mut self, code: &mut GhostToken<'brand>, place: ArrayMut<'brand, '_, 1>, value: u8) {
        let codegen = self.codegen.codegen.borrow_mut(code);
        codegen.basic_blocks[codegen.block].push(Instruction::LDA_IMM as u8);
        codegen.basic_blocks[codegen.block].push(value);
        codegen.basic_blocks[codegen.block].push(Instruction::STA_ABS as u8);
        codegen.basic_blocks[codegen.block].push(place.range_start as u8);
        codegen.basic_blocks[codegen.block].push((place.range_start >> 8) as u8);
    }
}
#[derive(Default)]
struct CodegenCell<'brand> {
    codegen: GhostCell<'brand, Codegen>,
}
impl core::fmt::Debug for CodegenCell<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CodegenCell").finish()
    }
}
struct Codegen {
    basic_blocks: Vec<Vec<u8>>,
    block: usize,
}
#[derive(Debug)]
struct ArrayMut<'brand, 'a, const LEN: usize> {
    range_start: u16,
    
    codegen: &'a CodegenCell<'brand>,
}
impl<'brand, 'a, const LEN: usize> ArrayMut<'brand, 'a, LEN> {
    fn addr(&self) -> u16 {
        self.as_ref().addr()
    }
    fn as_ref(&self) -> ArrayRef<'brand, '_, LEN> {
        ArrayRef { range_start: self.range_start, codegen: self.codegen }
    }

}
#[derive(Debug)]
struct R6532<'brand, 'a> {
    // The location of the R6532 in the bus
    location: u16,
    mem: SliceMut<'brand, 'a>,
}
impl<'brand, 'a> R6532<'brand, 'a> {
    fn reborrow(&mut self) -> R6532<'brand, '_> {
        R6532 { location: self.location, mem: self.mem.reborrow() }
    }
    fn split(self, at: u16) -> (R6532<'brand, 'a>, R6532<'brand, 'a>) {
        let (left, right) = self.mem.split_at_mut(at);
        (R6532 { location: self.location, mem: left }, R6532 { location: self.location, mem: right })
    }
    fn mem(self) -> SliceMut<'brand, 'a> {
        let start = self.mem.range_start.max(self.location);
        let end = self.mem.range_end.min(self.location + 128);
        self.mem.reslice(start, end)
    }
    fn at<const LEN: usize>(self, pos: u16) -> ArrayMut<'brand, 'a, LEN> {
        self.mem.suffix(self.location + pos).to_array()
    }
    fn DRA(self) -> ArrayMut<'brand, 'a, 1> {
        self.at(128 + 0)
    }
    fn DDRA(self) -> ArrayMut<'brand, 'a, 1> {
        self.at(128 + 1)
    }
    fn DRB(self) -> ArrayMut<'brand, 'a, 1> {
        self.at(128 + 2)
    }
    fn DDRB(self) -> ArrayMut<'brand, 'a, 1> {
        self.at(128 + 3)
    }
    fn timer(self, enable_interrupt: bool, eights_exponent: u8) -> ArrayMut<'brand, 'a, 1> {
        self.at(128 + 0x14 + (enable_interrupt as u16 * 8) + eights_exponent as u16)
    }
}
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

type CodegenCursor<'cpu, 'a> = &'a mut GhostToken<'cpu>;
trait CodegenCursorExt<'cpu, 'a> {
    fn get_jump_target(self, operand: impl AsRef<CodegenCell<'cpu>>) -> (usize, usize);
}

impl<'cpu, 'a> CodegenCursorExt<'cpu, 'a> for CodegenCursor<'cpu, 'a> {
    fn get_jump_target(self, operand: impl AsRef<CodegenCell<'cpu>>) -> (usize, usize) {
        let codegen = operand.as_ref().codegen.borrow_mut(self);
        let target = codegen.basic_blocks[codegen.block].len();
        (codegen.block, target)
    }
}
fn fill<'cpu>(code: CodegenCursor<'cpu, '_>, mut slice: SliceMut<'cpu, '_>, values: Values<'cpu, '_>, mut math: Math<'cpu, '_>) {
    let (mut a, mut x,y) = values.split();
    x.load(code, (slice.len() - 1).try_into().unwrap());
    a.load(code, MemIshOperand::Immediate(0));
    let clearzp = code.get_jump_target(&a);
    a.store(code, MemIshOperandMut::XIndexedDeref(slice.reborrow(), &x));
    x.decrement(code, math.reborrow());
    math.zero.else_jump(code, clearzp);
    a.store(code, MemIshOperandMut::XIndexedDeref(slice.reborrow(), &x));

}
impl<'cpu> AsRef<CodegenCell<'cpu>> for SliceMut<'cpu, '_> {
    fn as_ref(&self) -> &CodegenCell<'cpu> {
        self.codegen
    }
}
fn increment<'cpu>(code: CodegenCursor<'cpu, '_>, operand: MemIshOperandMut<'cpu, '_>) {

}
fn reset<'a>(code: &mut GhostToken<'a>, cpu: &mut MOS6507<'a>) {
    let (registers, bus) = cpu.as_mut_parts();
    let (mut status, _, sp, mut values) = registers;
    let (decimal_mode, n, _, z, _, mut interrupt_disable) = status.as_mut_parts();
    let math = Math::init(code, decimal_mode, n, z);
    interrupt_disable.set_interrupt_disable(code);
    
    let (mem, mut io) = R6532 { location: 0, mem: bus.subslice(0..0x100) }.split(0x80);
    let mut mem = mem.mem();
    
    let _stack = sp.init_stack(code, mem.reborrow(), values.reborrow().split().1);// cpu.set_stack_pointer(heap.(end-1))
    fill(code, mem.reborrow(), values.reborrow(), math);
    // values.store_constant(code, io.reborrow().DDRA(), 0x00);
    values.store_constant(code, io.reborrow().DDRB(), 0xFF);

    let main = code.get_jump_target(mem);
    increment(code, MemIshOperandMut::Deref(io.reborrow().DRB()));

    // loop {
    //     cpu.inc(riot.DRB)
    //     cpu.bit(riot.DRA) // sets cpu.negative to bit 7
    //     eights_exponent = if !cpu.negative: 3 else : 2
    //     riot.timer(.interrupt_off, eights_exponent) := 244
        
    //     cpu.while_(|| riot.timer(.interrupt_off).* != 0, || {})
    // }
    // }
}
enum AsmOperand {
    Accumulator,
    Deref(u16),
    XIndexedDeref(u16),
    YIndexedDeref(u16),
    Immediate(u8),
    Implied,
    Indirect(u16),
    XIndexedIndirectDeref(u8),
    IndirectYIndexedDeref(u8),
    Relative(i8),
}
impl core::fmt::Debug for AsmOperand {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            AsmOperand::Accumulator => write!(f, "A"),
            AsmOperand::Deref(addr) => write!(f, "({:#X})", addr),
            AsmOperand::XIndexedDeref(addr) => write!(f, "({:#X}, X)", addr),
            AsmOperand::YIndexedDeref(addr) => write!(f, "({:#X}), Y", addr),
            AsmOperand::Immediate(imm) => write!(f, "#{:#X}", imm),
            AsmOperand::Implied => write!(f, ""),
            AsmOperand::Indirect(addr) => write!(f, "({:#X})", addr),
            AsmOperand::XIndexedIndirectDeref(addr) => write!(f, "({:#X}, X)", addr),
            AsmOperand::IndirectYIndexedDeref(addr) => write!(f, "({:#X}), Y", addr),
            AsmOperand::Relative(offset) => write!(f, "{}{:#X}", if *offset < 0 { "-" } else { ""}, offset.abs()),
        }
    }
}
fn decode_block(insrs: &[u8]) -> impl Iterator<Item = (Instruction, AsmOperand)> + '_ {
    let mut insrs = insrs.iter();
    core::iter::from_fn(move || insrs.next().map(|ins| {
        match ins {
            0x69 => (Instruction::ADC_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0x65 => (Instruction::ADC_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x75 => (Instruction::ADC_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x6D => (Instruction::ADC_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x7D => (Instruction::ADC_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x79 => (Instruction::ADC_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x61 => (Instruction::ADC_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0x71 => (Instruction::ADC_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0x29 => (Instruction::AND_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0x25 => (Instruction::AND_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x35 => (Instruction::AND_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x2D => (Instruction::AND_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x3D => (Instruction::AND_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x39 => (Instruction::AND_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x21 => (Instruction::AND_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0x31 => (Instruction::AND_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0x0A => (Instruction::ASL_ACC, AsmOperand::Accumulator),
            0x06 => (Instruction::ASL_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x16 => (Instruction::ASL_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x0E => (Instruction::ASL_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x1E => (Instruction::ASL_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x90 => (Instruction::BCC, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0xB0 => (Instruction::BCS, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0xF0 => (Instruction::BEQ, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0x24 => (Instruction::BIT_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x2C => (Instruction::BIT_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x30 => (Instruction::BMI, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0xD0 => (Instruction::BNE, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0x10 => (Instruction::BPL, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0x00 => (Instruction::BRK, AsmOperand::Implied),
            0x50 => (Instruction::BVC, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0x70 => (Instruction::BVS, AsmOperand::Relative(*insrs.next().unwrap() as i8)),
            0x18 => (Instruction::CLC, AsmOperand::Implied),
            0xD8 => (Instruction::CLD, AsmOperand::Implied),
            0x58 => (Instruction::CLI, AsmOperand::Implied),
            0xB8 => (Instruction::CLV, AsmOperand::Implied),
            0xC9 => (Instruction::CMP_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xC5 => (Instruction::CMP_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xD5 => (Instruction::CMP_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0xCD => (Instruction::CMP_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xDD => (Instruction::CMP_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xD9 => (Instruction::CMP_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xC1 => (Instruction::CMP_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0xD1 => (Instruction::CMP_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0xE0 => (Instruction::CPX_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xE4 => (Instruction::CPX_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xEC => (Instruction::CPX_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xC0 => (Instruction::CPY_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xC4 => (Instruction::CPY_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xCC => (Instruction::CPY_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xC6 => (Instruction::DEC_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xD6 => (Instruction::DEC_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0xCE => (Instruction::DEC_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xDE => (Instruction::DEC_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xCA => (Instruction::DEX, AsmOperand::Implied),
            0x88 => (Instruction::DEY, AsmOperand::Implied),
            0x49 => (Instruction::EOR_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0x45 => (Instruction::EOR_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x55 => (Instruction::EOR_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x4D => (Instruction::EOR_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x5D => (Instruction::EOR_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x59 => (Instruction::EOR_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x41 => (Instruction::EOR_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0x51 => (Instruction::EOR_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0xE6 => (Instruction::INC_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xF6 => (Instruction::INC_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0xEE => (Instruction::INC_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xFE => (Instruction::INC_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xE8 => (Instruction::INX, AsmOperand::Implied),
            0xC8 => (Instruction::INY, AsmOperand::Implied),
            0x4C => (Instruction::JMP_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x6C => (Instruction::JMP_IND, AsmOperand::Indirect(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x20 => (Instruction::JSR, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xA9 => (Instruction::LDA_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xA5 => (Instruction::LDA_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xB5 => (Instruction::LDA_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0xAD => (Instruction::LDA_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xBD => (Instruction::LDA_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xB9 => (Instruction::LDA_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xA1 => (Instruction::LDA_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0xB1 => (Instruction::LDA_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0xA2 => (Instruction::LDX_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xA6 => (Instruction::LDX_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xB6 => (Instruction::LDX_ZPY, AsmOperand::YIndexedDeref(*insrs.next().unwrap() as u16)),
            0xAE => (Instruction::LDX_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xBE => (Instruction::LDX_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xA0 => (Instruction::LDY_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xA4 => (Instruction::LDY_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xB4 => (Instruction::LDY_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0xAC => (Instruction::LDY_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xBC => (Instruction::LDY_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x4A => (Instruction::LSR_ACC, AsmOperand::Accumulator),
            0x46 => (Instruction::LSR_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x56 => (Instruction::LSR_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x4E => (Instruction::LSR_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x5E => (Instruction::LSR_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xEA => (Instruction::NOP, AsmOperand::Implied),
            0x09 => (Instruction::ORA_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0x05 => (Instruction::ORA_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x15 => (Instruction::ORA_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x0D => (Instruction::ORA_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x1D => (Instruction::ORA_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x19 => (Instruction::ORA_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x01 => (Instruction::ORA_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0x11 => (Instruction::ORA_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0x48 => (Instruction::PHA, AsmOperand::Implied),
            0x08 => (Instruction::PHP, AsmOperand::Implied),
            0x68 => (Instruction::PLA, AsmOperand::Implied),
            0x28 => (Instruction::PLP, AsmOperand::Implied),
            0x2A => (Instruction::ROL_ACC, AsmOperand::Accumulator),
            0x26 => (Instruction::ROL_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x36 => (Instruction::ROL_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x2E => (Instruction::ROL_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x3E => (Instruction::ROL_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x6A => (Instruction::ROR_ACC, AsmOperand::Accumulator),
            0x66 => (Instruction::ROR_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x76 => (Instruction::ROR_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x6E => (Instruction::ROR_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x7E => (Instruction::ROR_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x40 => (Instruction::RTI, AsmOperand::Implied),
            0x60 => (Instruction::RTS, AsmOperand::Implied),
            0xE9 => (Instruction::SBC_IMM, AsmOperand::Immediate(*insrs.next().unwrap())),
            0xE5 => (Instruction::SBC_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0xF5 => (Instruction::SBC_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0xED => (Instruction::SBC_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xFD => (Instruction::SBC_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xF9 => (Instruction::SBC_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xE1 => (Instruction::SBC_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0xF1 => (Instruction::SBC_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0x38 => (Instruction::SEC, AsmOperand::Implied),
            0xF8 => (Instruction::SED, AsmOperand::Implied),
            0x78 => (Instruction::SEI, AsmOperand::Implied),
            0x85 => (Instruction::STA_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x95 => (Instruction::STA_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x8D => (Instruction::STA_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x9D => (Instruction::STA_ABSX, AsmOperand::XIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x99 => (Instruction::STA_ABSY, AsmOperand::YIndexedDeref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x81 => (Instruction::STA_INDX, AsmOperand::XIndexedIndirectDeref(*insrs.next().unwrap())),
            0x91 => (Instruction::STA_INDY, AsmOperand::IndirectYIndexedDeref(*insrs.next().unwrap())),
            0x86 => (Instruction::STX_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x96 => (Instruction::STX_ZPY, AsmOperand::YIndexedDeref(*insrs.next().unwrap() as u16)),
            0x8E => (Instruction::STX_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0x84 => (Instruction::STY_ZP, AsmOperand::Deref(*insrs.next().unwrap() as u16)),
            0x94 => (Instruction::STY_ZPX, AsmOperand::XIndexedDeref(*insrs.next().unwrap() as u16)),
            0x8C => (Instruction::STY_ABS, AsmOperand::Deref(u16::from_le_bytes([*insrs.next().unwrap(), *insrs.next().unwrap()]))),
            0xAA => (Instruction::TAX, AsmOperand::Implied),
            0xA8 => (Instruction::TAY, AsmOperand::Implied),
            0xBA => (Instruction::TSX, AsmOperand::Implied),
            0x8A => (Instruction::TXA, AsmOperand::Implied),
            0x9A => (Instruction::TXS, AsmOperand::Implied),
            0x98 => (Instruction::TYA, AsmOperand::Implied),

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
            println!("  {} {op:?}", insr.mnemonic());
        }
        println!("");
    }
}