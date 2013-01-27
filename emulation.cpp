/* macloader - m68k Mac executable loader
 *
 * (c) 2012 by Johannes Schickel <lordhoto at gmail dot com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include "emulation.h"
#include "mac-memory.h"
#include "traps.h"

#include <cstdio>
#include <cstring>

#define SIGN_EXTEND_8(x)  (int32_t)( (int8_t)((x) & 0x00FF))
#define SIGN_EXTEND_16(x) (int32_t)((int16_t)((x) & 0xFFFF))

#define GET_SIGN_BIT(size) (1 << (((size) * 8) - 1))

#define FLAG_SET_CARRY       registers.ccr |=   1
#define FLAG_CLEAR_CARRY     registers.ccr &= ~ 1
#define FLAG_QUERY_CARRY    (registers.ccr &    1)

#define FLAG_SET_OVERFLOW    registers.ccr |=   2
#define FLAG_CLEAR_OVERFLOW  registers.ccr &= ~ 2
#define FLAG_QUERY_OVERFLOW (registers.ccr &    2)

#define FLAG_SET_ZERO        registers.ccr |=   4
#define FLAG_CLEAR_ZERO      registers.ccr &= ~ 4
#define FLAG_QUERY_ZERO     (registers.ccr &    4)
#define FLAG_APPLY_ZERO(x) \
	do { \
		if ((x)) { \
			FLAG_CLEAR_ZERO; \
		} else { \
			FLAG_SET_ZERO; \
		} \
	} while (0)

#define FLAG_SET_NEG        registers.ccr |=   8
#define FLAG_CLEAR_NEG      registers.ccr &= ~ 8
#define FLAG_QUERY_NEG     (registers.ccr &    8)
#define FLAG_APPLY_NEG(x, size) \
	do { \
		if ((x) & GET_SIGN_BIT((size))) { \
			FLAG_SET_NEG; \
		} else { \
			FLAG_CLEAR_NEG; \
		} \
	} while (0)

#define FLAG_SET_EXTEND      registers.ccr |=  16
#define FLAG_CLEAR_EXTEND    registers.ccr &= ~16
#define FLAG_QUERY_EXTEND   (registers.ccr &   16)
#define FLAG_GET_EXTEND     ((registers.ccr >> 5) & 1)

#define PUSH_VALUE(value, size) \
	writeParam(getParameter(4, 7, size), value, 4, 7, size)

#define POP_VALUE(size, signExtend) \
	readParam(getParameter(3, 7, size), 3, 7, size, signExtend)

namespace Emulation {
RegisterSet registers;
bool stopEmulation;
uint32_t instructionStart;

void executeJumpTableEntry(const uint32_t offset, const bool firstEntry) {
	// The first 16 bits in an jump table entry is the function offset,
	// this is only relevant to the loader. We need to start execution
	// on the third byte.

	const uint16_t instruction = Memory::fetchUint16(offset + 2);
	// Only start execution, when the segment is not yet loaded.
	if (instruction == 0x3F3C || instruction == 0xA9F0) {
		registers.a[5] = Memory::a5BasePointer;
		execute(offset + 2);

		// If this is the first entry also run the code in it
		if (firstEntry) {
			execute(offset + 2);
		}
	// Some jump tables have _LoadSeg at the first offset
	} else if (Memory::fetchUint16(offset) == 0xA9F0) {
		registers.a[5] = Memory::a5BasePointer;
		execute(offset);
	}
}

namespace {
bool evalCondition(const uint8_t condition) {
	switch (condition) {
	// True
	case 0:
		return true;

	// False
	case 1:
		return false;

	// HI (high)
	case 2:
		return (!FLAG_QUERY_CARRY) && (!FLAG_QUERY_ZERO);

	// LS (low or same)
	case 3:
		return FLAG_QUERY_CARRY || FLAG_QUERY_ZERO;

	// CC (carry clear)
	case 4:
		return !FLAG_QUERY_CARRY;

	// CS (carry set)
	case 5:
		return FLAG_QUERY_CARRY;

	// NE (not equal)
	case 6:
		return !FLAG_QUERY_ZERO;

	// EQ (equal)
	case 7:
		return FLAG_QUERY_ZERO;

	// VC (overflow clear)
	case 8:
		return !FLAG_QUERY_OVERFLOW;

	// VS (overflow set)
	case 9:
		return FLAG_QUERY_OVERFLOW;

	// PL (plus)
	case 10:
		return !FLAG_QUERY_NEG;

	// MI (minus)
	case 11:
		return FLAG_QUERY_NEG;

	// GE (greater or equal)
	case 12:
		return (FLAG_QUERY_NEG && FLAG_QUERY_OVERFLOW) || ((!FLAG_QUERY_NEG) && (!FLAG_QUERY_OVERFLOW));

	// LT (less than)
	case 13:
		return (FLAG_QUERY_NEG && (!FLAG_QUERY_OVERFLOW)) || ((!FLAG_QUERY_NEG) && FLAG_QUERY_OVERFLOW);

	// GT (greater than)
	case 14:
		return ((FLAG_QUERY_NEG && FLAG_QUERY_OVERFLOW) || ((!FLAG_QUERY_NEG) && (!FLAG_QUERY_OVERFLOW))) && (!FLAG_QUERY_ZERO);

	// LE (less or equal)
	case 15:
		return ((FLAG_QUERY_NEG && (!FLAG_QUERY_OVERFLOW)) || ((!FLAG_QUERY_NEG) && FLAG_QUERY_OVERFLOW)) || FLAG_QUERY_ZERO;

	default:
		// XXX warning?
		return false;
	}
}

uint32_t fetchOperand(const uint8_t size) {
	uint32_t operand;

	switch (size) {
	case 1:
		operand = Memory::fetchUint16(registers.pc) & 0xFF;
		registers.pc += 2;
		break;

	case 2:
		operand = Memory::fetchUint16(registers.pc);
		registers.pc += 2;
		break;

	case 4:
		operand = Memory::fetchUint32(registers.pc);
		registers.pc += 4;
		break;

	default:
		// XXX error?
		break;
	}

	return operand;
}

uint32_t getExtensionWordValue() {
	const uint16_t briefExtensionWord = Memory::fetchUint16(registers.pc);
	registers.pc += 2;

	uint32_t value = SIGN_EXTEND_8(briefExtensionWord & 0xFF);
	const uint8_t scale = (1 << ((briefExtensionWord >> 9) & 0x3));

	const uint8_t addReg = (briefExtensionWord >> 12) & 0x7;
	uint32_t regValue = 0;
	if (briefExtensionWord & 0x8000) {
		regValue = registers.a[addReg];
	} else {
		regValue = registers.d[addReg];
	}

	if (!(briefExtensionWord & 0x0800)) {
		regValue = SIGN_EXTEND_16(regValue & 0xFFFF);
	}

	return value + regValue * scale;
}

uint32_t getParameter(const uint8_t mode, const uint8_t reg, const uint8_t size) {
	switch (mode & 0x7) {
	// data register
	case 0:
		switch (size) {
		case 1:
			return registers.d[reg] & 0xFF;

		case 2:
			return registers.d[reg] & 0xFFFF;

		case 4:
			return registers.d[reg];

		default:
			// XXX warning
			return registers.d[reg];
		}

	// address register
	case 1:
		return registers.a[reg];

	case 7:
		// absolute short
		if (reg == 0) {
			int32_t value = (int16_t)Memory::fetchUint16(registers.pc);
			registers.pc += 2;
			return value;
		// absolute long
		} else if (reg == 1) {
			uint32_t value = Memory::fetchUint32(registers.pc);
			registers.pc += 4;
			return value;
		// immediate
		} else if (reg == 4) {
			return fetchOperand(size);
		}
		// fall through
	default:
		break;
	}

	uint32_t address;

	switch (mode) {
	// address
	case 2:
		address = registers.a[reg];
		break;

	// address with postincrement
	case 3:
		address = registers.a[reg];
		registers.a[reg] += size;
		break;

	// address with predecrement
	case 4:
		registers.a[reg] -= size;
		address = registers.a[reg];
		break;

	// address with displacement
	case 5:
		address = registers.a[reg];
		address += (int16_t)Memory::fetchUint16(registers.pc);
		registers.pc += 2;
		break;

	// address with index
	case 6:
		address = registers.a[reg] + getExtensionWordValue();
		break;

	// pc counter related
	case 7:
		address = registers.pc;
		// pc with displacement
		if (reg == 2) {
			address += (int16_t)Memory::fetchUint16(registers.pc);
			registers.pc += 2;
		// pc with index
		} else if (reg == 3) {
			address += getExtensionWordValue();
		}
		break;

	default:
		// shouldn't happen
		break;
	}

	return address;
}

inline bool isMemoryAccess(const uint8_t mode, const uint8_t reg) {
	return (mode >= 2 && mode <= 6) || (mode == 7 && reg != 4);
}

uint32_t readParam(const uint32_t value, const uint8_t mode, const uint8_t reg, const uint8_t size, const bool signExtend) {
	uint32_t param;

	if (isMemoryAccess(mode, reg)) {
		switch (size) {
		case 1:
			param = Memory::fetchByte(value);
			break;

		case 2:
			param = Memory::fetchUint16(value);
			break;

		case 4:
			param = Memory::fetchUint32(value);
			break;
		}
	} else {
		param = value;
	}

	if (signExtend) {
		if (size == 1) {
			param = SIGN_EXTEND_8(param);
		} else if (size == 2) {
			param = SIGN_EXTEND_16(param);
		}
	}

	return param;
}

void writeParam(const uint32_t dst, const uint32_t value, const uint8_t mode, const uint8_t reg, const uint8_t size) {
	if (isMemoryAccess(mode, reg)) {
		switch (size) {
		case 1:
			Memory::writeByte(dst, value);
			break;

		case 2:
			Memory::writeUint16(dst, value);
			break;

		case 4:
			Memory::writeUint32(dst, value);
			break;
		}
	} else {
		// data register
		if (mode == 0) {
			switch (size) {
			case 1:
				registers.d[reg] &= ~0xFF;
				registers.d[reg] |= (value & 0xFF);
				break;

			case 2:
				registers.d[reg] &= ~0xFFFF;
				registers.d[reg] |= (value & 0xFFFF);
				break;

			default:
				registers.d[reg] = value;
			}
		// address register
		} else if (mode == 1) {
			registers.a[reg] = value;
		}
	}
}

void executeMove(const uint16_t instruction) {
	// Get the size
	uint8_t size    = (instruction >> 12) & 0x3;
	switch (size) {
	case 2:
		size = 4;
		break;

	case 3:
		size = 2;
		break;

	default:
		break;
	}
	// Get destination information
	const uint8_t dstReg  = (instruction >>  9) & 0x7;
	const uint8_t dstMode = (instruction >>  6) & 0x7;
	// Get source information
	const uint8_t srcMode = (instruction >>  3) & 0x7;
	const uint8_t srcReg  = (instruction >>  0) & 0x7;

	// Process src Parameter
	const uint32_t srcParam = getParameter(srcMode, srcReg, size);
	const uint32_t srcValue = readParam(srcParam, srcMode, srcReg, size, (dstMode == 1));

	// Process dst Parameter
	const uint32_t dstParam = getParameter(dstMode, dstReg, size);

	writeParam(dstParam, srcValue, dstMode, dstReg, size);

	if (dstMode != 1) {
		// Clear C and V
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;

		// Set Z if needed
		FLAG_APPLY_ZERO(srcValue);

		// Set N if needed
		FLAG_APPLY_NEG(srcValue, size);
	}
}

bool executeTrap(const uint16_t trap) {
	const Memory::TrapAddressMap *trapAddr = Memory::getTrapAddress(trap);
	if (trapAddr != nullptr) {
		PUSH_VALUE(registers.pc, 4);
		registers.pc = trapAddr->curAddress;
		return true;
	} else {
		return false;
	}
}

namespace {
uint32_t subtract(const uint32_t first, const uint32_t second, const uint8_t size, const bool useXBit, const bool processFlags, const bool ignoreXFlag) {
	uint32_t endResult;

	#define doSubtraction(type) \
		do { \
			const type result = (type)first - (type)second - (type)(useXBit ? FLAG_GET_EXTEND : 0); \
			endResult = result; \
		} while (0)
	switch (size) {
	case 1:
		doSubtraction(uint8_t);
		break;

	case 2:
		doSubtraction(uint16_t);
		break;

	case 4:
		doSubtraction(uint32_t);
		break;

	default:
		// XXX error
		break;
	}
	#undef doSubtraction

	// Handle flags, if there is need for it.
	if (processFlags) {
		// Set carry/extend if applicable
		if (first < second) {
			FLAG_SET_CARRY;
			if (!ignoreXFlag) {
				FLAG_SET_EXTEND;
			}
		} else {
			FLAG_CLEAR_CARRY;
			if (!ignoreXFlag) {
				FLAG_CLEAR_EXTEND;
			}
		}

		// Set overflow if applicable
		if (((first ^ second) & (first ^ endResult)) & GET_SIGN_BIT(size)) {
			FLAG_SET_OVERFLOW;
		} else {
			FLAG_CLEAR_OVERFLOW;
		}

		// Set zero if applicable
		FLAG_APPLY_ZERO(endResult);

		// Set neg if applicable
		FLAG_APPLY_NEG(endResult, size);
	}

	return endResult;
}

uint32_t add(const uint32_t first, const uint32_t second, const uint8_t size, const bool useXBit, const bool processFlags, const bool ignoreXFlag) {
	uint32_t endResult;

	#define doAddition(type) \
		do { \
			const type result = (type)first + (type)second + (type)(useXBit ? FLAG_GET_EXTEND : 0); \
			endResult = result; \
		} while (0)
	switch (size) {
	case 1:
		doAddition(uint8_t);
		break;

	case 2:
		doAddition(uint16_t);
		break;

	case 4:
		doAddition(uint32_t);
		break;

	default:
		// XXX error
		break;
	}
	#undef doAddition

	// Handle flags, if there is need for it.
	if (processFlags) {
		// Set carry/extend if applicable
		if (endResult < second) {
			FLAG_SET_CARRY;
			if (!ignoreXFlag) {
				FLAG_SET_EXTEND;
			}
		} else {
			FLAG_CLEAR_CARRY;
			if (!ignoreXFlag) {
				FLAG_CLEAR_EXTEND;
			}
		}

		// Set overflow if applicable
		if (((first ^ second ^ GET_SIGN_BIT(size)) & (endResult ^ second)) & GET_SIGN_BIT(size)) {
			FLAG_SET_OVERFLOW;
		} else {
			FLAG_CLEAR_OVERFLOW;
		}

		// Set zero if applicable
		FLAG_APPLY_ZERO(endResult);

		// Set neg if applicable
		FLAG_APPLY_NEG(endResult, size);
	}

	return endResult;
}
} // End of anonymous namespace

bool executeMisc(const uint16_t instruction) {
	// JMP/JSR
	if ((instruction & 0x0F80) == 0x0E80) {
		// Address parameter
		const uint8_t mode = (instruction >> 3) & 0x7;
		const uint8_t reg  = (instruction >> 0) & 0x7;

		const uint32_t destination = getParameter(mode, reg, 4);
		// Save PC register for JSR
		if (!(instruction & 0x40)) {
			PUSH_VALUE(registers.pc, 4);
		}
		registers.pc = destination;

		if (Memory::isInJumpTable(destination)) {
			std::printf("INFO: Jump at 0x%08X jumps into jump table 0x%08X\n", instructionStart, destination);
			stopEmulation = true;
		}
	// CLR
	} else if ((instruction & 0x0F00) == 0x0200) {
		// Size
		const uint8_t size = (instruction >> 6) & 0x3;

		// Destination
		const uint8_t mode = (instruction >> 3) & 0x7;
		const uint8_t reg  = (instruction >> 0) & 0x7;

		// Get the destination
		const uint32_t dest = getParameter(mode, reg, (1 << size));

		// Clear the operand
		writeParam(dest, 0, mode, reg, (1 << size));

		// Set the flags
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_SET_ZERO;
		FLAG_CLEAR_NEG;
	// LEA
	} else if ((instruction & 0x01C0) == 0x01C0) {
		// Source
		const uint8_t srcMode = (instruction >> 3) & 0x7;
		const uint8_t srcReg  = (instruction >> 0) & 0x7;

		// Destination register
		const uint8_t dstReg  = (instruction >> 9) & 0x7;

		// Write the result.
		registers.a[dstReg] = getParameter(srcMode, srcReg, 4);
	// RTS
	} else if ((instruction & 0x0FFF) == 0x0E75) {
		// Load the return address from the stack
		registers.pc = POP_VALUE(4, false);

		if (Memory::isInJumpTable(registers.pc)) {
			std::printf("INFO: RTS at 0x%08X returns into jump table 0x%08X\n", instructionStart, registers.pc);
			stopEmulation = true;
		}
	// EXT
	} else if ((instruction & 0x0EB8) == 0x0880) {
		const bool toLong      = (instruction >> 6) & 1;
		const bool specialByte = (instruction >> 8) & 1;
		const uint8_t reg      = (instruction >> 0) & 0x7;

		const uint8_t srcSize = (specialByte ? 1 : (toLong ? 2 : 1));
		const uint8_t dstSize = toLong ? 4 : 2;

		// Read source
		const uint32_t srcValue = getParameter(0, reg, srcSize);

		// Sign extend
		const int32_t dstValue = (srcSize == 2) ? SIGN_EXTEND_16(srcValue) : SIGN_EXTEND_8(srcValue);

		// Write result
		writeParam(0, dstValue, 0, reg, dstSize);

		// Set the flags accordingly
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_APPLY_ZERO(dstValue);
		FLAG_APPLY_NEG(dstValue, dstSize);
	// SWAP
	} else if ((instruction & 0x0FF8) == 0x0840) {
		const uint8_t reg = (instruction >> 0) & 0x3;

		// Swap the register halves
		const uint32_t srcValue = registers.d[reg];
		const uint32_t dstValue = ((srcValue >> 16) & 0xFFFF) | ((srcValue & 0xFFFF) << 16);

		// Write back
		registers.d[reg] = dstValue;

		// Set flags
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_APPLY_ZERO(dstValue);
		FLAG_APPLY_NEG(dstValue, 4);
	// NEG
	} else if ((instruction & 0x0F00) == 0x0400 && (instruction & 0xC0) != 0xC0) {
		const uint8_t mode = (instruction >> 3) & 0x7;
		const uint8_t reg  = (instruction >> 0) & 0x7;
		const uint8_t size = 1 << ((instruction >> 6) & 0x3);

		// Get source
		const uint32_t param = getParameter(mode, reg, size);
		const uint32_t srcValue = readParam(param, mode, reg, size, false);

		// Calculate result
		const uint32_t result = subtract(0, srcValue, size, false, true, false);

		// Write back
		writeParam(param, result, mode, reg, size);
	// MOVEM
	} else if ((instruction & 0x0B80) == 0x0880) {
		const uint8_t size = 1 << (((instruction >> 6) & 1) + 1);
		const uint8_t mode = (instruction >> 3) & 0x7;
		const uint8_t reg  = (instruction >> 0) & 0x7;

		// (An)+ is only mem-to-reg, -(An) is only reg-to-mem, use direction bit otherwise
		const bool memToReg = (mode == 3) ? true : ((mode == 4) ? false : (instruction & 0x400));

		// Check whether the register value needs update
		const bool updateRegValue = (mode == 3) || (mode == 4);

		// Get register mask list
		const uint16_t mask = Memory::fetchUint16(registers.pc);
		registers.pc += 2;

		// Get the parameter address
		uint32_t address = getParameter((mode != 3 && mode != 4) ? mode : 1, reg, 4);

		// Get displacement value
		const int8_t displacement = (mode == 4) ? -size : size;

		// Write/Read the data
		for (unsigned int i = 0; i < 16; ++i) {
			// Invert mask list if required
			const unsigned int realReg = (displacement < 0) ? (15 - i) : i;

			// Check whether to process the register
			if (mask & (1 << i)) {
				// Check for predecrement
				if (displacement < 0) {
					address += displacement;
				}

				// Check whether to read or write
				if (memToReg) {
					const uint32_t value = readParam(address, 7, 1, size, size == 2);

					// address registers
					if (realReg & 0x80) {
						registers.a[realReg ^ 0x80] = value;
					// data registers
					} else {
						registers.d[realReg       ] = value;
					}
				} else {
					uint32_t value;

					// address registers
					if (realReg & 0x80) {
						value = registers.a[realReg ^ 0x80];
					// data registers
					} else {
						value = registers.d[realReg       ];
					}

					writeParam(address, value, 7, 1, size);
				}

				// Check for postincrement
				if (displacement > 0) {
					address += displacement;
				}
			}
		}

		// Update the register, if we need to.
		if (updateRegValue) {
			registers.a[reg] = address;
		}
	// PEA
	} else if ((instruction & 0x0FC0) == 0x0840) {
		// Get operand
		const uint8_t  mode  = (instruction >> 3) & 0x7;
		const uint8_t  reg   = (instruction >> 0) & 0x7;
		const uint32_t param = getParameter(mode, reg, 4);

		// Write value onto the stack
		PUSH_VALUE(param, 4);
	// TST
	} else if ((instruction & 0x0F00) == 0x0A00 && (instruction & 0xC0) != 0xC0) {
		const uint8_t size = 1 << ((instruction >> 6) & 0x3);

		// Get operand
		const uint8_t  mode  = (instruction >> 3) & 0x7;
		const uint8_t  reg   = (instruction >> 0) & 0x7;
		const uint32_t param = getParameter(mode, reg, size);
		const uint32_t value = readParam(param, mode, reg, size, false);

		// Set flags
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_APPLY_ZERO(value);
		FLAG_APPLY_NEG(value, size);
	// LINK with WORD operand
	} else if ((instruction & 0xFFF8) == 0x4E50) {
		// Get the register to use
		const uint8_t reg = instruction & 0x7;

		// Save the specified register
		PUSH_VALUE(registers.a[reg], 4);

		// Load the stack pointer into the specified register
		registers.a[reg] = registers.a[7];

		// Read the displacement (16 bit signed word)
		const int32_t displacement = getParameter(7, 0, 2);

		// Update the stack pointer
		registers.a[7] += displacement;
	// UNLK
	} else if ((instruction & 0xFFF8) == 0x4E58) {
		// Get the register to use
		const uint8_t reg = instruction & 0x7;

		// Load the stack pointer
		registers.a[7] = registers.a[reg];

		// Load the old register value from the stack
		registers.a[reg] = POP_VALUE(4, false);
	} else {
		return false;
	}

	return true;
}

void executeSub(const uint16_t instruction) {
	bool processFlags     = true;
	bool signExtendSource = false;
	bool useXBit          = false;

	uint8_t size    = (instruction >> 6) & 0x3;
	uint8_t srcMode = (instruction >> 3) & 0x7;
	uint8_t srcReg  = (instruction >> 0) & 0x7;
	uint8_t dstMode = 0;
	uint8_t dstReg  = (instruction >> 9) & 0x7;

	// Fix up size
	size = 1 << size;

	// Detect SUB cases
	// SUBA
	if ((instruction & 0xC0) == 0xC0) {
		processFlags = false;
		signExtendSource = true;
		dstMode = 1;
		if ((instruction & 0x100)) {
			size = 4;
		} else {
			size = 2;
		}
	// SUBX
	} else if ((instruction & 0x130) == 0x100) {
		useXBit = true;
		if ((instruction & 8)) {
			// -(An)
			srcMode = dstMode = 4;
		} else {
			// Dn
			srcMode = 0;
		}
	// SUB
	} else {
		// Check direction
		if ((instruction & 0x100)) {
			std::swap(srcMode, dstMode);
			std::swap(srcReg,  dstReg);
		}
	}

	// Read source
	const uint32_t src = readParam(getParameter(srcMode, srcReg, size), srcMode, srcReg, size, signExtendSource);

	// Read destination
	const uint32_t dstParam = getParameter(dstMode, dstReg, size);
	const uint32_t dst = readParam(dstParam, dstMode, dstReg, size, false);

	// We use a 4 byte operand for sign extended sources, because sign
	// extension is used for suba, which exclusivly produces 4 byte results.
	const uint32_t result = subtract(dst, src, signExtendSource ? 4 : size, useXBit, processFlags, false);

	// Write destination
	writeParam(dstParam, result, dstMode, dstReg, size);
}

void executeCmpEor(const uint32_t instruction) {
	// Detect EOR instruction
	if ((instruction & 0x100) && (instruction & 0xC0) != 0xC0 && (instruction & 0x38) != 8) {
		const uint8_t dstMode = (instruction >> 3) & 0x7;
		const uint8_t dstReg  = (instruction >> 0) & 0x7;
		const uint8_t srcMode = 0;
		const uint8_t srcReg  = (instruction >> 9) & 0x7;
		const uint8_t size    = 1 << ((instruction >> 6) & 0x3);

		// Read source
		const uint32_t src = readParam(getParameter(srcMode, srcReg, size), srcMode, srcReg, size, false);

		// Read destination
		const uint32_t dstParam = getParameter(dstMode, dstReg, size);
		const uint32_t dst = readParam(dstParam, dstMode, dstReg, size, false);

		// Calculate the result
		const uint32_t result = src ^ dst;

		// Write destination
		writeParam(dstParam, result, dstMode, dstReg, size);

		// Set the flags
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_APPLY_ZERO(result);
		FLAG_APPLY_NEG(result, size);

		return;
	}

	bool signExtendSource = false;

	uint8_t size    = (instruction >> 6) & 0x3;
	uint8_t srcMode = (instruction >> 3) & 0x7;
	const uint8_t srcReg  = (instruction >> 0) & 0x7;
	uint8_t dstMode;
	const uint8_t dstReg  = (instruction >> 9) & 0x7;

	// Fix up size
	size = 1 << size;

	// Detect CMP type
	// CMPA
	if ((instruction & 0xC0) == 0xC0) {
		signExtendSource = true;
		dstMode = 1;
		if ((instruction & 0x100)) {
			size = 4;
		} else {
			size = 2;
		}
	// CMPM
	} else if (instruction & 0x100) {
		dstMode = srcMode = 3;
	// CMP
	} else {
		dstMode = 0;
	}

	// Read source
	const uint32_t src = readParam(getParameter(srcMode, srcReg, size), srcMode, srcReg, size, signExtendSource);

	// Read destination
	const uint32_t dstParam = getParameter(dstMode, dstReg, size);
	const uint32_t dst = readParam(dstParam, dstMode, dstReg, size, false);

	// We use a 4 byte operand for sign extended sources, because sign
	// extension is used for cmpa, which exclusivly produces 4 byte results.
	subtract(dst, src, signExtendSource ? 4 : size, false, true, true);
}

void executeBcc(const uint32_t instruction) {
	const uint8_t condition = (instruction >> 8) & 0xF;

	// Save the current pc register, since we will need it
	// for the base address.
	const uint32_t pc = registers.pc;

	// Get the displacement value
	int32_t displacement = (int8_t)(instruction & 0xFF);
	if (!displacement) {
		displacement = (int16_t)Memory::fetchUint16(registers.pc);
		registers.pc += 2;
	} else if (displacement == -1) {
		displacement = Memory::fetchUint32(registers.pc);
		registers.pc += 4;
	}

	// BRA
	if (condition == 0) {
		// Jump to the destination
		registers.pc = pc + displacement;
	// BSR
	} else if (condition == 1) {
		// Save the current position
		PUSH_VALUE(registers.pc, 4);
		// Jump to the destination
		registers.pc = pc + displacement;
	// Bcc
	} else {
		// Jump if condition is true
		if (evalCondition(condition)) {
			registers.pc = pc + displacement;
		}
	}
}

void executeAddqSubqSccDbcc(const uint32_t instruction) {
	// Scc/DBcc
	if ((instruction & 0xC0) == 0xC0) {
		const uint8_t condition = (instruction >> 8) & 0xF;
		const uint8_t reg       = (instruction >> 0) & 0x7;

		// DBcc
		if ((instruction & 0x38) == 8) {
			// Save the instruction pointer.
			const uint32_t pc = registers.pc;

			// Read the dispalcement.
			const int16_t displacement = (int16_t)Memory::fetchUint16(registers.pc);
			registers.pc += 2;

			// Evaluate the condition
			if (!evalCondition(condition)) {
				// Read the register.
				int16_t regValue = (int16_t)getParameter(0, reg, 2);
				// Decrement
				--regValue;
				// Write back
				writeParam(0, regValue, 0, reg, 2);

				// Branch in case we didn't hit -1
				if (regValue != -1) {
					registers.pc = pc + displacement;
				}
			}
		// Scc
		} else {
			const uint8_t mode = (instruction >> 3) & 0x7;
			// Get the parameter
			const uint32_t param = getParameter(mode, reg, 1);
			const uint8_t result = evalCondition(condition) ? 0xFF : 0x00;
			// Write the result
			writeParam(param, result, mode, reg, 1);
		}
	// ADDQ/SUBQ
	} else {
		const uint8_t size = 1 << ((instruction >> 6) & 0x3);
		uint8_t value      = (instruction >> 9) & 0x7;
		// Fix up operand
		if (!value) {
			value = 8;
		}

		const uint8_t dstMode = (instruction >> 3) & 0x7;
		const uint8_t dstReg  = (instruction >> 0) & 0x7;

		// Read destination
		const uint32_t dstParam = getParameter(dstMode, dstReg, size);
		const uint32_t dst = readParam(dstParam, dstMode, dstReg, size, false);

		uint32_t result;

		// SUBQ
		if (instruction & 0x100) {
			// Do the subtraction, for An parameters we use the full width
			result = subtract(dst, value, (dstMode == 1) ? 4 : size, false, dstMode != 1, false);
		// ADDQ
		} else {
			// Do the addition, for An parameters we use the full width
			result = add(dst, value, (dstMode == 1) ? 4 : size, false, dstMode != 1, false);
		}

		// Write destination
		writeParam(dstParam, result, dstMode, dstReg, size);
	}
}

bool executeAndMulAbcdExg(const uint32_t instruction) {
	// MUL(S)/(U)
	if ((instruction & 0xC0) == 0xC0) {
		// Destination
		const uint8_t dstMode = 0;
		const uint8_t dstReg  = (instruction >> 9) & 0x7;

		// Source
		const uint8_t srcMode = (instruction >> 3) & 0x7;
		const uint8_t srcReg  = (instruction >> 0) & 0x7;

		// Read parameters
		const uint32_t srcParam = getParameter(srcMode, srcReg, 2);
		const uint32_t srcValue = readParam(srcParam, srcMode, srcReg, 2, false);

		const uint32_t dstParam = getParameter(dstMode, dstReg, 2);
		const uint32_t dstValue = readParam(dstParam, dstMode, dstReg, 2, false);

		uint32_t result;
		// Check whether we should do a signed or unsigned multiplication
		if (instruction & 0x100) {
			result = (int16_t)srcValue * (int16_t)dstValue;
		} else {
			result = (uint16_t)srcValue * (uint16_t)dstValue;
		}

		// Write back the result
		writeParam(dstParam, result, dstMode, dstReg, 4);

		// Set the flags
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_APPLY_ZERO(result);
		FLAG_APPLY_NEG(result, 4);
	// ABCD
	} else if ((instruction & 0x1F0) == 0x100) {
		return false;
	// EXG
	} else if ((instruction & 0x1C0) == 0x140 || (instruction & 0x1C0) == 0x180) {
		// Get register numbers
		const uint8_t reg1 = (instruction >> 9) & 0x7;
		const uint8_t reg2 = (instruction >> 0) & 0x7;

		// Get the operation mode
		const uint8_t opmode = (instruction >> 3) & 0x1F;

		if (opmode == 0x08) {
			std::swap(registers.d[reg1], registers.d[reg2]);
		} else if (opmode == 0x09) {
			std::swap(registers.d[reg1], registers.a[reg2]);
		} else if (opmode == 0x11) {
			std::swap(registers.a[reg1], registers.a[reg2]);
		} else {
			printf("ERROR: Caught invalid opmode %2X for EXG\n", opmode);
			return false;
		}
	// AND
	} else {
		const uint8_t size = 1 << ((instruction >> 6) & 0x3);

		uint8_t srcMode = (instruction >> 3) & 0x7;
		uint8_t srcReg  = (instruction >> 0) & 0x7;
		uint8_t dstMode = 0;
		uint8_t dstReg  = (instruction >> 9) & 0x7;

		// Check for swapped operands
		if (instruction & 0x100) {
			std::swap(srcMode, dstMode);
			std::swap(srcReg,  dstReg);
		}
	
		// Read source
		const uint32_t src = readParam(getParameter(srcMode, srcReg, size), srcMode, srcReg, size, false);

		// Read destination
		const uint32_t dstParam = getParameter(dstMode, dstReg, size);
		const uint32_t dst = readParam(dstParam, dstMode, dstReg, size, false);

		// Calculate result
		const uint32_t result = dst & src;

		// Set the flags accordingly
		FLAG_CLEAR_CARRY;
		FLAG_CLEAR_OVERFLOW;
		FLAG_APPLY_ZERO(result);
		FLAG_APPLY_NEG(result, size);

		// Write destination
		writeParam(dstParam, result, dstMode, dstReg, size);
	}

	return true;
}

bool executeBitManMovepImm(const uint32_t instruction) {
	// Bit manipulation
	if ((instruction & 0x0E00) == 0x0800 || ((instruction & 0x100) && (instruction & 0x38) != 8)) {
		uint32_t srcValue;
		// Acquire source operand
		if (!(instruction & 0x100)) {
			srcValue = Memory::fetchUint16(registers.pc) & 0xFF;
			registers.pc += 2;
		} else {
			srcValue = registers.d[(instruction >> 9) & 0x7];
		}

		// Extract operation type
		const uint8_t opType    = (instruction >> 6) & 0x3;
		const bool    writeBack = opType != 0;

		// Read the destination
		const uint8_t dstMode = (instruction >> 3) & 0x7;
		const uint8_t dstReg  = (instruction >> 0) & 0x7;
		const uint8_t size    = (dstMode == 0) ? 4 : 1;
		const uint32_t dstParam = getParameter(dstMode, dstReg, size);
		uint32_t result = readParam(dstParam, dstMode, dstReg, size, false);

		// Apply modulo to bit number
		if (size == 4) {
			srcValue %= 32;
		} else {
			srcValue %=  8;
		}

		// Create bit mask
		srcValue = 1 << srcValue;

		// Check original bit value and set flags
		FLAG_APPLY_ZERO(result & srcValue);

		// Apply operation
		switch (opType) {
		// CHG
		case 1:
			result ^= srcValue;
			break;

		// CLR
		case 2:
			result &= ~srcValue;
			break;

		// SET
		case 3:
			result |= srcValue;
			break;

		// TST
		default:
			break;
		}

		// Write back the result
		if (writeBack) {
			writeParam(dstParam, result, dstMode, dstReg, size);
		}
	// ORI/ANDI/EORI
	} else if ((instruction & 0x0F00) == 0x0000
			   || (instruction & 0x0F00) == 0x0200
			   || ((instruction & 0x0F00) == 0x0A00 && (instruction & 0x0038) == 0x0038)) {
		// SR destination is not supported.
		if ((instruction & 0xFF) == 0x7C) {
			return false;
		}

		// Detect operation type.
		const uint8_t operation = (instruction >> 9) & 0x7;

		// Get parameters
		const uint8_t size = 1 << ((instruction >> 6) & 0x3);
		const uint8_t mode = (instruction >> 3) & 0x3;
		const uint8_t reg  = (instruction >> 0) & 0x3;

		const uint32_t operand = fetchOperand(size);

		// Read parameter
		const bool ccrDest = (mode == 7) && (reg == 4);
		uint32_t param, value;

		if (ccrDest) {
			value = registers.ccr;
		} else {
			param = getParameter(mode, reg, size);
			value = readParam(param, mode, reg, size, false);
		}

		// Do operation
		if (operation == 0) {
			value |= operand;
		} else if (operation == 1) {
			value &= operand;
		} else if (operation == 5) {
			value ^= operand;
		}

		// Write back result
		if (ccrDest) {
			registers.ccr = value;
		} else {
			writeParam(param, value, mode, reg, size);
		}

		// Set flags if applicable
		if (!ccrDest) {
			FLAG_CLEAR_CARRY;
			FLAG_CLEAR_OVERFLOW;
			FLAG_APPLY_ZERO(value);
			FLAG_APPLY_NEG(value, size);
		}
	// SUBI/ADDI/CMPI
	} else {
		// Extract size
		const uint8_t size = 1 << ((instruction >> 6) & 0x3);

		// Destination
		const uint8_t dstMode = (instruction >> 3) & 0x7;
		const uint8_t dstReg  = (instruction >> 0) & 0x7;

		const uint32_t operand = fetchOperand(size);

		// Read destination
		const uint32_t dstParam = getParameter(dstMode, dstReg, size);
		const uint32_t dstValue = readParam(dstParam, dstMode, dstReg, size, false);

		// Whether we need to write back to the result
		const bool writeBack = !(instruction & 0x0800);

		uint32_t result;
		// Do operation
		if (!(instruction & 0x0200)) {
			result = subtract(dstValue, operand, size, false, true, !writeBack);
		} else {
			result = add(dstValue, operand, size, false, true, false);
		}

		// Write back if requsted
		if (writeBack) {
			writeParam(dstParam, result, dstMode, dstReg, size);
		}
	}

	return true;
}

void executeAdd(const uint16_t instruction) {
	bool processFlags     = true;
	bool signExtendSource = false;
	bool useXBit          = false;

	uint8_t size    = (instruction >> 6) & 0x3;
	uint8_t srcMode = (instruction >> 3) & 0x7;
	uint8_t srcReg  = (instruction >> 0) & 0x7;
	uint8_t dstMode = 0;
	uint8_t dstReg  = (instruction >> 9) & 0x7;

	// Fix up size
	size = 1 << size;

	// Detect ADD cases
	// ADDA
	if ((instruction & 0xC0) == 0xC0) {
		processFlags = false;
		signExtendSource = true;
		dstMode = 1;
		if ((instruction & 0x100)) {
			size = 4;
		} else {
			size = 2;
		}
	// ADDX
	} else if ((instruction & 0x130) == 0x100) {
		useXBit = true;
		if ((instruction & 8)) {
			// -(An)
			srcMode = dstMode = 4;
		} else {
			// Dn
			srcMode = 0;
		}
	// ADD
	} else {
		// Check direction
		if ((instruction & 0x100)) {
			std::swap(srcMode, dstMode);
			std::swap(srcReg,  dstReg);
		}
	}

	// Read source
	const uint32_t srcParam = getParameter(srcMode, srcReg, size);
	const uint32_t src = readParam(srcParam, srcMode, srcReg, size, signExtendSource);

	// Read destination
	const uint32_t dstParam = getParameter(dstMode, dstReg, size);
	const uint32_t dst = readParam(dstParam, dstMode, dstReg, size, false);

	// We use a 4 byte operand for sign extended sources, because sign
	// extension is used for adda, which exclusivly produces 4 byte results.
	const uint32_t result = add(dst, src, signExtendSource ? 4 : size, useXBit, processFlags, false);

	// Write destination
	writeParam(dstParam, result, dstMode, dstReg, size);
}

void executeMoveq(const uint32_t instruction) {
	// Get source value
	const uint32_t source = SIGN_EXTEND_8(instruction & 0xFF);

	// Write to data registers
	const uint8_t  reg    = (instruction >> 9) & 0x7;
	registers.d[reg] = source;

	// Set flags
	FLAG_CLEAR_CARRY;
	FLAG_CLEAR_OVERFLOW;
	FLAG_APPLY_ZERO(source);
	FLAG_APPLY_NEG(source, 4);
}

void executeShiftRotate(const uint32_t instruction) {
	// Check whether it is a memory operation
	const bool isMemoryOperation = (instruction & 0xC0) == 0xC0;

	// Determine parameters
	uint8_t operation;
	uint32_t count;
	uint8_t mode, reg;
	uint8_t size;

	if (isMemoryOperation) {
		operation = (instruction >> 9) & 0x3;
		count     = 1;
		mode      = (instruction >> 3) & 0x7;
		reg       = (instruction >> 0) & 0x7;
		size      = 2;
	} else {
		operation = (instruction >> 3) & 0x3;
		if (instruction & 0x10) {
			count = registers.d[(instruction >> 9) & 0x7] % 64;
		} else {
			count = (instruction >> 9) & 0x7;
			if (!count) {
				count = 8;
			}
		}
		size = 1 << ((instruction >> 6) & 0x3);
		mode = 0;
		reg  = (instruction >> 0) & 0x7;
	}

	// Read the source value
	const uint32_t param = getParameter(mode, reg, size);
	const uint32_t value = readParam(param, mode, reg, size, false);

	// Get direction
	const bool left = (instruction & 0x100);

	uint32_t result = value;

	const uint32_t msb = GET_SIGN_BIT(size);

	// Always clear carry and overflow
	FLAG_CLEAR_CARRY;
	FLAG_CLEAR_OVERFLOW;

	// Do processing
	switch (operation) {
	// ASd
	case 0: {
		uint32_t lastMSB = result & msb;

		for (uint32_t i = 0; i < count; ++i) {
			if (left) {
				// Shift MSB into C/E
				if (result & msb) {
					FLAG_SET_CARRY;
					FLAG_SET_EXTEND;
				} else {
					FLAG_CLEAR_CARRY;
					FLAG_CLEAR_EXTEND;
				}

				// Shift left
				result <<= 1;
			} else {
				// Shift LSB into C/E
				if (result & 1) {
					FLAG_SET_CARRY;
					FLAG_SET_EXTEND;
				} else {
					FLAG_CLEAR_CARRY;
					FLAG_CLEAR_EXTEND;
				}

				// Shift right, but duplicate MSB
				result = lastMSB | (result >> 1);
			}

			// Set overflow if MSB changed.
			if (lastMSB != (result & msb)) {
				FLAG_SET_OVERFLOW;
			}
			lastMSB = result & msb;
		}
		} break;
	
	// LSd
	case 1:
		for (uint32_t i = 0; i < count; ++i) {
			if (left) {
				// Shift MSB into C/E
				if (result & msb) {
					FLAG_SET_CARRY;
					FLAG_SET_EXTEND;
				} else {
					FLAG_CLEAR_CARRY;
					FLAG_CLEAR_EXTEND;
				}

				// Shift left
				result <<= 1;
			} else {
				// Shift LSB into C/E
				if (result & 1) {
					FLAG_SET_CARRY;
					FLAG_SET_EXTEND;
				} else {
					FLAG_CLEAR_CARRY;
					FLAG_CLEAR_EXTEND;
				}

				// Shift right
				result >>= 1;
			}
		}
		break;

	// ROXd
	case 2:
		if (FLAG_QUERY_EXTEND) {
			FLAG_SET_CARRY;
		}

		for (uint32_t i = 0; i < count; ++i) {
			const bool lastXValue = FLAG_QUERY_EXTEND;
			if (left) {
				if (result & msb) {
					FLAG_SET_CARRY;
					FLAG_SET_EXTEND;
				} else {
					FLAG_CLEAR_CARRY;
					FLAG_CLEAR_EXTEND;
				}

				result <<= 1;
				if (lastXValue) {
					result |= 1;
				}
			} else {
				if (result & 1) {
					FLAG_SET_CARRY;
					FLAG_SET_EXTEND;
				} else {
					FLAG_CLEAR_CARRY;
					FLAG_CLEAR_EXTEND;
				}

				result >>= 1;
				if (lastXValue) {
					result |= msb;
				}
			}
		}
		break;

	// ROd
	case 3:
		for (uint32_t i = 0; i < count; ++i) {
			if (left) {
				// Safe top bit
				const bool hadTopBit = (result & msb);

				result <<= 1;
				if (hadTopBit) {
					result |= 1;
					FLAG_SET_CARRY;
				} else {
					FLAG_CLEAR_CARRY;
				}
			} else {
				// Safe bottom bit
				const bool hadBottomBit = (result & 1);

				result >>= 1;
				if (hadBottomBit) {
					result |= msb;
					FLAG_SET_CARRY;
				} else {
					FLAG_CLEAR_CARRY;
				}
			}
		}
		break;
	}

	// Write back
	writeParam(param, result, mode, reg, size);

	// Set flags
	FLAG_APPLY_ZERO(value);
	FLAG_APPLY_NEG(value, size);
}
} // End of anonymous namespace

void initialize() {
	std::memset(&registers, 0, sizeof(registers));
}

void execute(const uint32_t startOffset) {
	stopEmulation = false;

	registers.pc = startOffset;
	registers.a[7] = MEM_PROG_AREA;

	while (!stopEmulation) {
		// Check whether we are at a built in system trap's address.
		if (registers.pc & MEM_TRAP_AREA) {
			// Execute it!
			Memory::executeBuiltinTrap(registers.pc);
			// Return to the former address.
			registers.pc = POP_VALUE(4, false);
			// Check whether we jumped into the jump table
			if (Memory::isInJumpTable(registers.pc)) {
				std::printf("INFO: Trap return to jump table at 0x%08X\n", registers.pc);
				stopEmulation = true;
			}
		} else {
			if (!executeInstruction()) {
				break;
			}
		}
	}
}

bool executeInstruction() {
	instructionStart = registers.pc;

	// Fetch the first 16 bits of the instruction
	const uint16_t instruction = Memory::fetchUint16(registers.pc);
	registers.pc += 2;

	// Decode what we have of the instruction so far.
	const uint8_t operationCode = instruction >> 12;
	switch (operationCode & 0x0F) {
	// Bit manipulation/movep/imm op
	case 0x0:
		if (executeBitManMovepImm(instruction)) {
			return true;
		}
		break;

	// Move Byte
	case 0x1:
	// Move Long
	case 0x2:
	// Move Word
	case 0x3:
		executeMove(instruction);
		return true;

	// misc
	case 0x4:
		if (executeMisc(instruction)) {
			return true;
		}
		break;

	// ADDQ/SUBQ/Scc/DBcc
	case 0x5:
		executeAddqSubqSccDbcc(instruction);
		return true;

	// Bcc
	case 0x6:
		executeBcc(instruction);
		return true;

	// MOVEQ
	case 0x7:
		executeMoveq(instruction);
		return true;

	case 0x8:
		break;

	// SUB/SUBA/SUBX
	case 0x9:
		executeSub(instruction);
		return true;

	// MacOS traps
	case 0xA:
		if (executeTrap(instruction)) {
			return true;
		}
		break;

	// CMP/EOR
	case 0xB:
		executeCmpEor(instruction);
		return true;

	// AND/MUL/ABCD/EXG
	case 0xC:
		if (executeAndMulAbcdExg(instruction)) {
			return true;
		}
		break;

	// ADD/ADDA/ADDX
	case 0xD:
		executeAdd(instruction);
		return true;

	// Shift/Rotate
	case 0xE:
		executeShiftRotate(instruction);
		return true;

	default:
		break;
	}

	std::fprintf(stderr, "ERROR: Unknown instruction at 0x%08X ", instructionStart);
	debugOutputUint16(instruction);
	dumpToFile("dump", &Memory::memory[0], Memory::memory.size());
	return false;
}

uint32_t popUint32() {
	return POP_VALUE(4, false);
}

uint16_t popUint16() {
	return POP_VALUE(2, false);
}

uint8_t popByte() {
	return POP_VALUE(1, false);
}

void pushUint32(uint32_t value) {
	PUSH_VALUE(value, 4);
}

void pushUint16(uint16_t value) {
	PUSH_VALUE(value, 2);
}

void pushByte(uint8_t value) {
	PUSH_VALUE(value, 1);
}
} // End of namespace Emulation
