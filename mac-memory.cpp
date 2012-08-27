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

#include "mac-memory.h"
#include "traps.h"
#include "emulation.h"

#include <cstring>
#include <cstdio>

namespace Memory {
MemoryContainer memory;
MemoryContainer stack;
namespace {
uint32_t stackBottom;
MemoryContainer handles;
} // End of anonymous namespace

struct SpecialAddresses {
	uint32_t addr;
	uint32_t size;
	uint8_t  data[4];
};

// Special addresses used by MacOS
SpecialAddresses systemMemory[] = {
	// Memory Manager pointer mask
	{ 0x031A,  4, { 0xFF, 0xFF, 0xFF, 0xFF } },
	// Resource Manager error code
	{ 0x0A60,  2, { 0x00, 0x00 } },
	// Whether the debugger should be used
	{ 0x012D,  1, { 0x00 } }
};

#define IN_SYSTEM_MEMORY_BLOCK(address, i) (address >= systemMemory[i].addr && address < systemMemory[i].addr + systemMemory[i].size)

uint8_t fetchByte(const uint32_t address) {
	// We lay out the memory a bit differently than a real MacOS.
	// We put the stack memory to a totally different location
	// to create a plain memory dump without any offsets.
	if (address & MEM_STACK_AREA) {
		return stack.at(address - stackBottom);
	// Place the executable at an abitrary high memory location too.
	// We need this to avoid syste memory access to mess up the
	// executable.
	} else if (address & MEM_PROG_AREA) {
		return memory.at(address & ~MEM_PROG_AREA);
	// We allocate a special area for handles.
	} else if (address & MEM_HANDLE_AREA) {
		return handles.at(address & ~MEM_HANDLE_AREA);
	} else {
		// First go through the special system memory and see whether we
		// can load it from there.
		for (size_t i = 0; i < sizeof(systemMemory)/sizeof(systemMemory[0]); ++i) {
			if (IN_SYSTEM_MEMORY_BLOCK(address, i)) {
				// Read the byte
				return systemMemory[i].data[address - systemMemory[i].addr];
			}
		}

		// The uint32_t at 0x908 is the stack base
		if (address >= 0x908 && address <= 0x90B) {
			if (address == 0x908) {
				return MEM_PROG_AREA >> 24;
			} else {
				return 0;
			}
		// The uint16_t at 0x934 is the jump table offset
		} else if (address >= 0x934 && address <= 0x935) {
			if (address == 0x934) {
				return ((jumpTablePointer - a5BasePointer) >> 8) & 0xFF;
			} else {
				return ((jumpTablePointer - a5BasePointer) >> 0) & 0xFF;
			}
		// The uint16_t at 0x28E describes: "holds a positive value if 128K or later ROM in Mac"
		} else if (address >= 0x28E && address <= 0x28F) {
			return 0x7F;
		// The uint32_t at 0x904 is the current a5 base
		} else if (address >= 0x904 && address <= 0x907) {
			return (a5BasePointer >> ((3 - (address - 0x904)) * 8)) & 0xFF;
		}

		std::fprintf(stderr, "WARNING: fetchByte: Read from unknown address 0x%08X from 0x%08X\n", address, Emulation::instructionStart);
		return 0;
	}
}

uint16_t fetchUint16(const uint32_t address) {
	const uint8_t highByte = fetchByte(address + 0);
	const uint8_t lowByte  = fetchByte(address + 1);
	return (highByte << 8) | lowByte;
}

uint32_t fetchUint32(const uint32_t address) {
	const uint16_t highWord = fetchUint16(address + 0);
	const uint16_t  lowWord = fetchUint16(address + 2);
	return (highWord << 16) | lowWord;
}

void writeByte(const uint32_t address, const uint8_t value) {
	if (address & MEM_STACK_AREA) {
		stack.at(address - stackBottom) = value;
	} else if (address & MEM_PROG_AREA) {
		memory.at(address & ~MEM_PROG_AREA) = value;
		/*if (isInJumpTable(address) && !(Emulation::instructionStart >= 0x20015FE2 && Emulation::instructionStart < 0x20016026)) {
			printf("0x%08X writes to jump table %08X\n", Emulation::instructionStart, address);
		}*/
	} else if (address & MEM_HANDLE_AREA) {
		handles.at(address & ~MEM_HANDLE_AREA) = value;
	} else {
		// First go through the special system memory and see whether we
		// can write it there.
		for (size_t i = 0; i < sizeof(systemMemory)/sizeof(systemMemory[0]); ++i) {
			if (IN_SYSTEM_MEMORY_BLOCK(address, i)) {
				// Write the byte
				systemMemory[i].data[address - systemMemory[i].addr] = value;
				return;
			}
		}

		std::fprintf(stderr, "WARNING: writeByte: Write to unknown address 0x%08X from 0x%08X\n", address, Emulation::instructionStart);
	}
}

void writeUint16(const uint32_t address, const uint16_t value) {
	writeByte(address + 0, (value >> 8) & 0xFF);
	writeByte(address + 1, (value >> 0) & 0xFF);
}

void writeUint32(const uint32_t address, const uint32_t value) {
	writeUint16(address + 0, (value >> 16) & 0xFFFF);
	writeUint16(address + 2, (value >>  0) & 0xFFFF);
}

void copyIntoMemory(const uint32_t address, const uint8_t *src, const uint32_t size) {
	if (!size) {
		return;
	}

	if (address & MEM_STACK_AREA) {
		std::memcpy(&stack.at(address - stackBottom), src, size);
	} else if (address & MEM_PROG_AREA) {
		std::memcpy(&memory.at(address & ~MEM_PROG_AREA), src, size);
	} else {
		// XXX warning?
	}
}

void initializeStack(const uint32_t size) {
	stackBottom = MEM_PROG_AREA - size;
	stack.resize(size);
}

uint32_t a5BasePointer;
uint32_t jumpTablePointer;
uint32_t jumpTableSize;

void allocateGlobals(const uint32_t size) {
	memory.resize(size);
	a5BasePointer = MEM_PROG_AREA | size;
	std::printf("INFO: a5 base is at 0x%08X\n", a5BasePointer);
}

void allocateAboveA5(const uint32_t size) {
	memory.resize(memory.size() + size);
}

void initializeParameters(const uint32_t size) {
}

void initializeJumpTable(const uint32_t offset, const uint32_t size, const uint32_t copySize, const uint8_t *src) {
	jumpTablePointer = a5BasePointer + offset;
	jumpTableSize = size;
	std::memcpy(&memory.at(jumpTablePointer & ~MEM_PROG_AREA), src, copySize);
	std::printf("INFO: Jump table is at 0x%08X with size 0x%X\n", jumpTablePointer, jumpTableSize);
}

uint32_t getJumpTableEntryOffset(const unsigned int entry) {
	return jumpTablePointer + entry * 8;
}

uint32_t allocateSegment(const uint32_t size) {
	// Save current address
	const uint32_t startAddress = MEM_PROG_AREA | memory.size();
	memory.resize(memory.size() + size + (size & 1));
	return startAddress;
}

namespace {
#define TRAP_ADDRESS(x) (MEM_TRAP_AREA | x), (MEM_TRAP_AREA | x)
TrapAddressMap trapAddresses[] = {
	{ 0xA9F0, TRAP_ADDRESS(0x00000000)          , &Traps::LoadSeg               },
	{ 0xA346, TRAP_ADDRESS(0x00000004)          , &Traps::GetOSTrapAddress      },
	{ 0xA746, TRAP_ADDRESS(0x00000008)          , &Traps::GetToolTrapAddress    },
	{ 0xA9A0, TRAP_ADDRESS(0x0000000C)          , &Traps::GetResource           },
	{ 0xA9A3, TRAP_ADDRESS(0x00000010)          , &Traps::ReleaseResource       },
	{ 0xA025, TRAP_ADDRESS(0x00000014)          , &Traps::GetHandleSize         },
	{ 0xA11E, TRAP_ADDRESS(0x00000018)          , &Traps::NewPtr                },
	{ 0xA146, TRAP_ADDRESS(0x0000001C)          , &Traps::GetTrapAddress        },
	{ 0xA047, TRAP_ADDRESS(0x00000020)          , &Traps::SetTrapAddress        },
	{ 0xA069, TRAP_ADDRESS(0x00000024)          , &Traps::HGetState             },
	{ 0xA31E, TRAP_ADDRESS(0x00000028)          , &Traps::NewPtrClear           },
	// This is actually SetToolTrapAddress:
	{ 0xA647, TRAP_ADDRESS(0x0000002C)          , &Traps::SetToolTrapAddress    },
	{ 0xA9A5, TRAP_ADDRESS(0x00000030)          , &Traps::GetResourceSizeOnDisk },
	{ 0xA029, TRAP_ADDRESS(0x00000034)          , &Traps::HLock                 },
	{ 0xA055, TRAP_ADDRESS(0x00000038)          , &Traps::StripAddress          },
	{ 0xA89F, TRAP_ADDRESS(TRAP_ADDRESS_INVALID), nullptr                       }
};
#undef TRAP_ADDRESS

TrapAddressMap *lookUpTrapAddress(const uint16_t trap) {
	for (size_t i = 0; i < sizeof(trapAddresses)/sizeof(trapAddresses[0]); ++i) {
		if (trapAddresses[i].trap == trap) {
			return &trapAddresses[i];
		}
	}

	return 0;
}
} // End of anonymous namespace

void initializeTraps() {
}

void setTrapAddress(const uint16_t trap, const uint32_t address) {
	TrapAddressMap *trapAddr = lookUpTrapAddress(trap);
	if (trapAddr != nullptr) {
		std::printf("INFO: Set address for trap 0x%04X to 0x%08X\n", trap, address);
		trapAddr->curAddress = address;
	} else {
		std::fprintf(stderr, "WARNING: setTrapAddress: Unknown trap 0x%04X\n", trap);
	}
}

const TrapAddressMap *getTrapAddress(const uint16_t trap) {
	return lookUpTrapAddress(trap);
}

void executeBuiltinTrap(const uint32_t address) {
	for (size_t i = 0; i < sizeof(trapAddresses)/sizeof(trapAddresses[0]); ++i) {
		if (trapAddresses[i].origAddress == address) {
			if (trapAddresses[i].func != nullptr) {
				trapAddresses[i].func();
			} else {
				// XXX warning?
			}
			break;
		}
	}
}

uint32_t allocateHandlePointer() {
	// Get address for new handle
	uint32_t address = MEM_HANDLE_AREA | handles.size();
	handles.resize(handles.size() + 8);
	return address;
}
} // End of namespace Memory
