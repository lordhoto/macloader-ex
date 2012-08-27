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

#ifndef MAC_MEMORY_H
#define MAC_MEMORY_H

#include "util.h"

#include <vector>

#define MEM_STACK_AREA  0x10000000
#define MEM_PROG_AREA   0x20000000
#define MEM_TRAP_AREA   0x40000000
#define MEM_HANDLE_AREA 0x80000000

#define TRAP_ADDRESS_INVALID (MEM_TRAP_AREA | 0x0FFFFFFF)

namespace Memory {
typedef std::vector<uint8_t> MemoryContainer;
extern MemoryContainer memory;
extern MemoryContainer stack;

uint8_t fetchByte(const uint32_t address);
uint16_t fetchUint16(const uint32_t address);
uint32_t fetchUint32(const uint32_t address);

void writeByte(const uint32_t address, const uint8_t value);
void writeUint16(const uint32_t address, const uint16_t value);
void writeUint32(const uint32_t address, const uint32_t value);

void copyIntoMemory(const uint32_t address, const uint8_t *src, const uint32_t size);

void initializeStack(const uint32_t size);

extern uint32_t a5BasePointer;
extern uint32_t jumpTablePointer;
extern uint32_t jumpTableSize;

void allocateGlobals(const uint32_t size);
void allocateAboveA5(const uint32_t size);
void initializeParameters(const uint32_t size);
void initializeJumpTable(const uint32_t offset, const uint32_t size, const uint32_t copySize, const uint8_t *src);

uint32_t allocateSegment(const uint32_t size);

uint32_t getJumpTableEntryOffset(const unsigned int entry);
inline bool isInJumpTable(const uint32_t address) {
	return (address >= jumpTablePointer && address < jumpTablePointer + jumpTableSize);
}

struct TrapAddressMap {
	uint16_t trap;
	uint32_t origAddress;
	uint32_t curAddress;
	void (*func)();
};

void initializeTraps();

void setTrapAddress(const uint16_t trap, const uint32_t address);
const TrapAddressMap *getTrapAddress(const uint16_t trap);
void executeBuiltinTrap(const uint32_t address);

uint32_t allocateHandlePointer();
} // End of namespace Memory

#endif
