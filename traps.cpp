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

#include "traps.h"
#include "emulation.h"
#include "mac-memory.h"
#include "macloader.h"

#include <boost/scoped_ptr.hpp>
#include <cstdio>
#include <vector>

namespace Traps {
namespace {
struct ResourceHandle {
	uint32_t type;
	uint32_t num;
	uint32_t handle;
	uint32_t state;
};

typedef std::vector<ResourceHandle> ResourceHandleContainer;
ResourceHandleContainer resourceHandles;

uint32_t findResourceHandle(const uint32_t type, const uint32_t num) {
	for (ResourceHandleContainer::const_iterator i = resourceHandles.begin(), end = resourceHandles.end(); i != end; ++i) {
		if (i->type == type && i->num == num) {
			return i->handle;
		}
	}

	return 0;
}

ResourceHandle *findHandle(const uint32_t handle) {
	for (ResourceHandleContainer::iterator i = resourceHandles.begin(), end = resourceHandles.end(); i != end; ++i) {
		if (i->handle == handle) {
			return &*i;
		}
	}

	return nullptr;
}

void addResourceHandle(const uint32_t type, const uint32_t num, const uint32_t handle) {
	ResourceHandle newHandle;
	newHandle.type   = type;
	newHandle.num    = num;
	newHandle.handle = handle;
	newHandle.state  = 0x80 | 0x20; // locked and is resource
	resourceHandles.push_back(newHandle);
}
} // End of anonymous namespace

void initializeResourceHandles() {
	// Allocate space for the handle
	const uint32_t handle = Memory::allocateHandlePointer();

	// Save the resource address in the handle
	Memory::writeUint32(handle + 0, Memory::a5BasePointer);

	// Save the handle size
	Memory::writeUint32(handle + 4, Memory::jumpTablePointer + Memory::jumpTableSize - Memory::a5BasePointer);

	// Save the handle
	addResourceHandle(kCodeTag, 0, handle);
}

void LoadSeg() {
	// Get the segment number
	const uint16_t segmentNumber = Memory::fetchUint16(Emulation::registers.a[7] + 4);
	boost::scoped_ptr<DataPair> code(resourceFork.getResource(kCodeTag, segmentNumber));

	const uint16_t jumpTableOffset  = readUint16BE(code->data + 0);
	const uint16_t jumpTableEntries = readUint16BE(code->data + 2);

	// Allocate space for the segment
	const uint32_t address = Memory::allocateSegment(code->length);

	// Load the segment into memory
	Memory::copyIntoMemory(address, code->data, code->length);

	// Allocate space for the handle
	const uint32_t handle = Memory::allocateHandlePointer();

	// Save the resource address in the handle
	Memory::writeUint32(handle + 0, address);

	// Save the handle size
	Memory::writeUint32(handle + 4, code->length);

	// Save the handle
	addResourceHandle(kCodeTag, segmentNumber, handle);

	// Output location
	std::printf("INFO: (Builtin loader) CODE segment 0x%X is at 0x%08X with size 0x%X\n", segmentNumber, address, code->length);

	// Patch up the jump table
	for (uint16_t i = 0; i < jumpTableEntries; ++i) {
		const uint32_t tableEntry = Memory::jumpTablePointer + jumpTableOffset + i * 8;

		// Write the JMP instruction
		Memory::writeUint16(tableEntry + 2, 0x4EF9);

		// Get the original function offset
		uint32_t functionAddress = Memory::fetchUint16(tableEntry + 0);
		// Add 4, since we also add the code segment header
		functionAddress += 4;
		functionAddress += address;
		// Write the offset
		Memory::writeUint32(tableEntry + 4, functionAddress);
	}
}

void GetOSTrapAddress() {
	// The trap for which we should query the address is in d0
	// TODO: 0xA000 bit or is guess work but seems required for some cases
	const uint16_t trap = Emulation::registers.d[0] | 0xA000;

	// The result will be in a0
	const Memory::TrapAddressMap *trapAddr = Memory::getTrapAddress(trap);
	if (trapAddr != nullptr) {
		Emulation::registers.a[0] = trapAddr->curAddress;
	} else {
		std::fprintf(stderr, "WARNING: GetOSTrapAddress: Unknown trap 0x%04X\n", trap);
		Emulation::registers.a[0] = TRAP_ADDRESS_INVALID;
	}
}

void GetToolTrapAddress() {
	// The trap for which we should query the address is in d0
	// TODO: 0xA800 bit or is guess work but seems required for some cases
	const uint16_t trap = Emulation::registers.d[0] | 0xA800;

	// The result will be in a0
	const Memory::TrapAddressMap *trapAddr = Memory::getTrapAddress(trap);
	if (trapAddr != nullptr) {
		Emulation::registers.a[0] = trapAddr->curAddress;
	} else {
		std::fprintf(stderr, "WARNING: GetToolTrapAddress: Unknown trap 0x%04X\n", trap);
		Emulation::registers.a[0] = TRAP_ADDRESS_INVALID;
	}
}

void GetTrapAddress() {
	// The trap for which we should query the address is in d0
	const uint16_t trap = Emulation::registers.d[0];

	// The result will be in a0
	const Memory::TrapAddressMap *trapAddr = Memory::getTrapAddress(trap);
	if (trapAddr != nullptr) {
		Emulation::registers.a[0] = trapAddr->curAddress;
	} else {
		std::fprintf(stderr, "WARNING: GetToolTrapAddress: Unknown trap 0x%04X\n", trap);
		Emulation::registers.a[0] = TRAP_ADDRESS_INVALID;
	}
}

void SetTrapAddress() {
	Memory::setTrapAddress(Emulation::registers.d[0], Emulation::registers.a[0]);
}

void GetResource() {
	// Save the return address.
	const uint32_t retAddr = Emulation::popUint32();

	// Get the resource number
	const uint16_t num = Emulation::popUint16();

	// Get the resource type
	const uint32_t type = Emulation::popUint32();

	// Pop the handle dummy
	Emulation::popUint32();

	uint32_t handle = findResourceHandle(type, num);
	// Try to reuse handles
	if (!handle) {
		boost::scoped_ptr<DataPair> res(resourceFork.getResource(type, num));
		if (res) {
			// Allocate space for the handle
			handle = Memory::allocateHandlePointer();

			// Allocate space for the resource
			const uint32_t resAddress = Memory::allocateSegment(res->length);

			// Load the resource into memory
			Memory::copyIntoMemory(resAddress, res->data, res->length);

			// Save the resource address in the handle
			Memory::writeUint32(handle + 0, resAddress);

			// Save the handle size
			Memory::writeUint32(handle + 4, res->length);

			// Save the handle
			addResourceHandle(type, num, handle);

			std::printf("INFO: %c%c%c%c segment 0x%X is at 0x%08X with size 0x%X\n", (type >> 24) & 0xFF, (type >> 16) & 0xFF, (type >> 8) & 0xFF, (type >> 0) & 0xFF, num, resAddress, res->length);
		} else {
			std::fprintf(stderr, "WARNING: GetResource: Resource %c%c%c%c%04X not found\n", (type >> 24) & 0xFF, (type >> 16) & 0xFF, (type >> 8) & 0xFF, (type >> 0) & 0xFF, num);
		}
	}

	// Write error code
	if (handle) {
		Memory::writeUint16(0x0A60, 0);
	} else {
		Memory::writeUint16(0x0A60, -192);
	}

	// Push the handle again
	Emulation::pushUint32(handle);

	// Restore the return address
	Emulation::pushUint32(retAddr);
}

void ReleaseResource() {
	// Save the return address.
	const uint32_t retAddr = Emulation::popUint32();

	// Pop the handle, we don't care about resource handling (oops)
	Emulation::popUint32();

	// Restore the return address
	Emulation::pushUint32(retAddr);
}

void GetHandleSize() {
	// Get the handle's address
	const uint32_t handle = Emulation::registers.a[0];

	// Save the size in d0
	Emulation::registers.d[0] = Memory::fetchUint32(handle + 4);
}

void NewPtr() {
	// Get the requestes (byte) size
	const uint32_t size = Emulation::registers.d[0];

	// Report success (Guesswork)
	Emulation::registers.d[0] = 0;

	// Write the address of the block
	Emulation::registers.a[0] = Memory::allocateSegment(size);
}

void HGetState() {
	// Try to obtain the handle description
	ResourceHandle *const handle = findHandle(Emulation::registers.a[0]);

	// Save the state
	Emulation::registers.d[0] = handle->state;
}

void NewPtrClear() {
	// Get the requestes (byte) size
	const uint32_t size = Emulation::registers.d[0];

	// Write the address of the block
	Emulation::registers.a[0] = Memory::allocateSegment(size);

	// Report success (Guesswork)
	Emulation::registers.d[0] = 0;

	// Clear the memory
	for (uint32_t i = 0; i < size; ++i) {
		Memory::writeByte(Emulation::registers.a[0] + i, 0);
	}
}

void SetToolTrapAddress() {
	// TODO: 0xA800 bit or is guess work but seems required for some cases
	Memory::setTrapAddress(Emulation::registers.d[0] | 0xA800, Emulation::registers.a[0]);
}

void GetResourceSizeOnDisk() {
	// Save the return address.
	const uint32_t retAddr = Emulation::popUint32();

	// Get the handle
	const uint32_t handle = Emulation::popUint32();

	// Pop the dummy result
	Emulation::popUint32();

	// Store the size
	Emulation::pushUint32(Memory::fetchUint32(handle + 4));

	// Restore the return address
	Emulation::pushUint32(retAddr);
}

void HLock() {
	// Dummy
}

void StripAddress() {
	// Dummy
}
} // End of namespace Traps
