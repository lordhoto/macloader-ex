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

#include "macloader.h"
#include "code0.h"
#include "mac-memory.h"
#include "emulation.h"
#include "traps.h"

ResourceFork resourceFork;

int main(int argc, char *argv[]) {
	if (argc < 2) {
		// XXX use message
		return -1;
	}

	if (!resourceFork.load(argv[1])) {
		// XXX error message
		return -1;
	}

	if (!loadCODE0Segment()) {
		return -1;
	}

	// Abitrarily pick 1024KB stack size.
	Memory::initializeStack(1024 * 1024);
	Traps::initializeResourceHandles();
	Emulation::initialize();
	for (uint32_t i = 0; i < Memory::jumpTableSize / 8; ++i) {
		Emulation::executeJumpTableEntry(Memory::getJumpTableEntryOffset(i), i == 0);
	}

	dumpToFile("dump", &Memory::memory[0], Memory::memory.size());
}
