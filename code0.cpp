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

#include "code0.h"
#include "macloader.h"
#include "mac-memory.h"

#include <boost/scoped_ptr.hpp>
#include <cstdio>

bool loadCODE0Segment() {
	boost::scoped_ptr<DataPair> code0(resourceFork.getResource(kCodeTag, 0));
	if (!code0) {
		// XXX error message
		return false;
	}

	const uint32_t sizeAboveA5     = readUint32BE(code0->data +  0);
	const uint32_t globalsSize     = readUint32BE(code0->data +  4);
	const uint32_t jumpTableSize   = readUint32BE(code0->data +  8);
	const uint32_t jumpTableOffset = readUint32BE(code0->data + 12);

	if (sizeAboveA5 < jumpTableOffset + jumpTableSize) {
		// XXX error message
		return false;
	}

	const uint32_t realJumpTableSize = sizeAboveA5 - jumpTableOffset;

	if (realJumpTableSize > jumpTableSize && jumpTableSize != 8) {
		// XXX error message
		return false;
	}

	Memory::allocateGlobals(globalsSize);
	Memory::allocateAboveA5(sizeAboveA5);
	Memory::initializeParameters(jumpTableOffset);
	Memory::initializeJumpTable(jumpTableOffset, realJumpTableSize, jumpTableSize, code0->data + 16);

	return true;
}
