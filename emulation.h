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

#ifndef EMULATION_H
#define EMULATION_H

#include "util.h"

namespace Emulation {
void executeJumpTableEntry(const uint32_t offset, const bool firstEntry);

struct RegisterSet {
	uint32_t d[8];
	uint32_t a[8];
	uint32_t pc;
	uint8_t ccr;
};

extern RegisterSet registers;
extern bool stopEmulation;
extern uint32_t instructionStart;
void initialize();

void execute(const uint32_t startOffset);
bool executeInstruction();

uint32_t popUint32();
uint16_t popUint16();
uint8_t popByte();

void pushUint32(uint32_t value);
void pushUint16(uint16_t value);
void pushByte(uint8_t value);
} // End of namespace Emulation

#endif
