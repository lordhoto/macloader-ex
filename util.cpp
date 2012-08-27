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

#include "util.h"

#include <cstdio>

uint16_t readUint16BE(const uint8_t *src) {
	return (src[0] << 8) | src[1];
}

uint32_t readUint32BE(const uint8_t *src) {
	return (src[0] << 24) | (src[1] << 16) | (src[2] << 8) | src[3];
}

void writeUint16BE(uint8_t *dst, const uint16_t data) {
	dst[0] = (data >> 8) & 0xFF;
	dst[1] = (data >> 0) & 0xFF;
}

void writeUint32BE(uint8_t *dst, const uint32_t data) {
	dst[0] = (data >> 24) & 0xFF;
	dst[1] = (data >> 16) & 0xFF;
	dst[2] = (data >>  8) & 0xFF;
	dst[3] = (data >>  0) & 0xFF;
}

void debugOutputUint16(uint16_t value) {
	std::fprintf(stderr, "%02X | ", value);
	for (int i = 0; i < 16; ++i, value <<= 1) {
		if (value & 0x8000) {
			std::fprintf(stderr, "1");
		} else {
			std::fprintf(stderr, "0");
		}
	}
	std::fprintf(stderr, "\n");
}

void dumpToFile(const char *const filename, const uint8_t *const memory, const uint32_t size) {
	FILE *file = std::fopen(filename, "wb");
	if (file) {
		std::fwrite(memory, size, 1, file);
		std::fclose(file);
	}
}
