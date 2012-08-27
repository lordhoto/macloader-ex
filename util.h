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

#ifndef UTIL_H
#define UTIL_H

#include <stdint.h>

uint16_t readUint16BE(const uint8_t *src);
uint32_t readUint32BE(const uint8_t *src);
void writeUint16BE(uint8_t *dst, const uint16_t data);
void writeUint32BE(uint8_t *dst, const uint32_t data);

void debugOutputUint16(uint16_t value);

void dumpToFile(const char *const filename, const uint8_t *const memory, const uint32_t size);

#if __cplusplus < 201103L
#define nullptr 0
#endif

template<class T>
inline void destroy(T *&t) {
	delete t;
	t = nullptr;
}

#endif
