/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */
module dutil.bittwiddle;

/* Test is a power of 2 (one bit set). Returns true for 0 */
bool isPow2z(T)(T num) {
	return (num & (num - 1)) == 0;
}

/* Test is a power of 2 (one bit set) */
bool isPow2(T)(T num) {
	return num && !(num & (num - 1));
}

/* Find rank of least significant bit. Returns 0 for 0 */
byte lsb(T)(T num) {
	byte ret = 0;
	num = num & -num; /* Isolate last bit */
	foreach (i, mask; HighMask!T) {
		ret |= ((num & mask) != 0) << i;
	}
	return ret;
}

/* Round down to 1 less than a power of 2 */
T roundToPow2m1(T)(T num) {
	foreach (i; Pow2Forward!T[0 .. $ - 1]) {
		num |= num >> i;
	}
	return num;
}

/* Find log_2 = rank of most significant bit, assumes is power of 2 */
byte log2Pow2(T)(T num) {
	byte ret;
	foreach (i, mask; HighMask!T) {
		ret |= ((num & mask) != 0) << i;
	}
	return ret;
}

/* Find log_2 = rank of most significant bit */
byte log2(T)(T num) {
	return log2pow2(roundToPow2m1(num) + 1);
}

/* Count number of set bits */
byte popcnt(T)(T num) {
	T ret = 0;
	foreach (i, mask; LowMask!T) {
		static if (i == 0) {
			ret = cast(T)(num - ((num >> 1) & mask));
		} else static if (i == 1) {
			ret = (ret & mask) + ((ret >> 2) & mask);
		} else {
			ret = (ret + (ret >> (1 << i))) & mask;
		}
	}
	return cast(byte)ret;
}

/* Returns whether the number of set bits is odd */
bool parity(T)(T num) {
	foreach (i; Pow2Reverse!T[1 .. $ - 2]) {
		num ^= num >> i;
	}
	return (0x6996 >> (num & 0xF)) & 1;
}

/* Reverses the bits */
T bitReverse(T)(T num) {
	foreach (i, mask; LowMask!T) {
		num = ((num >> (1 << i)) & mask) | ((num & mask) << (1 << i));
	}
	return num;
}

template Tuple(T...) {
	alias Tuple = T;
}

private:
template HighMask(T) {
	static if (is(T == ubyte)) {
		alias HighMask = Tuple!(0xAA, 0xCC, 0xF0);
	} else static if (is(T == ushort)) {
		alias HighMask = Tuple!(0xAAAA, 0xCCCC, 0xF0F0, 0xFF00);
	} else static if (is(T == uint)) {
		alias HighMask = Tuple!(0xAAAAAAAA, 0xCCCCCCCC, 0xF0F0F0F0, 0xFF00FF00,
			0xFFFF0000);
	} else static if (is(T == ulong)) {
		alias HighMask = Tuple!(0xAAAAAAAAAAAAAAAA, 0xCCCCCCCCCCCCCCCC,
			0xF0F0F0F0F0F0F0F0, 0xFF00FF00FF00FF00, 0xFFFF0000FFFF0000,
			0xFFFFFFFF00000000);
	} else static assert (0);
}

template LowMask(T) {
	static if (is(T == ubyte)) {
		alias LowMask = Tuple!(0x55, 0x33, 0x0F);
	} else static if (is(T == ushort)) {
		alias LowMask = Tuple!(0x5555, 0x3333, 0x0F0F, 0x00FF);
	} else static if (is(T == uint)) {
		alias LowMask = Tuple!(0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF,
			0x0000FFFF);
	} else static if (is(T == ulong)) {
		alias LowMask = Tuple!(0x5555555555555555, 0x3333333333333333,
			0x0F0F0F0F0F0F0F0F, 0x00FF00FF00FF00FF, 0x0000FFFF0000FFFF,
			0x00000000FFFFFFFF);
	} else static assert (0);
}

template Pow2Forward(T) {
	static if (is(T == ubyte)) {
		alias Pow2Forward = Tuple!(1, 2, 4, 8);
	} else static if (is(T == ushort)) {
		alias Pow2Forward = Tuple!(Pow2Forward!ubyte, 16);
	} else static if (is(T == uint)) {
		alias Pow2Forward = Tuple!(Pow2Forward!ushort, 32);
	} else static if (is(T == ulong)) {
		alias Pow2Forward = Tuple!(Pow2Forward!uint, 64);
	} else static assert(0);
}

template Pow2Reverse(T) {
	static if (is(T == ubyte)) {
		alias Pow2Reverse = Tuple!(8, 4, 2, 1);
	} else static if (is(T == ushort)) {
		alias Pow2Reverse = Tuple!(16, Pow2Reverse!ubyte);
	} else static if (is(T == uint)) {
		alias Pow2Reverse = Tuple!(32, Pow2Reverse!ushort);
	} else static if (is(T == ulong)) {
		alias Pow2Reverse = Tuple!(64, Pow2Reverse!uint);
	} else static assert(0);
}

