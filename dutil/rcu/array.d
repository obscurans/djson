module dutil.rcu.array;

import std.traits, std.exception, core.atomic, core.stdc.string;

/* An implementation of a one-writer append-only array with RCU synchronization
 * implicitly done by the GC. Writer calls ~=, readers take a const reference
 * and calls [].
 *
 * Smashes the type system when trying to deal with immutable types. */
shared final class AppendOnlyArray(T) {
	this() {
		this(0);
	}

	this(size_t length) {
		Data* tl_data = ensureAddable(new Data(), length);
		data = cast(shared)tl_data;
	}

	this(T[] array) {
		data = cast(shared)(new Data(array));
	}

	@property shared(const inout T[]) array() const inout {
		auto tl_data = atomicLoad!(MemoryOrder.acq)(data);
		return tl_data.array;
	}

	@property size_t length() const {
		return array.length;
	}

	shared(const inout T[]) opIndex() const inout {
		return array;
	}

	shared(const inout T) opIndex(size_t ind) const inout {
		return array[ind];
	}

	shared(const inout T[]) opIndex(Pair ind) const inout {
		return array[ind.a .. ind.b];
	}

	Pair opSlice(size_t dim)(size_t a, size_t b) const {
		return Pair(a, b);
	}

	size_t opDollar() const {
		return data.array.length;
	}

	void opOpAssign(string op : "~")(const T item) {
		shared Data* old_data;
		Data* new_data;
		do {
			old_data = atomicLoad!(MemoryOrder.acq)(data);
			Data* tl_data = ensureAddable(cast(Data*)old_data, 1);
			immutable len = tl_data.array.length;

			/* Cast from immutable: single write after the end of published array */
			memcpy(cast(Unqual!T*)tl_data.array.ptr + len, &item, T.sizeof);
			T[] x_array = tl_data.array.ptr[0 .. len + 1];

			new_data = new Data(x_array, tl_data.capacity);
			atomicFence(); // Too heavy: only store barrier required
		} while (!cas(&data, old_data, cast(shared)new_data));
	}

	void opOpAssign(string op : "~")(const T[] items) {
		shared Data* old_data;
		Data* new_data;
		do {
			old_data = atomicLoad!(MemoryOrder.acq)(data);
			Data* tl_data = ensureAddable(cast(Data*)old_data, items.length);
			immutable len = tl_data.array.length;

			/* Cast from immutable: write after the end of published array */
			memcpy(cast(Unqual!T*)tl_data.array.ptr + len, items.ptr, items.length * T.sizeof);
			T[] x_array = tl_data.array.ptr[0 .. len + items.length];

			new_data = new Data(x_array, tl_data.capacity);
			atomicFence(); // Too heavy: only store barrier required
		} while (!cas(&data, old_data, cast(shared)new_data));
	}

private:
	struct Data {
		T[] array = [];
		size_t capacity = 0;

		this(T[] array) {
			this(array, array.capacity);
		}

		this(T[] array, size_t capacity) {
			this.array = array;
			this.capacity = capacity;
		}
	}

	struct Pair {
		size_t a;
		size_t b;
	}

	Data* data;

	static Data* ensureAddable(Data* data, size_t nelems)
	in {
	} out(ret) {
		assert(ret.capacity - ret.array.length >= nelems);
	} body {
		import core.memory;

		immutable len = data.array.length;
		immutable len_req = data.array.length + nelems;
		immutable len_good = data.array.length * 3 / 2 + nelems;

		if (data.capacity >= len_req) {
			return data;
		}

		/* Cast from immutable: GC extending block of memory, presumed no movement */
		auto ext = GC.extend(cast(Unqual!T*)data.array.ptr, nelems * T.sizeof, (len_good - len) * T.sizeof);
		if (ext) {
			return new Data(data.array, data.capacity + ext / T.sizeof);
		}

		auto n_array = GC.qalloc(len_good * T.sizeof, blockAttribute!T);
		if (len) {
			memcpy(n_array.base, data.array.ptr, len * T.sizeof);
		}
		return new Data((cast(T*)n_array.base)[0 .. len], n_array.size / T.sizeof);
	}
}

private template blockAttribute(T) {
	import core.memory;
	static if (hasIndirections!T || is(T == void)) {
		enum blockAttribute = 0;
	} else {
		enum blockAttribute = GC.BlkAttr.NO_SCAN;
	}
}

version(unittest) {
	import std.stdio, std.algorithm, std.random, core.thread;
}

unittest {
	final class Producer(T) : Thread {
		shared AppendOnlyArray!T array;
		T[][] chunk_buffer;

		this(T[][] chunks, ptrdiff_t length) {
			chunk_buffer = chunks;
			if (length < 0) { //  Use initialize with array path
				array = new shared AppendOnlyArray!T(chunk_buffer[0]);
				chunk_buffer = chunk_buffer[1 .. $];
			} else { // Use blank initialize path
				array = new shared AppendOnlyArray!T(length);
			}
			super(&run);
		}

		void run() {
			sleep(dur!"usecs"(100));
			bool flip;
			while (chunk_buffer.length) {
				if (chunk_buffer[0].length == 1) {
					if (flip) {
						array ~= chunk_buffer[0]; // Use append array path
					} else {
						array ~= chunk_buffer[0][0]; // Use append singleton path
					}
					flip = !flip;
				} else {
					array ~= chunk_buffer[0];
				}
				chunk_buffer = chunk_buffer[1 .. $];
				sleep(dur!"usecs"(100 + uniform(0, 100)));
			}
		}
	}

	final class Consumer(T) : Thread {
		shared const AppendOnlyArray!T array;
		shared const T[][size_t] expected;
		size_t total;
		bool mode;

		this(shared const AppendOnlyArray!T array, shared const T[][size_t] expected, size_t total, bool mode) {
			this.array = array;
			this.expected = expected;
			this.total = total;
			this.mode = mode;
			super(&run);
		}

		void run() {
			if (mode) { // One-load consistency testing
				shared const(T)[] tl_array;
				do {
					tl_array = array.array;
					assert(tl_array.length in expected);
					assert(tl_array == expected[tl_array.length]);
				} while (tl_array.length < total);
				sleep(dur!"usecs"(75 + uniform(0, 100)));
			} else { // Multi-load consistency testing
				do {
					bool flip;
					assert(array.length in expected);
					if (flip) { // Use slice indexing path
						foreach (i, j; array[]) {
							assert(j == expected[array.length][i]);
						}
					} else { // Use element indexing path
						foreach (i; 1 .. array.length) {
							assert(array[i] == expected[array.length][i]);
						}
					}
					sleep(dur!"usecs"(75 + uniform(0, 100)));
				} while (array.length < total);
			}
		}
	}

	static pure T[][size_t] construct_expected(T)(T[][] chunks) {
		T[][size_t] ret;
		T[] aggregate = [];

		ret[aggregate.length] = aggregate;
		while (chunks.length) {
			aggregate ~= chunks[0];
			ret[aggregate.length] = aggregate;
			chunks = chunks[1 .. $];
		}

		return ret;
	}

	long[][] test_chunks_1 = [
		[1, 2, 5],
		[-1, -2, -5],
		[0],
		[9, 8, 7, 6, 5, 4, 3, 2, 1],
		[-9, 8, -7, 6, -5, 4, -3, 2, -1],
		[-10],
		[1, 10, 100, 1000, 10000],
		[],
		[100000],
		[-1],
		[-10, -100, -1000, -10000, -100000],
		[12345]];
	auto expected_1 = cast(shared)construct_expected(test_chunks_1);

	foreach (repeat; 0 .. 10) {
		auto prod_1 = new Producer!long(test_chunks_1, -1);
		auto array_1 = prod_1.array;
		Consumer!long[] cons_1;
		foreach (i; 0 .. 10) {
			cons_1 ~= new Consumer!long(array_1, expected_1, sum(map!(a => a.length)(test_chunks_1)), (i & 1) == 0);
		}

		prod_1.start();
		foreach (c; cons_1) {
			c.start();
		}
		prod_1.join();
		foreach (c; cons_1) {
			c.join();
		}
	}

	string[] test_chunks_2 = [
		"",
		"foo",
		"bar",
		"a",
		"suddenly a longer phrase",
		"b",
		"and another long phrase",
		"",
		"deadbeef",
		"badf00d",
		"c",
		""];
	auto expected_2 = cast(shared)construct_expected(test_chunks_2);

	foreach (repeat; 0 .. 10) {
		auto prod_2 = new Producer!(immutable(char))(test_chunks_2, 5);
		auto array_2 = prod_2.array;
		Consumer!(immutable(char))[] cons_2;
		foreach (i; 0 .. 10) {
			cons_2 ~= new Consumer!(immutable(char))(array_2, expected_2, sum(map!(a => a.length)(test_chunks_2)), (i & 1) == 0);
		}

		foreach (i; 0 .. 10) {
			cons_2[i].start();
		}
		prod_2.start();
		prod_2.join();
		foreach (i; 0 .. 10) {
			cons_2[i].join();
		}
	}

	debug(1) {
		writeln("dutil.rcu.array: single-writer multiple-reader consistency test passed");
	}
}

