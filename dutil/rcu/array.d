module dutil.rcu.array;

import std.traits, core.atomic;

/* An implementation of a one-writer append-only array with RCU synchronization
 * implicitly done by the GC. Writer calls ~=, readers take a const reference
 * and calls []. */
shared final class appendOnlyArray(T) {
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

	@property shared(inout(T[])) array const {
		auto tl_data = atomicLoad!(MemoryOrder.acq)(data);
		return tl_data.array;
	}

	@property size_t length() const {
		return array.length;
	}

	shared(inout(T[])) opIndex() inout {
		return array;
	}

	shared(inout(T)) opIndex(size_t ind) inout {
		return array[ind];
	}

	shared(inout(T[])) opIndex(Pair ind) inout {
		return array[ind.a .. ind.b];
	}

	Pair opSlice(size_t dim)(size_t a, size_t b) const {
		return Pair(a, b);
	}

	size_t opDollar() const {
		return data.array.length;
	}

	void opOpAssign(string op : "~")(const T item) {
		Data* tl_data = ensureAddable(cast(Data*)data, 1);
		immutable len = tl_data.array.length;

		T[] x_array = tl_data.array.ptr[0 .. len + 1];
		x_array[$ - 1] = item;

		Data* new_data = new Data(x_array, tl_data.capacity);
		atomicStore!(MemoryOrder.rel)(data, cast(shared)new_data);
	}

	void opOpAssign(string op : "~")(const T[] items) {
		Data* tl_data = ensureAddable(cast(Data*)data, items.length);
		immutable len = tl_data.array.length;

		T[] x_array = tl_data.array.ptr[0 .. len + items.length];
		x_array[len .. len + items.length] = items;

		Data* new_data = new Data(x_array, tl_data.capacity);
		atomicStore!(MemoryOrder.rel)(data, cast(shared)new_data);
	}

private:
	struct Data {
		Unqual!T[] array = [];
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
		import core.memory, core.stdc.string;

		immutable len = data.array.length;
		immutable len_req = data.array.length + nelems;
		immutable len_good = data.array.length * 3 / 2 + nelems;

		if (data.capacity >= len_req) {
			return data;
		}

		auto ext = GC.extend(data.array.ptr, nelems * T.sizeof, (len_good - len) * T.sizeof);
		if (ext) {
			return new Data(data.array, data.capacity + ext / T.sizeof);
		}

		auto n_array = GC.qalloc(len_good * T.sizeof, blockAttribute!T);
		if (len) {
			memcpy(n_array.base, data.array.ptr, len * T.sizeof);
		}
		return new Data((cast(Unqual!T*)n_array.base)[0 .. len], n_array.size / T.sizeof);
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

