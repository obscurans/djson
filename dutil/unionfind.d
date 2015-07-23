/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */
module dutil.unionfind;

import std.range, std.traits;

/* Classic Union-Find data structure: stores a disjoint forest of objects, with
 * the root of the tree being the representative of the set. Starred methods are
 * meaningless with void payload.
 *
 * this(): construct singleton set with no payload
 * *this(T): construct singleton set with opaque, unused payload
 * static makeList(size_t): construct number of singletons with no payload
 * *static makeList(InputRange!T): convenience function to construct list of singletons
 * isRoot(): whether this is the root of a set
 * find(): returns the root of this set
 * unionSet(UnionFind!T): joins this set with other set and returns new root
 *
 * *static UnionFind!T[] equivalencePartition(InputRange!T, bool callable(T, T)):
 *   Takes a list of input elements, presumed unique, and an oracle that
 *   computes an equivalence relation on elements. Returns pre-flattened
 *   union-find forest of the equivalence classes of the input elements.
 *   Representative is the first element in list of the class.
 *   Runs in O(n) space, and O(nm) worst case time, m is number of classes.
 *   Optimal time of O(n+m^2) is unachievable.
 */
class UnionFind(T) {
	static if (!is(T == void)) {
		T payload;
	}
	ubyte rank; /* Overflow impossible with union by rank + path compression */
	UnionFind!T parent;

	this() {
		rank = 0;
		parent = null;
	}

	static if (!is(T == void)) {
		this(T payload) {
			this.payload = payload;
			this();
		}
	}

	static UnionFind!T[] makeList(size_t count) {
		UnionFind!T[] ret;
		ret.length = count;
		foreach (ref r; ret) {
			r = new UnionFind!T();
		}
		return ret;
	}

	static if (!is(T == void)) {
		static UnionFind!T[] makeList(TR)(TR payloads)
		if (isInputRange!TR && is(ElementType!TR == T)) {
			UnionFind!T[] ret;
			static if (hasLength!TR) {
				ret.reserve(payloads.length);
			}
			foreach (p; payloads) {
				ret ~= new UnionFind!T(p);
			}
			return ret;
		}
	}

	pure bool isRoot() const {
		return parent is null;
	}

	UnionFind!T find() {
		if (parent !is null) {
			parent = parent.find(); /* Path compression */
			return parent;
		} else {
			return this;
		}
	}

	UnionFind!T unionSet(UnionFind!T other) {
		UnionFind!T root = find();
		UnionFind!T other_root = other.find();

		if (root is other_root) {
			return root;
		}

		/* Union by rank: attach lower rank to higher ranked root */
		if (root.rank < other_root.rank) {
			root.parent = other_root;
			return other_root;
		} else if (root.rank > other_root.rank) {
			other_root.parent = root;
			return root;
		} else {
			other_root.parent = root; /* Arbitrary choice at equal rank */
			rank++;
			return root;
		}
	}

	static if (!is(T == void)) {
		static UnionFind!T[] equivalencePartition(TR, FPtr)(TR elements, FPtr oracle)
		if (isInputRange!TR && is(ElementType!TR == T) &&
		isCallable!FPtr && is(typeof(oracle(elements.front, elements.front)) : bool)) {
			if (elements.empty) {
				return [];
			}

			/* Store roots of trees as well as entire list of elements */
			UnionFind!T[] roots = [new UnionFind!T(elements.front)];
			UnionFind!T[] ret = roots.dup;
			static if (hasLength!TR) {
				ret.reserve(elements.length);
			}
			elements.popFront();

			foreach (e; elements) {
				bool new_class = true;
				UnionFind!T element = new UnionFind!T(e);
				ret ~= element;

				foreach (j; roots) {
					if (oracle(j.payload, e)) { /* element is in the class represented by j */
						j.unionSet(element); /* j remains root */
						new_class = false;
						break;
					}
				}

				if (new_class) {
					roots ~= element; /* element is constructively a new equivalence class */
				}
			}

			return ret;
		}
	}
}

/* Static array-based version of UnionFind. Has no payloads, only contiguous
 * indices stored. Index of a set is immutable on creation.
 *
 * this(size_t): initialize with this number of singleton sets
 * makeSet(): append one singleton set
 * makeSet(size_t): append this number of singleton sets
 * @property length: number of sets
 * isRoot(size_t): is this index a root of a set
 * find(size_t): find root of set with this index
 * unionSet(size_t, size_t): join two sets with these indices, returns new root
 *
 * static UnionFindStatic equivalencePartition(size_t, bool callable(size_t, size_t)):
 *   Takes a count of input elements, and an oracle that computes an equivalence
 *   relation on indices. Returns pre-flattened union-find forest of the
 *   equivalence classes of the input elements. Representative is lowest index.
 *   Runs in O(n) space, and O(nm) worst case time, m is number of classes.
 *   Optimal time of O(n+m^2) is unachievable.
 */
struct UnionFindStatic {
	this(size_t count) {
		set_info.length = count;
		set_info[] = -1; /* Set all to singleton sets */
	}

	void makeSet() {
		set_info ~= -1;
	}

	void makeSet(size_t count) {
		set_info.length += count;
		set_info[$ - count .. $] = -1; /* Set to singleton sets */
	}

	@property pure size_t length() const {
		return length;
	}

	pure bool isRoot(size_t index) const {
		return set_info[index] < 0;
	}

	size_t find(size_t index) {
		if (set_info[index] >= 0) {
			return set_info[index] = find(set_info[index]); /* Path compression */
		} else {
			return index;
		}
	}

	size_t unionSet(size_t first, size_t second) {
		size_t first_root = find(first);
		size_t second_root = find(second);

		if (first_root == second_root) {
			return first_root;
		}

		ptrdiff_t first_rank = set_info[first_root]; /* Two's complemented */
		ptrdiff_t second_rank = set_info[second_root];
		assert(first_rank < 0);
		assert(second_rank < 0);

		/* Union by rank: attach lower rank to higher ranked root */
		if (first_rank > second_rank) { /* Reverse test under complementation */
			set_info[first_root] = second_root;
			return second_root;
		} else if (first_rank < second_rank) {
			set_info[second_root] = first_root;
			return first_root;
		} else if (first_root < second_root) { /* Choose lower index at equal rank */
			set_info[second_root] = first_root;
			set_info[first_root]--; /* Increase rank by 1 */
			return first_root;
		} else {
			set_info[first_root] = second_root;
			set_info[second_root]--;
			return second_root;
		}
	}

	static UnionFindStatic equivalencePartition(FPtr)(size_t count, FPtr oracle)
	if (isCallable!FPtr && is(typeof(oracle(cast(size_t)0, cast(size_t)0)) : bool)) {
		UnionFindStatic partition = UnionFindStatic(count);
		size_t[] roots = [0];
		roots.reserve(count);

		foreach (i; 1 .. count) {
			bool new_class = true;
			foreach (j; roots) {
				if (oracle(j, i)) { /* i is in the class represented by j */
					partition.unionSet(j, i); /* j remains root */
					new_class = false;
					break;
				}
			}

			if (new_class) { /* i is constructively a new equivalence class */
				roots ~= i;
			}
		}

		return partition;
	}

private:
	/* Array storage for each set. If positive, points to the index of its
	 * parent. If negative, is the root of a tree, rank is two's complemented */
	ptrdiff_t[] set_info;

	this(this) { /* Postblit, copy set_info */
		set_info = set_info.dup;
	}
}

version(unittest) {
	debug(1) {
		import std.stdio;
	}
}

unittest {
	UnionFind!int[] o = UnionFind!int.makeList([1, 2, 3, 4, 5]);
	assert(o[3].find() is o[3]);
	assert(o[3].unionSet(o[4]) is o[3]); /* 3<-4 */
	assert(o[4].find() is o[3]);
	assert(o[1].unionSet(o[0]) is o[1]); /* 1<-0; 3<-4 */
	assert(o[0].unionSet(o[4]) is o[1]); /* 1<-0,3,4 */
	o ~= new UnionFind!int(6);
	o ~= new UnionFind!int();
	assert(o[6].find() is o[6]);
	assert(o[6].unionSet(o[3]) is o[1]); /* 1<-0,3,4,6 */
	assert(!o[6].isRoot());
	assert(o[1].isRoot());

	debug(1) {
		writeln("dutil.unionfind: UnionFind basic functionality test passed");
	}
}

unittest {
	UnionFindStatic o = UnionFindStatic(5);

	assert(o.find(3) == 3);
	assert(o.unionSet(3, 4) == 3); /* 3<-4 */
	assert(o.find(4) == 3);
	assert(o.unionSet(1, 0) == 0); /* 0<-1; 3<-4 */
	assert(o.unionSet(1, 4) == 0); /* 0<-1,3,4 */
	o.makeSet(3);
	assert(o.find(7) == 7);
	assert(o.unionSet(6, 3) == 0); /* 0<-1,3,4,6 */
	assert(!o.isRoot(6));
	assert(o.isRoot(0));

	debug(1) {
		writeln("dutil.unionfind: UnionFindStatic basic functionality test passed");
	}
}

unittest {
	/* Test list of numbers under "mod 5" */
	int[] array = [3, 13, 5, 15, 8, 18, 11, 21];

	UnionFind!int[] eqs = UnionFind!int.equivalencePartition(array, (int x, int y) => (x - y) % 5 == 0);
	assert(eqs[0].payload == 3);
	assert(eqs[0].isRoot());
	assert(eqs[1].parent is eqs[0]);
	assert(eqs[2].payload == 5);
	assert(eqs[2].isRoot());
	assert(eqs[3].parent is eqs[2]);
	assert(eqs[4].parent is eqs[0]);
	assert(eqs[5].parent is eqs[0]);
	assert(eqs[6].payload == 11);
	assert(eqs[6].isRoot());
	assert(eqs[7].parent is eqs[6]);

	UnionFindStatic eqs2 = UnionFindStatic.equivalencePartition(array.length, (size_t x, size_t y) => (array[x] - array[y]) % 5 == 0);
	assert(eqs2.set_info == [-2, 0, -2, 2, 0, 0, -2, 6]);

	debug(1) {
		writeln("dutil.unionfind: equivalencePartition test passed");
	}
}

