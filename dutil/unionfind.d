/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */
module dutil.unionfind;

/* Classic Union-Find data structure: stores a disjoint forest of objects, with
 * the root of the tree being the representative of the set
 *
 * this(): construct singleton set with no payload
 * this(T): construct singleton set with opaque, unused payload
 * static makeList(size_t): construct number of singletons with no payload
 * static makeList(T[]): convenience function to construct list of singletons
 * find(): returns the root of this set
 * unionSet(UnionFind!T): joins this set with other set and returns new root
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
		static UnionFind!T[] makeList(T[] payloads) {
			UnionFind!T[] ret;
			ret.length = payloads.length;
			foreach (i, p; payloads) {
				ret[i] = new UnionFind!T(p);
			}
			return ret;
		}
	}

	UnionFind!T find() {
		if (parent !is null) {
			parent = parent.find(); /* Path compression */
		}
		return parent;
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
}

/* Static array-based version of UnionFind. Has no payloads, only contiguous
 * indices stored. Index of a set is immutable on creation.
 *
 * this(size_t): initialize with this number of singleton sets
 * makeSet(): append one singleton set
 * makeSet(size_t): append this number of singleton sets
 * @property length: number of sets
 * find(size_t): find root of set with this index
 * unionSet(size_t, size_t): join two sets with these indices, returns new root
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

	size_t find(size_t index) {
		if (set_info[index] >= 0) {
			set_info[index] = find(set_info[index]); /* Path compression */
		}
		return index;
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
		} else {
			set_info[second_root] = first_root; /* Arbitrary choice at equal rank */
			set_info[first_root]-- /* Increase rank by 1 */
			return first_root;
		}
	}

private:
	/* Array storage for each set. If positive, points to the index of its
	 * parent. If negative, is the root of a tree, rank is two's complemented */
	ptrdiff_t[] set_info;
}

