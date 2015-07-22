/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */
module dutil.unionfind;

/* Classic Union-Find data structure: stores a disjoint forest of objects, with
 * the root of the tree being the representative of the set
 *
 * this(T): construct singleton set with opaque, unused payload
 * static makeList(T[]): convenience function to construct list of singletons
 * find(): returns the root of this set
 * unionSet(UnionFind!T): joins this set with other set and returns new root
 */
class(T) UnionFind {
	T payload;
	ubyte rank; /* Overflow impossible with union by rank + path compression */
	UnionFind!T parent;

	this(T payload) {
		this.payload = payload;
		rank = 0;
		parent = null;
	}

	static UnionFind!T[] makeList(T[] payloads) {
		UnionFind!T[] ret;
		ret.length = payloads.length;
		foreach (i, p; payloads) {
			ret[i] = new UnionFind!T(p);
		}
		return ret;
	}

	UnionFind!T find() {
		if (parent !is null) {
			parent = find(parent); /* Path compression */
		}
		return parent;
	}

	UnionFind!T unionSet(UnionFind!T other) {
		root = find();
		other_root = other.find();

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

