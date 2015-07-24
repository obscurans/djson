/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */
module dutil.partialorder;

import std.traits;

/* Utility function to compute the maximal elements under a strict partial order
 *
 * Takes a count of elements to process, and an oracle that takes two indices
 * i, j and returns whether element i < element j
 *
 * Returns sorted array of indices of maximal elements */
size_t[] maximalElements(FPtr)(size_t count, FPtr oracle)
if (isCallable!FPtr && is(typeof(oracle(cast(size_t)0, cast(size_t)0)) : bool)) {
	if (!count) {
		return [];
	}

	/* Essentially dense singly linked list
	 * -1: not maximal; n: next maximal at index n; -2: last maximal */
	ptrdiff_t[] currentMaximal;
	size_t firstMaximal = 0; /* Head node */
	size_t maximalCount = 1;

	currentMaximal.length = count;
	currentMaximal[] = -1;
	currentMaximal[0] = -2;

L:	foreach (i; 1 .. count) {
		if (oracle(i, firstMaximal)) {
			continue; /* Strictly less than a maximal element */
		}

		while (oracle(firstMaximal, i)) {
			if (currentMaximal[firstMaximal] == -2) {
				/* Change unique maximal element */
				currentMaximal[firstMaximal] = -1;
				firstMaximal = i;
				currentMaximal[i] = -2;
				continue L;
			} else {
				/* Delete this proven non-maximal element */
				size_t next = currentMaximal[firstMaximal];
				currentMaximal[firstMaximal] = -1;
				firstMaximal = next;
				assert(maximalCount > 1);
				maximalCount--;
			}
		}

		size_t back_ptr = firstMaximal; /* Index of past element, for deletion */
		ptrdiff_t ptr = currentMaximal[firstMaximal]; /* Index of current element */
		bool knownMaximal = false;
		bool knownNonMaximal = false;
		assert(ptr == -2 || ptr > back_ptr);

		while (ptr != -2) {
			if (!knownMaximal && oracle(i, ptr)) {
				knownNonMaximal = true;
				break; /* Strictly less than a maximal element */
			}

			if (oracle(ptr, i)) {
				knownMaximal = true;
				/* Delete this proven non-maximal element */
				currentMaximal[back_ptr] = currentMaximal[ptr];
				currentMaximal[ptr] = -1;
				assert(maximalCount > 0);
				maximalCount--;
				ptr = currentMaximal[back_ptr];
			} else {
				back_ptr = ptr;
				ptr = currentMaximal[ptr];
			}
		}

		if (!knownNonMaximal) {
			assert(ptr == -2);
			/* Add this proven maximal element */
			currentMaximal[back_ptr] = i;
			currentMaximal[i] = -2;
			maximalCount++;
			assert(maximalCount > 0);
		}
	}

	size_t[] ret;
	ret.reserve(maximalCount);

	size_t ptr = firstMaximal;
	while (ptr != -2) {
		ret ~= ptr;
		ptr = currentMaximal[ptr];
	}

	assert(ret.length == maximalCount);
	return ret;
}

version(unittest) {
	debug(1) {
		import std.stdio;
	}
}

unittest {
	string[] list = ["abc", "abcde", "abcfg", "abcd", "abcdfg", "abcfg", "abcf", "abcdef", "abcfgh"];

	auto oracle = (size_t i, size_t j) {
		/* list[i] is a strict prefix of list[j] */
		return list[i].length < list[j].length && list[i] == list[j][0 .. list[i].length];
	};

	assert(maximalElements(list.length, oracle) == [4, 7, 8]); /* "abcdfg", "abcdef", "abcfgh" */

	debug(1) {
		writeln("dutil.partialorder: maximalElements test passed");
	}
}

