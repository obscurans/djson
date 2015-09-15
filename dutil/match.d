/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */
module dutil.match;

import std.conv, std.range, std.typecons, std.traits, std.functional;

alias ubstring = immutable(ubyte)[];

struct AhoCorasickPrinting {
	string in_dictionary = "*";
	string common_prefix = "";
	string separator = "\n";
}

pure char print_NA_(ubyte letter) {
	if (letter < 10) {
		return cast(char)(letter + '0');
	} else if (letter < 36) {
		return cast(char)(letter - 10 + 'A');
	} else if (letter == 36) {
		return '-';
	} else {
		assert(0);
	}
}

/* The information passed to the callback on a match: the dictionary word that
 * matched; the sorted indices of said word that are skipped during the match;
 * the zero-based index of the last character at the matching point */
struct Match {
	ubstring word;
	ubyte[] skip;
	size_t index;
}

enum bool isValidPrintLetter(alias print_letter) = (is(typeof("" ~ print_letter(cast(ubyte)0))));

/* Implements the Aho-Corasick algorithm (doi:10.1145/360825.360855) for
 * parallel multi-string matching. Essential idea is to extend the usual single-
 * string matching DFA by the concept of a fail transition: on a failure to
 * extend a partial match, jump to the longest suffix of the match that is still
 * a State within the combined prefix tree - computable in linear time by BFS
 * processing.
 *
 * Extension here to an NFA, by allowing some of the letters to be skipped (an
 * epsilon transition). Naive implementation by direct lockstep-over-all-threads
 * simulation, as skip possibilites are assumed rare. Thread identity consists
 * of current state as well as boolean vector of taken skips; no further state
 * information.
 *
 * Matching time is O(n*2^s), linear in length of input, EXPONENTIAL in the
 * highest count of skippable letters in any of the matched patterns. This is
 * the maximal thread count when all split machines taking all combinations of
 * skips are valid. Intended usage should have much less than 3 skips total.
 *
 * Space overhead is about 47 bytes per State = total pattern prefix (assuming
 * 64-bit pointers), plus another 296 per branching pattern prefix (for storing
 * a full transition table). Should be reasonable up through the order of 100k
 * total characters across all patterns. Could be crushed by a factor of ~3 if
 * storing indices into State list instead of pointers, at the cost of extra
 * indirection per access.
 */
class AhoCorasick(ubyte num_letter, bool skip_allowed = true, bool compress_multiple_skips = false, alias print_letter = to!char, AhoCorasickPrinting Printing = AhoCorasickPrinting.init)
if (isValidPrintLetter!print_letter) {

	alias ptrState = Rebindable!(const(State)); /* Current std.typecons hack to get mutable ref to const object */

	State root;
	uint state_count = 0; /* Only concern is unique IDs per State */
	ubstring[] dictionary; /* List of words to match */
	bool dictionary_changed = true; /* Whether fail transitions need recomputed */

	this() {
		root = new State(null);
		root.parent = root; /* Makes computing fail transitions easier */
		assert(root.state_id == 0);
	}

	void printAllWords() const {
		root.printAll(Printing.common_prefix);
	}

	uint getNextStateID() {
		return state_count++;
	}

	/* Adds a single word from array of letters, recording a positive ID.
	 * Returns true if the word did not already exist in the dictionary. */
	bool addWord(ubstring word) {
		State newstate = root.addWord(word, cast(int)(dictionary.length + 1));
		if (newstate is null) {
			return false;
		}

		assert(newstate.depth == word.length);
		dictionary ~= word;
		dictionary_changed = true;
		return true;
	}

	/* Convenience function to add a list of words at once. Returns the count of
	 * new words that did not already exist in the dictionary. */
	size_t addWords(ubstring[] words) {
		size_t counter = 0;

		foreach (word; words) {
			counter += addWord(word) == true;
		}

		return counter;
	}

	/* To be called after all words have been added. Canonicalizes storage and
	 * computes fail transitions and indirect matches (anew). */
	void finalize() {
		root.minimizeAllMultiple();
		if (dictionary_changed) {
			clearDerivedTransitions();
			computeDerivedTransitions();
		}
	}

	/* Main function that takes the input array of letters to match, and the
	 * callback in case of a match. Caller is presumed to handle nested matches
	 * (i.e. function reports once for "123", will not report again if "23" is
	 * also a dictionary word).
	 *
	 * In case of indirect matches, similarly reports the longest indirect match
	 * (i.e. if currently at "0123" which is not in the dictionary, but "123"
	 * is, latter gets reported).
	 *
	 * Guarantees (through anti-duplication hash) reports are given once per
	 * unique combination of (dictionary word, skip trace, end index). Will
	 * report multiple times if adjacent skips on identical letters exist (all
	 * distinct combinations are unique).
	 *
	 * If callback returns a boolean-true value, matching instantly terminates.
	 * Callback returning void is fine (no early termination).
	 */
	void matchInput(FPtr)(ubstring input, FPtr callback) const
	if (isCallable!FPtr && is(typeof(callback(Match()))))
	in {
		assert(!dictionary_changed);
	} body {
		static if (skip_allowed) { /* Nondeterministic matcher body */
			size_t index = 0;
			bool[Thread] pool; /* List of active Threads */
			Thread init = Thread(rebindable(root), []);
			do {
				pool[init] = true; /* Initialize Threads starting at root */
			} while (State.skipMatcher(init));

			while (index < input.length) {
				bool[Thread] pool_next; /* List of active Threads after this letter */
				bool[Thread] pool_sub; /* List of tertiary transition heads from this letter */
				bool[Match] pool_match; /* List of matched Threads at this letter */

				foreach (thread; pool.byKey) {
					State.stepMatcher(thread, input[index]); /* Run matcher, updates thread */

					/* Loop over all tertiary transitions (voluntary fail
					 * transitions that lead to other epsilon transitions) */
					do {
						if (thread !in pool_sub) {
							Thread subthread = thread;
							pool_sub[thread] = true;

							/* Loop over all epsilon transitions starting here */
							do {
								/* Mark new Threads */
								if (subthread !in pool_next) {
									pool_next[subthread] = true;

									if (subthread.ptr.dictionary_id != 0) {
										Match match = computeMatch(subthread, index);
										/* Mark new Matches, as indirect matches can equal a separate Thread's direct match */
										if (match !in pool_match) {
											static if (is(typeof(callback(match)) : bool)) {
												if (callback(match)) {
													return; /* Early termination */
												}
											} else {
												callback(match);
											}
											pool_match[match] = true;
										}
									}
								}
							} while (State.skipMatcher(subthread));
						}
					} while (State.epsSkipMatcher(thread));
				}

				pool = pool_next; /* Overwrite thread pool in one step */
				index++;
				static if (compress_multiple_skips) {
					/* When adjacent input characters are skippable, do not process
					 * more than 1 */
					while (index < input.length && input[index] == num_letter - 1 && input[index - 1] == num_letter - 1) {
						index++;
					}
				}
			}
		} else { /* Much simplified deterministic matcher body */
			size_t index = 0;
			Thread thread = Thread(rebindable(root), []); /* Without skips, deterministic matching */

			while (index < input.length) {
				State.stepMatcher(thread, input[index]); /* Run matcher, updates thread */

				if (thread.ptr.dictionary_id != 0) {
					Match match = computeMatch(thread, index);
					static if (is(typeof(callback(match)) : bool)) {
						if (callback(match)) {
							return; /* Early termination */
						}
					} else {
						callback(match);
					}
				}

				index++;
			}
		}
	}

	pure Match computeMatch(Thread thread, size_t index) const
	in {
		assert(thread.ptr !is null && thread.ptr.dictionary_id != 0);
	} out(ret) {
		assert(ret.word !is null);
	} body {
		with (thread) {
			if (ptr.dictionary_id > 0) { /* Direct match */
				return Match(dictionary[ptr.dictionary_id - 1], skip, index);
			} else {
				Match ret;
				ret.skip = skip.dup;
				ret.word = dictionary[-ptr.dictionary_id - 1];
				ret.index = index;

				assert(ptr.depth > ret.word.length);
				shiftSkip(ret.skip, cast(ubyte)(ptr.depth - ret.word.length));

				return ret;
			}
		}
	}

	void clearDerivedTransitions() {
		root.clearDerivedTransitions();
		dictionary_changed = true;
	}

	void computeDerivedTransitions() {
		State[] queue = [root];
		while (queue.length) {
			queue[0].computeDerivedTransitions();

			/* Add all children to the queue in BFS */
			queue[0].applyToTransitions((ubyte letter, State target) {
				queue ~= target;
			});

			queue.popFront;
		}
		dictionary_changed = false;
	}

private:
	struct Thread {
		ptrState ptr;
		ubyte[] skip;
	}

	/* Shifts all the (ordered) skips in trace downward by a shift factor */
	static void shiftSkip(ref ubyte[] skip, ubyte shift) {
		while (skip.length && skip[0] < shift) {
			skip = skip[1 .. $]; /* Delete skips shifted off the end */
		}
		foreach (ref i; skip) {
			i -= shift;
		}
	}

	/* One state in the DFA constructed for (multiple) string matching using
	 * the Aho-Corasick algorithm. Provides two storage methods for the
	 * transition table: for single-forward transitions (a common case), it
	 * stores the letter and target; for multiple it stores the full transition
	 * table. Converts between the two as needed. */
	final class State {
		State parent = null;
		State fail_transition = null;
		static if (skip_allowed) {
			/* Next State in the fail chain with an epsilon transition */
			State eps_transition = null;
		}
		uint state_id;
		/* If positive, this State is a direct word match; if negative, match is
		 * indirect (suffix of the implicitly saved string at this State is
		 * itself a match). Zero means no match. */
		int dictionary_id = 0;
		ubyte from_letter;
		ubyte depth; /* Length of the prefix match at this State */
		bool is_multiple = false;
		Transitions transitions = { single: { null, 0 } };

		union Transitions {
			static struct Single {
				State to_transition;
				ubyte to_letter;
			}
			static struct Multiple {
				State[] transition_table;
			}

			Single single;
			Multiple multiple;
		}

		/* Field-based constructor, omitting fail transition */
		this(State parent) {
			this.parent = parent;
			if (parent is null) {
				depth = 0;
			} else {
				depth = cast(ubyte)(parent.depth + 1);
			}
			state_id = getNextStateID();
		}

		this(State parent, ubyte from_letter)
		in {
			assert(from_letter < num_letter);
		} body {
			this.from_letter = from_letter;
			this(parent);
		}

		/* Basic recursive printer, is highly inefficient if called across the
		 * entire tree */
		override pure string toString() const {
			if (this is root) {
				return "";
			} else {
				return parent.toString ~ print_letter(from_letter);
			}
		}

		/* State ID serves as a guaranteed no-collision hash function */
		override nothrow size_t toHash() const {
			return state_id;
		}

		/* Adds a single word, creating all necessary States, and records a
		 * positive ID. Returns the new State if not extant in the dictionary */
		State addWord(ubstring word, int id)
		in {
			assert(id > 0);
		} body {
			/* Base case: at the required State representing the entire word */
			if (!word.length) {
				if (dictionary_id > 0) {
					return null;
				} else {
					dictionary_id = id; /* Override indirect matches as well */
					return this;
				}
			}

			/* Retrieve/create the State representing the current prefix plus
			 * next letter */
			State next = getTransition(word[0]);
			if (next is null) {
				next = new State(this, word[0]);
				modifyTransition(word[0], next);
			}

			/* Standard forward recursion */
			return next.addWord(word[1 .. $], id);
		}

		/* Primary function that advances the AC state machine, from Thread
		 * (current State ptr, current skip trace), and the next letter.
		 *
		 * Returns true if successful extend; false if backtracking */
		static bool stepMatcher(ref Thread thread, ubyte letter)
		in {
			assert(letter < num_letter);
		} body {
			with (thread) {
				debug (2) {
					import std.stdio;
					write(ptr, skip, "+", letter);
					scope(exit) writeln("=>", ptr, skip);
				}

				ptrState next = ptr.getTransition(letter);
				if (next !is null) {
					ptr = next;
					return true;
				}

				ubyte old_depth = ptr.depth;
				do {
					ptr = ptr.fail_transition;
					next = ptr.getTransition(letter);
					if (next !is null) {
						ptr = next;

						assert(ptr.depth <= old_depth);
						/* Shift the difference in depth from all skips */
						shiftSkip(skip, cast(ubyte)(old_depth - ptr.depth + 1));

						return false;
					}
				} while (ptr !is ptr.fail_transition);

				/* Jumped back to root */
				skip = [];
				return false;
			}
		}

		/* Secondary matcher function, which advances the AC state machine by
		 * epsilon-transitioning on a "skippable" letter. Returns true if such
		 * a transition succeeded. */
		static bool skipMatcher(ref Thread thread) {
			with (thread) {
				ptrState next = ptr.getSkipTransition();
				if (next is null) {
					return false;
				}

				ptr = next;
				skip = skip.dup;
				skip ~= cast(ubyte)(ptr.depth - 1);
				return true;
			}
		}

		pure inout(State) getSkipTransition() inout {
			static if (skip_allowed) {
				return getTransition(num_letter - 1); /* top letter hardcoded as skippable right now */
			} else {
				return null;
			}
		}

		/* Tertiary matcher function, which voluntarily takes a fail transition
		 * that leads to an available epsilon-transition. Returns true if such a
		 * transition succeeded. */
		static bool epsSkipMatcher(ref Thread thread) {
			static if (skip_allowed) {
				with (thread) {
					const State next = ptr.eps_transition;
					if (next is null) {
						return false;
					}

					assert(next.depth < ptr.depth);
					skip = skip.dup;
					shiftSkip(skip, cast(ubyte)(ptr.depth - next.depth));
					ptr = next;
					return true;
				}
			} else {
				return false;
			}
		}

		void clearDerivedTransitions() {
			static void clearDerivedTransitionsImpl(ubyte letter, State self) {
				self.fail_transition = null;
				static if (skip_allowed) {
					self.eps_transition = null;
				}
				/* Clear indirect match designations */
				if (self.dictionary_id < 0) {
					self.dictionary_id = 0;
				}
				self.applyToTransitions(&clearDerivedTransitionsImpl);
			}

			clearDerivedTransitionsImpl(0, this);
		}

		void computeDerivedTransitions() {
			State ancestor = parent;
			while (ancestor !is root) {
				/* Nontrivial fail transition if some direct ancestor's fail
				 * transition has a valid forward transition */
				State test = ancestor.fail_transition.getTransition(from_letter);
				if (test !is null) {
					fail_transition = test;
					break;
				} else {
					ancestor = ancestor.fail_transition;
				}
			}
			if (ancestor is root) {
				fail_transition = root;
			}

			static if (skip_allowed) {
				/* Check for epsilon transitions that can occur after a fail
				 * transition, as splitting to take the fail then epsilon is a
				 * valid path. Similar to computing a higher-order fail
				 * transition. */
				ancestor = fail_transition;
				while (ancestor !is root) {
					State test = ancestor.getSkipTransition();
					if (test !is null) {
						/* Transition to the base of the epsilon chain, to
						 * allow further fail transitions later */
						eps_transition = ancestor;
						break;
					} else {
						ancestor = ancestor.fail_transition;
					}
				}
				/* Epsilon-taking transition null if nonexistent or trivial */
				if (ancestor is root) {
					if (this !is root && root.getSkipTransition() !is null) {
						eps_transition = root;
					} else {
						eps_transition = null;
					}
				}
			}

			/* Check for indirect matches: if a fail transition causes a match,
			 * that should trigger as well at this State */
			if (dictionary_id == 0) {
				if (fail_transition.dictionary_id > 0) {
					dictionary_id = -fail_transition.dictionary_id;
				} else if (fail_transition.dictionary_id < 0) {
					dictionary_id = fail_transition.dictionary_id;
				}
			}
		}

		/* Prints this State and all children, using a common prefix.
		 * Implementation is corecursive */
		void printAll(string prefix) const {
			import std.stdio;

			write(prefix);
			if (dictionary_id > 0) {
				write(Printing.in_dictionary);
			}
			debug(1) {
				write("=>", fail_transition);
				static if (skip_allowed) {
					write("=>", eps_transition);
				}
			}
			write(Printing.separator);

			/* Register a lambda to be called on all forward transitions, which
			 * chains back to printAll with the extended prefix (current prefix
			 * is captured, acting as the DFS call stack) */
			applyToTransitions((ubyte letter, const State target) {
				target.printAll(prefix ~ print_letter(letter));
			});
		}

		/* Retrieves the State target, if any, on transition on this letter. */
		inout(State) getTransition(ubyte letter) inout
		in {
			assert(letter < num_letter);
		} body {
			if (is_multiple) {
				return transitions.multiple.transition_table[letter];
			} else if (letter == transitions.single.to_letter) {
				return transitions.single.to_transition;
			} else {
				return null;
			}
		}

		/* Drops transition tables for States that have been reduced to less
		 * than 2 forward transitions */
		void minimizeAllMultiple() {
			static void minimizeAllMultipleImpl(ubyte letter, State self) {
				self.minimizeMultiple();
				self.applyToTransitions(&minimizeAllMultipleImpl);
			}

			minimizeAllMultipleImpl(0, this);
		}

		bool minimizeMultiple() {
			if (is_multiple) with (transitions.multiple) {
				ubyte count = 0;
				ubyte letter = 0;
				foreach (ubyte i, t; transition_table) {
					if (t !is null) {
						count++;
						letter = i; /* Save the (last) non-null letter */
					}
				}

				if (count < 2) {
					/* letter stores the only non-null letter, change to single-
					 * transition storage */
					State last_target = transition_table[letter];
					is_multiple = false;
					transitions.single.to_letter = letter;
					transitions.single.to_transition = last_target;
					return true;
				}
			}

			return false;
		}

		/* Modifies (possibly adds/deletes) the transition on a letter to
		 * the State. Can cause allocation of a transition table. */
		void modifyTransition(ubyte letter, State target)
		in {
			assert(letter < num_letter);
		} out {
			assert(getTransition(letter) is target);
		} body {
			if (is_multiple) with (transitions.multiple) {
				transition_table[letter] = target;
			} else with (transitions.single) {
				if (to_transition is null) {
					/* Overwrite letter if no stored target */
					to_letter = letter;
					to_transition = target;
				} else if (letter == to_letter) {
					/* Overwrite target if equalling stored letter */
					to_transition = target;
				} else if (target is null) {
					assert(0); /* Delete nonexistent transition */
				} else {
					ubyte old_letter = transitions.single.to_letter;
					State old_target = transitions.single.to_transition;

					is_multiple = true;
					transitions.multiple.transition_table = new State[num_letter];
					transitions.multiple.transition_table[old_letter] = old_target;
					transitions.multiple.transition_table[letter] = target;
				}
			}
		}

		/* Iterator method to apply a function to all forward-transition target
		 * States */
		void applyToTransitions(void function(ubyte, State) func) {
			mixin applyToTransitionsImpl;
			applyToTransitionsImpl();
		}

		void applyToTransitions(void function(ubyte, const State) func) const {
			mixin applyToTransitionsImpl;
			applyToTransitionsImpl();
		}

		void applyToTransitions(void delegate(ubyte, State) func) {
			mixin applyToTransitionsImpl;
			applyToTransitionsImpl();
		}

		void applyToTransitions(void delegate(ubyte, const State) func) const {
			mixin applyToTransitionsImpl;
			applyToTransitionsImpl();
		}

		private mixin template applyToTransitionsImpl() {
			void applyToTransitionsImpl() {
				if (is_multiple) with (transitions.multiple) {
					foreach (ubyte l, t; transition_table) {
						if (t !is null) {
							func(l, t);
						}
					}
				} else with (transitions.single) {
					if (to_transition !is null) {
						func(to_letter, to_transition);
					}
				}
			}
		}
	}
}

version(unittest) {
	debug(1) {
		import std.stdio;
	}
}

/* State single/multiple-transition storage testing */
unittest {
	AhoCorasick!4 o = new AhoCorasick!4();
	with (AhoCorasick!4) {
		State s = o.new State(null);
		State s2 = o.new State(null);

		assert(!s.is_multiple);
		assert(s.getTransition(0) is null);
		assert(!s.minimizeMultiple());

		s.modifyTransition(0, s);
		assert(!s.is_multiple);
		assert(s.getTransition(0) is s);
		assert(s.getTransition(1) is null);

		s.modifyTransition(0, s2);
		assert(!s.is_multiple);
		assert(s.getTransition(0) is s2);
		assert(s.getTransition(1) is null);

		s.modifyTransition(0, null);
		assert(!s.is_multiple);
		assert(s.getTransition(0) is null);

		s.modifyTransition(1, s2);
		assert(!s.is_multiple);
		assert(!s.minimizeMultiple());

		s.modifyTransition(0, s);
		assert(s.is_multiple);
		assert(s.getTransition(0) is s);
		assert(s.getTransition(1) is s2);
		assert(s.getTransition(2) is null);

		s.modifyTransition(2, s);
		s.modifyTransition(3, s2);
		assert(s.is_multiple);

		s.modifyTransition(0, null);
		s.modifyTransition(1, null);
		assert(s.is_multiple);
		assert(!s.minimizeMultiple());

		s.modifyTransition(2, null);
		assert(s.is_multiple);
		assert(s.minimizeMultiple());
		assert(!s.is_multiple);

		s.modifyTransition(3, null);
		assert(!s.is_multiple);
	}

	debug(1) {
		writeln("dutil.match: State single/multiple-transition storage test passed");
	}
}

/* Testing deterministic string matching behaviour */
unittest {
	auto o = new AhoCorasick!(4, false, false, print_NA_);
	ubstring[] dictionary = [
		[0, 1, 2, 3],
		[0, 2, 2, 3],
		[2, 3],
		[3],
		[1, 2]];
	assert(o.addWords(dictionary) == 5);
	o.finalize();

	ubstring input = [3, 2, 3, 0, 1, 2, 3, 0, 2, 2, 3];
	Match[] expected = [
		{[3], [], 0},
		{[2, 3], [], 2},
		{[1, 2], [], 5}, /* Indirect match */
		{[0, 1, 2, 3], [], 6},
		{[0, 2, 2, 3], [], 10}];

	auto checkMatches = (Match match) {
		assert(match == expected[0]);
		expected = expected[1 .. $];
	};

	o.matchInput(input, checkMatches);
	assert(!expected.length);

	debug(1) {
		writeln("dutil.match: DFA string matching test passed");
	}
}

/* Testing NFA (skip-enabled) string matching behaviour */
unittest {
	auto o = new AhoCorasick!(5, true, false, print_NA_);
	ubstring[] dictionary = [
		[4, 0, 1, 2],
		[0, 1, 2],
		[0, 4, 1, 2],
		[0, 4, 4, 2],
		[4, 1]];
	assert(o.addWords(dictionary) == 5);
	o.finalize();

	ubstring input = [0, 4, 2, 0, 1, 2, 4, 0, 4, 1, 2];
	Match[] expected = [
		{[0, 4, 4, 2], [1], 2},
		{[0, 4, 4, 2], [2], 2}, /* Distinct match, even though semantically identical - adjacent skips not recommended */
		{[4, 1], [0], 4},
		{[0, 1, 2], [], 5},
		{[4, 0, 1, 2], [0], 5}, /* Initial skips always cause multiple semantically identical matches */
		{[0, 4, 1, 2], [1], 5},
		{[4, 1], [], 9},
		{[4, 1], [0], 9},
		{[0, 4, 1, 2], [], 10}];

	auto checkMatches = (Match match) {
		size_t i = 0;
		while (i < expected.length && match.index <= expected[i].index) {
			/* Reporting order is unspecified, linear search */
			if (match == expected[i]) {
				expected = expected[0 .. i] ~ expected[i + 1 .. $];
				return;
			}
			i++;
		}
		assert(0);
	};

	o.matchInput(input, checkMatches);
	assert(!expected.length);

	debug(1) {
		writeln("dutil.match: NFA string matching test passed");
	}
}

