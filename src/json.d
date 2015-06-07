/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */

import std.string, std.format, std.conv, std.traits, std.math;

alias JSONobject(Specification...) = JSONobjectImpl!(validateJSONspec!Specification);

struct JSONattributes {
	bool required = false;
}

immutable JSONattributes Default = JSONattributes.init;
immutable JSONattributes Required = JSONattributes(true);

private:
/* Basic sanitization to turn a string into a valid D identifier. Simply drops
 * all non-identifier-allowed characters, keeping [a-zA-Z_][0-9a-zA-Z_]*
 */
pure string sanitize(string name) {
	string ret = removechars(name, "^0-9a-zA-z_");
	munch(ret, "0-9");
	return ret;
}

class ParseException : Exception {
	this(string message) {
		super(message);
	}
}

/* Standard dummy template that simply wraps its template arguments as a
 * palpable type, for chaining template argument manipulations */
template Tuple(T...) {
	alias Tuple = T;
}

/* Compile-time validation template for JSON specifications
 *
 * Functional-style mutually recursive template metaprogramming to validate and
 * expand the given specification with omitted default arguments.
 *
 * Base case: empty specification is valid and a fixed point
 */
template validateJSONspec() {
	alias validateJSONspec = Tuple!();
}

/* Expands a single string into an identical pair for JSON/D names */
template validateJSONspec(string name, Tail...) {
	alias validateJSONspec = validateJSONspec!([name, name], Tail);
}

/* Expands a singleton string array into an identical pair for JSON/D names */
template validateJSONspec(string[] name, Tail...)
if (name.length == 1) {
	alias validateJSONspec = validateJSONspec!([name[0], name[0]], Tail);
}

/* Using template specialization rules, detects omission of the JSONattributes
 * parameter, and inserts the default */
template validateJSONspec(string[] name, Type, Tail...)
if (name.length == 2) {
	alias validateJSONspec = validateJSONspec!(name, Type, Default, Tail);
}

/* Fully expanded single property in the declaration. Sanitizes the D name to
 * ensure it is a valid identifier, and chains the template */
template validateJSONspec(string[] name, Type, JSONattributes attr, Tail...)
if (name.length == 2) {
	alias validateJSONspec = Tuple!([name[0], sanitize(name[1])], Type, attr, validateJSONspec!Tail);
}

/* Extra templates to cover the field of invalid specifications, to help with
 * error messages. */
template validateJSONspec(Orphan, Tail...) {
	mixin JSONspecError!(Orphan, Tail); /* Template to construct error message string */
	/* Raise a compiler error */
	static assert(0, "Invalid JSON specification (property name(s) expected) at: " ~ specErrorString);
}

template validateJSONspec(alias Orphan, Tail...) {
	mixin JSONspecError!(Orphan, Tail);
	static assert(0, "Invalid JSON specification (property name(s) expected) at: " ~ specErrorString);
}

template validateJSONspec(string[] name, alias Orphan, Tail...) {
	mixin JSONspecError!(Orphan, Tail);
	static assert(0, "Invalid JSON specification (type expected) at: " ~ specErrorString);
}

template validateJSONspec(string[] name, Type, Orphan, Tail...) {
	mixin JSONspecError!(Orphan, Tail);
	static assert(0, "Invalid JSON specification (attributes or property name(s) expected) at: " ~ specErrorString);
}

template validateJSONspec(string[] name, Type, alias Orphan, Tail...)
if (!is(typeof(Orphan) : string) && !is(typeof(Orphan) : string[]) && !is(typeof(Orphan) : JSONattributes)) {
	mixin JSONspecError!(Orphan, Tail);
	static assert(0, "Invalid JSON specification (attributes or property name(s) expected) at: " ~ specErrorString);
}

mixin template JSONspecError(Orphan, Tail...) {
	enum string specErrorString = Tail.length ? Tuple!(Orphan, Tail).stringof[6 .. $ - 1] : Orphan.stringof;
}

mixin template JSONspecError(alias Orphan, Tail...) {
	enum string specErrorString = Tail.length ? Tuple!(Orphan, Tail).stringof[6 .. $ - 1] : Orphan.stringof;
}

/* Completely generic templated struct (with the names and types of fields given
 * as parameters) which automagically generates members/parser/printer
 * functionality.
 */
struct JSONobjectImpl(Specification...) {

	static assert(is(typeof(this) == JSONobjectImpl!(validateJSONspec!Specification)), "Invalid JSON specification");

	mixin JSONvar_decl!Specification;

	pure string toString() const {
		mixin JSONprinter!Specification;
		return "{" ~ print() ~ "}";
	}

	this(string input) {
		mixin JSONparser!Specification;
		parse(input);
	}

private:
	/* Creates valid D code that declares all the variables listed in the JSON
	 * specification.
	 */
	mixin template JSONvar_decl() {}

	mixin template JSONvar_decl(string[] name, Type, JSONattributes attr, Tail...) {
		mixin(Unqual!Type.stringof ~ " " ~ sanitize(name[1]) ~ ";");
		mixin JSONvar_decl!Tail;
	}

	/* Recursive templates to compile-time construct the JSON-format pretty
	 * printer function */
	mixin template JSONprinter() {
		string print() {
			return "";
		}
	}

	mixin template JSONprinter(string[] name, Type, JSONattributes attr, Tail...) {
		string print() {
			string ret = "";

			void printvar() {
				ret = `"` ~ name[0] ~ `":`;
				static if (isSomeString!Type) {
					ret ~= `"`;
				}
				ret ~= to!string(mixin(name[1]));
				static if (isSomeString!Type) {
					ret ~= `"`;
				}
			}

			/* Expression tests if null can be implicitly converted to Type, in
			 * which case an explicit null value can be stored for field "name"
			 */
			static if (!attr.required && is(typeof(null) : Type)) {
				if (mixin(name[1]) !is null) {
					printvar();
				}
			} else {
				printvar();
			}

			mixin JSONprinter!Tail tailprinter;
			string tail = tailprinter.print();
			return ret ~ (ret.length && tail.length ? "," : "") ~ tail;
		}
	}

	/* Compile-time string for the switch statement to handle all the property
	 * names. Requires a significant string mixin as regular mixins must be
	 * complete declarations, i.e. inserting a single case statement is not
	 * possible. Chains onto the _readvar template to minimize usage of the
	 * utterly powerful string mixing */
	enum string JSONvar_parsecases() = "";
	enum string JSONvar_parsecases(string[] name, Type, JSONattributes attr, Tail...) =
		`case "` ~ name[0] ~ `":` ~
			"skipWhitespace();" ~
			"matchCharacter(':');" ~
			"skipWhitespace();" ~
			`mixin JSONreadvar!("` ~ name[1] ~ `",` ~ Type.stringof ~ ") _read_" ~ name[1] ~ ";" ~
			"_read_" ~ name[1] ~ ".read();" ~
			"break;" ~ JSONvar_parsecases!Tail;

	/* Various cases for parsing properties, by type. Currently only strings are
	 * implemented */
	mixin template JSONreadvar(string name, Type) {
		void read() {
			static assert(0, "Field parsing of type " ~ Type.stringof ~ " currently not implemented");
		}
	}

	mixin template JSONreadvar(string name, Type : void) {
		void read() {
		}
	}

	mixin template JSONreadvar(string name, Type : bool) {
		void read() {
		}
	}

	mixin template JSONreadvar(string name, Type : long) {
		void read() {
		}
	}

	mixin template JSONreadvar(string name, Type : real) {
		void read() {
		}
	}

	mixin template JSONreadvar(string name, Type : string) {
		void read() {
			mixin(sanitize(name)) = parseString();
		}
	}

	/* Recursive templates to validate the read-in JSON object. Currently just
	 * checks whether non-optional properties that could be null are not null */
	mixin template JSONvar_validate() {
		void validate() {}
	}

	mixin template JSONvar_validate(string[] name, Type, JSONattributes attr, Tail...) {
		void validate() {
			static if (attr.required && is(typeof(null) : Type)) {
				if (mixin(name[1]) is null) {
					throw new ParseException("Mandatory field " ~ name[0] ~ " not given");
				}
			}

			mixin JSONvar_validate!Tail tail;
			tail.validate();
		}
	}

	/* Compile-time construction of the parser, which takes an input string and
	 * fills in the properties of the JSON object */
	mixin template JSONparser(Specification...) {
		void parse(string input) {
			/* Nested functions (and templates) are allowed, and they have full
			 * access to variables in enclosing scope
			 */
			void skipWhitespace() {
				munch(input, " \t\r\n");
			}

			void matchCharacter(char c) {
				if (input[0] == c) {
					input = input[1 .. $];
				} else {
					throw new ParseException(format("Invalid input, %c expected in: %s", c, input));
				}
			}

			string parseString() {
				ptrdiff_t index = 0;
				skipWhitespace();
				matchCharacter('"');
				do {
					index = indexOfAny(input, `\"`, index);
					if (index == -1) {
						throw new ParseException("Invalid input, unterminated string");
					}

					if (input[index] == '"') {
						break; /* Unescaped closing " */
					} else {
						index += 2; /* Blindly skip next character after \ */
					}
				} while (1);

				if (index > input.length) {
					throw new ParseException("Invalid input, unterminated escape sequence in string");
				}

				string ret = input[0 .. index];
				input = input[index .. $];
				matchCharacter('"');

				return ret;
			}

			/* Begin body of parse(string) */
			skipWhitespace();
			matchCharacter('{');

			do {
				do {
					skipWhitespace();
				} while (munch(input, ",").length);
				if (input[0] == '}') {
					break;
				}

				string name = parseString();
				switch (name) {
					mixin(JSONvar_parsecases!Specification);
					default: throw new ParseException("Invalid field name: " ~ name);
				}
			} while (1);

			mixin JSONvar_validate!Specification;
			validate();
		}
	}
}

