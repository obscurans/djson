/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */

import std.string, std.format, std.conv, std.math, std.traits, std.exception;

alias JSONobject(Specification...) = JSONobjectImpl!(validateJSONspec!Specification);

struct JSONattributes {
	bool required = false;
}

immutable JSONattributes Default = JSONattributes.init;
immutable JSONattributes Required = JSONattributes(true);

enum JSONnull : bool { BLANK, GIVEN };

private:
/* Basic sanitization to turn a string into a valid D identifier. Simply drops
 * all non-identifier-allowed characters, keeping [a-zA-Z_][0-9a-zA-Z_]* */
pure string sanitize(string name) {
	string ret = removechars(name, "^0-9a-zA-z_");
	munch(ret, "0-9");
	return ret;
}

class ParseException : Exception {
	pure this(string message) {
		super(message);
	}
}

enum bool isJSONobject(T) = (hasMember!(T, "JSONparser") && is(typeof({T x; string y; x.JSONparser(y);}())));

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

/* Changes a void type into a special null wrapper type */
template validateJSONspec(string[] name, Type : void, Tail...)
if (name.length == 2) {
	alias validateJSONspec = validateJSONspec!(name, JSONnull, Tail);
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
	mixin JSONspecError!(Orphan, Tail); // Template to construct error message string
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

/* Template constrained to not trigger on valid start-of-next-field values, per
 * greedy template matching */
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

version(unittest) {
	enum string SpecGroupNames(string prefix, int num : 0) = "";
	enum string SpecGroupNames(string prefix, int num : 1) = `"` ~ prefix ~ `1"`;
	enum string SpecGroupNames(string prefix, int num) = SpecGroupNames!(prefix, num - 1) ~ `,"` ~ prefix ~ to!string(num) ~ `"`;
	enum string createSpecGroup(string prefix, int num) = "alias SpecGroup" ~ prefix ~ "= Tuple!(" ~ SpecGroupNames!("Spec" ~ prefix, num) ~ ");";
}

/* Static unittest: JSON specification validation/expansion */
unittest {
	/* Testing methodology: storing Tuples as compile-time static arrays of
	 * either JSONobject specifications, or groups of specifications */
	/* All these specs are valid and equivalent */
	mixin(createSpecGroup!("A", 6));
	alias SpecA1 = Tuple!("foo", int);
	alias SpecA2 = Tuple!("foo", int, Default);
	alias SpecA3 = Tuple!(["foo"], int);
	alias SpecA4 = Tuple!(["foo"], int, Default);
	alias SpecA5 = Tuple!(["foo", "foo"], int);
	alias SpecA6 = Tuple!(["foo", "foo"], int, Default);

	/* All these specs are valid and equivalent */
	mixin(createSpecGroup!("B", 12));
	alias SpecB1 = Tuple!("foo", int, "bar", int);
	alias SpecB2 = Tuple!("foo", int, "bar", int, Default);
	alias SpecB3 = Tuple!("foo", int, ["bar"], int);
	alias SpecB4 = Tuple!("foo", int, ["bar"], int, Default);
	alias SpecB5 = Tuple!("foo", int, ["bar", "bar"], int);
	alias SpecB6 = Tuple!("foo", int, ["bar", "bar"], int, Default);
	alias SpecB7 = Tuple!(["foo", "foo"], int, Default, "bar", int);
	alias SpecB8 = Tuple!(["foo", "foo"], int, Default, "bar", int, Default);
	alias SpecB9 = Tuple!(["foo", "foo"], int, Default, ["bar"], int);
	alias SpecB10 = Tuple!(["foo", "foo"], int, Default, ["bar"], int, Default);
	alias SpecB11 = Tuple!(["foo", "foo"], int, Default, ["bar", "bar"], int);
	alias SpecB12 = Tuple!(["foo", "foo"], int, Default, ["bar", "bar"], int, Default);

	/* All these specs are valid */
	mixin(createSpecGroup!("I", 1));
	alias SpecI1 = Tuple!();

	/* All these specs are invalid */
	mixin(createSpecGroup!("F", 23));
	alias SpecF1 = Tuple!(3);
	alias SpecF2 = Tuple!([]);
	alias SpecF3 = Tuple!([3]);
	alias SpecF4 = Tuple!(["foo"]);
	alias SpecF5 = Tuple!(int);
	alias SpecF6 = Tuple!(string);
	alias SpecF7 = Tuple!(Default);
	alias SpecF8 = Tuple!("foo", 3);
	alias SpecF9 = Tuple!("foo", []);
	alias SpecF10 = Tuple!("foo", [3]);
	alias SpecF11 = Tuple!("foo", ["foo"]);
	alias SpecF12 = Tuple!("foo", Default);
	alias SpecF13 = Tuple!("foo", int, 3);
	alias SpecF14 = Tuple!("foo", int, []);
	alias specF15 = Tuple!("foo", int, [3]);
	alias SpecF16 = Tuple!("foo", int, ["bar"]);
	alias SpecF17 = Tuple!("foo", int, int);
	alias SpecF18 = Tuple!("foo", int, string);
	alias SpecF19 = Tuple!("foo", int, "bar", 3);
	alias SpecF20 = Tuple!("foo", int, "bar", []);
	alias SpecF21 = Tuple!("foo", int, "bar", [3]);
	alias SpecF22 = Tuple!("foo", int, "bar", ["foo"]);
	alias SpecF23 = Tuple!("foo", int, "bar", Default);

	/* foreach over a tuple is compile-time unrolled to automate test farming */
	foreach (S; Tuple!(SpecGroupA, SpecGroupB, SpecGroupI)) {
		/* All of these specs are valid */
		static assert(__traits(compiles, mixin("JSONobject!" ~ S)));
	}

	foreach (S1; SpecGroupA) {
		foreach (S2; SpecGroupA) {
			/* All of these specs are equivalent to each other */
			static assert(mixin("is(JSONobject!" ~ S1 ~ " == JSONobject!" ~ S2 ~ ")"));
		}
	}

	foreach (S1; SpecGroupB) {
		foreach (S2; SpecGroupB) {
			/* All of these specs are equivalent to each other */
			static assert(mixin("is(JSONobject!" ~ S1 ~ " == JSONobject!" ~ S2 ~ ")"));
		}
	}

	foreach (S; SpecGroupF) {
		/* All of these specs are invalid */
		static assert(!__traits(compiles, mixin("JSONobject!" ~ S)));
	}
}

unittest {
	alias JSONobject!(
		"field", void)
		TestNull;

	alias JSONobject!(
		"single", bool,
		"array", bool[])
		TestBoolean;

	alias JSONobject!(
		"single", int,
		"array", int[])
		TestIntegral;

	alias JSONobject!(
		"single", double,
		"array", double[])
		TestReal;

	alias JSONobject!(
		"single", string,
		"array", string[])
		TestString;

	alias JSONobject!(
		"Array", TestBoolean[],
		"String", TestString,
		"Real", TestReal,
		"Integral", TestIntegral,
		"Boolean", TestBoolean,
		"Null", TestNull)
		TestNested;

	/* Comprehensive test of parsing/printing capabilities */
	TestNested x = TestNested(` { "Null" : { "field" : null } ,
		"Boolean": {"single": true, "array": [true, false, true]},
		"Integral" :{"single" :-1234 ,"array" :[ -1 ,23 ,4 ]} ,
		"Real" : { "single" : 0.123e-4, "array" : [ 0.1, 2, 3e-4 ] },
		"String":{"single":"test","array":["foo","bar","baz"]},
		"Array" : [ {}, { "single":true } ]
		} `);

	assert(x.Boolean.single == true);
	assert(x.Boolean.array == [true, false, true]);
	assert(x.Integral.single == -1234);
	assert(x.Integral.array == [-1, 23, 4]);
	assert(x.Real.single == 0.123e-4);
	assert(x.Real.array == [0.1, 2, 3e-4]);
	assert(x.String.single == "test");
	assert(x.String.array == ["foo", "bar", "baz"]);
	assert(x.Array[1].single == true);

	/* TODO: properly predict and test printing capabilities in toto
	 * Issues: floating-point output */
	assert(x.Null.toString() == `{"field":null}`);
	assert(x.Boolean.toString() == `{"single":true,"array":[true,false,true]}`);
	assert(x.Integral.toString() == `{"single":-1234,"array":[-1,23,4]}`);
	assert(x.String.toString() == `{"single":"test","array":["foo","bar","baz"]}`);

	assertThrown!ParseException(TestNested(`{`));
}

/* Completely generic templated struct (with the names and types of properties
 * given as parameters) which automagically generates members/parser/printer
 * functionality. */
struct JSONobjectImpl(Specification...) {

	static assert(is(this == JSONobjectImpl!(validateJSONspec!Specification)), "Invalid JSON specification");

	mixin JSONvar_decl!Specification;

	string toString() const {
		return "{" ~ JSONprinter!Specification() ~ "}";
	}

	this(string input) {
		JSONparser(input);
	}

private:
	/* Creates valid D code that declares all the variables listed in the JSON
	 * specification. */
	mixin template JSONvar_decl() {}

	mixin template JSONvar_decl(string[] name, Type, JSONattributes attr, Tail...) {
		mixin(Unqual!Type.stringof ~ " " ~ name[1] ~ ";");
		mixin JSONvar_decl!Tail;
	}

	/* Recursive templates to compile-time construct the JSON-format pretty
	 * printer function */
	string JSONprinter()() const {
		return "";
	}

	string JSONprinter(string[] name, Type, JSONattributes attr, Tail...)() const {
		string ret = "";

		void printvar() {
			ret = `"` ~ name[0] ~ `":` ~ JSONprintvar!Type(mixin(name[1]));
		}

		/* Expression tests if null can be implicitly converted to Type, in
		 * which case an explicit null value can be stored for field "name" */
		static if (!attr.required && is(typeof(null) : Type)) {
			if (mixin(name[1]) !is null) {
				printvar();
			}
		} else {
			printvar();
		}

		string tail = JSONprinter!Tail();
		return ret ~ (ret.length && tail.length ? "," : "") ~ tail;
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
			`mixin JSONreadvar!(` ~ Type.stringof ~ ") _read_" ~ name[1] ~ ";" ~
			"_read_" ~ name[1] ~ ".JSONreadvar(" ~ name[1] ~ ");" ~
			"break;" ~
			JSONvar_parsecases!Tail;

	/* Recursive templates to validate the read-in JSON object. Currently just
	 * checks whether non-optional properties that could be null are not null */
	pure void JSONvar_validate()() {}

	pure void JSONvar_validate(string[] name, Type : JSONnull, JSONattributes attr, Tail...)() {
		static if (attr.required) {
			if (mixin(name[1]) == JSONnull.BLANK) {
				throw new ParseException("Mandatory field " ~ name[0] ~ " not given");
			}
		}

		JSONvar_validate!Tail();
	}

	pure void JSONvar_validate(string[] name, Type, JSONattributes attr, Tail...)() {
		static if (attr.required && is(typeof(null) : Type)) {
			if (mixin(name[1]) is null) {
				throw new ParseException("Mandatory field " ~ name[0] ~ " not given");
			}
		}

		JSONvar_validate!Tail();
	}

	/* Compile-time construction of the parser, which takes an input string and
	 * fills in the properties of the JSON object. Destructively modifies the
	 * input string, stripping off the part read. */
	pure void JSONparser(ref string input) {
		void skipWhitespace() {
			munch(input, " \t\r\n");
		}

		bool testCharacter(char c) {
			if (!input.length) {
				throw new ParseException(format("Invalid input, input exhausted"));
			} else {
				return input[0] == c;
			}
		}

		void matchCharacter(char c) {
			if (input.length && input[0] == c) {
				input = input[1 .. $];
			} else {
				throw new ParseException(format("Invalid input, %c expected in: %s", c, input));
			}
		}

		void matchOptionalSeparator() {
			skipWhitespace();
			static if (false) {
				/* Extension of ECMA404 JSON: accepts extraenous ',' property
				 * separators and ignores them */
				// Could be replaced with munch(input, whitespace ~ ",")
				while (munch(input, ",").length) {
					skipWhitespace();
				}
			}
		}

		void matchSeparator() {
			skipWhitespace();
			matchCharacter(',');
			matchOptionalSeparator();
		}

		string parseString() {
			ptrdiff_t lastindex = 0, index = 0;
			string ret = "";

			skipWhitespace();
			matchCharacter('"');
			do {
				index = indexOfAny(input, `\"`, index);
				if (index == -1) {
					throw new ParseException("Invalid input, unterminated string");
				}

				if (input[index] == '"') {
					break; // Unescaped closing "
				} else {
					ret ~= input[lastindex .. index]; // Copy up to escape sequence
					if (index + 1 >= input.length) {
						throw new ParseException("Invalid input, unterminated escape sequence in string");
					}

					switch (input[index + 1]) {
					case '"': case '\\': case '/': ret ~= input[index + 1]; break;
					case 'b': ret ~= '\b'; break;
					case 'f': ret ~= '\f'; break;
					case 'n': ret ~= '\n'; break;
					case 'r': ret ~= '\r'; break;
					case 't': ret ~= '\t'; break;
					case 'u': /*TODO*/ break;
					default:
						static if (false) {
							/* Extension of ECMA404 JSON: accepts invalid escapes,
							 * treated as literal character ignoring backslash */
							ret ~= input[index + 1];
						} else {
							throw new ParseException(format("Invalid input, invalid escape sequence at: %s", input[index .. $]));
						}
					}
					lastindex = index + 2;
				}
			} while (1);

			ret ~= input[lastindex .. index]; // Copy end of string
			input = input[index .. $];
			matchCharacter('"');

			return ret;
		}

		/* Begin body of parse(string) */
		skipWhitespace();
		matchCharacter('{');
		matchOptionalSeparator();

		while (!testCharacter('}')) {
			string name = parseString();
			switch (name) {
				mixin(JSONvar_parsecases!Specification);
				default: throw new ParseException("Invalid field name: " ~ name);
			}

			skipWhitespace();
			if (testCharacter('}')) {
				break;
			}
			matchSeparator();
		}
		input = input[1 .. $];

		mixin JSONvar_validate!Specification;
		JSONvar_validate();
	}
}

/* Various template functions used to print variables to string
 *
 * Base case, directly use toString(), which also handles nested JSONobjects
 */
string JSONprintvar(Type)(const Type var) {
	return to!string(var);
}

/* Null wrapper type should be printed as "null" token */
pure string JSONprintvar(Type : JSONnull)(const Type var) {
	return "null";
}

/* String types need quotation and escaping */
string JSONprintvar(Type : string)(const Type var) {
	/* TODO: re-escape the string */
	return `"` ~ to!string(var) ~ `"`;
}

/* Array types need [] wrapping and , separators, printing each element */
string JSONprintvar(Type : Type[])(const Type[] var) {
	string ret = "[";

	if (var.length) {
		ret ~= JSONprintvar!Type(var[0]);
		foreach (x; var[1 .. $]) {
			ret ~= "," ~ JSONprintvar!Type(x);
		}
	}

	return ret ~ "]";
}

/* Various cases for parsing properties, by type.
 *
 * These functions are invoked by mixin within JSONparser
 */
void JSONreadvar(Type)(ref Type var)
if (!isJSONobject!Type) {
	static assert(0, "Field parsing of type " ~ Type.stringof ~ " currently not implemented");
}

/* A null property requires the exact token "null" */
void JSONreadvar(Type : JSONnull)(ref Type var) {
	if (input[0 .. 4] == "null") {
		var = JSONnull.GIVEN;
		input = input[4 .. $];
	} else {
		throw new ParseException(format(`Invalid input, "null" expected in: %s`, input));
	}
}

/* A boolean property requires one of the exact tokens "true" or "false" */
void JSONreadvar(Type : bool)(ref Type var) {
	if (input[0 .. 4] == "true") {
		var = true;
		input = input[4 .. $];
	} else if (input[0 .. 5] == "false") {
		var = false;
		input = input[5 .. $];
	} else {
		throw new ParseException(format(`Invalid input, "true" or "false" expected in: %s`, input));
	}
}

/* Simple hand-written state machine to match ECMA404 number tokens, returning
 * the prefix matched without semantic interpretation. Destructive read on the
 * input string to match other uses. */
static pure string parseNumber(ref string input) {
	size_t length = 0;

	if (length < input.length && input[length] == '-') { // -?
		length++;
	}

	if (length >= input.length) { // 0|[1-9][0-9]*
	} else if (input[length] == '0') {
		length++;
	} else if (input[length] >= '1' && input[length] <= '9') {
		do {
			length++;
		} while (length < input.length && (input[length] >= '0' && input[length] <= '9'));
	} else {
		throw new ParseException(format("Invalid input, invalid number token at: %s", input));
	}

	if (length < input.length && input[length] == '.') { // (\.[0-9]*)?
		do {
			length++;
		} while (length < input.length && (input[length] >= '0' && input[length] <= '9'));
	}

	if (length < input.length && (input[length] == 'e' || input[length] == 'E')) { // ([eE][+-]?[0-9]*)?
		length++;
		if (length < input.length && (input[length] == '+' || input[length] == '-')) {
			length++;
		}

		while (length < input.length && (input[length] >= '0' && input[length] <= '9')) {
			length++;
		}
	}

	string ret = input[0 .. length];
	input = input[length .. $];
	return ret;
}

/* Integral numeric properties are parsed then interpreted by standard library,
 * which agrees semantically with ECMA404 */
void JSONreadvar(Type : long)(ref Type var) {
	var = to!Type(parseNumber(input));
}

/* Real numeric properties are parsed then interpreted by standard library,
 * which agrees semantically with ECMA404 */
void JSONreadvar(Type : real)(ref Type var) {
	var = to!Type(parseNumber(input));
	static if (false) {
		/* Eventual hook to handle special float values as strings */
	}
}

/* String parsing is already present in JSONparser */
void JSONreadvar(Type : string)(ref Type var) {
	var = parseString();
}

/* Handles statically-typed JSON arrays */
void JSONreadvar(Type : Type[])(ref Type[] var) {
	skipWhitespace();
	matchCharacter('[');
	matchOptionalSeparator();

	while (!testCharacter(']')) {
		var.length++;

		mixin .JSONreadvar!(Type) element;
		element.JSONreadvar(var[$ - 1]);

		skipWhitespace();
		if (testCharacter(']')) {
			break;
		}
		matchSeparator();
	}
	input = input[1 .. $];
}

/* If Type is itself a JSONobject (duck typing only), read it by chaining onto
 * the parser for the inner object. Requires parser to destructively read from
 * input. */
void JSONreadvar(Type)(ref Type var)
if (isJSONobject!Type) {
	var.JSONparser(input);
}

