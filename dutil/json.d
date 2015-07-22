/** Copyright (C) 2015 Jeffrey Tsang. All rights reserved. See /LICENCE.md */

import std.string, std.utf, std.format, std.conv, std.math, std.traits, std.exception;

alias JSONobject(Specification...) = JSONobjectImpl!(validateJSONspec!Specification);

struct JSONattributes {
	bool required = false;
}

immutable JSONattributes Default = JSONattributes.init;
immutable JSONattributes Required = JSONattributes(true);

enum JSONnull : bool { BLANK, GIVEN };

private:
/* Basic sanitization to turn a string into a valid D identifier. Replaces all
 * non-identifier-allowed characters with _, including a leading digit. Length
 * of the input and output strings are equal in code points. */
pure string sanitize(string name)
out(ret) {
	assert(std.utf.count(ret) == std.utf.count(name));
} body {
	if (name.length > 0 && name[0] >= '0' && name[0] <= '9') {
		return "_" ~ tr(name[1 .. $], "0-9a-zA-Z_", "_", "c").idup;
	} else {
		return tr(name, "0-9a-zA-Z_", "_", "c").idup;
	}
}

unittest {
	assert(sanitize("") == "");
	assert(sanitize("_1_Valid_IDENTIFIER") == "_1_Valid_IDENTIFIER");
	assert(sanitize("a-Few@0~BAD!eggs") == "a_Few_0_BAD_eggs");
	assert(sanitize("000_Möŕè") == "_00_M___");
}

class ParseException : Exception {
	pure this(string message) {
		super(message);
	}
}

/* Duck typing test for whether this is a JSON object. Relevant function is the
 * existence of a parser method to fill it in */
enum bool isJSONobject(T) = (hasMember!(T, "JSONparser") && is(typeof({T x; string y; x.JSONparser(y);}())));

/* Helper to enumerate all control codes from 00 to 1F */
immutable string ControlCodes(int x : 0) = "\x00";
immutable string ControlCodes(int x) = ControlCodes!(x - 1) ~ cast(char)x;

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
	/* Raise a compiler error */
	static assert(0, "Invalid JSON specification (property name(s) expected) at: " ~ JSONspecError!(Orphan, Tail));
}

template validateJSONspec(alias Orphan, Tail...) {
	static assert(0, "Invalid JSON specification (property name(s) expected) at: " ~ JSONspecError!(Orphan, Tail));
}

template validateJSONspec(string[] name, alias Orphan, Tail...) {
	static assert(0, "Invalid JSON specification (type expected) at: " ~ JSONspecError!(Orphan, Tail));
}

template validateJSONspec(string[] name, Type, Orphan, Tail...) {
	static assert(0, "Invalid JSON specification (attributes or property name(s) expected) at: " ~ JSONspecError!(Orphan, Tail));
}

/* Template constrained to not trigger on valid start-of-next-field values, per
 * greedy template matching */
template validateJSONspec(string[] name, Type, alias Orphan, Tail...)
if (!is(typeof(Orphan) : string) && !is(typeof(Orphan) : string[]) && !is(typeof(Orphan) : JSONattributes)) {
	static assert(0, "Invalid JSON specification (attributes or property name(s) expected) at: " ~ JSONspecError!(Orphan, Tail));
}

/* Templates to construct compiler error string (strips "Tuple(" off >1 item) */
enum string JSONspecError(Orphan, Tail...) = Tail.length ? Tuple!(Orphan, Tail).stringof[6 .. $ - 1] : Orphan.stringof;
enum string JSONspecError(alias Orphan, Tail...) = Tail.length ? Tuple!(Orphan, Tail).stringof[6 .. $ - 1] : Orphan.stringof;

/* Static unittests: JSON specification validation/expansion
 *
 * Testing methodology: storing Tuples as compile-time static arrays of
 * either JSONobject specifications, or groups of specifications
 */
version(unittest) {
	enum string SpecGroupNames(string prefix, int num : 0) = "";
	enum string SpecGroupNames(string prefix, int num : 1) = `"` ~ prefix ~ `1"`;
	enum string SpecGroupNames(string prefix, int num) = SpecGroupNames!(prefix, num - 1) ~ `,"` ~ prefix ~ to!string(num) ~ `"`;
	enum string createSpecGroup(string prefix, int num) = "alias SpecGroup" ~ prefix ~ "= Tuple!(" ~ SpecGroupNames!("Spec" ~ prefix, num) ~ ");";
}

unittest {
	mixin(createSpecGroup!("A", 6));
	alias SpecA1 = Tuple!("foo", int);
	alias SpecA2 = Tuple!("foo", int, Default);
	alias SpecA3 = Tuple!(["foo"], int);
	alias SpecA4 = Tuple!(["foo"], int, Default);
	alias SpecA5 = Tuple!(["foo", "foo"], int);
	alias SpecA6 = Tuple!(["foo", "foo"], int, Default);

	/* foreach over a tuple is compile-time unrolled to automate test farming */
	foreach (S; SpecGroupA) {
		/* All of these specs are valid */
		static assert(__traits(compiles, mixin("JSONobject!" ~ S)));
	}

	foreach (S1; SpecGroupA) {
		foreach (S2; SpecGroupA) {
			/* All of these specs are equivalent to each other */
			static assert(mixin("is(JSONobject!" ~ S1 ~ " == JSONobject!" ~ S2 ~ ")"));
		}
	}
}

unittest {
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

	foreach (S; SpecGroupB) {
		/* All of these specs are valid */
		static assert(__traits(compiles, mixin("JSONobject!" ~ S)));
	}

	foreach (S1; SpecGroupB) {
		foreach (S2; SpecGroupB) {
			/* All of these specs are equivalent to each other */
			static assert(mixin("is(JSONobject!" ~ S1 ~ " == JSONobject!" ~ S2 ~ ")"));
		}
	}
}

unittest {
	mixin(createSpecGroup!("I", 1));
	alias SpecI1 = Tuple!();

	foreach (S; SpecGroupI) {
		/* All of these specs are valid */
		static assert(__traits(compiles, mixin("JSONobject!" ~ S)));
	}
}

unittest {
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

	foreach (S; SpecGroupF) {
		/* All of these specs are invalid */
		static assert(!__traits(compiles, mixin("JSONobject!" ~ S)));
	}
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
			ret = `"` ~ name[0] ~ `":` ~ JSONprintvar(mixin(name[1]));
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
	 * possible. Chains onto the readvar template to minimize usage of the
	 * utterly powerful string mixing */
	enum string JSONvar_parsecases() = "";
	enum string JSONvar_parsecases(string[] name, Type, JSONattributes attr, Tail...) =
		`case "` ~ name[0] ~ `":` ~
			"skipWhitespace();" ~
			"matchCharacter(':');" ~
			"skipWhitespace();" ~
			"JSONreadvar(input, " ~ name[1] ~ ");" ~
			"break;" ~
			JSONvar_parsecases!Tail;

	/* Recursive templates to validate the read-in JSON object. Currently just
	 * checks whether non-optional properties that could be null are not null */
	pure void JSONvar_validate()() {}

	pure void JSONvar_validate(string[] name, Type : JSONnull, JSONattributes attr, Tail...)() {
		static if (attr.required) {
			if (mixin(name[1]) == JSONnull.BLANK) {
				throw new ParseException("Mandatory property " ~ name[0] ~ " not given");
			}
		}

		JSONvar_validate!Tail();
	}

	pure void JSONvar_validate(string[] name, Type, JSONattributes attr, Tail...)() {
		static if (attr.required && is(typeof(null) : Type)) {
			if (mixin(name[1]) is null) {
				throw new ParseException("Mandatory property " ~ name[0] ~ " not given");
			}
		}

		JSONvar_validate!Tail();
	}

	/* Compile-time construction of the parser, which takes an input string and
	 * fills in the properties of the JSON object. Destructively modifies the
	 * input string, stripping off the part read. */
	pure void JSONparser(ref string input) {
		mixin JSONstringUtilities;

		skipWhitespace();
		matchCharacter('{');
		matchOptionalSeparator();

		while (!testCharacter('}')) {
			string name = JSONparseString(input);
			switch (name) {
				mixin(JSONvar_parsecases!Specification);
				default: throw new ParseException("Invalid property name: " ~ name);
			}

			skipWhitespace();
			if (testCharacter('}')) {
				break;
			}
			matchSeparator();
		}
		matchCharacter('}');

		JSONvar_validate!Specification();
	}
}

/* Various simple string-parsing utility functions and shorthands */
mixin template JSONstringUtilities() {
	void skipWhitespace() {
		munch(input, " \t\r\n");
	}

	bool testCharacter(bool optional = false)(char c) {
		if (!input.length) {
			static if (optional) {
				return false;
			} else {
				throw new ParseException(format("Invalid input, input exhausted"));
			}
		} else {
			return input[0] == c;
		}
	}

	void matchCharacter(size_t offset = 0)(char c) {
		if (input.length > offset && input[offset] == c) {
			input = input[1 .. $];
		} else {
			throw new ParseException(format("Invalid input, %c expected in: %s", c, input[offset .. $]));
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
}

/* Template function used to print variables to string by type */
string JSONprintvar(Type)(const Type var) {
	/* Null wrapper type should be printed as "null" token */
	static if (is(Type : JSONnull)) {
		return "null";
	}
	/* String types need quotation and escaping */
	else static if (is(Type : string)) {
		return `"` ~ JSONescapeString(var) ~ `"`;
	}
	/* Array types need [] wrapping and , separators, printing each element */
	else static if (isArray!Type) {
		string ret = "[";

		if (var.length) {
			ret ~= JSONprintvar(var[0]);
			foreach (x; var[1 .. $]) {
				ret ~= "," ~ JSONprintvar(x);
			}
		}

		return ret ~ "]";
	}
	/* Base case: directly use toString(), which also handles booleans (natively
	 * printed to true/false), and nested JSONobjects (using their own
	 * JSONprinter) */
	else {
		return to!string(var);
	}
}

/* Helper function to properly escape a string for JSON format output */
pure string JSONescapeString(string input) {
	string ret = "";
	ptrdiff_t index;

	while ((index = indexOfAny(input, `\"` ~ ControlCodes!0x1F)) != -1) {
		ret ~= input[0 .. index];
		input = input[index .. $];

		switch (input[0]) {
		case '"': ret ~= `\"`; break;
		case '\\': ret ~= `\\`; break;
		case '\b': ret ~= `\b`; break;
		case '\f': ret ~= `\f`; break;
		case '\n': ret ~= `\n`; break;
		case '\r': ret ~= `\r`; break;
		case '\t': ret ~= `\t`; break;
		default:
			assert(input[0] >= 0x00 && input[0] <= 0x1F);
			ret ~= format(`\u%04X`, input[0]);
		}
		input = input[1 .. $];
	}

	return ret ~ input;
}

/* String encoding unittest */
unittest {
	assert(JSONescapeString("\"\\\b\f\n\r\t") == `\"\\\b\f\n\r\t`);

	foreach (c; 0x00 .. 0x1F) {
		/* Skip control codes with special shortcuts */
		if (c == '\b' || c == '\f' || c == '\n' || c == '\r' || c == '\t') {
			continue;
		}
		/* Remaining control codes are converted correctly to hex escapes */
		assert(JSONescapeString(" " ~ cast(char)c ~ " ") == format(` \u%04X `, c));
	}
}

/* Template function used for parsing properties, by type. */
void JSONreadvar(Type)(ref string input, ref Type var) {
	/* A null property requires the exact token "null" */
	static if (is(Type : JSONnull)) {
		if (input[0 .. 4] == "null") {
			var = JSONnull.GIVEN;
			input = input[4 .. $];
		} else {
			throw new ParseException(format(`Invalid input, "null" expected in: %s`, input));
		}
	}
	/* A boolean property requires one of the exact tokens "true" or "false" */
	else static if (is(Type : bool)) {
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
	/* Integral numeric properties are parsed then interpreted by standard
	 * library, which agrees semantically with ECMA404 */
	else static if (is(Type : long)) {
		var = to!Type(parseNumber(input));
	}
	/* Real numeric properties are parsed then interpreted by standard library,
	 * which agrees semantically with ECMA404 */
	else static if (is(Type : real)) {
		var = to!Type(parseNumber(input));
		static if (false) {
			/* Eventual hook to handle special float values as strings */
		}
	}
	/* Chain onto string parser function */
	else static if (is(Type : string)) {
		var = JSONparseString(input);
	}
	/* Handles statically-typed JSON arrays much the same as objects */
	else static if (isArray!Type) {
		mixin JSONstringUtilities;

		skipWhitespace();
		matchCharacter('[');
		matchOptionalSeparator();

		while (!testCharacter(']')) {
			var.length++;

			JSONreadvar(input, var[$ - 1]);

			skipWhitespace();
			if (testCharacter(']')) {
				break;
			}
			matchSeparator();
		}
		matchCharacter(']');
	}
	/* If Type is itself a JSONobject (duck typing only), read it by chaining
	 * onto the parser for the inner object. Requires parser to destructively
	 * read from input. */
	else static if (isJSONobject!Type) {
		var.JSONparser(input);
	} else {
		static assert(0, "Property parsing of type " ~ Type.stringof ~ " not implemented");
	}
}

/* Simple hand-written state machine to match ECMA404 number tokens, returning
 * the prefix matched without semantic interpretation. Destructive read on the
 * input string to match other uses. */
pure string parseNumber(ref string input) {
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

/* Main JSON string parsing function. Handles all escape sequences and decodes
 * them. Destructive read on the input string to match other uses. */
pure string JSONparseString(ref string input) {
	mixin JSONstringUtilities;

	ptrdiff_t index = 0;
	string ret = "";

	/* Handles the mess that is \uXXXX escapes within JSON strings.
	 *
	 * Precondition: first two characters in input already match `\u`
	 * Function maintains an artifical offset of 2 chars into input to simplify
	 * post-handling of all escape codes (always strip 2 chars)
	 *
	 * Strategy: decode consecutive \uXXXX into width-2 wchar buffer. Whenever
	 * buffer fills or no more \uXXXX in sequence, decode first codepoint
	 * destructively from buffer to output (as UTF-8)
	 */
	void handleHexEscapes() {
		wstring buf;
		char[4] encoded;

		wchar readHexEscapeDigits() {
			wchar ret = 0;

			foreach (i; 0 .. 4) {
				if (input.length - 2 <= 0) {
					throw new ParseException("Invalid input, unterminated hex escape");
				} else if (inPattern(input[2 + 0], "0-9")) {
					ret <<= 4;
					ret |= (input[2 + 0] - '0') & 15;
				} else if (inPattern(input[2 + 0], "A-F")) {
					ret <<= 4;
					ret |= (input[2 + 0] - 'A' + 10) & 15;
				} else {
					throw new ParseException(format("Invalid input, invalid hex digit in escape code at: %s", input));
				}

				input = input[1 .. $];
			}

			return ret;
		}

		wchar readHexEscape() {
			matchCharacter!2('\\');
			matchCharacter!2('u');
			return readHexEscapeDigits();
		}

		void decodeBuffer() {
			dchar decoded;
			try {
				decoded = decodeFront(buf);
			} catch (UTFException e) {
				throw new ParseException(format("Invalid input, invalid UTF-16 escape codes before: %s", input));
			}

			size_t en_len = encode(encoded, decoded);
			ret ~= encoded[0 .. en_len];
		}

		/* Begin body of void JSONparseString.handleHexEscapes() */
		buf = [readHexEscapeDigits()];

		while (input.length - 2 >= 2 && input[2 + 0] == '\\' && input[2 + 1] == 'u') {
			buf ~= readHexEscape();

			if (buf.length == 2) {
				decodeBuffer();
			}
		}

		if (buf.length == 1) { // Decode dangling last UTF-16 codeunit
			decodeBuffer();
		} else if (buf.length == 2) {
			assert(0);
		}
	}

	/* Begin body of pure string JSONparseString(ref string input) */
	skipWhitespace();
	matchCharacter('"');
	do {
		/* For whatever reason, patterns don't work on control codes, which
		 * ECMA404 disallows naked in strings */
		index = indexOfAny(input, `\"` ~ ControlCodes!0x1F);
		if (index == -1) {
			throw new ParseException("Invalid input, unterminated string");
		} else if (input[index] >= 0x00 && input[index] <= 0x1F) {
			throw new ParseException(format("Invalid input, naked 00-1F control code in string at: %s", input[index .. $]));
		} else if (input[index] == '"') {
			break; // Unescaped closing "
		} else {
			ret ~= input[0 .. index]; // Copy up to escape sequence
			input = input[index .. $];

			if (input.length <= 1) {
				throw new ParseException("Invalid input, unterminated escape sequence in string");
			}

			switch (input[1]) {
			case '"':
			case '\\':
			case '/': ret ~= input[1]; break;
			case 'b': ret ~= '\b'; break;
			case 'f': ret ~= '\f'; break;
			case 'n': ret ~= '\n'; break;
			case 'r': ret ~= '\r'; break;
			case 't': ret ~= '\t'; break;
			case 'u': handleHexEscapes(); break;
			default:
				static if (false) {
					/* Extension of ECMA404 JSON: accepts invalid escapes,
					 * treated as literal character ignoring backslash */
					ret ~= input[1];
				} else {
					throw new ParseException(format("Invalid input, invalid escape sequence at: %s", input));
				}
			}

			input = input[2 .. $];
		}
	} while (1);

	ret ~= input[0 .. index]; // Copy end of string
	input = input[index .. $];
	matchCharacter('"');

	return ret;
}

/* String decoding unittest */
unittest {
	string input = `"\"\\\/\b\f\n\r\t"`;
	assert(JSONparseString(input) == "\"\\/\b\f\n\r\t");

	input = `"\u0022\u005C\u002F\u0008\u000C\u000A\u000D\u0009"`;
	assert(JSONparseString(input) == "\"\\/\b\f\n\r\t");

	foreach (c; 0x00 .. 0x1F) {
		/* Control codes in hex escapes are converted correctly */
		input = format(`" \u%04X "`, c);
		assert(JSONparseString(input) == " " ~ cast(char)c ~ " ");
	}

	/* Test character: U+10428 DESERET SMALL LETTER LONG I
	 * Representation in UTF-8: 0xF0_90_90_A8
	 * Representation in UTF-16: 0xD801_DC28 */
	input = `"\n\uD801\uDC28\t"`;
	assert(JSONparseString(input) == "\n\xF0\x90\x90\xA8\t");

	input = `"\f\uD801"`;
	assertThrown!ParseException(JSONparseString(input));
}

/* General parsing/writing unittest */
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

