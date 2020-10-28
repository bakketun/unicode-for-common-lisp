# Basic protocol for Unicode in Common Lisp

## Type code-point

An object of type `code-point` is a designator for a Unicode code point.

### Numbers as code-point

A number of type `(integer 0 1114111)` denotes the code point with the
numbers value.

### Characters as code-point

Standard characters denotes code points as in the table below.

If an implenetation supports any of the semi standard characters, they
denote code points as in the table below.

It is implenetation defined what code point, if any, `Newline`
denotes. If supported, `Linefeed` always denotes U+000A. This implies
that if `Newline` and `Linefeed` are the same character, they both
denotes U+000A.

`code-point` and `character` are always overlapping types. Depending
on implenetation `code-point` can be a subtype of `character` or
`character` can be a subtype of `code-point`. A conforming program
must not assume that all code points can be denoted by a character or
that all characters denotes a code point.

### Function code-point-p

Returns true if object denotes a code point, false otherwise.

When object is a code point, the returned value is the integer
represantion of that code point.

### Function code-point-code

Same as code-point-p, but signals an error if object does not denote a
code point.

### Semi standard characters as code point

| Character   | Code Point | Unicode Name         |
|:------------|:----------:|:---------------------|
| `Backspace` | U+0008     | BACKSPACE            |
| `Tab`       | U+0009     | CHARACTER TABULATION |
| `Linefeed`  | U+000A     | LINE FEED (LF)       |
| `Page`      | U+000C     | FORM FEED (FF)       |
| `Return`    | U+000D     | CARRIAGE RETURN (CR) |
| `Rubout`    | U+007F     | DELETE               |
    
### Standard characters as code point

| Character | Code Point | Unicode Name           |
|:----------|:----------:|:-----------------------|
| `Newline` | ?          |                        |
| `Space`   | U+0020     | SPACE                  |
| `!`       | U+0021     | EXCLAMATION MARK       |
| `"`       | U+0022     | QUOTATION MARK         |
| `#`       | U+0023     | NUMBER SIGN            |
| `$`       | U+0024     | DOLLAR SIGN            |
| `%`       | U+0025     | PERCENT SIGN           |
| `&`       | U+0026     | AMPERSAND              |
| `'`       | U+0027     | APOSTROPHE             |
| `(`       | U+0028     | LEFT PARENTHESIS       |
| `)`       | U+0029     | RIGHT PARENTHESIS      |
| `*`       | U+002A     | ASTERISK               |
| `+`       | U+002B     | PLUS SIGN              |
| `,`       | U+002C     | COMMA                  |
| `-`       | U+002D     | HYPHEN-MINUS           |
| `.`       | U+002E     | FULL STOP              |
| `/`       | U+002F     | SOLIDUS                |
| `0`       | U+0030     | DIGIT ZERO             |
| ⋮         | ⋮          | ⋮                      |
| `9`       | U+0039     | DIGIT NINE             |
| `:`       | U+003A     | COLON                  |
| `;`       | U+003B     | SEMICOLON              |
| `<`       | U+003C     | LESS-THAN SIGN         |
| `=`       | U+003D     | EQUALS SIGN            |
| `>`       | U+003E     | GREATER-THAN SIGN      |
| `?`       | U+003F     | QUESTION MARK          |
| `@`       | U+0040     | COMMERCIAL AT          |
| `A`       | U+0041     | LATIN CAPITAL LETTER A |
| ⋮         | ⋮          |                        |
| `Z`       | U+005A     | LATIN CAPITAL LETTER Z |
| `[`       | U+005B     | LEFT SQUARE BRACKET    |
| `\`       | U+005C     | REVERSE SOLIDUS        |
| `]`       | U+005D     | RIGHT SQUARE BRACKET   |
| `^`       | U+005E     | CIRCUMFLEX ACCENT      |
| `_`       | U+005F     | LOW LINE               |
| `` ` ``   | U+0060     | GRAVE ACCENT           |
| `a`       | U+0061     | LATIN SMALL LETTER A   |
| ⋮         | ⋮          |                        |
| `z`       | U+007A     | LATIN SMALL LETTER Z   |
| `{`       | U+007B     | LEFT CURLY BRACKET     |
| `\|`      | U+007C     | VERTICAL LINE          |
| `}`       | U+007D     | RIGHT CURLY BRACKET    |
| `~`       | U+007E     | TILDE                  |
