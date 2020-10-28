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

| Character   | Code Point |
|:------------|:----------:|
| `Backspace` | U+0008     |
| `Tab`       | U+0009     |
| `Linefeed`  | U+000A     |
| `Page`      | U+000C     |
| `Return`    | U+000D     |
| `Rubout`    | U+007F     |
    
### Standard characters as code point

| Character | Code Point |
|:----------|:----------:|
| `Newline` | ?          |
| `Space`   | U+0020     |
| `!`       | U+0021     |
| `"`       | U+0022     |
| `#`       | U+0023     |
| `$`       | U+0024     |
| `%`       | U+0025     |
| `&`       | U+0026     |
| `'`       | U+0027     |
| `(`       | U+0028     |
| `)`       | U+0029     |
| `*`       | U+002A     |
| `+`       | U+002B     |
| `,`       | U+002C     |
| `-`       | U+002D     |
| `.`       | U+002E     |
| `/`       | U+002F     |
| `0`       | U+0030     |
| ⋮         | ⋮          |
| `9`       | U+0039     |
| `:`       | U+003A     |
| `;`       | U+003B     |
| `<`       | U+003C     |
| `=`       | U+003D     |
| `>`       | U+003E     |
| `?`       | U+003F     |
| `@`       | U+0040     |
| `A`       | U+0041     |
| ⋮         | ⋮          |
| `Z`       | U+005A     |
| `[`       | U+005B     |
| `\`       | U+005C     |
| `]`       | U+005D     |
| `^`       | U+005E     |
| `_`       | U+005F     |
| `` ` ``   | U+0060     |
| `a`       | U+0061     |
| ⋮         | ⋮          |
| `z`       | U+007A     |
| `{`       | U+007B     |
| `\|`      | U+007C     |
| `}`       | U+007D     |
| `~`       | U+007E     |
