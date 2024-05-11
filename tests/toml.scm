;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Lars-Dominik Braun <lars@6xq.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (tests toml)
  #:use-module (rustup build toml)
  #:use-module (guix tests)
  #:use-module (srfi srfi-19) ; For datetime.
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "toml")

;; Tests taken from https://toml.io/en/v1.0.0

(test-error "parse-toml: Unspecified key"
  &file-not-consumed
  (parse-toml "key = # INVALID"))

(test-error "parse-toml: Missing EOL"
  &file-not-consumed
  (parse-toml "first = \"Tom\" last = \"Preston-Werner\" # INVALID"))

(test-equal "parse-toml: Bare keys"
  '(("key" . "value") ("bare_key" . "value") ("bare-key" . "value") ("1234" . "value"))
  (parse-toml "key = \"value\"
bare_key = \"value\"
bare-key = \"value\"
1234 = \"value\""))

(test-equal "parse-toml: Quoted keys"
  '(("127.0.0.1" . "value")
    ("character encoding" . "value")
    ("ʎǝʞ" . "value")
    ("key2" . "value")
    ("quoted \"value\"" . "value"))
  (parse-toml "\"127.0.0.1\" = \"value\"
\"character encoding\" = \"value\"
\"ʎǝʞ\" = \"value\"
'key2' = \"value\"
'quoted \"value\"' = \"value\""))

(test-equal "parse-toml: No key"
  #f
  (parse-toml "= \"no key name\""))

(test-equal "parse-toml: Empty key"
  '(("" . "blank"))
  (parse-toml "\"\" = \"blank\""))

(test-equal "parse-toml: Dotted keys"
  '(("name" . "Orange")
    ("physical" ("color" . "orange")
                ("shape" . "round"))
    ("site" ("google.com" . #t)))
  (parse-toml "name = \"Orange\"
physical.color = \"orange\"
physical.shape = \"round\"
site.\"google.com\" = true"))

(test-equal "parse-toml: Dotted keys with whitespace"
  '(("fruit" ("name" . "banana") ("color" . "yellow") ("flavor" . "banana")))
  (parse-toml "fruit.name = \"banana\"     # this is best practice
fruit. color = \"yellow\"    # same as fruit.color
fruit . flavor = \"banana\"   # same as fruit.flavor"))

(test-error "parse-toml: Multiple keys"
  &already-defined
  (parse-toml "name = \"Tom\"
name = \"Pradyun\""))

(test-equal "parse-toml: Implicit tables"
  '(("fruit" ("apple" ("smooth" . #t)) ("orange" . 2)))
  (parse-toml "fruit.apple.smooth = true
fruit.orange = 2"))

(test-error "parse-toml: Write to value"
  &already-defined
  (parse-toml "fruit.apple = 1
fruit.apple.smooth = true"))

(test-equal "parse-toml: String"
  '(("str" . "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."))
  (parse-toml "str = \"I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\\tSF.\""))

(test-equal "parse-toml: Empty string"
  '(("str1" . "")
    ("str2" . "")
    ("str3" . "")
    ("str4" . ""))
  (parse-toml "str1 = \"\"
str2 = ''
str3 = \"\"\"\"\"\"
str4 = ''''''"))

(test-equal "parse-toml: Multi-line basic strings"
  '(("str1" . "Roses are red\nViolets are blue")
    ("str2" . "The quick brown fox jumps over the lazy dog.")
    ("str3" . "The quick brown fox jumps over the lazy dog.")
    ("str4" . "Here are two quotation marks: \"\". Simple enough.")
    ("str5" . "Here are three quotation marks: \"\"\".")
    ("str6" . "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\".")
    ("str7" . "\"This,\" she said, \"is just a pointless statement.\""))
  (parse-toml "str1 = \"\"\"
Roses are red
Violets are blue\"\"\"

str2 = \"\"\"
The quick brown \\


  fox jumps over \\
    the lazy dog.\"\"\"

str3 = \"\"\"\\
       The quick brown \\
       fox jumps over \\
       the lazy dog.\\
       \"\"\"

str4 = \"\"\"Here are two quotation marks: \"\". Simple enough.\"\"\"
# str5 = \"\"\"Here are three quotation marks: \"\"\".\"\"\"  # INVALID
str5 = \"\"\"Here are three quotation marks: \"\"\\\".\"\"\"
str6 = \"\"\"Here are fifteen quotation marks: \"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\".\"\"\"

# \"This,\" she said, \"is just a pointless statement.\"
str7 = \"\"\"\"This,\" she said, \"is just a pointless statement.\"\"\"\""))

(test-equal "parse-toml: Literal string"
  '(("winpath" . "C:\\Users\\nodejs\\templates")
    ("winpath2" . "\\\\ServerX\\admin$\\system32\\")
    ("quoted" . "Tom \"Dubs\" Preston-Werner")
    ("regex" . "<\\i\\c*\\s*>"))
  (parse-toml "winpath  = 'C:\\Users\\nodejs\\templates'
winpath2 = '\\\\ServerX\\admin$\\system32\\'
quoted   = 'Tom \"Dubs\" Preston-Werner'
regex    = '<\\i\\c*\\s*>'"))

(test-equal "parse-toml: Multi-line literal strings"
  '(("regex2" . "I [dw]on't need \\d{2} apples")
    ("lines" . "The first newline is\ntrimmed in raw strings.\n   All other whitespace\n   is preserved.\n")
    ("quot15" . "Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"")
    ("apos15" . "Here are fifteen apostrophes: '''''''''''''''")
    ("str" . "'That,' she said, 'is still pointless.'"))
  (parse-toml "regex2 = '''I [dw]on't need \\d{2} apples'''
lines  = '''
The first newline is
trimmed in raw strings.
   All other whitespace
   is preserved.
'''
quot15 = '''Here are fifteen quotation marks: \"\"\"\"\"\"\"\"\"\"\"\"\"\"\"'''

# apos15 = '''Here are fifteen apostrophes: ''''''''''''''''''  # INVALID
apos15 = \"Here are fifteen apostrophes: '''''''''''''''\"

# 'That,' she said, 'is still pointless.'
str = ''''That,' she said, 'is still pointless.''''"))

(test-equal "parse-toml: Decimal integer"
  '(("int1" . 99) ("int2" . 42) ("int3" . 0) ("int4" . -17))
  (parse-toml "int1 = +99
int2 = 42
int3 = 0
int4 = -17"))

(test-equal "parse-toml: Decimal integer underscores"
 '(("int5" . 1000) ("int6" . 5349221) ("int7" . 5349221) ("int8" . 12345))
 (parse-toml "int5 = 1_000
int6 = 5_349_221
int7 = 53_49_221  # Indian number system grouping
int8 = 1_2_3_4_5  # VALID but discouraged"))

(test-equal "parse-toml: Hexadecimal"
 `(("hex1" . ,#xdeadbeef) ("hex2" . ,#xdeadbeef) ("hex3" . ,#xdeadbeef))
 (parse-toml "hex1 = 0xDEADBEEF
hex2 = 0xdeadbeef
hex3 = 0xdead_beef"))

(test-equal "parse-toml: Octal"
 `(("oct1" . ,#o01234567) ("oct2" . #o755))
 (parse-toml "oct1 = 0o01234567
oct2 = 0o755"))

(test-equal "parse-toml: Binary"
 `(("bin1" . ,#b11010110))
 (parse-toml "bin1 = 0b11010110"))

(test-equal "parse-toml: Float"
 '(("flt1" . 1.0)
   ("flt2" . 3.1415)
   ("flt3" . -0.01)
   ("flt4" . 5e+22)
   ("flt5" . 1e06)
   ("flt6" . -2e-2)
   ("flt7" . 6.626e-34)
   ("flt8" . 224617.445991228))
 (parse-toml "# fractional
flt1 = +1.0
flt2 = 3.1415
flt3 = -0.01

# exponent
flt4 = 5e+22
flt5 = 1e06
flt6 = -2E-2

# both
flt7 = 6.626e-34

flt8 = 224_617.445_991_228"))

(test-equal "parse-toml: Float"
 '(("sf1" . +inf.0)
   ("sf2" . +inf.0)
   ("sf3" . -inf.0)
   ("sf4" . +nan.0)
   ("sf5" . +nan.0)
   ("sf6" . -nan.0))
 (parse-toml "# infinity
sf1 = inf  # positive infinity
sf2 = +inf # positive infinity
sf3 = -inf # negative infinity

# not a number
sf4 = nan  # actual sNaN/qNaN encoding is implementation-specific
sf5 = +nan # same as `nan`
sf6 = -nan # valid, actual encoding is implementation-specific"))

(test-equal "parse-toml: Boolean"
 '(("bool1" . #t)
   ("bool2" . #f))
 (parse-toml "bool1 = true
bool2 = false"))

(test-equal "parse-toml: Offset date-time"
 `(("odt1" . ,(make-date #f 0 32 7 27 5 1979 0))
   ("odt2" . ,(make-date #f 0 32 0 27 5 1979 (* -7 60 60)))
   ("odt3" . ,(make-date 999999 0 32 0 27 5 1979 (* 7 60 60)))
   ("odt4" . ,(make-date #f 0 32 7 27 5 1979 0)))
 (parse-toml "odt1 = 1979-05-27T07:32:00Z
odt2 = 1979-05-27T00:32:00-07:00
odt3 = 1979-05-27T00:32:00.999999+07:00
odt4 = 1979-05-27 07:32:00Z"))

(test-equal "parse-toml: Local date-time"
 `(("ldt1" . ,(make-date #f 0 32 7 27 5 1979 #f))
   ("ldt2" . ,(make-date 999999 0 32 0 27 5 1979 #f)))
 (parse-toml "ldt1 = 1979-05-27T07:32:00
ldt2 = 1979-05-27T00:32:00.999999"))

(test-equal "parse-toml: Local date"
 `(("ld1" . ,(make-date #f #f #f #f 27 5 1979 #f)))
 (parse-toml "ld1 = 1979-05-27"))

(test-equal "parse-toml: Local time"
 `(("lt1" . ,(make-date #f 0 32 7 #f #f #f #f))
   ("lt2" . ,(make-date 999999 0 32 0 #f #f #f #f)))
 (parse-toml "lt1 = 07:32:00
lt2 = 00:32:00.999999"))

(test-equal "parse-toml: Arrays"
 '(("integers" 1 2 3)
   ("colors" "red" "yellow" "green")
   ("nested_arrays_of_ints" (1 2) (3 4 5))
   ("nested_mixed_array" (1 2) ("a" "b" "c"))
   ("string_array" "all" "strings")
   ("numbers" 0.1 0.2 0.5 1 2 5)
   ("contributors" "Foo Bar <foo@example.com>" (("name" . "Baz Qux") ("email" . "bazqux@example.com") ("url" . "https://example.com/bazqux")))
   ("integers2" 1 2 3)
   ("integers3" 1 2))
 (parse-toml "integers = [ 1, 2, 3 ]
colors = [ \"red\", \"yellow\", \"green\" ]
nested_arrays_of_ints = [ [ 1, 2 ], [3, 4, 5] ]
nested_mixed_array = [ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]
string_array = [ \"all\", 'strings' ]

# Mixed-type arrays are allowed
numbers = [ 0.1, 0.2, 0.5, 1, 2, 5 ]
contributors = [
  \"Foo Bar <foo@example.com>\",
  { name = \"Baz Qux\", email = \"bazqux@example.com\", url = \"https://example.com/bazqux\" }
]

integers2 = [
  1, 2, 3
]

integers3 = [
  1,
  2, # this is ok
]"))

(test-equal "parse-toml: Tables"
 '(("table-1" ("key1" . "some string")
              ("key2" . 123))
   ("table-2" ("key1" . "another string")
              ("key2" . 456)))
 (parse-toml "[table-1]
key1 = \"some string\"
key2 = 123

[table-2]
key1 = \"another string\"
key2 = 456"))


(test-equal "parse-toml: Dotted table"
 '(("dog" ("tater.man" ("type" ("name" . "pug")))))
 (parse-toml "[dog.\"tater.man\"]
type.name = \"pug\""))


(test-equal "parse-toml: Dotted table with whitespace"
 '(("a" ("b" ("c" ("x" . 1))))
   ("d" ("e" ("f" ("x" . 1))))
   ("g" ("h" ("i" ("x" . 1))))
   ("j" ("ʞ" ("l" ("x" . 1)))))
 (parse-toml "[a.b.c]            # this is best practice
x=1
[ d.e.f ]          # same as [d.e.f]
x=1
[ g . h . i ]    # same as [g.h.i]
x=1
[ j . \"ʞ\" . 'l' ]  # same as [j.\"ʞ\".'l']
x=1"))

;; XXX: technically this is not allowed, but we permit it.
(test-equal "parse-toml: Multiple tables"
 '(("fruit" ("apple" . "red") ("orange" . "orange")))
 (parse-toml "[fruit]
apple = \"red\"

[fruit]
orange = \"orange\""))

(test-equal "parse-toml: Assignment to non-table"
 #f
 (parse-toml "[fruit]
apple = \"red\"

[fruit.apple]
texture = \"smooth\""))

(test-equal "parse-toml: Dotted keys create tables"
 '(("fruit" ("apple" ("color" . "red") ("taste" ("sweet" . #t)))))
 (parse-toml "fruit.apple.color = \"red\"
fruit.apple.taste.sweet = true"))

(test-equal "parse-toml: Inline tables"
 '(("name" ("first" . "Tom") ("last" . "Preston-Werner"))
   ("point" ("x" . 1) ("y" . 2))
   ("animal" ("type" ("name" . "pug"))))
 (parse-toml "name = { first = \"Tom\", last = \"Preston-Werner\" }
point = { x = 1, y = 2 }
animal = { type.name = \"pug\" }"))

(test-error "parse-toml: Invalid assignment to inline table"
 #t
 (parse-toml "[product]
type = { name = \"Nail\" }
type.edible = false  # INVALID"))

;; We do not catch this semantic error yet.
(test-expect-fail 1)
(test-error "parse-toml: Invalid assignment to implicit table"
 #f
 (parse-toml "[product]
type.name = \"Nail\"
type = { edible = false }  # INVALID"))

;; Not implemented.
(test-expect-fail 1)
(test-equal "parse-toml: Array of tables"
 '(("products" (("name" . "Hammer") ("sku" . 738594937))
               ()
               (("name" . "Nail") ("sku" . 284758393) ("color" . "gray"))))
 (parse-toml "[[products]]
name = \"Hammer\"
sku = 738594937

[[products]]  # empty table within the array

[[products]]
name = \"Nail\"
sku = 284758393

color = \"gray\""))

;; Not implemented.
(test-expect-fail 1)
(test-equal "parse-toml: Array of tables"
 '(("fruits" ((("name" . "apple")
               ("physical" (("color" . "red") ("shape" . "round")))
               ("varieties" ((("name" . "red delicious")) (("name" . "granny smith")))))
              (("name" . "banana")
               ("varieties" (((("name" . "plantain")))))))))
 (parse-toml "[[fruits]]
name = \"apple\"

[fruits.physical]  # subtable
color = \"red\"
shape = \"round\"

[[fruits.varieties]]  # nested array of tables
name = \"red delicious\"

[[fruits.varieties]]
name = \"granny smith\"


[[fruits]]
name = \"banana\"

[[fruits.varieties]]
name = \"plantain\""))

;; Not implemented.
(test-expect-fail 1)
(test-error "parse-toml: Assignment to statically defined array"
 #f
 (parse-toml "fruits = []

[[fruits]]
x=1"))

(test-end "toml")
