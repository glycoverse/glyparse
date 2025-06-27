# GlycoCT to `glycan_structure()` graphs

A GlycoCT string has two parts: RES and LIN.
For example:

```r
glycoct <- paste0(
"RES\n",
"1b:a-dgal-HEX-1:5\n",
"2s:n-acetyl\n",
"3b:b-dgal-HEX-1:5\n",
"LIN\n",
"1:1d(2+1)2n\n",
"2:1o(3+1)3d"
)
```

RES is the residue part (contains monosaccharides and substituents).
LIN is the linkage part (contains linkages).

Monosaccharides and substituents can be tell by the second character of each line.
"b" means a monosaccharide, "s" means a substituent.
For monosaccharides, the first character after ":" is the anomeric state, which can be "a", "b", or "x".

Each line in LIN is a linkage, in the format of `\d+:<from_mono>.(<from_pos>+<to_pos>)<to_mono>.`
<from_mono> and <to_mono> are the indices of the monosaccharides or substituents in RES.
<from_pos> and <to_pos> are the positions of the monosaccharides or substituents in <from_mono> and <to_mono>. For example, "2:1o(3+1)3d" means the linkage is from the first RES ("1o") to the third RES ("3d"). Specifically, from position 3 of the first RES to position 1 of the third RES ("3+1").
To write the linkage out, we need to take the anomeric state of the <to_mono> and write it as:
`<to_mono_anomeric_state><to_pos>-<from_pos>`.
For example, "2:1o(3+1)3d" becomes "b1-3" ("b" is from the third RES).

Some combinations of monosaccharides and substituents are regarded as a whole, with a name.
See "glyparse/mono_glycoct.txt" for the list of monosaccharides and substituents that are regarded as a whole.