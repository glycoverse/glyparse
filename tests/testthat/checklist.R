# This file is a checklist for testing a parse.
#
# To thoroughly test a parser, the following should be tested:
#
# - Check if monosaccharide names are parsed correctly. Pay attention to
#   special cases like "Neu5Ac", "Neu5Gc", "MurNGc", "Api", "LDmanHep",
#   and "DDmanHep". Also make sure some monosaccharides are not parsed
#   as substituents, e.g. "GlcNAc" should not be parsed as "Glc" and "2NAc".
#
# - Check if generic monosaccharide names are parsed correctly, e.g. "Hex".
#
# - Check if substituents are parsed correctly. This includes substituent
#   names and positions. Positions can be integers or "?".
#
# - Check if anomers are parsed correctly. Anomers can be "a", "b", or "?".
#
# - Check if linkages are parsed correctly. Make sure unknown parts are
#   handled correctly, e.g. "?1-4", "a1-?", "?1-?".
#
# - Check if the parser can handle glycans with only one monosaccaride.
#
# - For format using general representation of monosaccharides,
#   e.g. GlycoCT, WURCS, etc., it is crucial to test all known monosaccharides.
#
# - Check if the parser can handle special cases where a monosaccharide name
#   is split by a substituent, e.g. "Neu4Ac5Ac". For IUPAC series, this might
#   not be an issue, because "Neup5Ac4Ac" is normally the correct way to
#   arrange the substituents.
#
# - It is also advised to parse some complex glycans.
#
# - Do not use `expect_snapshot()`, use IUPAC-condensed format directly.
