# GlyGen SMILES fixtures

Source: GlyGen data portal, release **2.12.1**, downloaded 2026-11-13.

- [GLY_000291: Glycan Sequences SMILES Isomeric](https://data.glygen.org/GLY_000291)
- [SMILES CSV](https://data.glygen.org/ln2releases/data/v-2.12.1/reviewed/glycan_sequences_smiles_isomeric.csv)
- [GLY_000292: Glycan Sequences WURCS](https://data.glygen.org/GLY_000292)
- [WURCS CSV](https://data.glygen.org/ln2releases/data/v-2.12.1/reviewed/glycan_sequences_wurcs.csv)

Attribution: GlyGen and its source providers, including PubChem and GlyTouCan.
The GlyGen dataset metadata declares **CC BY 4.0** for data:
<https://creativecommons.org/licenses/by/4.0/>. These data fixtures retain
that license; the surrounding parser and tests are package code.

Source file SHA-256:

- SMILES: `d5322620e7c0b1b4ebbd5ed6b0364ebea67c12326eb6ee792e841c37d32ef3ab`
- WURCS: `084f1efa9bb1623629250e7c9c07f045505c0d1ccaed0ac7c4bb8e42852933b3`

## Changes made to the source data

`glygen-smiles-wurcs.csv` is a 543-row subset joined by GlyTouCan accession,
including supported monosaccharides, a seeded sample of multi-residue glycans,
non-reducing glycans, reducing-end alditols (including terminal-deoxy forms),
methyl and carboxyl derivatives, and the special cases below. The SMILES, WURCS, accession,
and PubChem ID values are copied unchanged.

`glygen-smiles-equivalent.csv` contains 230 source SMILES and alternative atom
traversals generated for testing with RDKit 2026.03.6
(`MolToSmiles(..., canonical=False, doRandom=True, isomericSmiles=True)`).
The `equivalent` column is a transformation of the source data. RDKit is **not**
a runtime or test-time dependency: only the resulting strings are stored.

## Source ambiguities and scope regressions

- G20934UH and G71958ED: SMILES have no tetrahedral tags, but paired WURCS
  specify beta and alpha respectively. The SMILES parser retains unknown
  stereochemistry rather than borrowing information from the paired record.
- G27289CV: a non-root alditol cannot be represented without losing its
  reduced state in the current glyrepr model. The SMILES parser rejects it;
  the existing WURCS parser warns and normalizes it to a regular residue.
- G89097YO: a bridge joins two non-anomeric carbon positions. It must be
  rejected, not relabeled as a conventional glycosidic bond.
- Non-reducing anomeric-to-anomeric bridges have no chemically distinguished
  reducing root. Tests compare labeled residue graphs with both linkage
  endpoint positions and each residue's anomer retained, independently of
  the conventional root.

Use `tools/validate-smiles-glygen.R` from the source checkout to repeat the
full paired-dataset comparison. Input data are not queried at package runtime.

## Additional stereochemical and symmetry regression tests

The WURCS forms of G20934UH and G71958ED explicitly carry `-1b` and `-1a`.
The [WURCS specification](https://www.wurcs-wg.org/doc/WURCS2.html) permits
known relative anomeric information alongside unknown absolute backbone
configuration. Replacing the explicit anomer with `x` produces an unknown
result in `parse_wurcs()`. The discrepancy is information absent from the
SMILES representation, not an inferred anomer in the WURCS parser.

Mannitol (`h1122h`) and a completely unknown hexitol (`hxxxxh`) can be
numbered from either end without changing the backbone. The comparison
helper permits reflection of **all** root attachment positions together
for these unmodified roots, and retains alditol status in graph identity.

`smiles-symmetric-alditols.csv` is synthetic regression data, not a GlyGen
extract. Its 162 strings describe two intentionally symmetric hexitol
backbones with (1) different Glc/Gal side chains or (2) methyl/acetyl ends.
The equivalent atom traversals were generated with the same development-only
RDKit installation. They exercise deterministic numbering when local backbone
and attachment-position scores tie, requiring the complete group identities.
