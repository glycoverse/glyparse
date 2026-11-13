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

`glygen-smiles-wurcs.csv` is a 368-row subset joined by GlyTouCan accession,
including supported monosaccharides, a seeded sample of multi-residue glycans,
non-reducing glycans, and the special cases below. The SMILES, WURCS, accession,
and PubChem ID values are copied unchanged.

`glygen-smiles-equivalent.csv` contains 99 source SMILES and alternative atom
traversals generated for testing with RDKit 2026.03.6
(`MolToSmiles(..., canonical=False, doRandom=True, isomericSmiles=True)`).
The `equivalent` column is a transformation of the source data. RDKit is **not**
a runtime or test-time dependency: only the resulting strings are stored.

## Source ambiguities and scope regressions

- G20934UH and G71958ED: SMILES have no tetrahedral tags, but paired WURCS
  specify beta and alpha respectively. The SMILES parser retains unknown
  stereochemistry rather than borrowing information from the paired record.
- G89097YO: a bridge joins two non-anomeric carbon positions. It must be
  rejected, not relabeled as a conventional glycosidic bond.
- Non-reducing anomeric-to-anomeric bridges have no chemically distinguished
  reducing root. Tests compare labeled residue graphs with both linkage
  endpoint positions and each residue's anomer retained, independently of
  the conventional root.

Use `tools/validate-smiles-glygen.R` from the source checkout to repeat the
full paired-dataset comparison. Input data are not queried at package runtime.
