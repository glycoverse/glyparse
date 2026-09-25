import json
import sys
from pathlib import Path
d=json.load(open(sys.argv[1]))
def q(x):return json.dumps(x,ensure_ascii=True)
def vec(x):
 if not isinstance(x,list): x=[x]
 return '{'+','.join(q(v) for v in x)+'}'
s=['// Format dictionaries frozen from the parity-validated R reference.','#ifndef GLYPARSE_TABLES_H','#define GLYPARSE_TABLES_H','#include <string>','#include <utility>','#include <vector>','namespace gp {','using Pairs = std::vector<std::pair<std::string,std::string>>;']
for k,v in d.items():
 if k.startswith('glycoct'):continue
 s.append('inline const Pairs '+k+'_table = {'+','.join('{'+q(a)+','+q(b)+'}' for a,b in v)+'};')
s.append('struct CTMapping { std::string name, core, bounds; bool alditol; std::vector<std::string> subs; };')
for k in ['glycoct_entries','glycoct_alditol_entries']:
 rows=[]
 for v in d[k]:rows.append('{'+','.join([q(v['name']),q(v['core']),q(v['bounds'] if isinstance(v['bounds'],str) else ''),str(v['alditol']).lower(),vec(v['substituents'])])+'}')
 s.append('inline const std::vector<CTMapping> '+k+' = {'+','.join(rows)+'};')
s+=['}','#endif'];Path(sys.argv[2]).write_text('\n'.join(s)+'\n')
