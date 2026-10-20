"""Download the pinned public GlyGen release and verify every CSV checksum."""
import hashlib
import json
from pathlib import Path
import sys
import urllib.request

output = Path(sys.argv[1])
output.mkdir(parents=True, exist_ok=True)
manifest = json.loads(Path(__file__).with_name("sources.json").read_text())
for entry in manifest:
    path = output / ("glycan_sequences_" + entry["format"] + ".csv")
    with urllib.request.urlopen(entry["url"], timeout=120) as response:
        content = response.read()
    if hashlib.sha256(content).hexdigest() != entry["sha256"]:
        raise RuntimeError("Checksum mismatch: " + entry["format"])
    path.write_bytes(content)
    print(entry["format"], len(content), flush=True)
