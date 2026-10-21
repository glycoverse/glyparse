"""Download the pinned public GlyGen release and verify every CSV checksum."""
import hashlib
import http.client
import json
from pathlib import Path
import sys
from urllib.parse import urlsplit

output = Path(sys.argv[1])
output.mkdir(parents=True, exist_ok=True)
manifest = json.loads(Path(__file__).with_name("sources.json").read_text())
for entry in manifest:
    path = output / ("glycan_sequences_" + entry["format"] + ".csv")
    url = urlsplit(entry["url"])
    if url.scheme != "https" or url.netloc != "data.glygen.org":
        raise ValueError("Expected an HTTPS URL on data.glygen.org")
    connection = http.client.HTTPSConnection(url.netloc, timeout=120)
    try:
        connection.request("GET", url.path + ("?" + url.query if url.query else ""))
        response = connection.getresponse()
        if response.status != 200:
            raise RuntimeError("Download failed: HTTP " + str(response.status))
        content = response.read()
    finally:
        connection.close()
    if hashlib.sha256(content).hexdigest() != entry["sha256"]:
        raise RuntimeError("Checksum mismatch: " + entry["format"])
    path.write_bytes(content)
    print(entry["format"], len(content), flush=True)
