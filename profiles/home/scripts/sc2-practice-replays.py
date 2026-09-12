"""Print paths of SC2 replays with no human opponent (vs AI, solo maps)."""
import sys
from pathlib import Path

import sc2reader

for f in sorted(Path(sys.argv[1]).glob("*.SC2Replay")):
    if len(sc2reader.load_replay(str(f), load_level=2).humans) < 2:
        print(f)
