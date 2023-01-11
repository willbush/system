#!/usr/bin/env bash
# A script to get convert opensnitch json rules into a nix format that gets most
# of the way to services.opensnitch.rules format.
# see https://search.nixos.org/options?channel=unstable&show=services.opensnitch.rules&from=0&size=50&sort=relevance&type=packages&query=opensnitch

set -e

echo '{' >> rules.nix

for file in /var/lib/opensnitch/rules/*.json; do
	if [ -L "$file" ]; then
		echo "\"$file\" = " >> rules.nix

		nix eval --impure --expr "
        let rules = builtins.fromJSON (builtins.readFile $file);
        in builtins.removeAttrs rules [ \"created\" \"updated\" \"precedence\" ]
      " >> rules.nix

		echo ';' >> rules.nix
	fi
done

echo '}' >> rules.nix

nixfmt ./rules.nix
