#!/usr/bin/env bash

set -e

[[ $EUID -ne 0 ]] && {
	echo "Please run as root"
	exit 1
}

echo "Enter new hostname:"
read hostname
FILENAME="./hosts/$hostname.nix"
IMPORTS=(
	../profiles/common/host-settings.nix
	../profiles/efi.nix
	../modules/unfree.nix
	../modules/secrets.nix
	../users/will
)

# Append host name to flake.nix hosts string list.
sed -i "/hosts = \[/a\        \"$hostname\"" ./flake.nix
cp /etc/nixos/hardware-configuration.nix $FILENAME
chown $(logname):users $FILENAME

# strip comments
sed -i '/^ *#/d;s/#.*//' $FILENAME

# comment out lines that are not needed for new hosts
# because these settings are imported from common settings.
sed -i '/nixpkgs.hostPlatform/s/^/#/' $FILENAME
sed -i '/networking.useDHCP/s/^/#/' $FILENAME

# fix the import for flakes.
sed -i 's|\(modulesPath + "/\)\(.*\.nix"\)|"${modulesPath}/\2|' ./hosts/"$hostname".nix

# Append imports
for i in "${IMPORTS[@]}"; do
	sed -i "/.nix\"/a\       $i" $FILENAME
done

# Append before availableKernelModules: secret settings and crypto_keyfile.bin
sed -i '/boot.initrd.availableKernelModules/i\  modules.secrets.enable = false;' $FILENAME
sed -i '/boot.initrd.availableKernelModules/i\  boot.initrd.secrets."/crypto_keyfile.bin" = null;' $FILENAME
