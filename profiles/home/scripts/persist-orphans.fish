set prune
for p in (findmnt -ln -o FSROOT | string match '/persist/*')
    set -a prune -path /nix$p -prune -o
end

find /nix/persist -xdev $prune -type f -print
