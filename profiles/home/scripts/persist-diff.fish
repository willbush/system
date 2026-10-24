set excludes \
    /etc/.clean \
    /etc/.updated \
    /etc/NIXOS \
    /etc/group \
    /etc/logrotate.conf \
    /etc/passwd \
    /etc/printcap \
    /etc/shadow \
    /etc/subgid \
    /etc/subuid \
    /etc/sudoers \
    /etc/wpa_supplicant/imperative.conf \
    /home/will/.cache/claude-cli-nodejs \
    /home/will/.cache/mpv \
    /home/will/.config/emacs \
    /home/will/.local/share/applications/claude-code-url-handler.desktop \
    /home/will/.local/state/lesshst \
    /root \
    /tmp \
    /var/.updated \
    /var/cache/cups \
    /var/lib/NetworkManager \
    /var/lib/cups

set prune
for p in $excludes
    set -a prune -path $p -prune -o
end

set -q argv[1]; or set argv /
find (path resolve -- $argv) -xdev $prune -fstype tmpfs -type f -print
