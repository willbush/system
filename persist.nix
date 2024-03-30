# https://github.com/nix-community/impermanence#module-usage
{
  environment.persistence."/nix/persist" = {
    hideMounts = true;
    directories = [
      "/etc/NetworkManager/system-connections"
      "/etc/mullvad-vpn"
      "/var/cache/libvirt"
      "/var/cache/mullvad-vpn"
      "/var/cache/tuigreet"
      "/var/lib/docker/"
      "/var/lib/libvirt"
      "/var/lib/OpenRGB/"
      "/var/lib/systemd"
      "/var/lib/swtpm-localca/" # needed for Windows VM via qemu
      "/var/log"
    ];
    files = [
      # machine-id is used by systemd for the journal, if you don't persist this
      # file you won't be able to easily use journalctl to look at journals for
      # previous boots.
      "/etc/machine-id"
      "/var/lib/logrotate.status"
    ];
    users.will = {
      directories = [
        ".cache/JetBrains"
        ".cache/bat"
        ".cache/dconf"
        ".cache/fontconfig"
        ".cache/maestral"
        ".cache/mesa_shader_cache"
        ".cache/mozilla"
        ".cache/nix"
        ".cache/oh-my-zsh"
        ".cache/org-persist"
        ".cache/remmina"
        ".cache/tealdeer"
        ".cache/virt-manager"
        ".cargo"
        ".config/JetBrains"
        ".config/Mullvad VPN"
        ".config/Signal"
        ".config/Slack"
        ".config/dconf"
        ".config/discord"
        ".config/emacs.default/.cache"
        ".config/emacs.default/backups"
        ".config/emacs.default/chatgpt"
        ".config/emacs.default/eln-cache"
        ".config/emacs.default/transient"
        ".config/emacs.default/tree-sitter"
        ".config/emacs.default/var"
        ".config/github-copilot"
        ".config/gopass/config"
        ".config/libreoffice"
        ".config/maestral"
        ".config/pulse"
        ".config/remmina"
        ".config/syncthing"
        ".config/zsh"
        ".dotnet"
        ".java"
        ".local/share/JetBrains"
        ".local/share/NuGet"
        ".local/share/Trash"
        ".local/share/direnv"
        ".local/share/keyrings"
        ".local/share/maestral"
        ".local/share/remmina"
        ".local/share/zoxide"
        ".local/share/zsh"
        ".local/state/wireplumber"
        ".minikube"
        ".mozilla" # firefox
        ".nuget"
        "code"
        "downloads"
        "dropbox"
        "images"
        "sync"
        { directory = ".SeedVaultAndroidBackup"; mode = "0700"; }
        { directory = ".config/gh"; mode = "0700"; }
        { directory = ".gnupg"; mode = "0700"; }
        { directory = ".password-store"; mode = "0700"; }
        { directory = ".pki"; mode = "0700"; }
        { directory = ".secrets"; mode = "0700"; }
        { directory = ".ssh"; mode = "0700"; }
        { directory = "keepass"; mode = "0700"; }
      ];
      files = [
        ".bash_history"
        ".cache/wofi-drun"
        ".config/emacs.default/.org-id-locations" # org-roam
        ".config/emacs.default/bookmarks"
        ".config/emacs.default/history"
        ".config/emacs.default/org-roam.db"
        ".config/emacs.default/projects"
        ".config/emacs.default/recentf"
        ".config/ncpamixer.conf"
        ".local/share/nix/repl-history"
      ];
    };
  };
}
