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
      "/var/lib/OpenRGB"
      "/var/lib/alsa"
      "/var/lib/docker"
      "/var/lib/libvirt"
      "/var/lib/nixos" # contains important state: https://github.com/nix-community/impermanence/issues/178
      "/var/lib/swtpm-localca" # needed for Windows VM via qemu
      "/var/lib/systemd"
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
        ".cache/gstreamer-1.0"
        ".cache/helix"
        ".cache/helm"
        ".cache/maestral"
        ".cache/mesa_shader_cache"
        ".cache/mesa_shader_cache_db"
        ".cache/mozilla"
        ".cache/nix"
        ".cache/org-persist"
        ".cache/remmina"
        ".cache/rust-script"
        ".cache/tealdeer"
        ".cache/thumbnails"
        ".cache/virt-manager"
        ".cargo"
        ".config/JetBrains"
        ".config/Mullvad VPN"
        ".config/OpenRGB"
        ".config/Signal"
        ".config/Slack"
        ".config/argocd"
        ".config/dconf"
        ".config/discord"
        ".config/emacs.default/.cache"
        ".config/emacs.default/backups"
        ".config/emacs.default/chatgpt"
        ".config/emacs.default/eln-cache"
        ".config/emacs.default/transient"
        ".config/emacs.default/tree-sitter"
        ".config/emacs.default/var"
        ".config/enchant"
        ".config/fish"
        ".config/github-copilot"
        ".config/gopass/config"
        ".config/helm"
        ".config/k9s"
        ".config/libreoffice"
        ".config/maestral"
        ".config/obs-studio"
        ".config/omni"
        ".config/pulse"
        ".config/remmina"
        ".config/syncthing"
        ".config/vlc"
        ".dotnet"
        ".java"
        ".krew"
        ".local/share/JetBrains"
        ".local/share/NuGet"
        ".local/share/direnv"
        ".local/share/fish"
        ".local/share/k9s"
        ".local/share/keyrings"
        ".local/share/maestral"
        ".local/share/omni"
        ".local/share/remmina"
        ".local/share/talos"
        ".local/share/wezterm"
        ".local/share/zoxide"
        ".local/state/k9s"
        ".local/state/wireplumber"
        ".mozilla" # firefox
        ".nuget"
        ".talos"
        "code"
        "downloads"
        "dropbox"
        "images"
        "public"
        "sync"
        "videos"
        "vms"
        {
          directory = ".SeedVaultAndroidBackup";
          mode = "0700";
        }
        {
          directory = ".gnupg";
          mode = "0700";
        }
        {
          directory = ".kube";
          mode = "0700";
        }
        {
          directory = ".password-store";
          mode = "0700";
        }
        {
          directory = ".pki";
          mode = "0700";
        }
        {
          directory = ".secrets";
          mode = "0700";
        }
        {
          directory = ".ssh";
          mode = "0700";
        }
        {
          directory = "keepass";
          mode = "0700";
        }
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
