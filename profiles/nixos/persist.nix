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
        ".cache/bat"
        ".cache/dconf"
        ".cache/fontconfig"
        ".cache/gstreamer-1.0"
        ".cache/helix"
        ".cache/mesa_shader_cache"
        ".cache/mesa_shader_cache_db"
        ".cache/mozilla"
        ".cache/nix"
        ".cache/nix-output-monitor"
        ".cache/org-persist"
        ".cache/rust-script"
        ".cache/tealdeer"
        ".cache/thumbnails"
        ".cache/virt-manager"
        ".cargo"
        ".config/Mullvad VPN"
        ".config/OpenRGB"
        ".config/Signal"
        ".config/dconf"
        ".config/emacs.default/.cache"
        ".config/emacs.default/backups"
        ".config/emacs.default/eln-cache"
        ".config/emacs.default/transient"
        ".config/emacs.default/tree-sitter"
        ".config/emacs.default/var"
        ".config/enchant"
        ".config/fish"
        ".config/gopass/config"
        ".config/libreoffice"
        ".config/obs-studio"
        ".config/pulse"
        ".config/syncthing"
        ".config/vesktop"
        ".config/vlc"
        ".java"
        ".local/share/direnv"
        ".local/share/fish"
        ".local/share/keyrings"
        ".local/share/wezterm"
        ".local/share/zoxide"
        ".local/state/wireplumber"
        ".mozilla" # firefox
        "code"
        "downloads"
        "images"
        "public"
        "sync"
        "videos"
        "vms"
        {
          directory = ".aws";
          mode = "0700";
        }
        {
          directory = ".SeedVaultAndroidBackup";
          mode = "0700";
        }
        {
          directory = ".gnupg";
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
