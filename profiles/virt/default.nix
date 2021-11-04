{ pkgs, ... }: {
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.runAsRoot = false;
    };

    docker.enable = true;
  };

  # virt-manager has a dependency on dconf.
  # see: https://nixos.wiki/wiki/Virt-manager
  # https://github.com/NixOS/nixpkgs/issues/52777
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    virt-manager # Add 'libvirtd' your user's extraGroups
    docker # Add 'docker' your user's extraGroups
    docker-compose
  ];
}
