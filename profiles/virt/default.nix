{ pkgs, ... }: {
  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.runAsRoot = false;
    };

    docker.enable = true;
  };

  programs.virt-manager.enable = true;

  environment.systemPackages = with pkgs; [
    docker # Add 'docker' your user's extraGroups
    docker-compose
  ];
}
