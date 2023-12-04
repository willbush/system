{ pkgs, ... }: {
  virtualisation = {
    libvirtd.enable = true;
    docker.enable = true;
  };

  programs.virt-manager.enable = true;

  environment.systemPackages = with pkgs; [
    docker # Add 'docker' your user's extraGroups
    docker-compose
  ];
}
